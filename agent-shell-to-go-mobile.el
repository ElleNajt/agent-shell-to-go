;;; agent-shell-to-go-mobile.el --- Mobile app backend integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Elle Najt

;; Author: Elle Najt
;; URL: https://github.com/ElleNajt/agent-shell-to-go
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.33.1"))
;; Keywords: convenience, tools, ai

;;; Commentary:

;; This module sends agent-shell events to the mobile backend server,
;; enabling the React Native mobile app to display and interact with agents.
;;
;; Quick start:
;;    (use-package agent-shell-to-go-mobile
;;      :after agent-shell
;;      :config
;;      (setq agent-shell-to-go-mobile-backend-url "http://100.x.x.x:8080")
;;      (setq agent-shell-to-go-mobile-token "your-secret-token")
;;      (agent-shell-to-go-mobile-setup))

;;; Code:

(require 'json)
(require 'url)

(defgroup agent-shell-to-go-mobile nil
  "Mobile app backend integration for agent-shell."
  :group 'agent-shell
  :prefix "agent-shell-to-go-mobile-")

(defcustom agent-shell-to-go-mobile-backend-url nil
  "URL of the mobile backend server (e.g., http://100.x.x.x:8080).
Should be a Tailscale IP for security."
  :type 'string
  :group 'agent-shell-to-go-mobile)

(defcustom agent-shell-to-go-mobile-token nil
  "Bearer token for authenticating with the mobile backend.
Can also be set via AGENT_SHELL_MOBILE_TOKEN environment variable."
  :type 'string
  :group 'agent-shell-to-go-mobile)

(defcustom agent-shell-to-go-mobile-token-file nil
  "Path to file containing the mobile backend token.
Alternative to setting `agent-shell-to-go-mobile-token' directly."
  :type 'string
  :group 'agent-shell-to-go-mobile)

(defcustom agent-shell-to-go-mobile-debug nil
  "When non-nil, log debug messages to *Messages*."
  :type 'boolean
  :group 'agent-shell-to-go-mobile)

(defcustom agent-shell-to-go-mobile-async t
  "When non-nil, send events asynchronously (non-blocking)."
  :type 'boolean
  :group 'agent-shell-to-go-mobile)

;;; Internal state

(defvar agent-shell-to-go-mobile--active-buffers nil
  "List of agent-shell buffers with active mobile backend integration.")

(defvar-local agent-shell-to-go-mobile--session-id nil
  "Unique session ID for this buffer.")

(defvar-local agent-shell-to-go-mobile--current-agent-message nil
  "Accumulator for streaming agent message chunks.")

;;; Utility functions

(defun agent-shell-to-go-mobile--debug (format-string &rest args)
  "Log a debug message if `agent-shell-to-go-mobile-debug' is non-nil."
  (when agent-shell-to-go-mobile-debug
    (apply #'message (concat "agent-shell-to-go-mobile: " format-string) args)))

(defun agent-shell-to-go-mobile--load-token ()
  "Load the auth token from various sources."
  (or agent-shell-to-go-mobile-token
      (and agent-shell-to-go-mobile-token-file
           (file-exists-p agent-shell-to-go-mobile-token-file)
           (with-temp-buffer
             (insert-file-contents agent-shell-to-go-mobile-token-file)
             (string-trim (buffer-string))))
      (getenv "AGENT_SHELL_MOBILE_TOKEN")))

(defun agent-shell-to-go-mobile--get-project-path ()
  "Get the project path for the current buffer."
  (or (and (fboundp 'projectile-project-root) (projectile-project-root))
      (and (fboundp 'project-current)
           (when-let ((proj (project-current)))
             (if (fboundp 'project-root)
                 (project-root proj)
               (car (project-roots proj)))))
      default-directory))

(defun agent-shell-to-go-mobile--get-project-name ()
  "Get the project name for the current buffer."
  (file-name-nondirectory
   (directory-file-name (agent-shell-to-go-mobile--get-project-path))))

(defun agent-shell-to-go-mobile--generate-session-id ()
  "Generate a unique session ID."
  (format "%s-%s" (buffer-name) (format-time-string "%Y%m%d%H%M%S")))

(defun agent-shell-to-go-mobile--iso-timestamp ()
  "Return current time as ISO 8601 timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))

;;; HTTP API

(defun agent-shell-to-go-mobile--post (endpoint data)
  "POST DATA to ENDPOINT on the mobile backend.
ENDPOINT is the path (e.g., \"/events/message\").
DATA is an alist that will be JSON-encoded."
  (let ((token (agent-shell-to-go-mobile--load-token)))
    (unless agent-shell-to-go-mobile-backend-url
      (agent-shell-to-go-mobile--debug "backend URL not configured")
      (cl-return-from agent-shell-to-go-mobile--post nil))
    (unless token
      (agent-shell-to-go-mobile--debug "token not configured")
      (cl-return-from agent-shell-to-go-mobile--post nil))
    (let* ((url (concat agent-shell-to-go-mobile-backend-url endpoint))
           (json-data (encode-coding-string (json-encode data) 'utf-8)))
      (if agent-shell-to-go-mobile-async
          ;; Async: fire and forget with curl
          (let ((proc (start-process
                       "agent-shell-mobile-post"
                       nil  ; no buffer
                       "curl" "-s" "-X" "POST"
                       "-H" (format "Authorization: Bearer %s" token)
                       "-H" "Content-Type: application/json"
                       "-d" json-data
                       url)))
            (set-process-query-on-exit-flag proc nil)
            (agent-shell-to-go-mobile--debug "async POST to %s" endpoint))
        ;; Sync: blocking call
        (with-temp-buffer
          (call-process "curl" nil t nil
                        "-s" "-X" "POST"
                        "-H" (format "Authorization: Bearer %s" token)
                        "-H" "Content-Type: application/json"
                        "-d" json-data
                        url)
          (agent-shell-to-go-mobile--debug "sync POST to %s: %s" endpoint (buffer-string)))))))

;;; Event sending

(defun agent-shell-to-go-mobile--send-spawn ()
  "Send agent-spawn event for the current buffer."
  (agent-shell-to-go-mobile--post
   "/events/agent-spawn"
   `((session_id . ,agent-shell-to-go-mobile--session-id)
     (buffer_name . ,(buffer-name))
     (project . ,(agent-shell-to-go-mobile--get-project-name))
     (parent_session_id . nil)
     (timestamp . ,(agent-shell-to-go-mobile--iso-timestamp)))))

(defun agent-shell-to-go-mobile--send-close ()
  "Send agent-close event for the current buffer."
  (when agent-shell-to-go-mobile--session-id
    (agent-shell-to-go-mobile--post
     "/events/agent-close"
     `((session_id . ,agent-shell-to-go-mobile--session-id)
       (timestamp . ,(agent-shell-to-go-mobile--iso-timestamp))))))

(defun agent-shell-to-go-mobile--send-message (role content)
  "Send message event with ROLE and CONTENT."
  (when (and agent-shell-to-go-mobile--session-id
             content
             (> (length content) 0))
    (agent-shell-to-go-mobile--post
     "/events/message"
     `((session_id . ,agent-shell-to-go-mobile--session-id)
       (role . ,role)
       (content . ,content)
       (timestamp . ,(agent-shell-to-go-mobile--iso-timestamp))))))

(defun agent-shell-to-go-mobile--send-status (status &optional detail)
  "Send status event with STATUS and optional DETAIL."
  (when agent-shell-to-go-mobile--session-id
    (agent-shell-to-go-mobile--post
     "/events/status"
     `((session_id . ,agent-shell-to-go-mobile--session-id)
       (status . ,status)
       (detail . ,(or detail ""))
       (timestamp . ,(agent-shell-to-go-mobile--iso-timestamp))))))

;;; Advice functions

(defun agent-shell-to-go-mobile--on-send-command (orig-fn &rest args)
  "Advice for agent-shell--send-command. Send user message to mobile backend.
ORIG-FN is the original function, ARGS are its arguments."
  (when (and agent-shell-to-go-mobile-mode
             agent-shell-to-go-mobile--session-id)
    (let ((prompt (plist-get args :prompt)))
      (when prompt
        (agent-shell-to-go-mobile--send-message "user" prompt)
        (agent-shell-to-go-mobile--send-status "processing")))
    (setq agent-shell-to-go-mobile--current-agent-message nil))
  (apply orig-fn args))

(defun agent-shell-to-go-mobile--on-notification (orig-fn &rest args)
  "Advice for agent-shell--on-notification. Send events to mobile backend.
ORIG-FN is the original function, ARGS are its arguments."
  (let* ((state (plist-get args :state))
         (buffer (alist-get :buffer state)))
    (when (and buffer
               (buffer-live-p buffer)
               (buffer-local-value 'agent-shell-to-go-mobile-mode buffer))
      (let* ((notification (plist-get args :notification))
             (params (alist-get 'params notification))
             (update (alist-get 'update params))
             (update-type (alist-get 'sessionUpdate update)))
        (pcase update-type
          ("agent_message_chunk"
           (let ((text (alist-get 'text (alist-get 'content update))))
             (with-current-buffer buffer
               (setq agent-shell-to-go-mobile--current-agent-message
                     (concat agent-shell-to-go-mobile--current-agent-message text)))))
          ("tool_call"
           ;; Flush agent message before tool call
           (with-current-buffer buffer
             (when (and agent-shell-to-go-mobile--current-agent-message
                        (> (length agent-shell-to-go-mobile--current-agent-message) 0))
               (agent-shell-to-go-mobile--send-message
                "agent" agent-shell-to-go-mobile--current-agent-message)
               (setq agent-shell-to-go-mobile--current-agent-message nil)))
           ;; Send tool call as tool message
           (let* ((title (alist-get 'title update))
                  (raw-input (alist-get 'rawInput update))
                  (command (alist-get 'command raw-input))
                  (display (or command title "Tool call")))
             (with-current-buffer buffer
               (agent-shell-to-go-mobile--send-message
                "tool" (format "[RUNNING] %s" display)))))
          ("tool_call_update"
           (let* ((status (alist-get 'status update))
                  (output (or (alist-get 'rawOutput update)
                              (alist-get 'output update))))
             (when (member status '("completed" "failed"))
               (with-current-buffer buffer
                 (agent-shell-to-go-mobile--send-message
                  "tool"
                  (format "[%s]%s"
                          (upcase status)
                          (if (and output (> (length output) 0))
                              (format " %s" (truncate-string-to-width output 500))
                            "")))))))))))
  (apply orig-fn args))

(defun agent-shell-to-go-mobile--on-heartbeat-stop (orig-fn &rest args)
  "Advice for agent-shell-heartbeat-stop. Flush message and update status.
ORIG-FN is the original function, ARGS are its arguments."
  (when (and agent-shell-to-go-mobile-mode
             agent-shell-to-go-mobile--session-id)
    ;; Flush pending agent message
    (when (and agent-shell-to-go-mobile--current-agent-message
               (> (length agent-shell-to-go-mobile--current-agent-message) 0))
      (agent-shell-to-go-mobile--send-message
       "agent" agent-shell-to-go-mobile--current-agent-message)
      (setq agent-shell-to-go-mobile--current-agent-message nil))
    ;; Update status to ready
    (agent-shell-to-go-mobile--send-status "ready"))
  (apply orig-fn args))

(defun agent-shell-to-go-mobile--on-request (orig-fn &rest args)
  "Advice for agent-shell--on-request. Send permission_required status.
ORIG-FN is the original function, ARGS are its arguments."
  (let* ((state (plist-get args :state))
         (request (plist-get args :request))
         (method (alist-get 'method request))
         (buffer (and state (alist-get :buffer state))))
    (when (and buffer
               (buffer-live-p buffer)
               (buffer-local-value 'agent-shell-to-go-mobile-mode buffer)
               (equal method "session/request_permission"))
      (with-current-buffer buffer
        (let* ((params (alist-get 'params request))
               (tool-call (alist-get 'toolCall params))
               (title (alist-get 'title tool-call))
               (raw-input (alist-get 'rawInput tool-call))
               (command (alist-get 'command raw-input)))
          (agent-shell-to-go-mobile--send-status
           "permission_required"
           (or command title "Permission required"))))))
  (apply orig-fn args))

;;; Minor mode

(defun agent-shell-to-go-mobile--enable ()
  "Enable mobile backend integration for this buffer."
  (unless agent-shell-to-go-mobile-backend-url
    (agent-shell-to-go-mobile--debug "backend URL not set, skipping")
    (cl-return-from agent-shell-to-go-mobile--enable nil))
  (unless (agent-shell-to-go-mobile--load-token)
    (agent-shell-to-go-mobile--debug "token not set, skipping")
    (cl-return-from agent-shell-to-go-mobile--enable nil))
  
  ;; Generate session ID
  (setq agent-shell-to-go-mobile--session-id
        (agent-shell-to-go-mobile--generate-session-id))
  
  ;; Track buffer
  (add-to-list 'agent-shell-to-go-mobile--active-buffers (current-buffer))
  
  ;; Add advice
  (advice-add 'agent-shell--send-command :around #'agent-shell-to-go-mobile--on-send-command)
  (advice-add 'agent-shell--on-notification :around #'agent-shell-to-go-mobile--on-notification)
  (advice-add 'agent-shell--on-request :around #'agent-shell-to-go-mobile--on-request)
  (advice-add 'agent-shell-heartbeat-stop :around #'agent-shell-to-go-mobile--on-heartbeat-stop)
  
  ;; Add kill-buffer hook
  (add-hook 'kill-buffer-hook #'agent-shell-to-go-mobile--on-buffer-kill nil t)
  
  ;; Send spawn event
  (agent-shell-to-go-mobile--send-spawn)
  
  (agent-shell-to-go-mobile--debug "enabled for %s (session: %s)"
                                   (buffer-name)
                                   agent-shell-to-go-mobile--session-id))

(defun agent-shell-to-go-mobile--on-buffer-kill ()
  "Hook to run when an agent-shell buffer is killed."
  (when agent-shell-to-go-mobile-mode
    (agent-shell-to-go-mobile--disable)))

(defun agent-shell-to-go-mobile--disable ()
  "Disable mobile backend integration for this buffer."
  (remove-hook 'kill-buffer-hook #'agent-shell-to-go-mobile--on-buffer-kill t)
  
  ;; Send close event
  (agent-shell-to-go-mobile--send-close)
  
  ;; Untrack buffer
  (setq agent-shell-to-go-mobile--active-buffers
        (delete (current-buffer) agent-shell-to-go-mobile--active-buffers))
  
  ;; Remove advice if no more buffers
  (unless agent-shell-to-go-mobile--active-buffers
    (advice-remove 'agent-shell--send-command #'agent-shell-to-go-mobile--on-send-command)
    (advice-remove 'agent-shell--on-notification #'agent-shell-to-go-mobile--on-notification)
    (advice-remove 'agent-shell--on-request #'agent-shell-to-go-mobile--on-request)
    (advice-remove 'agent-shell-heartbeat-stop #'agent-shell-to-go-mobile--on-heartbeat-stop))
  
  (agent-shell-to-go-mobile--debug "disabled"))

;;;###autoload
(define-minor-mode agent-shell-to-go-mobile-mode
  "Send agent-shell events to mobile backend.
View and interact with agents from the mobile app."
  :lighter " Mobile"
  :group 'agent-shell-to-go-mobile
  (if agent-shell-to-go-mobile-mode
      (agent-shell-to-go-mobile--enable)
    (agent-shell-to-go-mobile--disable)))

;;;###autoload
(defun agent-shell-to-go-mobile-auto-enable ()
  "Automatically enable mobile backend for agent-shell buffers."
  (when (derived-mode-p 'agent-shell-mode)
    (agent-shell-to-go-mobile-mode 1)))

;;;###autoload
(defun agent-shell-to-go-mobile-setup ()
  "Set up automatic mobile backend integration for all agent-shell sessions."
  (add-hook 'agent-shell-mode-hook #'agent-shell-to-go-mobile-auto-enable))

;;;###autoload
(defun agent-shell-to-go-mobile-test-connection ()
  "Test connection to the mobile backend."
  (interactive)
  (let ((token (agent-shell-to-go-mobile--load-token)))
    (unless agent-shell-to-go-mobile-backend-url
      (user-error "agent-shell-to-go-mobile-backend-url not set"))
    (unless token
      (user-error "No token found (set agent-shell-to-go-mobile-token or AGENT_SHELL_MOBILE_TOKEN)"))
    (let* ((url (concat agent-shell-to-go-mobile-backend-url "/health"))
           (result (with-temp-buffer
                     (call-process "curl" nil t nil
                                   "-s" "-w" "\n%{http_code}"
                                   url)
                     (buffer-string)))
           (lines (split-string result "\n" t))
           (body (car lines))
           (status (cadr lines)))
      (if (and (equal status "200") (equal body "ok"))
          (message "Connection successful: %s" agent-shell-to-go-mobile-backend-url)
        (message "Connection failed: HTTP %s - %s" status body)))))

(provide 'agent-shell-to-go-mobile)
;;; agent-shell-to-go-mobile.el ends here
