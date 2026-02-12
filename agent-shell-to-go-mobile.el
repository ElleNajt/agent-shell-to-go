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

(defcustom agent-shell-to-go-mobile-log-directory "~/.dendrite/debug/logs"
  "Directory for debug log files."
  :type 'string
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

(defvar-local agent-shell-to-go-mobile--injecting-from-mobile nil
  "Non-nil when message is being injected from mobile app.
Used to prevent echoing the message back to the backend.")

(defvar-local agent-shell-to-go-mobile--pending-permission nil
  "Pending permission request info for this buffer.
Plist with :request-id, :options, :tool-call-id.")

;;; Utility functions

(defvar agent-shell-to-go-mobile--log-file nil
  "Current log file path, set on first log write each day.")

(defun agent-shell-to-go-mobile--ensure-log-directory ()
  "Ensure the log directory exists."
  (let ((dir (expand-file-name agent-shell-to-go-mobile-log-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun agent-shell-to-go-mobile--get-log-file ()
  "Get the current log file path, creating a new one each day."
  (let* ((dir (agent-shell-to-go-mobile--ensure-log-directory))
         (today (format-time-string "%Y-%m-%d"))
         (expected-file (expand-file-name (concat "dendrite-" today ".log") dir)))
    (unless (equal agent-shell-to-go-mobile--log-file expected-file)
      (setq agent-shell-to-go-mobile--log-file expected-file))
    agent-shell-to-go-mobile--log-file))

(defun agent-shell-to-go-mobile--debug (format-string &rest args)
  "Log a debug message to the log file."
  (let* ((timestamp (format-time-string "%H:%M:%S.%3N"))
         (msg (apply #'format format-string args))
         (log-line (format "%s %s\n" timestamp msg))
         (log-file (agent-shell-to-go-mobile--get-log-file)))
    (write-region log-line nil log-file t 'silent)))

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
                       "dendrite-post"
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
     (project_path . ,(agent-shell-to-go-mobile--get-project-path))
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

(defun agent-shell-to-go-mobile--send-event (event-type payload)
  "Send a generic EVENT-TYPE with PAYLOAD to the backend."
  (when agent-shell-to-go-mobile--session-id
    (agent-shell-to-go-mobile--post
     "/events/custom"
     `((session_id . ,agent-shell-to-go-mobile--session-id)
       (event_type . ,event-type)
       (payload . ,payload)
       (timestamp . ,(agent-shell-to-go-mobile--iso-timestamp))))))

;;; Advice functions

(defun agent-shell-to-go-mobile--on-send-command (orig-fn &rest args)
  "Advice for agent-shell--send-command. Send user message to mobile backend.
ORIG-FN is the original function, ARGS are its arguments."
  ;; Get buffer from current buffer (send-command is called in the agent buffer)
  (let ((buffer (current-buffer)))
    (when (and (buffer-live-p buffer)
               (buffer-local-value 'agent-shell-to-go-mobile-mode buffer)
               (buffer-local-value 'agent-shell-to-go-mobile--session-id buffer))
      (let ((prompt (plist-get args :prompt)))
        (when prompt
          (with-current-buffer buffer
            ;; Only send user message if it wasn't injected from mobile
            ;; (mobile already stored it - echoing back causes duplicates)
            (unless agent-shell-to-go-mobile--injecting-from-mobile
              (agent-shell-to-go-mobile--send-message "user" prompt))
            (agent-shell-to-go-mobile--send-status "processing")
            (setq agent-shell-to-go-mobile--current-agent-message nil))))))
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
           ;; Send tool call as tool message with useful context
           (let* ((title (alist-get 'title update))
                  (raw-input (alist-get 'rawInput update))
                  (command (alist-get 'command raw-input))
                  (file-path (alist-get 'file_path raw-input))
                  (pattern (alist-get 'pattern raw-input))
                  (content (alist-get 'content raw-input))
                  (old-string (alist-get 'old_string raw-input))
                  (new-string (alist-get 'new_string raw-input))
                  (query (alist-get 'query raw-input))
                  (url (alist-get 'url raw-input))
                  (prompt (alist-get 'prompt raw-input))
                  ;; Build a display string with relevant info
                  ;; For file ops, include content after newline for frontend to parse
                  (display (cond
                            ;; Bash/terminal commands - show the command
                            (command command)
                            ;; Edit operations - show path + diff
                            ((and file-path old-string new-string)
                             (format "%s: %s\n--- old\n%s\n+++ new\n%s"
                                     (or title "Edit") file-path old-string new-string))
                            ;; Write operations - show path + content
                            ((and file-path content)
                             (format "%s: %s\n%s" (or title "Write") file-path content))
                            ;; Read operations - just show path (content comes in tool_call_update)
                            (file-path (format "%s: %s" (or title "Read") file-path))
                            ;; Search operations - show pattern
                            (pattern (format "%s: %s" (or title "Search") pattern))
                            ;; Web search - show query
                            (query (format "%s: %s" (or title "Search") query))
                            ;; Web fetch - show url
                            (url (format "%s: %s" (or title "Fetch") url))
                            ;; Task/prompt - show prompt
                            (prompt (format "%s: %s" (or title "Task")
                                            (truncate-string-to-width prompt 50)))
                            ;; Fallback - show raw-input summary if available
                            (raw-input (format "%s: %s" (or title "Tool")
                                               (truncate-string-to-width
                                                (format "%s" raw-input) 60)))
                            ;; Last resort
                            (title title)
                            (t "Tool call"))))
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
                              (format " %s" output)
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
  "Advice for agent-shell--on-request. Send permission request to mobile.
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
        (let* ((request-id (alist-get 'id request))
               (params (alist-get 'params request))
               (options (alist-get 'options params))
               (tool-call (alist-get 'toolCall params))
               (tool-call-id (alist-get 'toolCallId tool-call))
               (title (alist-get 'title tool-call))
               (raw-input (alist-get 'rawInput tool-call))
               (command (alist-get 'command raw-input))
               (description (or command title "Permission required"))
               ;; Convert options to simpler format for mobile
               (mobile-options
                (mapcar (lambda (opt)
                          (list (cons 'id (alist-get 'id opt))
                                (cons 'label (alist-get 'label opt))
                                (cons 'kind (alist-get 'kind opt))))
                        options)))
          ;; Store pending permission info
          (setq agent-shell-to-go-mobile--pending-permission
                (list :request-id request-id
                      :options options
                      :tool-call-id tool-call-id))
          ;; Send permission request event to backend
          (agent-shell-to-go-mobile--send-event
           "permission_request"
           `((request_id . ,request-id)
             (description . ,description)
             (options . ,(vconcat mobile-options))))
          ;; Also send status update for UI
          (agent-shell-to-go-mobile--send-status
           "permission_required"
           description)))))
  (apply orig-fn args))

(defun agent-shell-to-go-mobile--on-subscribe-to-client-events (orig-fn &rest args)
  "Advice for agent-shell--subscribe-to-client-events. Add our own error subscription.
ORIG-FN is the original function, ARGS are its arguments."
  ;; Call original first to set up all subscriptions
  (apply orig-fn args)
  ;; Add our own error subscription if mobile mode is enabled
  (let* ((state (plist-get args :state))
         (buffer (and state (alist-get :buffer state)))
         (client (and state (alist-get :client state))))
    (when (and buffer
               (buffer-live-p buffer)
               (buffer-local-value 'agent-shell-to-go-mobile-mode buffer)
               client)
      (acp-subscribe-to-errors
       :client client
       :buffer buffer
       :on-error (lambda (error)
                   (when (and (buffer-live-p buffer)
                              (buffer-local-value 'agent-shell-to-go-mobile-mode buffer))
                     (with-current-buffer buffer
                       (let* ((message (or (alist-get 'message error)
                                           (alist-get 'data error)
                                           "Unknown error"))
                              (code (alist-get 'code error))
                              (details (alist-get 'details (alist-get 'data error)))
                              (error-text (if details
                                              (format "%s: %s" message details)
                                            message)))
                         (agent-shell-to-go-mobile--debug "Error received: %s (code: %s)" error-text code)
                         (agent-shell-to-go-mobile--send-message
                          "error"
                          error-text)))))))))

;;; Minor mode

(cl-defun agent-shell-to-go-mobile--enable ()
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
  (advice-add 'agent-shell--subscribe-to-client-events :around #'agent-shell-to-go-mobile--on-subscribe-to-client-events)
  
  ;; Add kill-buffer hook
  (add-hook 'kill-buffer-hook #'agent-shell-to-go-mobile--on-buffer-kill nil t)
  
  ;; Send spawn event
  (agent-shell-to-go-mobile--send-spawn)
  
  ;; Connect WebSocket if not already connected
  (unless (and agent-shell-to-go-mobile--websocket
               (websocket-openp agent-shell-to-go-mobile--websocket))
    (agent-shell-to-go-mobile--websocket-connect))
  
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
  
  ;; Remove advice and disconnect WebSocket if no more buffers
  (unless agent-shell-to-go-mobile--active-buffers
    (advice-remove 'agent-shell--send-command #'agent-shell-to-go-mobile--on-send-command)
    (advice-remove 'agent-shell--on-notification #'agent-shell-to-go-mobile--on-notification)
    (advice-remove 'agent-shell--on-request #'agent-shell-to-go-mobile--on-request)
    (advice-remove 'agent-shell-heartbeat-stop #'agent-shell-to-go-mobile--on-heartbeat-stop)
    (advice-remove 'agent-shell--subscribe-to-client-events #'agent-shell-to-go-mobile--on-subscribe-to-client-events)
    (agent-shell-to-go-mobile--websocket-disconnect))
  
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

;;;###autoload
(defun agent-shell-to-go-mobile-debug-status ()
  "Show current status of mobile backend integration for debugging."
  (interactive)
  (let ((ws-open (and agent-shell-to-go-mobile--websocket
                      (websocket-openp agent-shell-to-go-mobile--websocket)))
        (last-msg (if agent-shell-to-go-mobile--websocket-last-message-time
                      (format-time-string "%Y-%m-%d %H:%M:%S"
                                          agent-shell-to-go-mobile--websocket-last-message-time)
                    "never"))
        (log-file (agent-shell-to-go-mobile--get-log-file)))
    (message (concat "Mobile Backend Status:\n"
                     "  Backend URL: %s\n"
                     "  WebSocket state: %s (open: %s)\n"
                     "  Connect attempts: %d\n"
                     "  Last WS message: %s\n"
                     "  Active buffers: %d\n"
                     "  Reconnect timer: %s\n"
                     "  Log file: %s")
             (or agent-shell-to-go-mobile-backend-url "not set")
             agent-shell-to-go-mobile--websocket-state
             ws-open
             agent-shell-to-go-mobile--websocket-connect-count
             last-msg
             (length agent-shell-to-go-mobile--active-buffers)
             (if agent-shell-to-go-mobile--websocket-reconnect-timer "active" "none")
             log-file)))

;;;###autoload
(defun agent-shell-to-go-mobile-open-log ()
  "Open the current debug log file."
  (interactive)
  (find-file (agent-shell-to-go-mobile--get-log-file)))

;;; WebSocket client for receiving messages from mobile app

(defvar agent-shell-to-go-mobile--websocket nil
  "WebSocket connection to the mobile backend.")

(defvar agent-shell-to-go-mobile--websocket-reconnect-timer nil
  "Timer for reconnecting WebSocket.")

(defvar agent-shell-to-go-mobile--websocket-state 'disconnected
  "Current WebSocket state: disconnected, connecting, connected, reconnecting.")

(defvar agent-shell-to-go-mobile--websocket-connect-count 0
  "Number of WebSocket connection attempts since last successful connection.")

(defvar agent-shell-to-go-mobile--websocket-last-message-time nil
  "Time of last WebSocket message received.")

(defun agent-shell-to-go-mobile--websocket-connect ()
  "Connect to the mobile backend WebSocket to receive messages."
  (when agent-shell-to-go-mobile--websocket
    (ignore-errors (websocket-close agent-shell-to-go-mobile--websocket)))
  (setq agent-shell-to-go-mobile--websocket-state 'connecting)
  (cl-incf agent-shell-to-go-mobile--websocket-connect-count)
  (agent-shell-to-go-mobile--debug "[WS] Connecting (attempt %d, %d active buffers)"
                                   agent-shell-to-go-mobile--websocket-connect-count
                                   (length agent-shell-to-go-mobile--active-buffers))
  (let* ((token (agent-shell-to-go-mobile--load-token))
         (ws-url (concat (replace-regexp-in-string "^http" "ws" agent-shell-to-go-mobile-backend-url)
                         "/ws?token=" (url-hexify-string (or token "")))))
    (condition-case err
        (setq agent-shell-to-go-mobile--websocket
              (websocket-open ws-url
                              :on-open (lambda (_ws)
                                         (setq agent-shell-to-go-mobile--websocket-state 'connected)
                                         (setq agent-shell-to-go-mobile--websocket-connect-count 0)
                                         ;; Cancel any pending reconnect timer
                                         (when agent-shell-to-go-mobile--websocket-reconnect-timer
                                           (cancel-timer agent-shell-to-go-mobile--websocket-reconnect-timer)
                                           (setq agent-shell-to-go-mobile--websocket-reconnect-timer nil))
                                         (agent-shell-to-go-mobile--debug "[WS] Connected successfully"))
                              :on-message #'agent-shell-to-go-mobile--on-websocket-message
                              :on-close (lambda (_ws)
                                          (let ((prev-state agent-shell-to-go-mobile--websocket-state))
                                            (setq agent-shell-to-go-mobile--websocket-state 'disconnected)
                                            (agent-shell-to-go-mobile--debug "[WS] Closed (was: %s, last msg: %s)"
                                                                             prev-state
                                                                             (if agent-shell-to-go-mobile--websocket-last-message-time
                                                                                 (format-time-string "%H:%M:%S" agent-shell-to-go-mobile--websocket-last-message-time)
                                                                               "never"))
                                            (agent-shell-to-go-mobile--websocket-schedule-reconnect)))
                              :on-error (lambda (_ws _type err)
                                          (agent-shell-to-go-mobile--debug "[WS] Error: %s (state: %s)"
                                                                           err agent-shell-to-go-mobile--websocket-state))))
      (error
       (setq agent-shell-to-go-mobile--websocket-state 'disconnected)
       (agent-shell-to-go-mobile--debug "[WS] Connect failed: %s" err)
       (agent-shell-to-go-mobile--websocket-schedule-reconnect)))))

(defun agent-shell-to-go-mobile--websocket-schedule-reconnect ()
  "Schedule WebSocket reconnection with exponential backoff."
  (when agent-shell-to-go-mobile--websocket-reconnect-timer
    (cancel-timer agent-shell-to-go-mobile--websocket-reconnect-timer))
  (when agent-shell-to-go-mobile--active-buffers
    (setq agent-shell-to-go-mobile--websocket-state 'reconnecting)
    ;; Exponential backoff: 5s, 10s, 20s, 40s, max 60s
    (let ((delay (min 60 (* 5 (expt 2 (min 3 agent-shell-to-go-mobile--websocket-connect-count))))))
      (agent-shell-to-go-mobile--debug "[WS] Scheduling reconnect in %ds" delay)
      (setq agent-shell-to-go-mobile--websocket-reconnect-timer
            (run-with-timer delay nil #'agent-shell-to-go-mobile--websocket-connect)))))

(defun agent-shell-to-go-mobile--websocket-disconnect ()
  "Disconnect WebSocket."
  (when agent-shell-to-go-mobile--websocket-reconnect-timer
    (cancel-timer agent-shell-to-go-mobile--websocket-reconnect-timer)
    (setq agent-shell-to-go-mobile--websocket-reconnect-timer nil))
  (when agent-shell-to-go-mobile--websocket
    (ignore-errors (websocket-close agent-shell-to-go-mobile--websocket))
    (setq agent-shell-to-go-mobile--websocket nil)))

(defun agent-shell-to-go-mobile--on-websocket-message (_ws frame)
  "Handle incoming WebSocket FRAME from mobile backend."
  (setq agent-shell-to-go-mobile--websocket-last-message-time (current-time))
  (condition-case err
      (let* ((payload (websocket-frame-text frame))
             (data (json-read-from-string payload))
             (type (alist-get 'type data))
             (event-payload (alist-get 'payload data)))
        (agent-shell-to-go-mobile--debug "[WS] Received: type=%s" type)
        (pcase type
          ("send_request"
           (agent-shell-to-go-mobile--handle-send-request event-payload))
          ("stop_request"
           (agent-shell-to-go-mobile--handle-stop-request event-payload))
          ("close_request"
           (agent-shell-to-go-mobile--handle-close-request event-payload))
          ("restart_request"
           (agent-shell-to-go-mobile--handle-restart-request event-payload))
          ("new_agent_request"
           (agent-shell-to-go-mobile--handle-new-agent-request event-payload))
          ("new_dispatcher_request"
           (agent-shell-to-go-mobile--handle-new-dispatcher-request event-payload))
          ("check_sessions_request"
           (agent-shell-to-go-mobile--handle-check-sessions-request event-payload))
          ("permission_response"
           (agent-shell-to-go-mobile--handle-permission-response event-payload))))
    (error
     (agent-shell-to-go-mobile--debug "WebSocket message error: %s" err))))

(defun agent-shell-to-go-mobile--handle-send-request (payload)
  "Handle a send_request PAYLOAD from mobile app.
Injects the message into the appropriate agent-shell buffer."
  (let* ((session-id (alist-get 'session_id payload))
         (content (alist-get 'content payload))
         (buffer (agent-shell-to-go-mobile--find-buffer-by-session-id session-id)))
    (if buffer
        (progn
          (agent-shell-to-go-mobile--debug "Injecting message to %s: %s"
                                           (buffer-name buffer) content)
          (with-current-buffer buffer
            (agent-shell-to-go-mobile--inject-message content)))
      (agent-shell-to-go-mobile--debug "No buffer found for session: %s" session-id))))

(defun agent-shell-to-go-mobile--find-buffer-by-session-id (session-id)
  "Find the agent-shell buffer with SESSION-ID."
  (cl-find-if
   (lambda (buf)
     (and (buffer-live-p buf)
          (equal session-id
                 (buffer-local-value 'agent-shell-to-go-mobile--session-id buf))))
   agent-shell-to-go-mobile--active-buffers))

(defun agent-shell-to-go-mobile--inject-message (text)
  "Inject TEXT from mobile app into the current agent-shell buffer."
  (when (derived-mode-p 'agent-shell-mode)
    ;; Set flag to prevent echoing this message back to backend
    ;; (it was already stored when mobile sent it)
    (setq agent-shell-to-go-mobile--injecting-from-mobile t)
    (unwind-protect
        (if (shell-maker-busy)
            ;; Shell is busy - queue the request
            (progn
              (agent-shell--enqueue-request :prompt text)
              (agent-shell-to-go-mobile--debug "Queued message (agent busy): %s" text))
          ;; Shell is ready - inject immediately
          (save-excursion
            (goto-char (point-max))
            (insert text))
          (goto-char (point-max))
          (call-interactively #'shell-maker-submit))
      ;; Clear flag after submit (or on error)
      (setq agent-shell-to-go-mobile--injecting-from-mobile nil))))

(defun agent-shell-to-go-mobile--handle-stop-request (payload)
  "Handle a stop_request PAYLOAD from mobile app.
Interrupts the agent in the appropriate buffer."
  (let* ((session-id (alist-get 'session_id payload))
         (buffer (agent-shell-to-go-mobile--find-buffer-by-session-id session-id)))
    (if buffer
        (with-current-buffer buffer
          (let ((was-busy (and (boundp 'agent-shell--state)
                               (alist-get :busy agent-shell--state))))
            (agent-shell-to-go-mobile--debug "[STOP] Interrupting %s (was-busy: %s)"
                                             (buffer-name buffer) was-busy)
            (if (fboundp 'agent-shell-interrupt)
                (progn
                  (agent-shell-interrupt t) ; force=t to skip prompt
                  (agent-shell-to-go-mobile--debug "[STOP] agent-shell-interrupt called")
                  ;; Send status to confirm interruption
                  (agent-shell-to-go-mobile--send-status "interrupted"))
              (agent-shell-to-go-mobile--debug "[STOP] agent-shell-interrupt not available!"))))
      (agent-shell-to-go-mobile--debug "[STOP] No buffer found for session: %s" session-id))))

(defun agent-shell-to-go-mobile--handle-close-request (payload)
  "Handle a close_request PAYLOAD from mobile app.
Kills the agent buffer."
  (let* ((session-id (alist-get 'session_id payload))
         (buffer (agent-shell-to-go-mobile--find-buffer-by-session-id session-id)))
    (if buffer
        (progn
          (agent-shell-to-go-mobile--debug "Closing agent: %s" (buffer-name buffer))
          (kill-buffer buffer))
      (agent-shell-to-go-mobile--debug "No buffer found for close: %s" session-id))))

(defun agent-shell-to-go-mobile--handle-restart-request (payload)
  "Handle a restart_request PAYLOAD from mobile app.
Kills the buffer and respawns a new agent with the same name.
Injects resume messages from the previous session as context.
TODO: Use proper acp/agent-shell resume functionality when available."
  (let* ((session-id (alist-get 'session_id payload))
         (buffer-name (alist-get 'buffer_name payload))
         (project (alist-get 'project payload))
         (project-path (alist-get 'project_path payload))
         (resume-messages (alist-get 'resume_messages payload))
         (buffer (agent-shell-to-go-mobile--find-buffer-by-session-id session-id)))
    (agent-shell-to-go-mobile--debug "Restart request: session=%s buffer=%s path=%s"
                                     session-id buffer-name project-path)
    ;; Kill the existing buffer if it exists
    (when buffer
      (agent-shell-to-go-mobile--debug "Killing existing buffer: %s" (buffer-name buffer))
      (kill-buffer buffer))
    ;; Spawn a new agent with the same buffer name
    (when (and project-path (file-directory-p project-path))
      (let* ((default-directory project-path)
             ;; Extract agent name from buffer name (remove " @ project" suffix and " Agent" suffix)
             ;; Buffer name is like "Box-Sizing Agent @ project" -> we want "Box-Sizing"
             (agent-name (when (string-match "\\(.+\\) Agent @ " buffer-name)
                           (match-string 1 buffer-name)))
             ;; Build resume context from previous messages
             (resume-context (when resume-messages
                               (agent-shell-to-go-mobile--format-resume-context resume-messages))))
        (agent-shell-to-go-mobile--debug "Spawning new agent: %s in %s" agent-name project-path)
        (if (and (fboundp 'meta-agent-shell-start-named-agent) agent-name)
            ;; Use meta-agent-shell if available, passing resume context as task
            (meta-agent-shell-start-named-agent project-path agent-name resume-context)
          ;; Fallback: start a regular agent-shell
          (agent-shell))))))

(defun agent-shell-to-go-mobile--format-resume-context (messages)
  "Format MESSAGES (alist of role/content) into a resume context string."
  (when (and messages (> (length messages) 0))
    (concat
     "You are resuming a previous session. Here is the recent conversation history for context:\n\n"
     (mapconcat
      (lambda (msg)
        (let ((role (alist-get 'role msg))
              (content (alist-get 'content msg)))
          (format "[%s]: %s" (upcase role) content)))
      messages
      "\n\n")
     "\n\nPlease continue from where we left off.")))

(defun agent-shell-to-go-mobile--handle-new-agent-request (payload)
  "Handle a new_agent_request PAYLOAD from mobile app.
Spawns a new named agent. Deferred via timer to avoid blocking websocket."
  (let* ((name (alist-get 'name payload))
         (path (alist-get 'path payload))
         (task (alist-get 'task payload)))
    (agent-shell-to-go-mobile--debug "Spawning agent: %s in %s" name path)
    ;; Defer to avoid blocking websocket callback
    (run-at-time 0 nil
                 (lambda ()
                   (if (and (fboundp 'meta-agent-shell-start-named-agent) path name)
                       (meta-agent-shell-start-named-agent path name task)
                     ;; Fallback if meta-agent-shell not available
                     (when (and path (file-directory-p path))
                       (let ((default-directory path))
                         (agent-shell))))))))

(defun agent-shell-to-go-mobile--handle-new-dispatcher-request (payload)
  "Handle a new_dispatcher_request PAYLOAD from mobile app.
Spawns a new dispatcher for a project. Deferred via timer to avoid blocking websocket."
  (let ((path (alist-get 'path payload)))
    (agent-shell-to-go-mobile--debug "Spawning dispatcher for: %s" path)
    ;; Defer to avoid blocking websocket callback
    (run-at-time 0 nil
                 (lambda ()
                   (if (fboundp 'meta-agent-shell-start-dispatcher)
                       (meta-agent-shell-start-dispatcher path)
                     ;; Fallback
                     (when (and path (file-directory-p path))
                       (let ((default-directory path))
                         (agent-shell))))))))

(defun agent-shell-to-go-mobile--handle-permission-response (payload)
  "Handle a permission_response PAYLOAD from mobile app.
Sends the permission response back to the ACP client."
  (let* ((session-id (alist-get 'session_id payload))
         (option-id (alist-get 'option_id payload))
         (buffer (agent-shell-to-go-mobile--find-buffer-by-session-id session-id)))
    (if buffer
        (with-current-buffer buffer
          (if agent-shell-to-go-mobile--pending-permission
              (let* ((request-id (plist-get agent-shell-to-go-mobile--pending-permission :request-id))
                     (tool-call-id (plist-get agent-shell-to-go-mobile--pending-permission :tool-call-id))
                     (state agent-shell--state)
                     (client (alist-get :client state)))
                (agent-shell-to-go-mobile--debug "Permission response: option=%s request=%s" option-id request-id)
                ;; Send the permission response
                (agent-shell--send-permission-response
                 :client client
                 :request-id request-id
                 :option-id option-id
                 :state state
                 :tool-call-id tool-call-id
                 :message-text (format "Selected from mobile: %s" option-id))
                ;; Clear pending permission
                (setq agent-shell-to-go-mobile--pending-permission nil)
                ;; Send status update
                (agent-shell-to-go-mobile--send-status "processing"))
            (agent-shell-to-go-mobile--debug "No pending permission for session: %s" session-id)))
      (agent-shell-to-go-mobile--debug "No buffer found for permission response: %s" session-id))))

(defun agent-shell-to-go-mobile--handle-check-sessions-request (_payload)
  "Handle a check_sessions_request from the backend.
Reports all agent-shell buffers back to the backend for full sync:
- Returns full info for ALL tracked sessions (backend will upsert)
- Also discovers and enables mobile mode on any untracked buffers"
  (agent-shell-to-go-mobile--debug "Checking sessions for sync")
  (let ((all-sessions nil))
    ;; First, discover any agent-shell buffers that aren't tracked yet
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (with-current-buffer buf
                   (and (derived-mode-p 'agent-shell-mode)
                        (not agent-shell-to-go-mobile-mode))))
        ;; This is an agent-shell buffer without mobile mode - enable it
        (with-current-buffer buf
          (agent-shell-to-go-mobile-mode 1))))
    ;; Now collect full info for ALL tracked sessions
    (dolist (buf agent-shell-to-go-mobile--active-buffers)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (push `((session_id . ,agent-shell-to-go-mobile--session-id)
                  (buffer_name . ,(buffer-name))
                  (project . ,(agent-shell-to-go-mobile--get-project-name))
                  (project_path . ,(agent-shell-to-go-mobile--get-project-path))
                  (timestamp . ,(agent-shell-to-go-mobile--iso-timestamp)))
                all-sessions))))
    (agent-shell-to-go-mobile--debug "Reporting %d sessions for sync" (length all-sessions))
    (agent-shell-to-go-mobile--post
     "/events/alive-sessions"
     `((sessions . ,(vconcat all-sessions))))))

;;; Integration with meta-agent-shell

(defvar agent-shell-to-go-mobile--spawner-session-id nil
  "Session ID of the agent that is spawning a new agent.
Set temporarily during spawn to pass parent info to the new agent.")

(defun agent-shell-to-go-mobile--before-spawn-hook ()
  "Capture the spawner's session ID before a new agent is created."
  (when (and agent-shell-to-go-mobile-mode
             agent-shell-to-go-mobile--session-id)
    (setq agent-shell-to-go-mobile--spawner-session-id
          agent-shell-to-go-mobile--session-id)))

(defun agent-shell-to-go-mobile--after-spawn-hook ()
  "Enable mobile mode on newly spawned agent with parent relationship."
  (when (and (derived-mode-p 'agent-shell-mode)
             agent-shell-to-go-mobile-backend-url
             (agent-shell-to-go-mobile--load-token)
             (not agent-shell-to-go-mobile-mode)) ; Don't enable twice
    ;; Enable the mode (this will send spawn event)
    (agent-shell-to-go-mobile-mode 1)
    ;; If we have a spawner, update the parent relationship (upsert, same session_id)
    (when agent-shell-to-go-mobile--spawner-session-id
      (agent-shell-to-go-mobile--post
       "/events/agent-spawn"
       `((session_id . ,agent-shell-to-go-mobile--session-id)
         (buffer_name . ,(buffer-name))
         (project . ,(agent-shell-to-go-mobile--get-project-name))
         (project_path . ,(agent-shell-to-go-mobile--get-project-path))
         (parent_session_id . ,agent-shell-to-go-mobile--spawner-session-id)
         (timestamp . ,(agent-shell-to-go-mobile--iso-timestamp))))
      (setq agent-shell-to-go-mobile--spawner-session-id nil))))

;;;###autoload
(defun agent-shell-to-go-mobile-setup-meta-agent-shell ()
  "Set up integration with meta-agent-shell for tracking spawn hierarchy.
Call this after both agent-shell-to-go-mobile and meta-agent-shell are loaded."
  (when (boundp 'meta-agent-shell-before-spawn-hook)
    (add-hook 'meta-agent-shell-before-spawn-hook
              #'agent-shell-to-go-mobile--before-spawn-hook))
  (when (boundp 'meta-agent-shell-after-spawn-hook)
    (add-hook 'meta-agent-shell-after-spawn-hook
              #'agent-shell-to-go-mobile--after-spawn-hook)))

;;; Dendrite backend process management

(defcustom agent-shell-to-go-mobile-backend-dir
  (expand-file-name "~/code/agent-shell-to-go/dendrite/backend/")
  "Directory containing the dendrite backend source and binary."
  :type 'directory
  :group 'agent-shell-to-go-mobile)

(defvar agent-shell-to-go-mobile--backend-restart-timer nil
  "Timer for restarting the backend after a crash.")

(defun agent-shell-to-go-mobile--backend-sentinel (process event)
  "Sentinel for the dendrite backend PROCESS. Restart on non-zero exit."
  (let ((event-str (string-trim event)))
    (message "dendrite-backend: %s" event-str)
    (when (and (memq (process-status process) '(exit signal))
               (not (eq (process-exit-status process) 0)))
      (setq agent-shell-to-go-mobile--backend-restart-timer
            (run-with-timer 10 nil #'agent-shell-to-go-mobile-start-backend)))))

;;;###autoload
(defun agent-shell-to-go-mobile-start-backend ()
  "Start the dendrite backend process. Builds if binary is missing."
  (interactive)
  (when (get-process "dendrite-backend")
    (delete-process "dendrite-backend"))
  (let ((default-directory agent-shell-to-go-mobile-backend-dir)
        (binary (expand-file-name "dendrite-backend" agent-shell-to-go-mobile-backend-dir)))
    (unless (file-exists-p binary)
      (message "dendrite-backend: building...")
      (shell-command "go build -o dendrite-backend ."))
    (let ((proc (start-process "dendrite-backend" "*dendrite-backend*" binary)))
      (set-process-sentinel proc #'agent-shell-to-go-mobile--backend-sentinel)
      (set-process-query-on-exit-flag proc nil)
      (message "dendrite-backend: started"))))

;;;###autoload
(defun agent-shell-to-go-mobile-stop-backend ()
  "Stop the dendrite backend process."
  (interactive)
  (when agent-shell-to-go-mobile--backend-restart-timer
    (cancel-timer agent-shell-to-go-mobile--backend-restart-timer)
    (setq agent-shell-to-go-mobile--backend-restart-timer nil))
  (when (get-process "dendrite-backend")
    (delete-process "dendrite-backend")
    (message "dendrite-backend: stopped")))

(provide 'agent-shell-to-go-mobile)
;;; agent-shell-to-go-mobile.el ends here
