;;; agent-shell-to-go-backend.el --- Backend transport for mobile app -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Elle Najt

;;; Commentary:

;; Transport that POSTs events to a backend server for the mobile app.
;; The backend aggregates agent state and exposes it via REST/WebSocket
;; to the Android app.
;;
;; Security: Backend should only listen on Tailscale IP + require bearer token.

;;; Code:

(require 'agent-shell-to-go-core)
(require 'json)

(defcustom agent-shell-to-go-backend-url nil
  "URL of the agent-shell-to-go backend server.
Should be a Tailscale IP like \"http://100.x.x.x:8080\".
Set to nil to disable backend integration."
  :type '(choice (const :tag "Disabled" nil)
          (string :tag "Backend URL"))
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-backend-token nil
  "Bearer token for authenticating with the backend server."
  :type '(choice (const :tag "Not set" nil)
          (string :tag "Token"))
  :group 'agent-shell-to-go)

;;; HTTP Posting

(defun agent-shell-to-go-backend--post (endpoint data)
  "POST DATA to backend ENDPOINT asynchronously (fire-and-forget)."
  (when (and agent-shell-to-go-backend-url agent-shell-to-go-backend-token)
    (let* ((url (concat agent-shell-to-go-backend-url endpoint))
           (json-data (encode-coding-string (json-encode data) 'utf-8)))
      (let ((proc (start-process
                   "agent-shell-backend-post" nil
                   "curl" "-s" "-X" "POST"
                   "-H" "Content-Type: application/json; charset=utf-8"
                   "-H" (concat "Authorization: Bearer " agent-shell-to-go-backend-token)
                   "-d" json-data
                   "--max-time" "5"
                   url)))
        (set-process-query-on-exit-flag proc nil)))))

(defun agent-shell-to-go-backend--timestamp ()
  "Return current timestamp in ISO 8601 format."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))

;;; Transport Handlers

(defun agent-shell-to-go-backend--on-session-start (buffer)
  "Handle session start for BUFFER. Return session data."
  (when agent-shell-to-go-backend-url
    (let ((session-id (format "%s-%s"
                              (buffer-name buffer)
                              (format-time-string "%s"))))
      (agent-shell-to-go-backend--post
       "/events/agent-spawn"
       `((session_id . ,session-id)
         (buffer_name . ,(buffer-name buffer))
         (project . ,(agent-shell-to-go--get-project-name buffer))
         (parent_session_id . nil)
         (timestamp . ,(agent-shell-to-go-backend--timestamp))))
      ;; Return session data
      `(:session-id ,session-id))))

(defun agent-shell-to-go-backend--on-session-end (_buffer session-data)
  "Handle session end for BUFFER with SESSION-DATA."
  (when-let ((session-id (plist-get session-data :session-id)))
    (agent-shell-to-go-backend--post
     "/events/agent-close"
     `((session_id . ,session-id)
       (timestamp . ,(agent-shell-to-go-backend--timestamp))))))

(defun agent-shell-to-go-backend--on-user-message (_buffer session-data text _injecting)
  "Handle user message TEXT in BUFFER with SESSION-DATA."
  (when-let ((session-id (plist-get session-data :session-id)))
    (agent-shell-to-go-backend--post
     "/events/message"
     `((session_id . ,session-id)
       (role . "user")
       (content . ,text)
       (timestamp . ,(agent-shell-to-go-backend--timestamp))))))

(defun agent-shell-to-go-backend--on-agent-message (_buffer session-data text)
  "Handle agent message TEXT in BUFFER with SESSION-DATA."
  (when-let ((session-id (plist-get session-data :session-id)))
    (agent-shell-to-go-backend--post
     "/events/message"
     `((session_id . ,session-id)
       (role . "agent")
       (content . ,text)
       (timestamp . ,(agent-shell-to-go-backend--timestamp))))))

(defun agent-shell-to-go-backend--on-tool-call (_buffer session-data update)
  "Handle tool call UPDATE in BUFFER with SESSION-DATA."
  (when-let ((session-id (plist-get session-data :session-id)))
    (let* ((title (alist-get 'title update))
           (raw-input (alist-get 'rawInput update))
           (command (alist-get 'command raw-input)))
      (agent-shell-to-go-backend--post
       "/events/message"
       `((session_id . ,session-id)
         (role . "tool")
         (content . ,(or command title "tool call"))
         (timestamp . ,(agent-shell-to-go-backend--timestamp)))))))

(defun agent-shell-to-go-backend--on-tool-result (_buffer _session-data _update)
  "Handle tool result UPDATE in BUFFER with SESSION-DATA."
  ;; Could post tool results too, but keeping it simple for now
  nil)

(defun agent-shell-to-go-backend--on-status-change (_buffer session-data status _detail)
  "Handle status change to STATUS in BUFFER with SESSION-DATA."
  (when-let ((session-id (plist-get session-data :session-id)))
    (agent-shell-to-go-backend--post
     "/events/status"
     `((session_id . ,session-id)
       (status . ,status)
       (detail . "")
       (timestamp . ,(agent-shell-to-go-backend--timestamp))))))

(defun agent-shell-to-go-backend--on-permission-request (_buffer session-data request)
  "Handle permission REQUEST in BUFFER with SESSION-DATA."
  (when-let ((session-id (plist-get session-data :session-id)))
    (agent-shell-to-go-backend--post
     "/events/status"
     `((session_id . ,session-id)
       (status . "permission_required")
       (detail . ,(or (alist-get 'title (alist-get 'toolCall (alist-get 'params request)))
                      "Permission required"))
       (timestamp . ,(agent-shell-to-go-backend--timestamp))))))

;;; Transport Definition

(defvar agent-shell-to-go-backend--transport
  '(:name "backend"
    :on-session-start agent-shell-to-go-backend--on-session-start
    :on-session-end agent-shell-to-go-backend--on-session-end
    :on-user-message agent-shell-to-go-backend--on-user-message
    :on-agent-message agent-shell-to-go-backend--on-agent-message
    :on-tool-call agent-shell-to-go-backend--on-tool-call
    :on-tool-result agent-shell-to-go-backend--on-tool-result
    :on-status-change agent-shell-to-go-backend--on-status-change
    :on-permission-request agent-shell-to-go-backend--on-permission-request)
  "Backend transport definition.")

;;;###autoload
(defun agent-shell-to-go-backend-setup ()
  "Enable the backend transport."
  (interactive)
  (when agent-shell-to-go-backend-url
    (agent-shell-to-go-register-transport agent-shell-to-go-backend--transport)
    (message "agent-shell-to-go: backend transport enabled")))

(provide 'agent-shell-to-go-backend)

;;; agent-shell-to-go-backend.el ends here
