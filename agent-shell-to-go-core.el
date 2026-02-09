;;; agent-shell-to-go-core.el --- Core hooks and transport dispatch -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Elle Najt

;;; Commentary:

;; Core module for agent-shell-to-go. Hooks into agent-shell events and
;; dispatches to registered transports (Slack, backend, etc.).
;;
;; This module handles:
;; - Transport registration and dispatch
;; - Advice on agent-shell functions
;; - Message injection (external -> agent)
;; - Buffer lifecycle tracking

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)

(defgroup agent-shell-to-go nil
  "Take your agent-shell sessions anywhere."
  :group 'agent-shell
  :prefix "agent-shell-to-go-")

(defcustom agent-shell-to-go-debug nil
  "When non-nil, log debug messages to *Messages*."
  :type 'boolean
  :group 'agent-shell-to-go)

;;; Transport Registry

(defvar agent-shell-to-go--transports nil
  "List of registered transports.
Each transport is a plist with :name and handler functions.")

(defvar agent-shell-to-go--active-buffers nil
  "List of agent-shell buffers with active mirroring.")

(defvar-local agent-shell-to-go--transport-data nil
  "Alist mapping transport name to session data for this buffer.")

(defvar-local agent-shell-to-go--current-agent-message nil
  "Accumulator for streaming agent message chunks.")

(defcustom agent-shell-to-go-debug-file "/tmp/agent-shell-to-go.log"
  "File to write debug logs to."
  :type 'string
  :group 'agent-shell-to-go)

(defun agent-shell-to-go--debug (format-string &rest args)
  "Log a debug message if `agent-shell-to-go-debug' is non-nil."
  (when agent-shell-to-go-debug
    (let ((msg (concat (format-time-string "%H:%M:%S ")
                       (apply #'format format-string args)
                       "\n")))
      (append-to-file msg nil agent-shell-to-go-debug-file))))

(defun agent-shell-to-go-register-transport (transport)
  "Register TRANSPORT for use.
TRANSPORT is a plist with :name and event handler functions."
  (let ((name (plist-get transport :name)))
    ;; Remove existing transport with same name
    (setq agent-shell-to-go--transports
          (cl-remove-if (lambda (tr) (equal (plist-get tr :name) name))
                        agent-shell-to-go--transports))
    ;; Add new transport
    (push transport agent-shell-to-go--transports)
    (agent-shell-to-go--debug "registered transport: %s" name)))

(defun agent-shell-to-go-unregister-transport (name)
  "Unregister transport with NAME."
  (setq agent-shell-to-go--transports
        (cl-remove-if (lambda (tr) (equal (plist-get tr :name) name))
                      agent-shell-to-go--transports))
  (agent-shell-to-go--debug "unregistered transport: %s" name))

;;; Dispatch

(defun agent-shell-to-go--dispatch (event buffer &rest args)
  "Dispatch EVENT for BUFFER with ARGS to all transports.
Retrieves transport-specific session data and passes it to handlers."
  (dolist (transport agent-shell-to-go--transports)
    (when-let ((handler (plist-get transport event)))
      (let* ((name (plist-get transport :name))
             (session-data (with-current-buffer buffer
                             (alist-get name agent-shell-to-go--transport-data
                                        nil nil #'equal))))
        (condition-case err
            (apply handler buffer session-data args)
          (error
           (agent-shell-to-go--debug "%s transport error on %s: %s"
                                     name event err)))))))

(defun agent-shell-to-go--set-transport-data (buffer transport-name data)
  "Set session DATA for TRANSPORT-NAME in BUFFER."
  (with-current-buffer buffer
    (setf (alist-get transport-name agent-shell-to-go--transport-data
                     nil nil #'equal)
          data)))

(defun agent-shell-to-go--get-transport-data (buffer transport-name)
  "Get session data for TRANSPORT-NAME in BUFFER."
  (with-current-buffer buffer
    (alist-get transport-name agent-shell-to-go--transport-data
               nil nil #'equal)))

;;; Session Lifecycle

(defun agent-shell-to-go--start-session (buffer)
  "Start a new session for BUFFER, initializing all transports."
  (with-current-buffer buffer
    (setq agent-shell-to-go--transport-data nil)
    (setq agent-shell-to-go--current-agent-message nil))
  
  (dolist (transport agent-shell-to-go--transports)
    (when-let ((handler (plist-get transport :on-session-start)))
      (let ((name (plist-get transport :name)))
        (condition-case err
            (let ((session-data (funcall handler buffer)))
              (when session-data
                (agent-shell-to-go--set-transport-data buffer name session-data)
                (agent-shell-to-go--debug "%s session started" name)))
          (error
           (agent-shell-to-go--debug "%s failed to start session: %s" name err))))))
  
  (add-to-list 'agent-shell-to-go--active-buffers buffer))

(defun agent-shell-to-go--end-session (buffer)
  "End session for BUFFER, cleaning up all transports."
  (agent-shell-to-go--dispatch :on-session-end buffer)
  
  (setq agent-shell-to-go--active-buffers
        (delete buffer agent-shell-to-go--active-buffers))
  
  (with-current-buffer buffer
    (setq agent-shell-to-go--transport-data nil)))

;;; Message Injection (external -> agent)

(defvar-local agent-shell-to-go--injecting nil
  "Non-nil when injecting a message from external source.")

(defun agent-shell-to-go-inject-message (buffer text &optional source)
  "Inject TEXT into BUFFER's agent-shell as if typed locally.
SOURCE is optional string identifying where the message came from."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-mode)
        (if (shell-maker-busy)
            ;; Shell is busy - queue the request
            (progn
              (agent-shell--enqueue-request :prompt text)
              (agent-shell-to-go--dispatch :on-message-queued buffer text source))
          ;; Shell is ready - inject immediately
          (setq agent-shell-to-go--injecting t)
          (unwind-protect
              (progn
                (save-excursion
                  (goto-char (point-max))
                  (insert text))
                (goto-char (point-max))
                (call-interactively #'shell-maker-submit))
            (setq agent-shell-to-go--injecting nil)))))))

;;; Advice Functions (hooks into agent-shell)

(defun agent-shell-to-go--on-send-command (orig-fn &rest args)
  "Advice for agent-shell--send-command. Dispatch user message event.
ORIG-FN is the original function, ARGS are its arguments."
  ;; agent-shell--state is a dynamically-bound variable in agent-shell
  (when (boundp 'agent-shell--state)
    (when-let ((buffer (map-elt agent-shell--state :buffer)))
      (when (and (buffer-live-p buffer)
                 (memq buffer agent-shell-to-go--active-buffers))
        (let ((prompt (plist-get args :prompt))
              (injecting (buffer-local-value 'agent-shell-to-go--injecting buffer)))
          (when prompt
            ;; Reset agent message accumulator
            (with-current-buffer buffer
              (setq agent-shell-to-go--current-agent-message nil))
            ;; Dispatch with injecting flag so transports can avoid echo
            (agent-shell-to-go--dispatch :on-user-message buffer prompt injecting)
            (agent-shell-to-go--dispatch :on-status-change buffer "processing" nil))))))
  (apply orig-fn args))

(defun agent-shell-to-go--on-notification (orig-fn &rest args)
  "Advice for agent-shell--on-notification. Dispatch message events.
ORIG-FN is the original function, ARGS are its arguments."
  (let* ((state (plist-get args :state))
         (buffer (map-elt state :buffer)))
    (agent-shell-to-go--debug "on-notification: state=%S buffer=%s active=%s"
                              state
                              (and buffer (buffer-name buffer))
                              (and buffer (memq buffer agent-shell-to-go--active-buffers)))
    (when (and buffer
               (buffer-live-p buffer)
               (memq buffer agent-shell-to-go--active-buffers))
      (let* ((notification (plist-get args :notification))
             (params (alist-get 'params notification))
             (update (alist-get 'update params))
             (update-type (alist-get 'sessionUpdate update)))
        (agent-shell-to-go--debug "on-notification: update-type=%s" update-type)
        (pcase update-type
          ("agent_message_chunk"
           (let ((text (alist-get 'text (alist-get 'content update))))
             (with-current-buffer buffer
               (setq agent-shell-to-go--current-agent-message
                     (concat agent-shell-to-go--current-agent-message text)))))
          ("tool_call"
           ;; Flush pending agent message first
           (with-current-buffer buffer
             (when (and agent-shell-to-go--current-agent-message
                        (> (length agent-shell-to-go--current-agent-message) 0))
               (agent-shell-to-go--dispatch :on-agent-message buffer
                                            agent-shell-to-go--current-agent-message)
               (setq agent-shell-to-go--current-agent-message nil)))
           ;; Dispatch tool call
           (agent-shell-to-go--dispatch :on-tool-call buffer update))
          ("tool_call_update"
           (agent-shell-to-go--dispatch :on-tool-result buffer update))))))
  (apply orig-fn args))

(defun agent-shell-to-go--on-request (orig-fn &rest args)
  "Advice for agent-shell--on-request. Dispatch permission request event.
ORIG-FN is the original function, ARGS are its arguments."
  (let* ((state (plist-get args :state))
         (request (plist-get args :request))
         (method (alist-get 'method request))
         (buffer (and state (map-elt state :buffer))))
    (when (and buffer
               (buffer-live-p buffer)
               (memq buffer agent-shell-to-go--active-buffers)
               (equal method "session/request_permission"))
      (agent-shell-to-go--dispatch :on-permission-request buffer request)))
  (apply orig-fn args))

(defun agent-shell-to-go--on-heartbeat-stop (orig-fn &rest args)
  "Advice for agent-shell-heartbeat-stop. Dispatch ready status and flush message.
ORIG-FN is the original function, ARGS are its arguments."
  (when (boundp 'agent-shell--state)
    (when-let ((buffer (map-elt agent-shell--state :buffer)))
      (when (and (buffer-live-p buffer)
                 (memq buffer agent-shell-to-go--active-buffers))
        ;; Flush any pending agent message
        (with-current-buffer buffer
          (when (and agent-shell-to-go--current-agent-message
                     (> (length agent-shell-to-go--current-agent-message) 0))
            (agent-shell-to-go--dispatch :on-agent-message buffer
                                         agent-shell-to-go--current-agent-message)
            (setq agent-shell-to-go--current-agent-message nil)))
        ;; Signal ready for input
        (agent-shell-to-go--dispatch :on-status-change buffer "ready" nil))))
  (apply orig-fn args))

(defun agent-shell-to-go--on-client-initialized (&rest _args)
  "After-advice for agent-shell--initialize-client.
Dispatch error if client creation failed."
  (when-let ((buffer (and (boundp 'agent-shell--state)
                          (map-elt agent-shell--state :buffer))))
    (when (and (buffer-live-p buffer)
               (memq buffer agent-shell-to-go--active-buffers)
               (not (map-elt agent-shell--state :client)))
      (agent-shell-to-go--dispatch :on-error buffer
                                   "Agent failed to start: No client created"))))

;;; Setup and Teardown

(defvar agent-shell-to-go--advice-installed nil
  "Non-nil when advice has been installed.")

(defun agent-shell-to-go--install-advice ()
  "Install advice on agent-shell functions."
  (unless agent-shell-to-go--advice-installed
    (advice-add 'agent-shell--send-command :around #'agent-shell-to-go--on-send-command)
    (advice-add 'agent-shell--on-notification :around #'agent-shell-to-go--on-notification)
    (advice-add 'agent-shell--on-request :around #'agent-shell-to-go--on-request)
    (advice-add 'agent-shell-heartbeat-stop :around #'agent-shell-to-go--on-heartbeat-stop)
    (advice-add 'agent-shell--initialize-client :after #'agent-shell-to-go--on-client-initialized)
    (setq agent-shell-to-go--advice-installed t)
    (agent-shell-to-go--debug "advice installed")))

(defun agent-shell-to-go--remove-advice ()
  "Remove advice from agent-shell functions."
  (when agent-shell-to-go--advice-installed
    (advice-remove 'agent-shell--send-command #'agent-shell-to-go--on-send-command)
    (advice-remove 'agent-shell--on-notification #'agent-shell-to-go--on-notification)
    (advice-remove 'agent-shell--on-request #'agent-shell-to-go--on-request)
    (advice-remove 'agent-shell-heartbeat-stop #'agent-shell-to-go--on-heartbeat-stop)
    (advice-remove 'agent-shell--initialize-client #'agent-shell-to-go--on-client-initialized)
    (setq agent-shell-to-go--advice-installed nil)
    (agent-shell-to-go--debug "advice removed")))

(defun agent-shell-to-go-core-setup ()
  "Set up the core module. Call this before registering transports."
  (agent-shell-to-go--install-advice)
  ;; Initialize transports
  (dolist (transport agent-shell-to-go--transports)
    (when-let ((init (plist-get transport :init)))
      (condition-case err
          (funcall init)
        (error
         (agent-shell-to-go--debug "%s init failed: %s"
                                   (plist-get transport :name) err))))))

(defun agent-shell-to-go-core-teardown ()
  "Tear down the core module."
  ;; End all sessions
  (dolist (buffer agent-shell-to-go--active-buffers)
    (when (buffer-live-p buffer)
      (agent-shell-to-go--end-session buffer)))
  ;; Cleanup transports
  (dolist (transport agent-shell-to-go--transports)
    (when-let ((cleanup (plist-get transport :cleanup)))
      (condition-case err
          (funcall cleanup)
        (error
         (agent-shell-to-go--debug "%s cleanup failed: %s"
                                   (plist-get transport :name) err)))))
  (agent-shell-to-go--remove-advice))

;;; Utilities for transports

(defun agent-shell-to-go--get-project-path (&optional buffer)
  "Get the project path for BUFFER (or current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (or (and (fboundp 'projectile-project-root) (projectile-project-root))
        (and (fboundp 'project-current)
             (when-let ((proj (project-current)))
               (if (fboundp 'project-root)
                   (project-root proj)
                 (car (project-roots proj)))))
        default-directory)))

(defun agent-shell-to-go--get-project-name (&optional buffer)
  "Get the project name for BUFFER (or current buffer)."
  (file-name-nondirectory
   (directory-file-name
    (agent-shell-to-go--get-project-path buffer))))

(defun agent-shell-to-go--truncate (text max-len)
  "Truncate TEXT to MAX-LEN characters."
  (if (> (length text) max-len)
      (concat (substring text 0 max-len) "...")
    text))

(provide 'agent-shell-to-go-core)

;;; agent-shell-to-go-core.el ends here
