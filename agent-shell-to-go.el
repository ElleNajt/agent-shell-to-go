;;; agent-shell-to-go.el --- Take your agent-shell sessions anywhere -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Elle Najt

;; Author: Elle Najt
;; URL: https://github.com/ElleNajt/agent-shell-to-go
;; Version: 0.3.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.33.1") (websocket "1.14"))
;; Keywords: convenience, tools, ai

;; This file is not part of GNU Emacs.

;;; Commentary:

;; agent-shell-to-go mirrors your agent-shell conversations to external services,
;; letting you interact with your AI agents from your phone or any device.
;;
;; Features:
;; - Pluggable transport system (Slack, mobile app backend, etc.)
;; - Each agent-shell session gets mirrored to enabled transports
;; - Messages you send from Emacs appear in external clients
;; - Messages from external clients get injected into agent-shell
;;
;; Quick start:
;;    (use-package agent-shell-to-go
;;      :after agent-shell
;;      :config
;;      ;; For Slack:
;;      (setq agent-shell-to-go-slack-bot-token "xoxb-...")
;;      (setq agent-shell-to-go-slack-channel-id "C...")
;;      (setq agent-shell-to-go-slack-app-token "xapp-...")
;;      (setq agent-shell-to-go-slack-authorized-users '("U..."))
;;
;;      ;; For mobile app backend (optional):
;;      (setq agent-shell-to-go-backend-url "http://100.x.x.x:8080")
;;      (setq agent-shell-to-go-backend-token "secret")
;;
;;      (agent-shell-to-go-setup))

;;; Code:

(require 'agent-shell-to-go-core)

;; Backwards compatibility: alias old variable names to new ones
(defvaralias 'agent-shell-to-go-bot-token 'agent-shell-to-go-slack-bot-token)
(defvaralias 'agent-shell-to-go-channel-id 'agent-shell-to-go-slack-channel-id)
(defvaralias 'agent-shell-to-go-app-token 'agent-shell-to-go-slack-app-token)
(defvaralias 'agent-shell-to-go-authorized-users 'agent-shell-to-go-slack-authorized-users)
(defvaralias 'agent-shell-to-go-env-file 'agent-shell-to-go-slack-env-file)
(defvaralias 'agent-shell-to-go-per-project-channels 'agent-shell-to-go-slack-per-project-channels)
(defvaralias 'agent-shell-to-go-channel-prefix 'agent-shell-to-go-slack-channel-prefix)
(defvaralias 'agent-shell-to-go-channels-file 'agent-shell-to-go-slack-channels-file)
(defvaralias 'agent-shell-to-go-user-id 'agent-shell-to-go-slack-user-id)
(defvaralias 'agent-shell-to-go-show-tool-output 'agent-shell-to-go-slack-show-tool-output)
(defvaralias 'agent-shell-to-go-default-folder 'agent-shell-to-go-slack-default-folder)
(defvaralias 'agent-shell-to-go-start-agent-function 'agent-shell-to-go-slack-start-agent-function)
(defvaralias 'agent-shell-to-go-hidden-messages-dir 'agent-shell-to-go-slack-hidden-messages-dir)
(defvaralias 'agent-shell-to-go-truncated-messages-dir 'agent-shell-to-go-slack-truncated-messages-dir)
(defvaralias 'agent-shell-to-go-todo-directory 'agent-shell-to-go-slack-todo-directory)
(defvaralias 'agent-shell-to-go-projects-directory 'agent-shell-to-go-slack-projects-directory)
(defvaralias 'agent-shell-to-go-new-project-function 'agent-shell-to-go-slack-new-project-function)

;;; Minor Mode

(defun agent-shell-to-go--on-buffer-kill ()
  "Hook to run when an agent-shell buffer is killed."
  (when agent-shell-to-go-mode
    (agent-shell-to-go--end-session (current-buffer))))

;;;###autoload
(define-minor-mode agent-shell-to-go-mode
  "Mirror agent-shell conversations to external services.
Take your AI agent sessions anywhere - chat from your phone!"
  :lighter " ToGo"
  :group 'agent-shell-to-go
  (if agent-shell-to-go-mode
      (progn
        (add-hook 'kill-buffer-hook #'agent-shell-to-go--on-buffer-kill nil t)
        (agent-shell-to-go--start-session (current-buffer)))
    (remove-hook 'kill-buffer-hook #'agent-shell-to-go--on-buffer-kill t)
    (agent-shell-to-go--end-session (current-buffer))))

;;;###autoload
(defun agent-shell-to-go-auto-enable ()
  "Automatically enable mirroring for agent-shell buffers."
  (when (derived-mode-p 'agent-shell-mode)
    (agent-shell-to-go-mode 1)))

;;;###autoload
(defun agent-shell-to-go-setup ()
  "Set up agent-shell-to-go with enabled transports."
  (interactive)
  
  ;; Load Slack transport if configured
  (when (bound-and-true-p agent-shell-to-go-slack-bot-token)
    (require 'agent-shell-to-go-slack)
    (agent-shell-to-go-slack-setup))

  ;; Load backend transport if configured
  (when (bound-and-true-p agent-shell-to-go-backend-url)
    (require 'agent-shell-to-go-backend)
    (agent-shell-to-go-backend-setup))
  
  ;; Initialize core (installs advice, calls transport :init)
  (agent-shell-to-go-core-setup)
  
  ;; Auto-enable for agent-shell buffers
  (add-hook 'agent-shell-mode-hook #'agent-shell-to-go-auto-enable)
  
  (message "agent-shell-to-go: setup complete (%d transports)"
           (length agent-shell-to-go--transports)))

;;;###autoload
(defun agent-shell-to-go-teardown ()
  "Tear down agent-shell-to-go."
  (interactive)
  (remove-hook 'agent-shell-mode-hook #'agent-shell-to-go-auto-enable)
  (agent-shell-to-go-core-teardown)
  (message "agent-shell-to-go: teardown complete"))

;;; Convenience Functions

;;;###autoload
(defun agent-shell-to-go-reconnect-buffer (&optional buffer)
  "Reconnect BUFFER (or current buffer) to all transports."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (unless (buffer-live-p buf)
      (user-error "Buffer is not live"))
    (with-current-buffer buf
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      ;; End existing session and start fresh
      (agent-shell-to-go--end-session buf)
      (agent-shell-to-go--start-session buf)
      (message "Reconnected %s" (buffer-name buf)))))

;;;###autoload
(defun agent-shell-to-go-reconnect-all ()
  "Reconnect all agent-shell buffers."
  (interactive)
  (let ((count 0))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (with-current-buffer buf
                   (derived-mode-p 'agent-shell-mode)))
        (condition-case err
            (progn
              (agent-shell-to-go-reconnect-buffer buf)
              (cl-incf count))
          (error
           (message "Failed to reconnect %s: %s" (buffer-name buf) err)))))
    (message "Reconnected %d buffers" count)))

(provide 'agent-shell-to-go)

;;; agent-shell-to-go.el ends here
