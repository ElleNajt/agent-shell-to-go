;;; agent-shell-to-go.el --- Take your agent-shell sessions anywhere -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Elle Najt

;; Author: Elle Najt
;; Maintainer: junyi.hou <junyi.yi.hou@gmail.com>
;; URL: https://github.com/ElleNajt/agent-shell-to-go
;; Version: 0.4.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.52.1") (acp "0.12.1") (shell-maker "0.91.2") (websocket "1.16"))
;; Keywords: convenience, tools, ai

;; This file is not part of GNU Emacs.

;;; Commentary:

;; agent-shell-to-go mirrors your agent-shell conversations to a remote
;; messaging transport, letting you interact with your AI agents from
;; your phone or any device.
;;
;; Features:
;; - Each agent-shell session gets its own remote thread
;; - Messages you send from Emacs appear remotely
;; - Messages from the remote transport get injected back into agent-shell
;; - Transport-agnostic core with pluggable backends (Slack, Discord)
;; - Works with any agent-shell agent (Claude, Gemini, etc.)
;;
;; Architecture:
;; - agent-shell-to-go-core.el          shared protocol, defcustoms, utilities
;; - agent-shell-to-go-slack.el         Slack transport implementation
;; - agent-shell-to-go-discord.el       Discord transport implementation
;; - agent-shell-to-go-bridge.el        agent-shell integration
;; - agent-shell-to-go.el               entry point: minor mode + public API
;;
;; Quick start:
;;    (use-package agent-shell-to-go
;;      :after agent-shell
;;      :config
;;      (setq agent-shell-to-go-slack-bot-token "xoxb-...")
;;      (setq agent-shell-to-go-slack-channel-id "C...")
;;      (setq agent-shell-to-go-slack-app-token "xapp-...")
;;      (setq agent-shell-to-go-slack-authorized-users '("U..."))
;;      (agent-shell-to-go-setup))
;;
;; See README.md for full setup instructions.

;;; Code:

(require 'agent-shell-to-go-core)
(require 'agent-shell-to-go-slack)
(require 'agent-shell-to-go-discord)
(require 'agent-shell-to-go-bridge)

; Minor mode

(declare-function agent-shell-to-go--bridge-enable "agent-shell-to-go-bridge" ())
(declare-function agent-shell-to-go--bridge-disable "agent-shell-to-go-bridge" ())

;;;###autoload
(define-minor-mode agent-shell-to-go-mode
  "Mirror agent-shell conversations to a remote transport.
Take your AI agent sessions anywhere — chat from your phone!"
  :lighter " ToGo"
  :group
  'agent-shell-to-go
  (if agent-shell-to-go-mode
      (agent-shell-to-go--bridge-enable)
    (agent-shell-to-go--bridge-disable)))

;;;###autoload
(defun agent-shell-to-go-auto-enable ()
  "Automatically enable mirroring for agent-shell buffers."
  (when (derived-mode-p 'agent-shell-mode)
    (agent-shell-to-go-mode 1)))

;;;###autoload
(defun agent-shell-to-go-setup ()
  "Set up automatic mirroring for all agent-shell sessions.
Connects each active transport eagerly so inbound events work
before any local agent starts."
  (add-hook 'agent-shell-mode-hook #'agent-shell-to-go-auto-enable)
  (dolist (transport (agent-shell-to-go--all-transport-objects))
    (unless (agent-shell-to-go-transport-connected-p transport)
      (condition-case err
          (agent-shell-to-go-transport-connect transport)
        (error
         (agent-shell-to-go--debug
          "failed to connect %s: %s"
          (agent-shell-to-go-transport-name transport)
          err))))))

;;;###autoload
(defun agent-shell-to-go-show-debug-log ()
  "Display the debug log buffer."
  (interactive)
  (display-buffer (get-buffer-create agent-shell-to-go--debug-buffer-name)))

; Public API — connection management

(declare-function agent-shell-to-go--bridge-reconnect-buffer "agent-shell-to-go-bridge"
                  (&optional buffer))
(declare-function agent-shell-to-go--bridge-buffer-connected-p
                  "agent-shell-to-go-bridge"
                  (&optional buffer))
(declare-function agent-shell-to-go--bridge-thread-active-p "agent-shell-to-go-bridge"
                  (transport channel thread-id))

;;;###autoload
(defun agent-shell-to-go-reconnect-buffer (&optional buffer)
  "Reconnect BUFFER (or current buffer) to its transport with a fresh thread."
  (interactive)
  (agent-shell-to-go--bridge-reconnect-buffer buffer))

;;;###autoload
(defun agent-shell-to-go-ensure-connected (&optional buffer)
  "Ensure BUFFER (or current buffer) is connected to its transport.
Idempotent.  Returns t if already connected, `connected' if newly
connected, nil on failure."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (if (agent-shell-to-go--bridge-buffer-connected-p buf)
        t
      (condition-case err
          (progn
            (agent-shell-to-go--bridge-reconnect-buffer buf)
            'connected)
        (error
         (agent-shell-to-go--debug "failed to connect %s: %s" (buffer-name buf) err)
         nil)))))

;;;###autoload
(defun agent-shell-to-go-ensure-all-connected ()
  "Ensure all agent-shell buffers are connected to their transports."
  (interactive)
  (dolist (transport (agent-shell-to-go--all-transport-objects))
    (unless (agent-shell-to-go-transport-connected-p transport)
      (agent-shell-to-go--debug
       "%s transport unhealthy, reconnecting…"
       (agent-shell-to-go-transport-name transport))
      (condition-case err
          (agent-shell-to-go-transport-connect transport)
        (error
         (agent-shell-to-go--debug "reconnect failed: %s" err)))))
  (let ((connected 0)
        (already 0)
        (failed 0)
        (bufs
         (cl-remove-if-not
          (lambda (b)
            (and (buffer-live-p b)
                 (with-current-buffer b
                   (derived-mode-p 'agent-shell-mode))))
          (buffer-list))))
    (dolist (buf bufs)
      (pcase (agent-shell-to-go-ensure-connected buf)
        ('t (cl-incf already))
        ('connected (cl-incf connected))
        (_ (cl-incf failed))))
    (when (or (> connected 0) (> failed 0))
      (agent-shell-to-go--debug
       "%d newly connected, %d already, %d failed" connected already failed))))

(defvar agent-shell-to-go--ensure-timer nil
  "Timer for periodic connection checks.")

;;;###autoload
(defun agent-shell-to-go-start-periodic-check (&optional interval)
  "Start periodic check to ensure all buffers stay connected.
INTERVAL is seconds between checks (default 60)."
  (interactive)
  (agent-shell-to-go-stop-periodic-check)
  (setq agent-shell-to-go--ensure-timer
        (run-with-timer 0 (or interval 60) #'agent-shell-to-go-ensure-all-connected))
  (agent-shell-to-go--debug "periodic connection check every %ds" (or interval 60)))

;;;###autoload
(defun agent-shell-to-go-stop-periodic-check ()
  "Stop periodic connection checks."
  (interactive)
  (when agent-shell-to-go--ensure-timer
    (cancel-timer agent-shell-to-go--ensure-timer)
    (setq agent-shell-to-go--ensure-timer nil)
    (agent-shell-to-go--debug "periodic check stopped")))

;;;###autoload
(defun agent-shell-to-go-reconnect-all ()
  "Reconnect all agent-shell buffers to their transports (new threads)."
  (interactive)
  (let ((reconnected 0)
        (bufs
         (cl-remove-if-not
          (lambda (b)
            (and (buffer-live-p b)
                 (with-current-buffer b
                   (derived-mode-p 'agent-shell-mode))))
          (buffer-list))))
    (dolist (buf bufs)
      (condition-case err
          (progn
            (agent-shell-to-go--bridge-reconnect-buffer buf)
            (cl-incf reconnected))
        (error
         (agent-shell-to-go--debug
          "failed to reconnect %s: %s" (buffer-name buf) err))))
    (agent-shell-to-go--debug "reconnected %d/%d buffers" reconnected (length bufs))))

;;;###autoload
(defun agent-shell-to-go-list-threads (&optional channel-id)
  "List threads across all configured transports, or just CHANNEL-ID if given.
Reports to the *Agent Shell Threads* buffer."
  (interactive)
  (let ((now (float-time))
        (buf (get-buffer-create "*Agent Shell Threads*")))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (transport (agent-shell-to-go--all-transport-objects))
        (let* ((name (agent-shell-to-go-transport-name transport))
               (channel-id
                (or channel-id
                    (ignore-errors
                      (agent-shell-to-go-transport-ensure-project-channel
                       transport default-directory)))))
          (when channel-id
            (let ((threads
                   (ignore-errors
                     (agent-shell-to-go-transport-list-threads transport channel-id))))
              (insert (format "=== %s :: %s ===\n" name channel-id))
              (if (not threads)
                  (insert "(no threads)\n\n")
                (dolist (thread
                         (sort threads
                               (lambda (a b)
                                 (> (or (map-elt a :last-timestamp) 0)
                                    (or (map-elt b :last-timestamp) 0)))))
                  (let* ((ts (map-elt thread :thread-id))
                         (last (or (map-elt thread :last-timestamp) 0))
                         (age-h (/ (- now last) 3600.0)))
                    (insert (format "  %s  %.1fh ago\n" ts age-h))))
                (insert (format "\n%d threads total\n\n" (length threads))))))))
      (goto-char (point-min))
      (display-buffer buf))))

;;;###autoload
(defun agent-shell-to-go-cleanup-old-threads (&optional channel-id dry-run)
  "Delete threads older than `agent-shell-to-go-cleanup-age-hours' across all configured transports.
Skips threads that are currently active (have a live buffer).
Signal prefix arg or DRY-RUN to only report."
  (interactive (list nil current-prefix-arg))
  (let ((now (float-time))
        (threshold-secs (* agent-shell-to-go-cleanup-age-hours 3600))
        (total-deleted 0)
        (total-skipped-active 0)
        (total-skipped-recent 0))
    (dolist (transport (agent-shell-to-go--all-transport-objects))
      (let* ((channel-id
              (or channel-id
                  (ignore-errors
                    (agent-shell-to-go-transport-ensure-project-channel
                     transport default-directory))))
             (threads
              (and channel-id
                   (ignore-errors
                     (agent-shell-to-go-transport-list-threads transport channel-id))))
             (to-delete nil))
        (dolist (thread threads)
          (let* ((ts (map-elt thread :thread-id))
                 (last (or (map-elt thread :last-timestamp) 0))
                 (age (- now last))
                 (active-p
                  (agent-shell-to-go--bridge-thread-active-p transport channel-id ts)))
            (cond
             (active-p
              (cl-incf total-skipped-active))
             ((< age threshold-secs)
              (cl-incf total-skipped-recent))
             (t
              (push ts to-delete)))))
        (when (and to-delete (not dry-run))
          (dolist (thread-id to-delete)
            (condition-case err
                (progn
                  (agent-shell-to-go-transport-delete-thread
                   transport channel-id thread-id)
                  (cl-incf total-deleted))
              (error
               (agent-shell-to-go--debug "delete-thread failed: %s" err)))))))
    (agent-shell-to-go--debug
     "cleanup: %s %d threads (active %d, recent %d)"
     (if dry-run
         "would delete"
       "deleted")
     total-deleted total-skipped-active total-skipped-recent)))

;;;###autoload
(defun agent-shell-to-go-cleanup-all-channels (&optional dry-run)
  "Clean up old threads across all transports' known channels.
With prefix arg or DRY-RUN non-nil, just report."
  (interactive "P")
  (agent-shell-to-go-cleanup-old-threads nil dry-run))

(provide 'agent-shell-to-go)
;;; agent-shell-to-go.el ends here
