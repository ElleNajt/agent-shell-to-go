;;; agent-shell-to-go.el --- Take your agent-shell sessions anywhere -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Elle Najt

;; Author: Elle Najt
;; URL: https://github.com/ElleNajt/agent-shell-to-go
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.33.1") (websocket "1.14"))
;; Keywords: convenience, tools, ai

;; This file is not part of GNU Emacs.

;;; Commentary:

;; agent-shell-to-go mirrors your agent-shell conversations to a mobile app
;; (Dendrite), letting you interact with your AI agents from your phone.
;;
;; Features:
;; - Real-time message streaming to mobile app
;; - Send messages to agents from your phone
;; - Stop/close/restart agents remotely
;; - Permission handling from mobile
;; - Multi-machine support
;;
;; Quick start:
;;    (use-package agent-shell-to-go
;;      :after agent-shell
;;      :config
;;      (setq agent-shell-to-go-mobile-backend-url "http://100.x.x.x:8080")
;;      (setq agent-shell-to-go-mobile-token "your-secret-token")
;;      (agent-shell-to-go-setup))

;;; Code:

(require 'agent-shell-to-go-mobile)

;;;###autoload
(defun agent-shell-to-go-setup ()
  "Set up agent-shell-to-go."
  (interactive)
  (agent-shell-to-go-mobile-setup))

;;;###autoload
(defun agent-shell-to-go-teardown ()
  "Tear down agent-shell-to-go."
  (interactive)
  (remove-hook 'agent-shell-mode-hook #'agent-shell-to-go-mobile-auto-enable))

(provide 'agent-shell-to-go)

;;; agent-shell-to-go.el ends here
