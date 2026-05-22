;;; config.el --- Example config for scripts/run-agent  -*- lexical-binding: t; -*-
;;
;; Copy to ~/.config/agent-shell-to-go/config.el and fill in your values.
;; Environment variables (SLACK_BOT_TOKEN, etc.) can be used to override anything set here.

;;; Slack

(setq agent-shell-to-go-slack-bot-token
      (string-trim (shell-command-to-string "pass slack/agent-shell-bot-token")))
(setq agent-shell-to-go-slack-app-token
      (string-trim (shell-command-to-string "pass slack/agent-shell-app-token")))
(setq agent-shell-to-go-slack-channel-id
      (string-trim (shell-command-to-string "pass slack/agent-shell-channel-id")))

;; Your Slack user ID (profile -> ⋯ -> Copy member ID)
(setq agent-shell-to-go-slack-authorized-users '("U01234567"))

;;; Discord

(setq agent-shell-to-go-discord-bot-token
      (string-trim (shell-command-to-string "pass discord/agent-shell-bot-token")))
(setq agent-shell-to-go-discord-guild-id
      (string-trim (shell-command-to-string "pass discord/agent-shell-guild-id")))
(setq agent-shell-to-go-discord-channel-id
      (string-trim (shell-command-to-string "pass discord/agent-shell-channel-id")))

;; Your Discord user ID (right-click your name → Copy User ID, with Developer Mode on)
(setq agent-shell-to-go-discord-authorized-users '("123456789012345678"))

;;; agent-shell

;; Authentication: agent-shell runs the underlying agent (claude-code, gemini-cli,
;; etc.) as a subprocess.  Any credentials the agent needs must be available in the
;; environment before run-agent starts.  For Claude Code that means ANTHROPIC_API_KEY;
;; for Gemini CLI it means GOOGLE_CLOUD_PROJECT / gcloud ADC; for codex it means
;; OPENAI_API_KEY.  Export them in your shell profile, pass them via
;; agent-shell-command-prefix ("env" "KEY=val" ...), or load them from your secrets
;; manager before this config is evaluated.

;; Which agent to use — skips the selection prompt on startup.
;; Common values: claude-code, gemini-cli, goose, opencode, codex
(setq agent-shell-preferred-agent-config 'claude-code)

;; Session strategy when the agent starts.
;; Use 'new to always start fresh; 'latest to resume the last session.
(setq agent-shell-session-strategy 'new)

;; Prefix every agent command with these args — useful for Docker/devcontainer.
;; (setq agent-shell-command-prefix '("docker" "exec" "-i" "my-container" "--"))

;;; General

;; Which transport to use by default
(setq agent-shell-to-go-default-transport 'slack)
