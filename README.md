# agent-shell-to-go

Take your [agent-shell](https://github.com/xenodium/agent-shell) sessions anywhere. Chat with your AI agents from your phone via Slack.

## Features

- Each agent-shell session gets its own Slack thread
- Messages you send from Emacs appear in Slack
- Messages you send from Slack get injected back into agent-shell
- Works with any agent-shell agent (Claude Code, Gemini, etc.)

## Setup

### 1. Create a Slack App

1. Go to https://api.slack.com/apps and create a new app
2. Add these Bot Token Scopes under OAuth & Permissions:
   - `chat:write`
   - `channels:history`
   - `channels:read`
3. Install the app to your workspace
4. Copy the Bot User OAuth Token (starts with `xoxb-`)
5. Invite the bot to your channel (`/invite @your-bot-name`)
6. Get the channel ID (right-click channel > View channel details > copy ID at bottom)

### 2. Configure credentials

Create a `.env` file (default: `~/.doom.d/.env`):

```
SLACK_BOT_TOKEN=xoxb-your-token-here
SLACK_CHANNEL_ID=C0123456789
```

### 3. Add to your Emacs config

```elisp
(use-package agent-shell-to-go
  :load-path "~/code/agent-shell-to-go"
  :after agent-shell
  :config
  (agent-shell-to-go-setup))
```

## Usage

Once set up, every new agent-shell session automatically:
1. Creates a Slack thread
2. Mirrors your conversation in real-time
3. Polls for replies from Slack and injects them into your session

You can now chat with Claude (or any agent) from your phone while away from your computer.

## Customization

```elisp
;; Change the .env file location
(setq agent-shell-to-go-env-file "~/.config/agent-shell/.env")

;; Change polling interval (default 5 seconds)
(setq agent-shell-to-go-poll-interval 3)

;; Or set credentials directly (not recommended)
(setq agent-shell-to-go-bot-token "xoxb-...")
(setq agent-shell-to-go-channel-id "C...")
```

## License

GPL-3.0
