# agent-shell-to-go

Take your [agent-shell](https://github.com/xenodium/agent-shell) sessions anywhere. Chat with your AI agents from your phone via Slack.

## Features

- Each agent-shell session gets its own Slack thread
- Messages you send from Emacs appear in Slack
- Messages you send from Slack get injected back into agent-shell
- Real-time via Slack Socket Mode (WebSocket) - no polling delay
- Permission requests appear in Slack with emoji reactions to approve/reject
- Mode switching via commands (`!yolo`, `!safe`, `!plan`)
- Works with any agent-shell agent (Claude Code, Gemini, etc.)

## Setup

### 1. Create a Slack App

1. Go to https://api.slack.com/apps and create a new app
2. Add these **Bot Token Scopes** under OAuth & Permissions:
   - `chat:write`
   - `channels:history`
   - `channels:read`
   - `reactions:read`
3. Enable **Socket Mode** (under Socket Mode in sidebar):
   - Generate an app-level token with `connections:write` scope
   - Copy the token (starts with `xapp-`)
4. Add **Event Subscriptions** (under Event Subscriptions > Subscribe to bot events):
   - `message.channels`
   - `reaction_added`
5. Install the app to your workspace
6. Copy the Bot User OAuth Token (starts with `xoxb-`)
7. Invite the bot to your channel (`/invite @your-bot-name`)
8. Get the channel ID (right-click channel > View channel details > copy ID at bottom)

### 2. Configure credentials

Create a `.env` file (default: `~/.doom.d/.env`):

```
SLACK_BOT_TOKEN=xoxb-your-bot-token
SLACK_CHANNEL_ID=C0123456789
SLACK_APP_TOKEN=xapp-your-app-token
```

### 3. Add to your Emacs config

```elisp
(use-package agent-shell-to-go
  :load-path "~/code/agent-shell-to-go"
  :after agent-shell
  :config
  (agent-shell-to-go-setup))
```

Requires the `websocket` package (available on MELPA).

## Usage

Once set up, every new agent-shell session automatically:
1. Creates a Slack thread
2. Connects via WebSocket for real-time updates
3. Mirrors your conversation bidirectionally

You can now chat with Claude (or any agent) from your phone while away from your computer.

### Commands

Send these in the Slack thread to control the session:

| Command | Description |
|---------|-------------|
| `!yolo` | Bypass all permissions (dangerous!) |
| `!safe` | Accept edits mode |
| `!plan` | Plan mode |
| `!mode` | Show current mode |
| `!help` | Show available commands |

### Permission Reactions

When Claude requests permission (e.g., to run a command), you'll see a message in Slack. React with:

| Emoji | Action |
|-------|--------|
| :white_check_mark: or :+1: | Allow once |
| :unlock: or :star: | Always allow |
| :x: or :-1: | Reject |

## Customization

```elisp
;; Change the .env file location
(setq agent-shell-to-go-env-file "~/.config/agent-shell/.env")

;; Or set credentials directly (not recommended)
(setq agent-shell-to-go-bot-token "xoxb-...")
(setq agent-shell-to-go-channel-id "C...")
(setq agent-shell-to-go-app-token "xapp-...")
```

## License

GPL-3.0
