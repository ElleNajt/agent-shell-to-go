# agent-shell-to-go

Take your [agent-shell](https://github.com/xenodium/agent-shell) sessions anywhere. Chat with your AI agents from your phone via Slack.

## Features

- Each agent-shell session gets its own Slack thread
- Messages you send from Emacs appear in Slack
- Messages you send from Slack get injected back into agent-shell
- Real-time via Slack Socket Mode (WebSocket) - no polling delay
- Permission requests appear in Slack with emoji reactions to approve/reject
- Mode switching via commands (`!yolo`, `!safe`, `!plan`)
- Slash commands to start new agents from Slack (`/new-agent`, `/projects`)
- Works with any agent-shell agent (Claude Code, Gemini, etc.)

## Security Warning

**Anyone with access to your Slack channel can control your agent-shell sessions.** This includes:
- Sending prompts to Claude Code running on your machine
- Approving permission requests (file edits, command execution, etc.)
- Starting new agent sessions via slash commands

This is powerful but risky. Be mindful of who has access to your Slack channel. Consider using a private channel with restricted membership.

## Setup

### 1. Create a Slack App

#### Step-by-step guide

1. **Create the app**
   - Go to https://api.slack.com/apps
   - Click "Create New App" → "From scratch"
   - Name it something like "agent-shell-to-go"
   - Select your workspace

2. **Enable Socket Mode**
   - In the sidebar, click "Socket Mode"
   - Toggle "Enable Socket Mode" ON
   - When prompted, create an app-level token:
     - Name it "websocket" (or anything)
     - Add the `connections:write` scope
     - Click "Generate"
   - **Save this token** (starts with `xapp-`) - you'll need it later

3. **Add Bot Token Scopes**
   - In the sidebar, click "OAuth & Permissions"
   - Scroll to "Scopes" → "Bot Token Scopes"
   - Add these scopes:
     - `chat:write` - send messages
     - `channels:history` - read channel messages
     - `channels:read` - see channel info
     - `reactions:read` - see emoji reactions

4. **Subscribe to Events**
   - In the sidebar, click "Event Subscriptions"
   - Toggle "Enable Events" ON
   - Expand "Subscribe to bot events"
   - Add these events:
     - `message.channels` - receive messages in channels
     - `reaction_added` - receive emoji reactions
   - Click "Save Changes"

5. **Add Slash Commands**
   - In the sidebar, click "Slash Commands"
   - Create these commands:
     - `/new-agent` - Description: "Start new agent in a folder"
     - `/new-agent-container` - Description: "Start new agent in a container"
     - `/projects` - Description: "List open projects from Emacs"

6. **Install the App**
   - In the sidebar, click "Install App"
   - Click "Install to Workspace"
   - Review permissions and click "Allow"
   - **Copy the "Bot User OAuth Token"** (starts with `xoxb-`)

7. **Set up your channel**
   - Create a channel or use an existing one (e.g., `#agent-shell`)
   - Invite the bot: type `/invite @your-bot-name` in the channel
   - Get the channel ID:
     - Right-click the channel name → "View channel details"
     - Scroll to the bottom and copy the Channel ID (starts with `C`)

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

### Slash Commands

Use these anywhere in the channel (not in threads):

| Command | Description |
|---------|-------------|
| `/new-agent [folder]` | Start a new agent in a folder (defaults to configured folder) |
| `/new-agent-container [folder]` | Start a new agent in a container (like `C-u` prefix) |
| `/projects` | List open projects from Emacs (each as a separate message for easy copy) |

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

;; Default folder for /new-agent when no folder is specified
(setq agent-shell-to-go-default-folder "~/code")

;; Custom function to start agents (e.g., your own claude-code wrapper)
(setq agent-shell-to-go-start-agent-function #'my/start-claude-code)
```

## License

GPL-3.0
