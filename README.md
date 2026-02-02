# agent-shell-to-go

Take your [agent-shell](https://github.com/xenodium/agent-shell) sessions anywhere. Chat with your AI agents from your phone or any device.

| Emacs | Slack (message from phone) | Slack (follow-up from Emacs) |
|-------|---------------------------|------------------------------|
| ![Emacs](screenshot-emacs.png) | ![Slack 1](screenshot-slack-1.png) | ![Slack 2](screenshot-slack-2.png) |

## Overview

agent-shell-to-go mirrors your agent-shell conversations to external messaging platforms, enabling bidirectional communication. Send messages from your phone, approve permissions on the go, and monitor your AI agents from anywhere.

Currently supported:
- **Slack** (via Socket Mode)

Planned/possible integrations:
- Matrix
- Discord
- Telegram

## Features

- **Per-project channels** - each project gets its own Slack channel automatically
- Each agent-shell session gets its own thread within the project channel
- Messages flow bidirectionally (Emacs ↔ messaging platform)
- Real-time updates via WebSocket
- Permission requests with reaction-based approval
- Mode switching via commands (`!yolo`, `!safe`, `!plan`)
- Start new agents remotely via slash commands
- Works with any agent-shell agent (Claude Code, Gemini, etc.)

## Security Warning

**Anyone with access to your messaging channel can control your agent-shell sessions.** This includes:
- Sending prompts to Claude Code running on your machine
- Approving permission requests (file edits, command execution, etc.)
- Starting new agent sessions via slash commands

This is powerful but risky. Be mindful of who has access to your channel. Consider using a private channel with restricted membership.

## Slack Setup

### 1. Create a Slack App

#### Quick setup (recommended)

1. Go to https://api.slack.com/apps
2. Click "Create New App" → "From an app manifest"
3. Select your workspace
4. Paste the contents of [`slack-app-manifest.yaml`](./slack-app-manifest.yaml)
5. Click "Create"
6. Go to "OAuth & Permissions" → "Install to Workspace" → copy the Bot Token (`xoxb-...`)
7. Go to "Basic Information" → "App-Level Tokens" → "Generate Token" with `connections:write` scope → copy it (`xapp-...`)
8. Get your channel ID (right-click channel → "View channel details" → scroll to bottom)
9. Invite the bot to your channel: `/invite @agent-shell-to-go`

Skip to [Configure credentials](#2-configure-credentials).

#### Manual setup

<details>
<summary>Click to expand step-by-step guide</summary>

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

</details>

### 2. Configure credentials

**These credentials are extremely sensitive.** Anyone with these tokens can send messages to your Slack channel - and your Emacs will execute them as agent-shell prompts. Treat them like SSH keys.

#### Option A: Using pass (recommended)

```elisp
(setq agent-shell-to-go-bot-token (string-trim (shell-command-to-string "pass slack/agent-shell-bot-token")))
(setq agent-shell-to-go-channel-id (string-trim (shell-command-to-string "pass slack/agent-shell-channel-id")))
(setq agent-shell-to-go-app-token (string-trim (shell-command-to-string "pass slack/agent-shell-app-token")))
```

#### Option B: Using macOS Keychain

```elisp
(defun my/keychain-get (service account)
  (string-trim (shell-command-to-string
                (format "security find-generic-password -s '%s' -a '%s' -w" service account))))

(setq agent-shell-to-go-bot-token (my/keychain-get "agent-shell-to-go" "bot-token"))
(setq agent-shell-to-go-channel-id (my/keychain-get "agent-shell-to-go" "channel-id"))
(setq agent-shell-to-go-app-token (my/keychain-get "agent-shell-to-go" "app-token"))
(setq agent-shell-to-go-user-id (my/keychain-get "agent-shell-to-go" "user-id"))
```

To add credentials to Keychain:
```bash
security add-generic-password -s "agent-shell-to-go" -a "bot-token" -w "xoxb-your-token"
security add-generic-password -s "agent-shell-to-go" -a "channel-id" -w "C0123456789"
security add-generic-password -s "agent-shell-to-go" -a "app-token" -w "xapp-your-token"
security add-generic-password -s "agent-shell-to-go" -a "user-id" -w "U0123456789"
```

To get your user ID: click your profile in Slack → three dots → "Copy member ID".

#### Option C: Using .env file (less secure)

Create a `.env` file (default: `~/.doom.d/.env`):

```
SLACK_BOT_TOKEN=xoxb-your-bot-token
SLACK_CHANNEL_ID=C0123456789
SLACK_APP_TOKEN=xapp-your-app-token
```

Make sure this file is gitignored if your config is in a repository.

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

### Reactions

React to messages in the thread:

**Permission requests:**
| Emoji | Action |
|-------|--------|
| :white_check_mark: or :+1: | Allow once |
| :unlock: or :star: | Always allow |
| :x: or :-1: | Reject |

**Message visibility:**
| Emoji | Action |
|-------|--------|
| :see_no_evil: or :no_bell: | Hide message completely (remove to unhide) |
| :eyes: | Expand truncated message (remove to re-truncate) |

Long messages are automatically truncated to 500 characters with `:eyes: _for more_` at the end. Add the :eyes: reaction to see the full content.

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

## Roadmap

- [ ] Image fetching - view images/screenshots from agent responses (via `files.upload` API, needs `files:write` scope)
- [ ] Bookmarks - bookmark interesting messages, retrieve with `/bookmarks`
- [ ] Better UTF-8 and unicode handling (currently stripped to ASCII)
- [x] Per-project channels - each project gets its own Slack channel automatically
- [ ] Matrix integration
- [ ] Discord integration
- [ ] Telegram integration

## License

GPL-3.0
