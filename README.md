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
- **Message queuing** - messages sent while the agent is busy are queued and processed automatically
- Permission requests with reaction-based approval
- Mode switching via commands (`!yolo`, `!safe`, `!plan`)
- Start new agents remotely via slash commands
- **Image uploads** - images created anywhere in the project are automatically uploaded to Slack (requires `fswatch`)
- Works with any agent-shell agent (Claude Code, Gemini, etc.)

## Security

No one can interact with your agents until you explicitly set an allowlist. Additionally, it is recommended to limit workspace membership to just you and your agent.

### Authorized Users (required)

You **must** set an allowlist of Slack user IDs who can interact with your agents:

```elisp
;; Allow specific users to control agents
(setq agent-shell-to-go-authorized-users '("U01234567" "U89ABCDEF"))

;; Or if you already have user-id configured, reuse it:
(setq agent-shell-to-go-authorized-users (list agent-shell-to-go-user-id))
```

Without this setting, all Slack interactions are ignored.

Unauthorized users:
- Cannot send messages to agent threads (silently ignored)
- Cannot use reactions to approve permissions or control messages
- Cannot use slash commands (get an ephemeral "not authorized" message)

To find your Slack user ID: click your profile → three dots → "Copy member ID".

### Why This Matters

Anyone authorized can:
- Send prompts to Claude Code running on your machine
- Approve permission requests (file edits, command execution, etc.)
- Start new agent sessions via slash commands

### Additional Recommendations

- **Run in a VM** - For maximum isolation, run Emacs and your agents inside a VM. This limits the blast radius if an agent is compromised or tricked into running malicious commands. Or just YOLO.
- **Limit workspace membership** - Only invite trusted people to your Slack workspace. The allowlist protects you, but defense in depth is wise.
- **Opt out of Slack's ML training** - Slack uses customer data for ML features like emoji/channel recommendations. To opt out, Workspace Owners can email `feedback@slack.com` with subject "Slack Global model opt-out request" and your workspace URL. See [Slack's privacy principles](https://slack.com/trust/data-management/privacy-principles).
- Keep your Slack tokens secure (treat them like SSH keys)

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
     - `reaction_removed` - receive reaction removals (for unhide/re-truncate)
   - Click "Save Changes"

5. **Add Slash Commands**
   - In the sidebar, click "Slash Commands"
   - Create these commands:
     - `/new-project` - Description: "Create a new project and start an agent"
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

For automatic image uploads, install fswatch:
```bash
brew install fswatch  # macOS
```

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
| `!stop` | Interrupt the agent |
| `!restart` | Kill and restart agent with transcript |
| `!queue` | Show pending queued messages |
| `!clearqueue` | Clear all pending queued messages |
| `!latest` | Jump to bottom of thread |
| `!debug` | Show session debug info |
| `!help` | Show available commands |

### Slash Commands

Use these anywhere in the channel (not in threads):

| Command | Description |
|---------|-------------|
| `/new-project <name>` | Create a new project folder (runs setup function if configured) and start an agent |
| `/new-agent [folder]` | Start a new agent (defaults to channel's project, then configured folder) |
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

**Feedback:**
| Emoji | Action |
|-------|--------|
| :heart: (or other hearts) | Send appreciation to the agent ("The user heart reacted to: ...") |
| :bookmark: | Create an org-mode TODO file from the message (scheduled for today) |

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

;; Custom function to set up new projects (for /new-project command)
;; Called with (PROJECT-NAME BASE-DIR CALLBACK), should call CALLBACK with project-dir when done
(setq agent-shell-to-go-new-project-function #'my/new-python-project)

;; Directory for bookmark TODOs (default: ~/org/todo/)
(setq agent-shell-to-go-todo-directory "~/org/todo/")

;; Image upload rate limit (default: 30 per minute, nil to disable)
(setq agent-shell-to-go-image-upload-rate-limit 30)
```

## Troubleshooting

### WebSocket keeps disconnecting (reconnect loop)

If you see repeated `WebSocket closed` / `WebSocket disconnect requested, reconnecting...` messages, Slack is actively rejecting the connection. Common causes:

1. **Events not enabled** - Go to your Slack app settings → "Event Subscriptions" → make sure "Enable Events" is toggled ON
2. **Missing event subscriptions** - Under "Subscribe to bot events", verify you have `message.channels`, `reaction_added`, and `reaction_removed`
3. **App token expired** - Regenerate the app-level token in "Basic Information" → "App-Level Tokens"

To debug, enable logging:
```elisp
(setq agent-shell-to-go-debug t)
```

Then check `*Messages*` buffer for `agent-shell-to-go:` prefixed logs.

### Agent gets stuck when writing to new directories

If your agent gets stuck when trying to create a file in a directory that doesn't exist, Emacs may be prompting for confirmation. Doom Emacs has a `doom-create-missing-directories-h` hook that prompts `y-or-n-p` before creating directories.

To auto-create directories without prompting, add this to your config:

```elisp
;; Override Doom's directory creation to not prompt (for agent-shell compatibility)
(advice-add 'doom-create-missing-directories-h :override
            (lambda ()
              (unless (file-remote-p buffer-file-name)
                (let ((parent-directory (file-name-directory buffer-file-name)))
                  (when (and parent-directory (not (file-directory-p parent-directory)))
                    (make-directory parent-directory 'parents)
                    t)))))
```

## Roadmap

- [x] Image uploads - images written by the agent are automatically uploaded to Slack
- [x] Bookmarks - bookmark reaction creates org-mode TODO scheduled for today
- [x] Better UTF-8 and unicode handling (now uses curl)
- [x] Per-project channels - each project gets its own Slack channel automatically
- [x] Message queuing - messages sent while agent is busy are queued automatically
- [ ] Cloudflare Worker relay - Slack's Socket Mode requires your laptop to be online; when it sleeps or loses WiFi, Slack accumulates delivery failures and eventually disables the app. A Cloudflare Worker relay would maintain the Slack Socket Mode connection 24/7, queue messages while you're offline, and forward them when Emacs reconnects.
- [ ] Meta-Claude channel - a `#claude-meta` channel with an always-on Claude that knows about active sessions, can summarize recent activity, route tasks to the right project, and orchestrate across sessions
- [ ] Matrix integration
- [ ] Discord integration
- [ ] Telegram integration

## License

GPL-3.0
