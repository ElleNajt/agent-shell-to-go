# agent-shell-to-go

> Forked from [ElleNajt's repo](https://github.com/ElleNajt/agent-shell-to-go). The development of the original package has been dropped and replaced by a more generic ACP multiplexer (one agent connecting to multiple clients) and dedicated clients.
>
> I believe that an agent-shell specific tool enabling remote control is still valuable since
> - I do not have a use case where I want to control the agent from both emacs and remote at the same time (or two clients), so a proper multiplexer and a dedicated frontend sounds an overkill to me.
> - My agent-specific config (launch flags, plugins, MCP, environment variables) are all set in emacs and agent-shell. Maintaining a separate configuration for a different agent feels repetitive.
> - It uses popular messaging apps, Slack and Discord (added in this fork) instead of a new, customized mobile client, as the entry point for remote interactions.

Take your [agent-shell](https://github.com/xenodium/agent-shell) sessions anywhere. Chat with your AI agents from your phone or any device.

| Emacs | Slack (message from phone) | Slack (follow-up from Emacs) |
|-------|---------------------------|------------------------------|
| ![Emacs](assets/screenshot-emacs.png) | ![Slack 1](assets/screenshot-slack-1.png) | ![Slack 2](assets/screenshot-slack-2.png) |

## Overview

agent-shell-to-go mirrors your agent-shell conversations to external messaging platforms, enabling bidirectional communication. Send messages from your phone, approve permissions on the go, and monitor your AI agents from anywhere.

Currently supported:
- **Slack** (via Socket Mode)
- **Discord** (via Discord Gateway)

Possible integrations:
- Matrix
- Telegram

## Features

- **Per-project channels** - each project gets its own channel automatically
    - Each agent-shell session gets its own thread within the project channel
    - Messages flow bidirectionally (Emacs <-> messaging platform)
    - Real-time updates via WebSocket
    - Resume past sessions using `/resume`. Lists available sessions via `/sessions`.
- **Message queuing** - messages sent while the agent is busy are queued and processed automatically
    - Permission requests with reaction-based approval
    - Mode switching via commands (`!yolo`, `!safe`, `!plan`)
    - Start new agents remotely via slash commands
- **Error forwarding** - agent startup failures and API errors are automatically reported to the thread
    - Works with any agent-shell agent (Claude Code, Gemini, etc.)

## Setup

### Platform-specific Setup

- [Slack](docs/slack-setup.md)
- [Discord](docs/discord-bot-setup.md)

### Configure Tokens

**These credentials are extremely sensitive.** Anyone with these tokens can send messages to your workspace - and your Emacs will execute them as agent-shell prompts. Treat them like SSH keys.

#### Option A: Using pass (recommended)

```elisp
(setq agent-shell-to-go-slack-bot-token
      (string-trim (shell-command-to-string "pass slack/agent-shell-bot-token")))
(setq agent-shell-to-go-slack-channel-id
      (string-trim (shell-command-to-string "pass slack/agent-shell-channel-id")))
(setq agent-shell-to-go-slack-app-token
      (string-trim (shell-command-to-string "pass slack/agent-shell-app-token")))
      
;; Or if you use discord
(setq agent-shell-to-go-discord-bot-token  ...)
(setq agent-shell-to-go-discord-guild-id   ...)
(setq agent-shell-to-go-discord-channel-id ...)
```

#### Option B: Using macOS Keychain

Add credentials to Keychain:
```bash
security add-generic-password -s "agent-shell-to-go" -a "bot-token" -w "xoxb-your-token" # replace "bot-token" with other fields
```

Retrieve them from keychain

```elisp
(defun my/keychain-get (service account)
  (string-trim (shell-command-to-string
                (format "security find-generic-password -s '%s' -a '%s' -w" service account))))

(setq agent-shell-to-go-slack-bot-token     (my/keychain-get "agent-shell-to-go" "bot-token"))
(setq agent-shell-to-go-slack-channel-id    (my/keychain-get "agent-shell-to-go" "channel-id"))
(setq agent-shell-to-go-slack-app-token     (my/keychain-get "agent-shell-to-go" "app-token"))
(setq agent-shell-to-go-slack-user-id       (my/keychain-get "agent-shell-to-go" "user-id"))

;; Or if you use discord
(setq agent-shell-to-go-discord-bot-token  ...)
(setq agent-shell-to-go-discord-guild-id   ...)
(setq agent-shell-to-go-discord-channel-id ...)
```

### Whitelisting users that can control the agent

You **must** set an allowlist of user IDs who can interact with your agents:

```elisp
(setq agent-shell-to-go-slack-authorized-users '("U01234567" "U89ABCDEF"))

;; Or if you use discord
(setq agent-shell-to-go-discord-authorized-users '("YOUR_DISCORD_USER_ID"))
```

Without this, all interactions are silently ignored. To find your user ID: click your profile -> three dots -> "Copy member ID". Unauthorized users cannot send messages, use reactions, or run slash commands.

Additional recommendations:
- **Run in a VM** - limits blast radius if an agent is tricked into running malicious commands
- **Limit workspace membership** - only invite trusted people; the allowlist protects you, but defense in depth is wise
- **Opt out of platform's data tracking** 
    - Slack: workspace Owners can email `feedback@slack.com` with subject "Slack Global model opt-out request" and your workspace URL. See [Slack's privacy principles](https://slack.com/trust/data-management/privacy-principles).
    - Discord: you can turn off some data sharing in your accout setting, see [this Discord's support article](https://support.discord.com/hc/en-us/articles/360004109911-Data-Privacy-Controls) for more information.

### `init.el` File set up

With `use-package`:

```elisp
(use-package agent-shell-to-go
  :load-path "path/to/agent-shell-to-go"
  :after agent-shell
  :config
  (setq agent-shell-to-go-slack-authorized-users '("U01234567"))
  ;; optionally symlink `scripts/run-agent` to ~/.local/bin
  (make-symbolic-link "path/to/agent-shell-to-go/scripts/run-agent" (expand-file-name "~/.local/bin/run-agent")))
```

## Usage

There are two ways that I use this package:

- On my laptop in an interactive agent-shell session: if I need to be AFK, then I can `M-x switch-to-buffer` to an agent-shell buffer and `M-x agent-shell-to-go-mode` and use my phone to continue interacting with the agent.
    - This method requires that the laptop stays awake and connected. Otherwise `agent-shell` will lose connection to either the LLM server and/or slack/discord, resulting in session interruption.
    - Therefore, I use this method only when I know that my AFK will be a short amount of time (e.g., coffee run)
    - To avoid worrying about disconnection, I instead
- Start an agent from a server (with emacs already set up): using `scripts/run-agent PROJ-PATH`, which runs `emacs -Q` with all package dependencies on the load path and opens a new agent-shell in PROJ-PATH. See [scripts/config.el](scripts/config.el) for an example config; copy it to `~/.config/agent-shell-to-go/config.el` and fill in your credentials. Environment variables (`SLACK_BOT_TOKEN`, `DISCORD_BOT_TOKEN`, etc.) can be used to override anything set in the config file.
    - **Security note:** `run-agent` sets `enable-local-variables` to `:all` so the headless Emacs doesn't hang waiting for confirmation on risky dir-local variables. This also silently applies any `eval` forms found in `.dir-locals.el` inside the project directory. Only point it at projects whose `.dir-locals.el` you trust.

### In-thread commands

Send these in the thread to control the session:

| Command | Description |
|---------|-------------|
| `!yolo` | Bypass all permissions (dangerous!) |
| `!safe` | Accept edits mode |
| `!plan` | Plan mode |
| `!mode` | Show current mode |
| `!stop` | Interrupt the agent |
| `!restart` | Kill and restart agent with transcript |
| `!kill` | Kill the agent without restarting |
| `!queue` | Show pending queued messages |
| `!clearqueue` | Clear all pending queued messages |
| `!info` | Show session info |
| `!help` | Show available commands |

### Slash commands

| Command | Description |
|---------|-------------|
| `/new-project <name>` | Create a new project folder and start an agent |
| `/new-agent [folder]` | Start a new agent |
| `/project` | List projects (immeidate subdirectories) under `agent-shell-to-go-projects-directory` |
| `/sessions [proj-name]` | List past sessions for a project, infer from the current channel if proj-name is not given |
| `/resume [N] [proj-name]` | Resume the Nth session (default: most recent) of proj-name |
| `/projects` | List projects (immeidate subdirectories) under `agent-shell-to-go-projects-directory` |

See each transport's setup doc for how to register slash commands.

### Reactions

**Permission requests:**

| Emoji | Action |
|-------|--------|
| ✅ or 👍 | Allow once |
| 🔓 or ⭐ | Always allow |
| ❌ or 👎 | Reject |

**Message visibility:**

Long messages are automatically truncated to 500 characters. Add 👀 to see more, 📕 or 📖 for the full text.

| Emoji | Action |
|-------|--------|
| 🙈 or 🔕 | Hide message (remove to unhide) |
| 👀 | Show ~500 chars (remove to collapse) |
| 📕 or 📖 | Show full output (remove to collapse) |

## Customization

```elisp
;; Custom function to start agents
(setq agent-shell-to-go-start-agent-function #'my/start-claude-code)

;; Custom function to set up new projects
;; Called with (PROJECT-NAME BASE-DIR CALLBACK), should call CALLBACK with project-dir when done
(setq agent-shell-to-go-new-project-function #'my/new-python-project)

;; Hide tool call outputs by default (just show ✅/❌)
(setq agent-shell-to-go-show-tool-output nil)
```

## Troubleshooting

### Slack: WebSocket keeps disconnecting

If you see repeated `WebSocket closed` / reconnecting messages, Slack is rejecting the connection. Common causes:

1. **Events not enabled** - Slack app settings -> "Event Subscriptions" -> toggle "Enable Events" ON
2. **Missing event subscriptions** - verify `message.channels`, `reaction_added`, `reaction_removed` are subscribed
3. **App token expired** - regenerate in "Basic Information" -> "App-Level Tokens"

Enable debug logging to investigate:
```elisp
(setq agent-shell-to-go-debug t)
```
Check the `*agent-shell-to-go-debug*` buffer (`M-x agent-shell-to-go-show-debug-log`).

### Slack: app disabled / events not arriving

1. Go to Slack app settings -> "Event Subscriptions" -> re-enable events
2. Reconnect the websocket in Emacs:
   ```elisp
   (agent-shell-to-go-transport-connect (agent-shell-to-go-get-transport 'slack))
   ```

The existing connection becomes stale when Slack disables/re-enables events.

### Slack: message history

Slack's free tier keeps 90 days of message history. Consider a paid workspace to avoid losing history.

Bulk message deletion is not supported by the Slack API. Use `M-x agent-shell-to-go-cleanup-old-threads` to delete old threads one by one.

### Claude Code: OAuth token expired

If agents show `Authentication required` errors, the Claude CLI's OAuth token needs refreshing. Run `claude setup-token` in a terminal to get a long-lived token:

```bash
export CLAUDE_CODE_OAUTH_TOKEN=<token>
```

Or persist it for Emacs:
```elisp
(setenv "CLAUDE_CODE_OAUTH_TOKEN"
        (string-trim (with-temp-buffer
                       (insert-file-contents "~/.ssh/claude-oauth-token")
                       (buffer-string))))
```

### Agent gets stuck when writing to new directories

Doom Emacs prompts `y-or-n-p` before creating missing directories. Override it:

```elisp
(advice-add 'doom-create-missing-directories-h :override
            (lambda ()
              (unless (file-remote-p buffer-file-name)
                (let ((parent-directory (file-name-directory buffer-file-name)))
                  (when (and parent-directory (not (file-directory-p parent-directory)))
                    (make-directory parent-directory 'parents)
                    t)))))
```

## Related Projects

**Pairs well with [meta-agent-shell](https://github.com/ElleNajt/meta-agent-shell)** - A supervisory agent that monitors all your sessions. Search across agents, send messages between them, and manage your fleet of AI agents from Slack.

## License

GPL-3.0
