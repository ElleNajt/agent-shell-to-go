# Architecture

agent-shell-to-go uses a pluggable transport architecture that allows multiple messaging platforms to mirror agent-shell conversations.

## Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                        agent-shell                               │
│                    (Emacs AI chat buffer)                        │
└───────────────────────────┬─────────────────────────────────────┘
                            │
                            │ advice hooks
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│                   agent-shell-to-go-core                         │
│                                                                  │
│  ┌──────────────────┐  ┌──────────────────┐  ┌───────────────┐  │
│  │ Transport        │  │ Event Dispatch   │  │ Session       │  │
│  │ Registry         │  │                  │  │ Management    │  │
│  └──────────────────┘  └──────────────────┘  └───────────────┘  │
└───────────────────────────┬─────────────────────────────────────┘
                            │
              ┌─────────────┴─────────────┐
              │                           │
              ▼                           ▼
┌─────────────────────────┐  ┌─────────────────────────┐
│ agent-shell-to-go-slack │  │ agent-shell-to-go-backend│
│                         │  │                         │
│  - WebSocket connection │  │  - HTTP POST to server  │
│  - Slack API            │  │  - Mobile app backend   │
│  - Reactions            │  │                         │
│  - Slash commands       │  │                         │
│  - Image uploads        │  │                         │
└─────────────────────────┘  └─────────────────────────┘
```

## Files

| File | Purpose |
|------|---------|
| `agent-shell-to-go.el` | Entry point. Loads transports, provides `agent-shell-to-go-setup` |
| `agent-shell-to-go-core.el` | Transport registry, advice hooks, event dispatch |
| `agent-shell-to-go-slack.el` | Slack transport implementation |
| `agent-shell-to-go-backend.el` | HTTP backend transport for mobile app |

## Core Module

### Transport Registry

Transports register themselves as plists with a `:name` and event handlers:

```elisp
(agent-shell-to-go-register-transport
 '(:name "slack"
   :init agent-shell-to-go-slack--init
   :cleanup agent-shell-to-go-slack--cleanup
   :on-session-start agent-shell-to-go-slack--on-session-start
   :on-session-end agent-shell-to-go-slack--on-session-end
   :on-user-message agent-shell-to-go-slack--on-user-message
   :on-agent-message agent-shell-to-go-slack--on-agent-message
   :on-tool-call agent-shell-to-go-slack--on-tool-call
   :on-tool-result agent-shell-to-go-slack--on-tool-result
   :on-status-change agent-shell-to-go-slack--on-status-change
   :on-permission-request agent-shell-to-go-slack--on-permission-request))
```

### Event Dispatch

When an event occurs in agent-shell, the core dispatches it to all registered transports:

```elisp
(agent-shell-to-go--dispatch :on-user-message buffer prompt injecting)
```

Each transport's handler receives:
1. The buffer where the event occurred
2. Transport-specific session data (retrieved from buffer-local storage)
3. Event-specific arguments

### Session Data

Each buffer maintains per-transport session data:

```elisp
;; Buffer-local alist: transport-name -> session plist
agent-shell-to-go--transport-data

;; Example for Slack:
(:thread-ts "1234567890.123456"
 :channel-id "C0123456789"
 :file-watcher <process>
 :uploaded-images #<hash-table>
 :mentioned-files #<hash-table>)
```

Transports store their own state here. The core provides helpers:

```elisp
(agent-shell-to-go--set-transport-data buffer "slack" session-data)
(agent-shell-to-go--get-transport-data buffer "slack")
```

### Advice Hooks

The core installs advice on agent-shell functions to capture events:

| Advised Function | Event Dispatched |
|------------------|------------------|
| `agent-shell--send-command` | `:on-user-message` |
| `agent-shell--on-notification` | `:on-agent-message`, `:on-tool-call`, `:on-tool-result` |
| `agent-shell--on-request` | `:on-permission-request` |
| `agent-shell-heartbeat-stop` | `:on-status-change` (ready) |
| `agent-shell--initialize-client` | `:on-error` (if client creation failed) |

### Message Injection

External sources can inject messages into agent-shell:

```elisp
(agent-shell-to-go-inject-message buffer text "slack")
```

This either submits immediately or queues if the shell is busy.

## Transport Events

Transports implement these handlers:

| Event | Arguments | Purpose |
|-------|-----------|---------|
| `:init` | (none) | Called once during setup |
| `:cleanup` | (none) | Called during teardown |
| `:on-session-start` | buffer | Initialize session, return session-data plist |
| `:on-session-end` | buffer, session-data | Clean up session |
| `:on-user-message` | buffer, session-data, text, injecting | User sent a message |
| `:on-agent-message` | buffer, session-data, text | Agent replied |
| `:on-tool-call` | buffer, session-data, update | Tool invocation started |
| `:on-tool-result` | buffer, session-data, update | Tool completed |
| `:on-status-change` | buffer, session-data, status, detail | Agent status changed |
| `:on-permission-request` | buffer, session-data, request | Permission needed |
| `:on-error` | buffer, message | Error occurred |
| `:on-message-queued` | buffer, text, source | Message queued (shell busy) |

## Data Flow Examples

### User types in Emacs

```
User types in agent-shell buffer
        │
        ▼
agent-shell--send-command (advised)
        │
        ▼
core: dispatch :on-user-message to all transports
        │
        ├──────────────────────┐
        ▼                      ▼
Slack transport           Backend transport
posts to thread           POSTs to HTTP server
```

### Message arrives from Slack

```
Slack WebSocket receives message
        │
        ▼
slack: --handle-message-event
        │
        ▼
core: agent-shell-to-go-inject-message
        │
        ▼
agent-shell--send-command
        │
        ▼
(message flows through normally,
 but :on-user-message gets `injecting=t`
 so Slack transport skips re-posting)
```

### Tool call with diff

```
agent-shell--on-notification receives tool_call
        │
        ▼
core: dispatch :on-tool-call
        │
        ▼
Slack transport:
  - Extract file paths for image watching
  - Extract diff if present
  - Format message with diff highlighting
  - Post to thread
```

## Slack Transport Details

The Slack transport (`agent-shell-to-go-slack.el`) is the most feature-rich:

### WebSocket / Socket Mode

Maintains a persistent WebSocket connection to Slack for real-time events:
- Incoming messages → inject into agent-shell
- Reactions → handle permissions, visibility, bookmarks
- Slash commands → start agents, list projects

### Per-Project Channels

Each project gets its own Slack channel:
- Channel name derived from project directory name
- Mapping cached in `agent-shell-to-go-channels-file`
- Falls back to default channel if creation fails

### Image Upload

Uses fswatch to monitor project directory:
- Only uploads images mentioned in recent tool calls
- Rate limited (default 30/minute)
- Tracks uploaded files to avoid duplicates

### Three-State Message Expansion

Tool outputs can be:
1. **Collapsed** - Just status icon (when `show-tool-output` is nil)
2. **Truncated** - First ~500 chars (add :eyes: reaction)
3. **Full** - Complete output up to Slack limit (add :book: reaction)

## Backend Transport Details

The backend transport (`agent-shell-to-go-backend.el`) is simpler:

- Fire-and-forget async HTTP POSTs via curl
- Endpoints: `/events/agent-spawn`, `/events/message`, `/events/status`
- No bidirectional communication (mobile app uses its own connection to backend)
- Designed for Tailscale-only access with bearer token auth

## Adding a New Transport

1. Create `agent-shell-to-go-<name>.el`
2. Define handler functions for relevant events
3. Create transport plist with `:name` and handlers
4. Provide `agent-shell-to-go-<name>-setup` that registers the transport
5. Optionally add to `agent-shell-to-go.el` for auto-loading

Example minimal transport:

```elisp
(defun my-transport--on-session-start (buffer)
  "Initialize session, return session data."
  `(:session-id ,(format-time-string "%s")))

(defun my-transport--on-user-message (buffer session-data text injecting)
  "Handle user message."
  (unless injecting
    (message "User said: %s" text)))

(defvar my-transport
  '(:name "my-transport"
    :on-session-start my-transport--on-session-start
    :on-user-message my-transport--on-user-message))

(defun my-transport-setup ()
  (agent-shell-to-go-register-transport my-transport))
```
