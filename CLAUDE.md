# agent-shell-to-go

Emacs package that mirrors agent-shell conversations to the Dendrite mobile app.

## Testing Changes

Reload after editing:
```bash
emacsclient -e '(load-file "/Users/elle/code/agent-shell-to-go/agent-shell-to-go-mobile.el")'
```

## Key State

Buffer-local variables (in agent-shell buffers):
- `agent-shell-to-go-mobile--session-id` - Unique session ID for this buffer
- `agent-shell-to-go-mobile--current-agent-message` - Accumulator for streaming chunks
- `agent-shell-to-go-mobile--injecting-from-mobile` - Flag to prevent echo
- `agent-shell-to-go-mobile--pending-permission` - Pending permission request info

Global state:
- `agent-shell-to-go-mobile--active-buffers` - List of buffers with active mirroring
- `agent-shell-to-go-mobile--websocket` - WebSocket connection to Go backend
- `agent-shell-to-go-mobile--websocket-state` - Connection state

## Debugging

Debug logs write to `~/.dendrite/debug/logs/dendrite-YYYY-MM-DD.log`.

```elisp
;; Open today's log
(agent-shell-to-go-mobile-open-log)

;; Show connection status
(agent-shell-to-go-mobile-debug-status)

;; Test backend connectivity
(agent-shell-to-go-mobile-test-connection)
```

## Architecture

```
agent-shell (Emacs)
    │ advice hooks
    ▼
agent-shell-to-go-mobile.el
    │ HTTP POST events
    │ WebSocket receive
    ▼
Go Backend (dendrite/backend/main.go)
    │ WebSocket broadcast
    │ SQLite storage
    ▼
React Native App (dendrite/app/)
```

- `agent-shell-to-go.el` - Entry point, delegates to mobile module
- `agent-shell-to-go-mobile.el` - All Emacs-side logic (advice, events, WebSocket client)
- `dendrite/backend/main.go` - Go server (REST API, WebSocket, SQLite)
- `dendrite/app/` - React Native mobile app
