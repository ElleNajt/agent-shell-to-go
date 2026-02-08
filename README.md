# agent-shell-to-go

Take your [agent-shell](https://github.com/xenodium/agent-shell) sessions anywhere. Chat with your AI agents from your phone or any device.

Pairs well with [meta-agent-shell](https://github.com/ElleNajt/meta-agent-shell) for monitoring and coordinating multiple agents.

## Overview

agent-shell-to-go mirrors your agent-shell conversations to external messaging platforms, enabling bidirectional communication. Send messages from your phone, approve permissions on the go, and monitor your AI agents from anywhere.

## Transports

| Transport | Status | Description |
|-----------|--------|-------------|
| [Slack](slack.org) | Stable | Real-time via Socket Mode, reactions, slash commands, image uploads |
| Backend | Beta | HTTP backend for mobile app |
| Matrix | Planned | |
| Discord | Planned | |
| Telegram | Planned | |

## Architecture

See [ARCHITECTURE.md](ARCHITECTURE.md) for details on the pluggable transport system.

```
agent-shell buffer
        │
        ▼
  agent-shell-to-go-core (dispatch)
        │
   ┌────┴────┐
   ▼         ▼
 Slack    Backend    (future transports...)
```

## Quick Start

### Slack

1. Create a Slack app using [`slack-app-manifest.yaml`](slack-app-manifest.yaml)
2. Configure credentials (see [slack.org](slack.org) for options)
3. Add to your config:

```elisp
(use-package agent-shell-to-go
  :load-path "~/code/agent-shell-to-go"
  :after agent-shell
  :config
  ;; Set credentials (required)
  (setq agent-shell-to-go-slack-bot-token "xoxb-...")
  (setq agent-shell-to-go-slack-channel-id "C...")
  (setq agent-shell-to-go-slack-app-token "xapp-...")
  (setq agent-shell-to-go-slack-authorized-users '("U..."))
  
  (agent-shell-to-go-setup))
```

Requires the `websocket` package (MELPA).

Full setup instructions: [slack.org](slack.org)

### Backend (Mobile App)

For the mobile app backend:

```elisp
(setq agent-shell-to-go-backend-url "http://100.x.x.x:8080")
(setq agent-shell-to-go-backend-token "your-secret-token")
```

## Features

Common features across transports:

- Each agent-shell session gets mirrored to the messaging platform
- Bidirectional messaging (Emacs ↔ platform)
- Message queuing when agent is busy
- Permission request forwarding
- Agent status updates (processing, ready)
- Error forwarding

Transport-specific features are documented in their respective files.

## Security

Each transport has its own authorization mechanism:

- **Slack**: Requires explicit `agent-shell-to-go-slack-authorized-users` allowlist
- **Backend**: Bearer token authentication, designed for Tailscale-only access

Without proper authorization configured, transports ignore all incoming messages.

## Customization

### Backwards Compatibility

Old variable names still work:

```elisp
;; These are aliased to their slack-prefixed versions
(setq agent-shell-to-go-bot-token ...)      ; → agent-shell-to-go-slack-bot-token
(setq agent-shell-to-go-channel-id ...)     ; → agent-shell-to-go-slack-channel-id
(setq agent-shell-to-go-authorized-users ...)  ; → agent-shell-to-go-slack-authorized-users
```

### Debug Logging

```elisp
(setq agent-shell-to-go-debug t)
```

Check `*Messages*` for `agent-shell-to-go:` prefixed logs.

## Related Projects

- [meta-agent-shell](https://github.com/ElleNajt/meta-agent-shell) - Supervisory agent for monitoring and coordinating multiple sessions

## License

GPL-3.0
