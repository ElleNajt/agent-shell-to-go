# agent-shell-to-go

Emacs package that mirrors agent-shell conversations to Slack.

## Testing Changes

Reload after editing:
```bash
emacsclient -e '(load-file "/Users/elle/code/agent-shell-to-go/agent-shell-to-go.el")'
```

## Key State

Buffer-local variables (in agent-shell buffers):
- `agent-shell-to-go--thread-ts` - Slack thread timestamp for this session
- `agent-shell-to-go--channel-id` - Slack channel ID (may differ per project)
- `agent-shell-to-go--current-agent-message` - Accumulator for streaming chunks
- `agent-shell-to-go--from-slack` - Flag to prevent echo when message came from Slack

Global state:
- `agent-shell-to-go--active-buffers` - List of buffers with active mirroring
- `agent-shell-to-go--project-channels` - Hash table: project path -> channel ID
- `agent-shell-to-go--websocket` - WebSocket connection to Slack
- `agent-shell-to-go--pending-permissions` - Alist of permission requests awaiting reaction

## Storage Locations

- `~/.agent-shell/slack/` - Hidden message originals (for 🙈 unhide)
- `~/.agent-shell/slack-truncated/` - Full text of truncated messages (for 👀 expand)
- Channel mappings saved to `agent-shell-to-go-channels-file`

## Debugging

Enable debug logging:
```elisp
(setq agent-shell-to-go-debug t)
```

Check `*Messages*` buffer for `agent-shell-to-go:` prefixed logs.

Use `!debug` command in Slack thread to get session info.

Common issues:
- **Reaction not working**: Check if reaction event shows in `*Messages*`. Slack reaction names don't include colons (e.g., "eyes" not ":eyes:")
- **Message not expanding**: Check if file exists in `~/.agent-shell/slack-truncated/CHANNEL/TIMESTAMP.txt`
- **UTF-8 issues**: Uses curl for API requests to handle encoding properly
- **Thread not found**: `--find-buffer-for-thread` matches on both thread-ts and channel-id

## Architecture

- `--api-request`: All Slack API calls go through curl (for UTF-8 support)
- `--send`: Central message sending, handles truncation and storage
- `--on-notification`: Advice on agent-shell to capture tool calls/responses
- `--handle-reaction-event`: Dispatches reactions to hide/expand/heart/permission handlers
- `--inject-message`: Sends Slack messages into agent-shell as if typed locally

## Three-State Message Expansion

Tool outputs (when `agent-shell-to-go-show-tool-output` is nil) have three states:
1. **Collapsed** - Just status icon (✅ or ❌)
2. **Truncated** (👀) - First ~500 chars + "... 📖 for full output"
3. **Full** (📖) - Complete output up to Slack's ~4k limit

Files stored in `~/.agent-shell/slack-truncated/CHANNEL/`:
- `TIMESTAMP.txt` - Full output text
- `TIMESTAMP.txt.collapsed` - Original collapsed form (status icon)

## Reaction Handlers

| Reaction | Function |
|----------|----------|
| see_no_evil, no_bell | `--hide-message` (remove to unhide) |
| eyes | `--expand-message` (truncated ~500 char view, remove to collapse) |
| book, open_book | `--full-expand-message` (full output, remove to collapse) |
| heart, heart_eyes, etc. | `--handle-heart-reaction` |
| bookmark | `--handle-bookmark-reaction` |
| white_check_mark, +1, unlock, star, x, -1 | Permission responses |
