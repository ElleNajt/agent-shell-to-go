# Tool call completion edits the start message instead of sending a new one

Currently each tool call produces two messages: a start line (`:hourglass: \`toolname\`` on Discord,
`:tools: \`toolname\`` on Slack) followed by a separate result message (`:white_check_mark:` /
`:x:` with output). This clutters the thread when an agent makes many tool calls in one turn.

We decided that tool call completion edits the start message in place. One tool call, one message.
The `agent-shell-to-go--tool-calls` alist changes from `tool-call-id → t` (a sentinel) to
`tool-call-id → msg-id`. The completion path looks up the cached msg-id, calls
`agent-shell-to-go-transport-edit-message`, then removes the entry from the alist — the message
ID is consumed on final update and never needed again. If the start failed to produce a msg-id
(transport not ready), completion falls back to `send-text`.

The completion message is formatted via `agent-shell-to-go-transport-format-tool-call-result`,
which produces `<icon> \`title\`` (no output) or `<icon> \`title\`\n\`\`\`\noutput\n\`\`\`` (with
output), matching the style of the start message. The `edit-message` call must use
`(or thread-id channel-id)` as the channel — Discord thread messages live under the thread's own
channel ID, not the parent channel.

## Considered Options

**Keep separate messages.** Cleaner code — no need to thread message IDs through the tool-call
lifecycle, no risk of editing a message that was already deleted. Rejected because the noise
cost is real: a turn with 10 tool calls produces 20 messages.

**Edit the start message.** Requires the start path to capture the returned message ID and
store it, and the completion path to look it up and call `edit-message`. If the start fails
to produce a message ID (transport not ready), the completion falls back to sending a new
message. Both Discord and Slack already implement `edit-message`.

## Consequences

- The start path stores the msg-id returned by `agent-shell-to-go--send` into the alist;
  the `already-sent` check remains unchanged (truthy msg-id works like the old `t` sentinel).
- The completion path looks up the msg-id, calls `edit-message`, then removes the entry.
  This bounds the cache: entries live only from the first update event to the final one.
- Abandoned tool calls (agent interrupted, error) leave entries that die with the
  buffer-local alist when the buffer is killed via `bridge-disable`.
- Deleted start messages (user or bot) will cause the edit to silently fail (Discord returns
  error 10008 "Unknown Message"), leaving no result visible. A future improvement could fall
  back to sending a new message when the edit returns an error.
- The `edit-message` channel argument must be `(or thread-id channel-id)`, not bare
  `channel-id`. Discord thread messages are owned by the thread's channel ID; patching via
  the parent channel returns 10008.
- `format-tool-call-result` receives `status` as a string from JSON parsing (`"completed"`,
  `"failed"`). Transport implementations must handle both string and symbol forms of status.
- Existing tool-call tests need updating to expect `edit-message` instead of `send-text`
  on completion, and to match the formatted `icon + title` content rather than a bare icon.
