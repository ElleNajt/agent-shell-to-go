# /sessions and /resume slash commands

Two new slash commands let users list and resume agent-shell sessions from a remote
messaging client. `/sessions [proj-name]` lists past sessions; `/resume [N] [proj-name]`
resumes the Nth one (defaulting to the most recent). Both commands accept an optional
project name that is resolved relative to `agent-shell-to-go-projects-directory`; when
omitted, the project is inferred from the channel the command is sent in.

## Session data comes from the ACP API, not Slack thread history

Slack threads are available (via `list-threads`) but carry only opaque timestamps. The
ACP `session/list` request returns session titles and timestamps directly. Since there
is always at least one live agent-shell process (a prerequisite for remote connectivity),
an ACP client is always available to make this call.

## Session list cache is keyed by project-path, not channel-id

The natural cache key for a slash command is the channel it was sent in, but `/sessions`
can query any project from any channel. Keying by channel-id would duplicate entries when
the same project is queried from multiple channels and would also record the wrong
project-path for a cross-channel `/sessions other-project` call. Keying by project-path
gives one canonical entry per project and lets `/resume` reliably recover both the session
list and the correct `default-directory`.

## /resume always creates a new Slack thread

`!restart` reuses the existing thread by carrying `thread-id` through `inherit-state`.
`/resume` does not inherit `channel-id` or `thread-id` — both are derived fresh from the
resumed buffer's `default-directory`. This keeps the two ID spaces (ACP session IDs and
Slack thread timestamps) independent: there is no reliable mapping between them, and a
session may have never had a Slack thread (e.g. a local-only session).
