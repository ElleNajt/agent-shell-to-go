# Replace per-message disk files with in-memory cache persisted per session

The current system (`origin/master`) stores tool call presentation state as
multiple files per message on disk:

- `hidden/<channel>/<msg-id>.txt` — backed-up text when 🙈 hides a message
- `truncated/<channel>/<msg-id>.txt` — full text when output exceeds 500 chars
- `truncated/<channel>/<msg-id>.txt.collapsed` — the 500-char summary form

The bridge sends tool-call messages with `(:truncate t)`, which the transport
`send-text` methods intercept — truncating to 500 chars and saving the full text
to disk.  The presentation-reaction handler uses three states (hide /
expand-truncated / expand-full) with 6 pcase arms, reading and writing these
individual files.  Reactions are not gated to tool calls only — any message can
be hidden or expanded.

## Decision

Replace all per-message disk files with a single in-memory cache per session,
persisted to one file per session.  Simultaneously simplify from three
presentation states to two: hidden (show title only) and expand (show title +
output).

**Cache:**
```elisp
;; Global: (transport-name channel-id thread-id) → alist of (msg-id . (title output expanded-p))
(defvar agent-shell-to-go--tool-call-cache (make-hash-table :test #'equal))
```

**State machine:**

| State | Shows | 🙈 add | 🙈 remove | 👀 add | 👀 remove |
|-------|-------|--------|-----------|--------|------------|
| hidden | title only | no-op | no-op | → expand | no-op |
| expand | title + output | → hidden | no-op | no-op | → hidden |

Cache membership replaces the need for tool-call marker files — if a message is
not in the cache, presentation reactions on it are rejected (append
"_reaction ignored — no cache entry_" if text is fetchable, silently ignore
otherwise).

**Lifecycle:**
- Populated on tool-call finish (every tool call, regardless of `show-tool-output`)
- Persisted to `{storage-base-dir}/{transport}/sessions/{thread-id}.el` on session end and idle timer
- Loaded on `bridge-enable` when `--inherit-state` carries a `:thread-id` (resume)
- Cleaned up via existing thread-cleanup timer (`agent-shell-to-go-cleanup-age-hours`)

**What goes away:**
- `hidden/`, `truncated/` disk directories and their `--save-file`/`--load-file` helpers
- `expand-truncated` canonical action (3-state → 2-state)
- `--truncated-view-length` constant
- `:truncate t` option from transport `send-text` / `--send` calls
- `--truncate-text` default-500 behavior (callers already pass explicit max-len for queued/session text)

**Why:**
- ~3N per-message disk files replaceable with N cache entries + 1 persistence file
- Tool-call identity is program state, not filesystem presence
- 2-state model is simpler and matches natural reaction semantics (hide vs. expand)
- Memory per session (~28KB for 50 tool calls) is negligible
