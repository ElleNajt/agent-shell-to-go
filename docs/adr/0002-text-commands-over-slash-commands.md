# Text-based !-commands over platform slash commands

Discord and Slack both support platform-native slash commands (registered via REST API
or app manifest), but registering them carries a fixed cost: maintaining command registration, handling
platform-specific ack/response protocols (`INTERACTION_CREATE` for Discord,
`slash_commands` for Slack), and keeping two dispatch paths in sync. The benefit —
typeahead autocomplete — is proportional to command complexity. Our commands are simple
`!word` invocations with at most one argument; there is nothing to discover that a help
message does not already cover. The fixed cost is not justified.

We consolidated all commands under the `!` prefix and deleted the slash-command
infrastructure: `agent-shell-to-go-slash-command-hook`, `agent-shell-to-go-transport-acknowledge-interaction`,
`agent-shell-to-go-transport-followup-interaction`, Discord command registration, and
Slack slash dispatch. The business logic lives in `agent-shell-to-go--handle-command`,
accessed through the existing `MESSAGE_CREATE` → message-hook path.

## Considered Options

**Keep slash commands and fix the registration bug.** This would mean ensuring
`agent-shell-to-go-discord-register-commands` runs before every deployment, handling the
global-vs-guild propagation delay, debugging the deferred ack + followup flow, and
maintaining two response code paths (interaction webhook vs channel message). All for a
UX improvement whose value is proportional to command complexity — and our commands are
simple enough that a `!help` message covers everything autocomplete would.

**Text-based commands via `!` prefix.** The user types `!projects`, `!new-agent`, etc.
as regular chat messages. The existing message hook picks them up, the parser splits
command from args, and the handler executes. One dispatch path, one response mechanism
(`agent-shell-to-go-transport-send-text`), no platform-specific protocols.

## Command Dispatch Design

Commands are registered in `agent-shell-to-go--commands`, a `defconst` alist where each
entry is `(handler cmd arg1-regex arg2-regex ...)`:

```elisp
(defconst agent-shell-to-go--commands
  `((,#'agent-shell-to-go--cmd-bypass "!yolo")
    (,#'agent-shell-to-go--cmd-resume "!resume")
    (,#'agent-shell-to-go--cmd-resume "!resume" "[0-9]+")))
```

Dispatch is two-phase. The dispatcher extracts the first word (`cmd`) and at most one
arg token — without splitting the full message — then:

1. **Phase 1 — command lookup**: filter entries whose command string equals `cmd`
   literally. If none match, the message is an unknown command and is passed through to
   the agent unchanged.
2. **Phase 2 — arg match**: among all entries, find one whose full regex (built by
   joining cmd and arg patterns with `\\s-+` and anchoring with `^...$`) matches the
   `cmd + arg` key:

   ```
   ("!resume" "[0-9]+")  →  "^!resume\\s-+[0-9]+$"
   ```

   If a full match is found, its handler is called with the parsed arg list. If the
   command is known but no full match is found (the arg does not satisfy any registered
   pattern), the **first** handler for that command is called with the raw arg. Handlers
   are responsible for validating unexpected args and returning a usage error message —
   the dispatcher never silently drops a message once the first word is recognised.

Adding a command is one edit in one place: a single entry in `agent-shell-to-go--commands`
with the handler, the command string, and any arg regexes. No separate argless list, no
parallel pcase clauses to keep in sync.

For commands whose valid arg values are only known at runtime (e.g., `!new-agent` needing
the live project directory listing), the arg regex slot can be a zero-argument function
that returns a regex string; the dispatcher calls it at match time.

### Arg validation in handlers

Commands with restricted arg syntax (`!new-agent`, `!new-project`) validate their own
args rather than relying solely on the alist regex. This means:

- `!new-agent <name>` — `name` must match `[a-zA-Z0-9_.-]+` and be an existing
  subdirectory of `agent-shell-to-go-projects-directory`. An absolute path or unknown
  name sends a usage error, not a pass-through.
- `!new-project <name>` — same character restriction. An invalid name sends a usage
  error rather than attempting to create a malformed directory.

### `!resume`

`!resume` always operates on the current project, resolved from the channel/thread
context. It does not accept a project name argument — if the project cannot be determined
from context, it errors rather than asking the user to repeat it with a name.

`!resume` with no argument lists sessions for the current project. `!resume N` resumes
session N. There is one command to remember for both the listing and the resumption step.

## Consequences

- Removing the `INTERACTION_CREATE` handler means Discord slash commands that *are*
  registered will silently fail (the deferred ack never resolves). Unregister any
  existing commands from the Discord developer portal or let them expire.
- The `acknowledge-interaction` and `followup-interaction` generics are removed from
  the transport protocol. Future platforms that need deferred interaction response
  (e.g., a web-based transport with OAuth) will need to reintroduce them.
