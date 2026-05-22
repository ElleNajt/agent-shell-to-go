;;; agent-shell-to-go-core.el --- Shared protocol core for agent-shell-to-go -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Elle Najt

;; Author: Elle Najt
;; Maintainer: junyi.hou <junyi.yi.hou@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Shared infrastructure required by all transport implementations:
;; defcustoms, shared utilities, the transport struct/generics, the
;; transport registry, inbound hook variables, storage helpers, and the
;; generic WebSocket state machine.
;;
;; Transport files (slack, discord, …) and the bridge all require this
;; file directly.  The top-level `agent-shell-to-go.el' requires this
;; plus the transport and bridge files.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'websocket)

(defgroup agent-shell-to-go nil
  "Take your `agent-shell' sessions anywhere."
  :group 'agent-shell
  :prefix "agent-shell-to-go-")

; custom variables 

(defcustom agent-shell-to-go-start-agent-function #'agent-shell-new-shell
  "Function to call to start a new agent-shell.
The default uses `agent-shell-new-shell' which always forces creation of a
new shell rather than reusing an existing one or toggling.
Override if you have a custom starter function."
  :type 'function
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-debug nil
  "When non-nil, log debug messages to *agent-shell-to-go-debug*."
  :type 'boolean
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-show-tool-output t
  "When non-nil, show tool call outputs in remote messages.
When nil, only status icons are shown (use expand reaction to reveal)."
  :type 'boolean
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-upload-transcript-on-end nil
  "When non-nil, upload the agent-shell transcript when the session ends."
  :type 'boolean
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-event-log-max-entries 200
  "Maximum number of entries to keep in the event log buffer."
  :type 'integer
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-storage-base-dir "~/.agent-shell/"
  "Base directory for per-transport state storage.
Each transport gets a subdirectory named after it."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-projects-directory "~/code/"
  "Directory where `/new-project' creates new project folders."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-channel-id nil
  "When non-nil, route all sessions to this transport channel.
If nil (the default), each project gets its own channel via
`agent-shell-to-go-transport-ensure-project-channel'."
  :type '(choice (const nil) string)
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-new-project-function nil
  "Function to call to set up a new project.
Called with (PROJECT-NAME BASE-DIR CALLBACK).
CALLBACK is called with PROJECT-DIR when setup is complete.
If nil, just creates the directory and starts the agent immediately."
  :type
  '(choice
    (const :tag "Just create directory" nil) (function :tag "Custom setup function"))
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-default-transport 'slack
  "Default transport to use for new agent-shell buffers.
Must be a symbol naming a registered transport (see
`agent-shell-to-go-register-transport')."
  :type 'symbol
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-project-transport-alist nil
  "Alist mapping project path prefixes to transport names.
Each entry is (PATH . TRANSPORT-NAME).  When an agent-shell buffer is
started, its `default-directory' is matched against each PATH as a
prefix; the transport of the longest match wins.  Falls back to
`agent-shell-to-go-default-transport' when no prefix matches."
  :type '(alist :key-type directory :value-type symbol)
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-cleanup-age-hours 168
  "Threads older than this many hours are eligible for cleanup.
Default is 7 days."
  :type 'number
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-tool-call-icons
  '((start . "🛠️") (completed . "✅") (failed . "❌"))
  "Alist mapping tool-call status symbols to emoji strings.
Keys: `start' (shown while running), `completed', `failed'."
  :type '(alist :key-type symbol :value-type string)
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-spinner-verbs
  '("Actioning"
    "Actualizing"
    "Architecting"
    "Baking"
    "Beaming"
    "Booping"
    "Bootstrapping"
    "Brewing"
    "Calculating"
    "Channeling"
    "Churning"
    "Contemplating"
    "Deciphering"
    "Deliberating"
    "Embellishing"
    "Enchanting"
    "Envisioning"
    "Forging"
    "Frosting"
    "Hashing"
    "Improvising"
    "Inferring"
    "Levitating"
    "Manifesting"
    "Metamorphosing"
    "Orchestrating"
    "Philosophising"
    "Pondering"
    "Puzzling"
    "Ruminating"
    "Shenaniganing"
    "Simmering"
    "Sketching"
    "Synthesizing"
    "Tinkering")
  "Pool of busy-indicator verbs sent to the transport while the agent works.
A random entry is picked each time, formatted as `_VERB..._' and sent
for remote-originated prompts via `--on-input-submitted'."
  :type '(repeat string)
  :group 'agent-shell-to-go)

(defun agent-shell-to-go--get-random-spinner-verb ()
  "Randomly get a spinner word from `agent-shell-to-go-spinner-verbs'."
  (format "_%s..._"
          (seq-elt
           agent-shell-to-go-spinner-verbs
           (random (seq-length agent-shell-to-go-spinner-verbs)))))

; Shared utilities 

(defconst agent-shell-to-go--debug-buffer-name "*agent-shell-to-go-debug*"
  "Name of the buffer used for debug logging.")

(defmacro agent-shell-to-go--debug (format-string &rest args)
  "Append a timestamped debug line to `agent-shell-to-go--debug-buffer-name'.
Does nothing when `agent-shell-to-go-debug' is nil; arguments are not evaluated."
  `(when agent-shell-to-go-debug
     (let* ((msg (format ,format-string ,@args))
            (line (format "[%s] %s\n" (format-time-string "%H:%M:%S") msg))
            (buf (get-buffer-create agent-shell-to-go--debug-buffer-name)))
       (with-current-buffer buf
         (goto-char (point-max))
         (insert line)
         (let ((excess
                (- (count-lines (point-min) (point-max))
                   agent-shell-to-go-event-log-max-entries)))
           (when (> excess 0)
             (goto-char (point-min))
             (forward-line excess)
             (delete-region (point-min) (point))))))))

(defun agent-shell-to-go--strip-non-ascii (text)
  "Strip non-ASCII characters from TEXT, replacing them with `?'."
  (when text
    (replace-regexp-in-string "[^[:ascii:]]" "?" text)))

(defun agent-shell-to-go--sanitize-channel-name (name)
  "Sanitize NAME for use as a channel name.
Lowercase, replace invalid characters with hyphens, max 80 chars."
  (let* ((clean (downcase name))
         (clean (replace-regexp-in-string "[^a-z0-9-]" "-" clean))
         (clean (replace-regexp-in-string "-+" "-" clean))
         (clean (replace-regexp-in-string "^-\\|-$" "" clean)))
    (if (> (length clean) 80)
        (substring clean 0 80)
      clean)))

(defun agent-shell-to-go--get-project-path ()
  "Get the project path for the current buffer."
  (or (and (fboundp 'projectile-project-root) (projectile-project-root))
      (and (fboundp 'project-current)
           (when-let* ((proj (project-current)))
             (if (fboundp 'project-root)
                 (project-root proj)
               (car (project-roots proj)))))
      default-directory))

; Transport protocol 

(cl-defstruct agent-shell-to-go-transport
  "Base struct for transport implementations.
Transports `:include' this and add their own slots."
  (name nil :read-only t))

;; Lifecycle

(cl-defgeneric agent-shell-to-go-transport-connect (transport)
  "Connect TRANSPORT to its remote service.")

(cl-defgeneric agent-shell-to-go-transport-disconnect (transport)
  "Disconnect TRANSPORT.")

(cl-defgeneric agent-shell-to-go-transport-connected-p (transport)
  "Return non-nil if TRANSPORT is currently connected.")

(cl-defgeneric agent-shell-to-go-transport-authorized-p (transport user-id)
  "Return non-nil if USER-ID is allowed to interact via TRANSPORT.
Each transport knows its own user-id format.")

(cl-defgeneric agent-shell-to-go-transport-bot-user-id (transport)
  "Return the bot/self user-id for TRANSPORT, used for dedup.")

;; Send / edit / upload

(cl-defgeneric agent-shell-to-go-transport-send-text
    (transport channel-id thread-id text &optional options)
  "Send TEXT on TRANSPORT to CHANNEL-ID under THREAD-ID.
OPTIONS is a plist, possibly including:
  :ephemeral  only visible to :user-id
  :user-id    target user for ephemeral
Returns a message-id string.")

(cl-defgeneric agent-shell-to-go-transport-edit-message
    (transport channel-id message-id text)
  "Edit MESSAGE-ID on TRANSPORT in CHANNEL-ID to be TEXT.
Returns non-nil if the edit succeeded.")

(cl-defgeneric agent-shell-to-go-transport-upload-file
    (transport channel-id thread-id path &optional comment)
  "Upload PATH to CHANNEL under THREAD-ID with optional COMMENT.")


;; Read

(cl-defgeneric agent-shell-to-go-transport-get-message-text
    (transport channel-id message-id)
  "Return the text of MESSAGE-ID in CHANNEL-ID on TRANSPORT.")

(cl-defgeneric agent-shell-to-go-transport-get-reactions
    (transport channel-id message-id)
  "Return canonical reaction actions for MESSAGE-ID in CHANNEL-ID on TRANSPORT.
Transports translate raw emoji to canonical actions before returning.")

(cl-defgeneric agent-shell-to-go-transport-fetch-thread-replies
    (transport channel-id thread-id)
  "Return a list of reply plists for THREAD-ID in CHANNEL-ID on TRANSPORT.
Each plist has keys :msg-id :user :text.")

;; Threads & channels

(cl-defgeneric agent-shell-to-go-transport-start-thread (transport channel-id label)
  "Start a new thread on TRANSPORT in CHANNEL-ID with LABEL.  Return thread id.")

(cl-defgeneric agent-shell-to-go-transport-update-thread-header
    (transport channel-id thread-id title)
  "Update the thread header on TRANSPORT for THREAD-ID in CHANNEL-ID to TITLE.")

(cl-defgeneric agent-shell-to-go-transport-ensure-project-channel-id
    (transport project-path)
  "Return the top-level posting destination on TRANSPORT for PROJECT-PATH.")

(cl-defgeneric agent-shell-to-go-transport-list-threads (transport channel-id)
  "Return a list of thread plists (:thread-id :last-timestamp) in CHANNEL-ID from TRANSPORT.")

(cl-defgeneric agent-shell-to-go-transport-delete-message
    (transport channel-id message-id)
  "Delete MESSAGE-ID in CHANNEL-ID on TRANSPORT.")

(cl-defgeneric agent-shell-to-go-transport-delete-thread
    (transport channel-id thread-id)
  "Delete THREAD-ID (all messages) in CHANNEL-ID on TRANSPORT.")

;; Formatting (semantic; transport renders its own markup)

(cl-defgeneric agent-shell-to-go-transport-format-tool-call-start (transport title)
  "Return rendered string announcing a tool call with TITLE on TRANSPORT.")

(cl-defgeneric agent-shell-to-go-transport-format-tool-call-result
    (transport title status output)
  "Return rendered string for a tool call result on TRANSPORT with TITLE.
STATUS is a symbol; OUTPUT is a string (may be empty or nil).")

(cl-defgeneric agent-shell-to-go-transport-format-diff (transport old-text new-text)
  "Return rendered diff string on TRANSPORT between OLD-TEXT and NEW-TEXT.")

(cl-defgeneric agent-shell-to-go-transport-format-user-message (transport text)
  "Return rendered user-authored TEXT on TRANSPORT.")

(cl-defgeneric agent-shell-to-go-transport-format-agent-message (transport text)
  "Return rendered agent-authored TEXT on TRANSPORT.")

(cl-defgeneric agent-shell-to-go-transport-format-markdown (transport markdown)
  "Convert MARKDOWN to TRANSPORT's native markup.")

;; Storage root (with default method)

(cl-defgeneric agent-shell-to-go-transport-storage-root (transport)
  "Return the TRANSPORT specific on-disk storage directory.")

(cl-defmethod agent-shell-to-go-transport-storage-root
    ((transport agent-shell-to-go-transport))
  "Default: `{storage-base-dir}/{transport-name}/'."
  (expand-file-name (format "%s/"
                            (symbol-name (agent-shell-to-go-transport-name transport)))
                    agent-shell-to-go-storage-base-dir))

; Transport registry 

(defvar agent-shell-to-go--transports nil
  "Alist of (NAME . TRANSPORT) for registered transports.")

(defun agent-shell-to-go-register-transport (name transport)
  "Register TRANSPORT under NAME (a symbol)."
  (setf (alist-get name agent-shell-to-go--transports) transport)
  (agent-shell-to-go--debug "registered transport: %s" name))

(defun agent-shell-to-go-get-transport (name)
  "Return the registered transport named NAME, or nil."
  (alist-get name agent-shell-to-go--transports))

(defun agent-shell-to-go--all-transport-objects ()
  "Return unique transport objects for all configured transports.
Includes the default and every transport named in
`agent-shell-to-go-project-transport-alist'."
  (let* ((names
          (cons
           agent-shell-to-go-default-transport
           (mapcar #'cdr agent-shell-to-go-project-transport-alist)))
         (unique (cl-remove-duplicates names)))
    (delq nil (mapcar #'agent-shell-to-go-get-transport unique))))

(defun agent-shell-to-go--get-transport ()
  "Return the transport for the current `default-directory', or error.
Matches `agent-shell-to-go-project-transport-alist' by longest prefix;
falls back to `agent-shell-to-go-default-transport'."
  (let* ((dir (expand-file-name default-directory))
         (match
          (car
           (sort (cl-remove-if-not
                  (lambda (entry)
                    (string-prefix-p (expand-file-name (car entry)) dir))
                  agent-shell-to-go-project-transport-alist)
                 (lambda (a b) (> (length (car a)) (length (car b)))))))
         (name
          (if match
              (cdr match)
            agent-shell-to-go-default-transport)))
    (or (agent-shell-to-go-get-transport name)
        (error "Transport `%s' not registered" name))))

; Canonical inbound events 

(defconst agent-shell-to-go--canonical-reaction-actions
  '(hide expand permission-allow permission-always permission-reject)
  "Closed set of canonical reaction action symbols.
Transports map raw reactions to these when firing the reaction hook.")

(defvar agent-shell-to-go-message-hook nil
  "Hook run when a remote message arrives.
Each function is called with a single plist argument:
  :transport  transport struct
  :channel-id channel id
  :thread-id  thread id
  :user       remote user id
  :text       message text
  :msg-id     remote message id")

(defvar agent-shell-to-go-reaction-hook nil
  "Hook run when a remote reaction is added or removed.
Plist argument:
  :transport  transport struct
  :channel-id channel id
  :thread-id  thread id (may be nil)
  :msg-id     target message id
  :user       remote user id
  :action     canonical symbol from `agent-shell-to-go--canonical-reaction-actions',
              or nil if the raw reaction didn't map to anything
  :raw-emoji  opaque raw emoji (do not assume stringp)
  :added-p    t if reaction was added, nil if removed")

; In-memory tool-call cache
;; Each session's tool-call messages are stored in a global hash table keyed by
;; (transport-name channel-id thread-id).  Entries are (title output expanded-p).
;; The cache is persisted to one file per session and loaded on resume.
;; Cache membership replaces tool-call marker files — presentation reactions on
;; non-tool-call messages hit the cache-miss fallback.

(defconst agent-shell-to-go--max-message-length 3800
  "Maximum body length for a transport message (with buffer for extra markup).")

(defconst agent-shell-to-go--truncation-note "\n_... (full text too long)_"
  "Note appended when an expanded message still exceeds transport limit.")

(defun agent-shell-to-go--truncate-text (text &optional max-len)
  "Truncate TEXT to MAX-LEN chars, adding a hint if cut."
  (when (and max-len (> (length text) max-len))
    (setq text (concat (substring text 0 max-len) "\n_… truncated_")))
  text)

(defun agent-shell-to-go--save-file (path text)
  "Save TEXT to PATH, creating directories as needed."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert text)))

(defun agent-shell-to-go--load-file (path)
  "Read file at PATH as a string, or return nil if missing."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

;; tool-call-cache

(defvar agent-shell-to-go--tool-call-cache (make-hash-table :test #'equal)
  "Global cache of tool-call presentation state.
Key: (transport-name channel-id thread-id).
Value: alist of (msg-id . (title output expanded-p)).")

(defun agent-shell-to-go--cache-key (transport channel-id thread-id)
  "Return the cache key for TRANSPORT, CHANNEL-ID, and THREAD-ID."
  (list (agent-shell-to-go-transport-name transport) channel-id thread-id))

(defun agent-shell-to-go--cache-get-session (transport channel-id thread-id)
  "Return the session alist for the given key, or nil."
  (gethash (agent-shell-to-go--cache-key transport channel-id thread-id)
           agent-shell-to-go--tool-call-cache))

(defun agent-shell-to-go--cache-put-entry (transport channel-id thread-id msg-id title output)
  "Store TITLE and OUTPUT for MSG-ID, defaulting to hidden (expanded-p nil)."
  (let* ((key (agent-shell-to-go--cache-key transport channel-id thread-id))
         (session (or (gethash key agent-shell-to-go--tool-call-cache) nil)))
    (setf (alist-get msg-id session nil nil #'equal) (list title output nil))
    (puthash key session agent-shell-to-go--tool-call-cache)))

(defun agent-shell-to-go--cache-get-entry (transport channel-id thread-id msg-id)
  "Return (title output expanded-p) for MSG-ID, or nil."
  (when-let* ((session (agent-shell-to-go--cache-get-session
                        transport channel-id thread-id)))
    (alist-get msg-id session nil nil #'equal)))

(defun agent-shell-to-go--cache-remove-session (transport channel-id thread-id)
  "Remove the session entry from the cache."
  (remhash (agent-shell-to-go--cache-key transport channel-id thread-id)
           agent-shell-to-go--tool-call-cache))

;; cache persistence

(defun agent-shell-to-go--cache-session-file (transport thread-id)
  "Return the path to the session cache file for TRANSPORT and THREAD-ID."
  (expand-file-name
   (format "sessions/%s.el" thread-id)
   (agent-shell-to-go-transport-storage-root transport)))

(defun agent-shell-to-go--cache-save-session (transport channel-id thread-id)
  "Persist the session cache for THREAD-ID to disk."
  (when-let* ((session (agent-shell-to-go--cache-get-session
                        transport channel-id thread-id)))
    (agent-shell-to-go--save-file
     (agent-shell-to-go--cache-session-file transport thread-id)
     (prin1-to-string session))))

(defun agent-shell-to-go--cache-load-session (transport channel-id thread-id)
  "Load the session cache for THREAD-ID from disk, storing it in the global cache."
  (let* ((path (agent-shell-to-go--cache-session-file transport thread-id))
         (text (agent-shell-to-go--load-file path)))
    (when text
      (let ((alist (condition-case nil
                       (read text)
                     (error nil))))
        (when alist
          (puthash (agent-shell-to-go--cache-key transport channel-id thread-id)
                   alist
                   agent-shell-to-go--tool-call-cache))))))

(defun agent-shell-to-go--cache-cleanup-old-sessions (transport)
  "Delete session cache files older than `agent-shell-to-go-cleanup-age-hours'."
  (let ((sessions-dir
         (expand-file-name "sessions/"
                           (agent-shell-to-go-transport-storage-root transport))))
    (when (file-directory-p sessions-dir)
      (let ((cutoff (- (float-time) (* agent-shell-to-go-cleanup-age-hours 3600))))
        (dolist (file (directory-files sessions-dir t "\\.el$"))
          (when (< (float-time (nth 5 (file-attributes file))) cutoff)
            (delete-file file)
            (agent-shell-to-go--debug "cleaned up session cache: %s" file)))))))

; Presentation-reaction dispatcher

(cl-defun agent-shell-to-go--handle-presentation-reaction
    (&key transport channel-id msg-id thread-id action added-p &allow-other-keys)
  "Handle presentation reactions (hide/expand) from a transport.
Gate on cache membership: only tool call messages are cached.
This runs before bridge handlers so the bridge never sees presentation reactions."
  (if-let ((entry (agent-shell-to-go--cache-get-entry
                   transport channel-id thread-id msg-id)))
      ;; Cache hit — process the reaction
      (let ((title (nth 0 entry))
            (output (nth 1 entry)))
        (pcase (cons added-p action)
          (`(t . hide)
           (agent-shell-to-go-transport-edit-message
            transport channel-id msg-id "_message hidden_")
           (setf (nth 2 entry) nil))
          (`(nil . hide)
           (agent-shell-to-go-transport-edit-message
            transport channel-id msg-id title)
           (setf (nth 2 entry) nil))
          (`(t . expand)
           (let* ((full (concat title "\n" output))
                  (display
                   (if (> (length full) agent-shell-to-go--max-message-length)
                       (concat (substring full 0 agent-shell-to-go--max-message-length)
                               agent-shell-to-go--truncation-note)
                     full)))
             (agent-shell-to-go-transport-edit-message
              transport channel-id msg-id display)
             (setf (nth 2 entry) t)))
          (`(nil . expand)
           (agent-shell-to-go-transport-edit-message
            transport channel-id msg-id title)
           (setf (nth 2 entry) nil))))
    ;; Cache miss — only for additions; removals self-heal on re-add
    (when (and added-p (memq action '(hide expand)))
      (when-let* ((text (agent-shell-to-go-transport-get-message-text
                         transport channel-id msg-id)))
        (agent-shell-to-go-transport-edit-message
         transport channel-id msg-id
         (concat text "\n_reaction ignored — no cache entry_"))))))

(add-hook
 'agent-shell-to-go-reaction-hook #'agent-shell-to-go--handle-presentation-reaction)

; Generic WebSocket state machine 
;; Transports that speak WebSocket use this via `agent-shell-to-go--ws-connect'.
;; They pass a URL-FN (callable returning the current ws URL) plus frame and
;; close handlers.  Reconnect and backoff live here.

(cl-defstruct agent-shell-to-go--ws
  "State container for a transport's websocket connection."
  name
  url-fn
  on-frame
  on-close
  websocket
  reconnect-timer
  intentional-close
  (reconnect-backoff 5)
  (get-active-p (lambda () t)))

(defun agent-shell-to-go--ws-make (&rest args)
  "Create a new ws state struct from ARGS plist.
Required keys: :name :url-fn :on-frame.
Optional: :on-close :get-active-p :reconnect-backoff."
  (apply #'make-agent-shell-to-go--ws args))

(defun agent-shell-to-go--ws-connect (ws)
  "Open the websocket described by WS.
Closes any existing socket first."
  (when (agent-shell-to-go--ws-websocket ws)
    (setf (agent-shell-to-go--ws-intentional-close ws) t)
    (ignore-errors
      (websocket-close (agent-shell-to-go--ws-websocket ws)))
    (setf (agent-shell-to-go--ws-intentional-close ws) nil))
  (let ((url (funcall (agent-shell-to-go--ws-url-fn ws)))
        (frame-fn (agent-shell-to-go--ws-on-frame ws))
        (close-fn (agent-shell-to-go--ws-on-close ws)))
    (setf (agent-shell-to-go--ws-websocket ws)
          (websocket-open
           url
           :on-message (lambda (_w frame) (funcall frame-fn frame))
           :on-close
           (lambda (_w)
             (agent-shell-to-go--debug "ws[%s] closed" (agent-shell-to-go--ws-name ws))
             (when close-fn
               (funcall close-fn))
             (unless (agent-shell-to-go--ws-intentional-close ws)
               (agent-shell-to-go--ws-reconnect ws)))
           :on-error
           (lambda (_w _t err)
             (agent-shell-to-go--debug "ws[%s] error: %s"
                                       (agent-shell-to-go--ws-name ws)
                                       err))))))

(defun agent-shell-to-go--ws-reconnect (ws)
  "Schedule WS to reconnect after its backoff."
  (when (agent-shell-to-go--ws-reconnect-timer ws)
    (cancel-timer (agent-shell-to-go--ws-reconnect-timer ws)))
  (when (funcall (agent-shell-to-go--ws-get-active-p ws))
    (setf (agent-shell-to-go--ws-reconnect-timer ws)
          (run-with-timer
           (agent-shell-to-go--ws-reconnect-backoff ws)
           nil
           (lambda () (agent-shell-to-go--ws-connect ws))))))

(defun agent-shell-to-go--ws-disconnect (ws)
  "Disconnect WS and cancel any pending reconnect."
  (when (agent-shell-to-go--ws-reconnect-timer ws)
    (cancel-timer (agent-shell-to-go--ws-reconnect-timer ws))
    (setf (agent-shell-to-go--ws-reconnect-timer ws) nil))
  (when (agent-shell-to-go--ws-websocket ws)
    (setf (agent-shell-to-go--ws-intentional-close ws) t)
    (ignore-errors
      (websocket-close (agent-shell-to-go--ws-websocket ws)))
    (setf (agent-shell-to-go--ws-websocket ws) nil)
    (setf (agent-shell-to-go--ws-intentional-close ws) nil)))

(defun agent-shell-to-go--ws-connected-p (ws)
  "Return non-nil if WS has an open connection."
  (let ((sock (and ws (agent-shell-to-go--ws-websocket ws))))
    (and sock (websocket-openp sock))))

(defun agent-shell-to-go--defer (fn &rest args)
  "Schedule FN with ARGS to run on the next event loop iteration."
  (apply #'run-at-time 0 nil fn args))

(provide 'agent-shell-to-go-core)
;;; agent-shell-to-go-core.el ends here
