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
  :truncate   truncate long content, store the rest for expansion
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
  '(hide
    expand-truncated expand-full permission-allow permission-always permission-reject)
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

; Storage helpers (for truncated/hidden texts)
;; TODO: Replace truncation/hide mechanism with message splitting across multiple
;; transport sends.  Splitting removes the need for expand reactions and handles
;; agent messages (currently not truncated), which can also exceed transport limits
;; (Discord: 2000 chars, Slack: 4000 chars).
;; TODO: Add a per-transport send queue to handle rate limits.  All send/edit calls
;; should be serialized through a timer-driven queue (one queue per transport instance)
;; so bursts don't trigger 429s.  On a 429, read Retry-After and reschedule.
;; Slack: ~1 msg/sec per channel; Discord: 5 requests/5 sec globally.

(defconst agent-shell-to-go--max-message-length 3800
  "Maximum body length for a transport message (with buffer for extra markup).")

(defconst agent-shell-to-go--truncation-note "\n_... (full text too long)_"
  "Note appended when an expanded message still exceeds transport limit.")

(defconst agent-shell-to-go--truncated-view-length 500
  "Length for truncated view (glance).")

(defun agent-shell-to-go--truncate-text (text &optional max-len)
  "Truncate TEXT to MAX-LEN (default 500), adding a hint."
  (let ((max-len (or max-len 500)))
    (if (> (length text) max-len)
        (concat (substring text 0 max-len) "\n_… expand for more_")
      text)))

(defun agent-shell-to-go--hidden-path (transport channel-id msg-id)
  "Return path for hidden MSG-ID in CHANNEL-ID on TRANSPORT."
  (expand-file-name (format "hidden/%s/%s.txt" channel-id msg-id)
                    (agent-shell-to-go-transport-storage-root transport)))

(defun agent-shell-to-go--truncated-path (transport channel-id msg-id)
  "Return path for truncated MSG-ID full text in CHANNEL-ID on TRANSPORT."
  (expand-file-name (format "truncated/%s/%s.txt" channel-id msg-id)
                    (agent-shell-to-go-transport-storage-root transport)))

(defun agent-shell-to-go--collapsed-path (transport channel-id msg-id)
  "Return path for collapsed form of MSG-ID in CHANNEL-ID on TRANSPORT."
  (concat (agent-shell-to-go--truncated-path transport channel-id msg-id) ".collapsed"))

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

(defun agent-shell-to-go--save-hidden-message (transport channel-id msg-id text)
  "Save original TEXT for hidden MSG-ID."
  (agent-shell-to-go--save-file
   (agent-shell-to-go--hidden-path transport channel-id msg-id) text))

(defun agent-shell-to-go--load-hidden-message (transport channel-id msg-id)
  "Load original text for hidden MSG-ID."
  (agent-shell-to-go--load-file
   (agent-shell-to-go--hidden-path transport channel-id msg-id)))

(defun agent-shell-to-go--delete-hidden-message-file (transport channel-id msg-id)
  "Delete hidden MSG-ID file."
  (let ((path (agent-shell-to-go--hidden-path transport channel-id msg-id)))
    (when (file-exists-p path)
      (delete-file path))))

(defun agent-shell-to-go--save-truncated-message
    (transport channel-id msg-id full-text &optional collapsed-text)
  "Save FULL-TEXT for MSG-ID.  Optionally also save COLLAPSED-TEXT."
  (agent-shell-to-go--save-file
   (agent-shell-to-go--truncated-path transport channel-id msg-id) full-text)
  (when collapsed-text
    (agent-shell-to-go--save-file
     (agent-shell-to-go--collapsed-path transport channel-id msg-id) collapsed-text)))

(defun agent-shell-to-go--load-truncated-message (transport channel-id msg-id)
  "Load full text for MSG-ID."
  (agent-shell-to-go--load-file
   (agent-shell-to-go--truncated-path transport channel-id msg-id)))

(defun agent-shell-to-go--load-collapsed-message (transport channel-id msg-id)
  "Load collapsed form for MSG-ID, if any."
  (agent-shell-to-go--load-file
   (agent-shell-to-go--collapsed-path transport channel-id msg-id)))

; Presentation-reaction dispatcher 

(cl-defun agent-shell-to-go--handle-presentation-reaction
    (&key transport channel-id msg-id action added-p &allow-other-keys)
  "Handle presentation reactions (hide/expand/collapse) from a transport.
This runs before bridge handlers so the bridge never sees presentation reactions."
  (pcase (cons added-p action)
    (`(t . hide)
     (when-let* ((text
                  (agent-shell-to-go-transport-get-message-text
                   transport channel-id msg-id)))
       (agent-shell-to-go--save-hidden-message transport channel-id msg-id text)
       (agent-shell-to-go-transport-edit-message
        transport channel-id msg-id "_message hidden_")))
    (`(nil . hide)
     (when-let* ((original
                  (agent-shell-to-go--load-hidden-message transport channel-id msg-id)))
       (agent-shell-to-go-transport-edit-message transport channel-id msg-id original)
       (agent-shell-to-go--delete-hidden-message-file transport channel-id msg-id)))
    (`(t . expand-truncated)
     (when-let* ((full
                  (agent-shell-to-go--load-truncated-message
                   transport channel-id msg-id)))
       (let* ((too-long (> (length full) agent-shell-to-go--truncated-view-length))
              (display
               (if too-long
                   (concat
                    (substring full 0 agent-shell-to-go--truncated-view-length)
                    "\n_… expand further for full output_")
                 full)))
         (agent-shell-to-go-transport-edit-message
          transport channel-id msg-id display))))
    (`(t . expand-full)
     (when-let* ((full
                  (agent-shell-to-go--load-truncated-message
                   transport channel-id msg-id)))
       (let* ((too-long (> (length full) agent-shell-to-go--max-message-length))
              (display
               (if too-long
                   (concat
                    (substring full 0 agent-shell-to-go--max-message-length)
                    agent-shell-to-go--truncation-note)
                 full)))
         (agent-shell-to-go-transport-edit-message
          transport channel-id msg-id display))))
    ((or `(nil . expand-truncated) `(nil . expand-full))
     (let* ((collapsed
             (agent-shell-to-go--load-collapsed-message transport channel-id msg-id))
            (full
             (agent-shell-to-go--load-truncated-message transport channel-id msg-id))
            (restore
             (or collapsed (and full (agent-shell-to-go--truncate-text full 500)))))
       (when restore
         (agent-shell-to-go-transport-edit-message
          transport channel-id msg-id restore))))))

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
