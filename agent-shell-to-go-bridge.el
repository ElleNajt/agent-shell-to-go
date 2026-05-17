;;; agent-shell-to-go-bridge.el --- agent-shell integration for agent-shell-to-go -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Elle Najt

;; Author: Elle Najt
;; Maintainer: junyi.hou <junyi.yi.hou@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Transport-agnostic bridge between agent-shell internals and the
;; agent-shell-to-go transport protocol.
;;
;; This file knows about agent-shell internals (via advice) but knows
;; NOTHING about Slack, mrkdwn, or any specific transport.  All sends
;; go through the transport protocol defined in agent-shell-to-go.el.

;;; Code:

(require 'agent-shell-to-go-core)

; Buffer-local state 

(defvar-local agent-shell-to-go--transport nil
  "The transport struct in use for this `agent-shell' buffer.")

(defvar-local agent-shell-to-go--channel-id nil
  "The remote channel id for this buffer.")

(defvar-local agent-shell-to-go--thread-id nil
  "The remote thread id for this buffer.")

(defvar-local agent-shell-to-go--current-agent-message nil
  "Accumulator for streaming agent message chunks.")

(defvar-local agent-shell-to-go--thread-title-updated nil
  "Non-nil after the thread header has been updated with a session title.")

(defvar-local agent-shell-to-go--turn-complete-subscription nil
  "Subscription token for the turn-complete event (session title fetch).")

(defvar-local agent-shell-to-go--ready-subscription nil
  "Subscription token for flushing agent message and sending ready signal.")

(defvar-local agent-shell-to-go--tool-call-update-subscription nil
  "Subscription token for tool-call-update events.")

(defvar-local agent-shell-to-go--init-client-subscription nil
  "Subscription token for init-client events (failure detection).")

(defvar-local agent-shell-to-go--init-finished-subscription nil
  "Subscription token for init-finished events (Ready signal for new sessions).")

(defvar-local agent-shell-to-go--error-subscription nil
  "Subscription token for error events.")

(defvar-local agent-shell-to-go--tool-calls nil
  "Alist tracking tool calls by toolCallId (id → t if sent).")

(defvar-local agent-shell-to-go--remote-queued nil
  "List of prompts injected from a remote transport, to suppress echo on submit.")

(defvar-local agent-shell-to-go--restarting nil
  "Non-nil when this buffer is being killed as part of a session restart.
Suppresses the '_Session ended_' message in bridge-disable.")

(defvar-local agent-shell-to-go--prev-permission-responder nil
  "Saved value of `agent-shell-permission-responder-function' before bridge-enable.")

; Global state 

(defvar agent-shell-to-go--active-buffers nil
  "List of `'agent-shell' buffers with active mirroring.")

(defvar agent-shell-to-go--pending-permissions nil
  "Alist of pending permission requests.
Key: (transport-name channel-id message-id)
Value: plist with :respond :options")

(defvar agent-shell-to-go--inherit-state nil
  "Plist of transport state for bridge-enable to inherit on a restarted session.
Keys: :transport :channel-id :thread-id.  Consumed (set to nil) on first use.")

(defvar agent-shell-to-go--session-list-cache nil
  "Alist mapping project-path strings to cached ACP session data.
Each entry: (PATH . (:sessions LIST :project-path PATH)).")

; Buffer lookup 

(defun agent-shell-to-go--bridge-active-buffers ()
  "Return the list of active agent-shell-to-go buffers."
  agent-shell-to-go--active-buffers)

(defun agent-shell-to-go--find-buffer-for-transport-channel-thread
    (transport channel-id &optional thread-id)
  "Find an active buffer matching TRANSPORT, CHANNEL-ID and optionally THREAD-ID."
  (cl-find-if
   (lambda (buf)
     (and (buffer-live-p buf)
          (process-live-p (get-buffer-process buf))
          (eq transport (buffer-local-value 'agent-shell-to-go--transport buf))
          (equal channel-id (buffer-local-value 'agent-shell-to-go--channel-id buf))
          (or (not thread-id)
              (equal
               thread-id (buffer-local-value 'agent-shell-to-go--thread-id buf)))))
   agent-shell-to-go--active-buffers))

(defun agent-shell-to-go--bridge-thread-active-p (transport channel-id thread-id)
  "Return non-nil if THREAD-ID in CHANNEL-ID on TRANSPORT has a live buffer."
  (cl-some
   (lambda (buf)
     (and (buffer-live-p buf)
          (process-live-p (get-buffer-process buf))
          (eq transport (buffer-local-value 'agent-shell-to-go--transport buf))
          (equal channel-id (buffer-local-value 'agent-shell-to-go--channel-id buf))
          (equal thread-id (buffer-local-value 'agent-shell-to-go--thread-id buf))))
   agent-shell-to-go--active-buffers))

; Diff extraction 

(defun agent-shell-to-go--parse-unified-diff (diff-string)
  "Parse unified DIFF-STRING into (old-text . new-text)."
  (let (old-lines
        new-lines
        in-hunk)
    (dolist (line (split-string diff-string "\n"))
      (cond
       ((string-match "^@@.*@@" line)
        (setq in-hunk t))
       ((and in-hunk (string-prefix-p " " line))
        (push (substring line 1) old-lines)
        (push (substring line 1) new-lines))
       ((and in-hunk (string-prefix-p "-" line))
        (push (substring line 1) old-lines))
       ((and in-hunk (string-prefix-p "+" line))
        (push (substring line 1) new-lines))))
    (cons
     (string-join (nreverse old-lines) "\n") (string-join (nreverse new-lines) "\n"))))

(defun agent-shell-to-go--extract-diff (update)
  "Extract (old-text . new-text) from tool call UPDATE, or nil."
  (let* ((content (map-elt update 'content))
         (raw-input (map-elt update 'rawInput))
         (content-list
          (cond
           ((vectorp content)
            (append content nil))
           ((and content (listp content) (not (map-elt content 'type)))
            content)
           (content
            (list content)))))
    (cond
     ((and content-list
           (seq-find (lambda (item) (equal (map-elt item 'type) "diff")) content-list))
      (let ((di
             (seq-find
              (lambda (item) (equal (map-elt item 'type) "diff")) content-list)))
        (cons (or (map-elt di 'oldText) "") (map-elt di 'newText))))
     ((and raw-input (map-elt raw-input 'new_str))
      (cons (or (map-elt raw-input 'old_str) "") (map-elt raw-input 'new_str)))
     ((and raw-input (map-elt raw-input 'diff))
      (agent-shell-to-go--parse-unified-diff (map-elt raw-input 'diff))))))

; helpers 

(defun agent-shell-to-go--send (text &optional options)
  "Send TEXT via the buffer-local transport.
OPTIONS is forwarded to `agent-shell-to-go-transport-send-text'."
  (when (and agent-shell-to-go--transport agent-shell-to-go--channel-id)
    (agent-shell-to-go-transport-send-text
     agent-shell-to-go--transport
     agent-shell-to-go--channel-id
     agent-shell-to-go--thread-id
     text
     options)))

(defun agent-shell-to-go--inject-message (text)
  "Inject TEXT into the current agent-shell buffer as if typed locally."
  (when (derived-mode-p 'agent-shell-mode)
    ;; Track every remote-originated prompt so --on-send-command can skip
    ;; mirroring it back (preventing echo), regardless of whether it is
    ;; submitted immediately or dequeued later by agent-shell.
    (push text agent-shell-to-go--remote-queued)
    (if (shell-maker-busy)
        (progn
          (agent-shell--enqueue-request :prompt text)
          (agent-shell-to-go--send
           (format "_Queued_: %s" (agent-shell-to-go--truncate-text text 100))))
      (agent-shell-insert :text text :submit t :no-focus t))))

(defun agent-shell-to-go--find-option-id (options action)
  "Find option id in OPTIONS matching canonical ACTION symbol.
OPTIONS is the enriched list from `agent-shell-permission-responder-function',
where each entry is a plist with :kind and :option-id."
  (let ((kinds
         (pcase action
           ('permission-allow '("allow" "accept" "allow_once"))
           ('permission-always '("always" "alwaysAllow" "allow_always"))
           ('permission-reject '("deny" "reject" "reject_once")))))
    (when-let* ((opt
                 (seq-find (lambda (opt) (member (map-elt opt :kind) kinds)) options)))
      (map-elt opt :option-id))))

(defun agent-shell-to-go--set-mode (buffer mode-id mode-name)
  "Set MODE-ID in BUFFER and notify the thread."
  (with-current-buffer buffer
    (let ((session-id (map-nested-elt agent-shell--state '(:session :id))))
      (if (not session-id)
          (agent-shell-to-go--send "No active session")
        (agent-shell--send-request
         :state agent-shell--state
         :client (map-elt agent-shell--state :client)
         :request (acp-make-session-set-mode-request :session-id session-id :mode-id mode-id)
         :buffer buffer
         :on-success
         (lambda (_)
           (let ((session (map-elt agent-shell--state :session)))
             (map-put! session :mode-id mode-id)
             (map-put! agent-shell--state :session session))
           (agent-shell--update-header-and-mode-line)
           (agent-shell-to-go--send (format "Mode: *%s*" mode-name)))
         :on-failure
         (lambda (err _)
           (agent-shell-to-go--send (format "Failed to set mode: %s" err))))))))

; !-command handler 


(defun agent-shell-to-go--cmd-bypass (_args buffer)
  (agent-shell-to-go--set-mode buffer "bypassPermissions" "Bypass Permissions"))

(defun agent-shell-to-go--cmd-safe (_args buffer)
  (agent-shell-to-go--set-mode buffer "acceptEdits" "Accept Edits"))

(defun agent-shell-to-go--cmd-plan (_args buffer)
  (agent-shell-to-go--set-mode buffer "plan" "Plan"))

(defun agent-shell-to-go--cmd-mode (_args _buffer)
  (let ((mode-id (map-nested-elt agent-shell--state '(:session :mode-id))))
    (agent-shell-to-go--send (format "Mode: *%s*" (or mode-id "unknown")))))

(defun agent-shell-to-go--cmd-help (_args _buffer)
  (agent-shell-to-go--send
   (concat
    "*Commands:*\n"
    "`!yolo` — bypass permissions\n"
    "`!safe` — accept edits mode\n"
    "`!plan` — plan mode\n"
    "`!mode` — show current mode\n"
    "`!stop` — interrupt agent\n"
    "`!restart` — kill and restart agent\n"
    "`!queue` — show queued messages\n"
    "`!clearqueue` — clear queued messages\n"
    "`!info` — show session info\n"
    "`!projects` — list projects\n"
    "`!new-agent [project]` — start agent in project, or new agent in current project\n"
    "`!new-project <name>` — create project and start agent\n"
    "`!resume [N]` — list sessions, or resume session N if given")))

(defun agent-shell-to-go--cmd-queue (_args _buffer)
  (let ((pending (map-elt agent-shell--state :pending-requests)))
    (if (seq-empty-p pending)
        (agent-shell-to-go--send "No pending requests")
      (agent-shell-to-go--send
       (format "*Pending (%d):*\n%s"
               (length pending)
               (mapconcat (lambda (r)
                            (format "- %s" (agent-shell-to-go--truncate-text r 80)))
                          pending
                          "\n"))))))

(defun agent-shell-to-go--cmd-clearqueue (_args _buffer)
  (let ((count (length (map-elt agent-shell--state :pending-requests))))
    (map-put! agent-shell--state :pending-requests nil)
    (agent-shell-to-go--send
     (format "Cleared %d request%s"
             count
             (if (= count 1)
                 ""
               "s")))))

(defun agent-shell-to-go--cmd-info (_args buffer)
  (let* ((state agent-shell--state)
         (session-id (map-nested-elt state '(:session :id)))
         (mode-id (map-nested-elt state '(:session :mode-id)))
         (truncated-count
          (let ((dir
                 (expand-file-name (format "truncated/%s/"
                                           agent-shell-to-go--channel-id)
                                   (agent-shell-to-go-transport-storage-root
                                    agent-shell-to-go--transport))))
            (if (file-directory-p dir)
                (length (directory-files dir nil "\\.txt$"))
              0))))
    (agent-shell-to-go--send
     (format
      "*Debug*\nBuffer: `%s`\nThread: `%s`\nChannel: `%s`\nSession: `%s`\nMode: `%s`\nTruncated: %d"
      (buffer-name buffer)
      agent-shell-to-go--thread-id
      agent-shell-to-go--channel-id
      (or session-id "none")
      (or mode-id "default")
      truncated-count))))

(defun agent-shell-to-go--cmd-stop (_args _buffer)
  (condition-case err
      (progn
        (agent-shell-interrupt t)
        (agent-shell-to-go--send "Agent interrupted"))
    (error
     (agent-shell-to-go--send (format "Stop failed: %s" err)))))

(defun agent-shell-to-go--cmd-restart (_args buffer)
  (condition-case err
      (let* ((session-id (map-nested-elt agent-shell--state '(:session :id))))
        (unless session-id
          (error "No active session to restart"))
        (setq agent-shell-to-go--inherit-state
              (list
               :transport agent-shell-to-go--transport
               :channel-id agent-shell-to-go--channel-id
               :thread-id agent-shell-to-go--thread-id))
        (setq agent-shell-to-go--restarting t)
        (agent-shell-to-go--send "Restarting agent…")
        (ignore-errors
          (agent-shell-interrupt t))
        (condition-case restart-err
            (when-let* ((win (agent-shell-restart :session-id session-id))
                        (new-buf (and (windowp win) (window-buffer win)))
                        (_ (buffer-live-p new-buf))
                        (_
                         (with-current-buffer new-buf
                           (derived-mode-p 'agent-shell-mode))))
              (with-current-buffer new-buf
                (agent-shell-to-go-mode 1)))
          (error
           (setq agent-shell-to-go--inherit-state nil)
           (agent-shell-to-go--debug "restart failed: %s" restart-err))))
    (error
     (setq agent-shell-to-go--inherit-state nil)
     (agent-shell-to-go--send (format "Restart failed: %s" err)))))

(defun agent-shell-to-go--cmd-projects (_args _buffer)
  (let* ((dir (expand-file-name agent-shell-to-go-projects-directory))
         (names
          (and (file-directory-p dir)
               (seq-filter
                (lambda (f)
                  (and (not (string-prefix-p "." f))
                       (file-directory-p (expand-file-name f dir))))
                (directory-files dir)))))
    (if (null names)
        (agent-shell-to-go--send (format "No projects found in `%s`." dir))
      (agent-shell-to-go--send
       (string-join (cons
                     (format "Projects in `%s`:" dir)
                     (mapcar (lambda (n) (format "• %s" n)) names))
                    "\n")))))

(defun agent-shell-to-go--cmd-new-agent (args _buffer)
  (let ((arg (car args)))
    (cond
     ((and arg (not (string-match-p (agent-shell-to-go--project-name-regexp) arg)))
      (agent-shell-to-go--send
       "Usage: `!new-agent [project]` — project must be an existing subdirectory name"))
     (arg
      (let ((folder (expand-file-name arg agent-shell-to-go-projects-directory)))
        (if (not (file-directory-p folder))
            (agent-shell-to-go--send
             (format "Unknown project: `%s`. Use `!projects` to see available projects."
                     arg))
          (agent-shell-to-go--send (format "Starting agent in `%s`..." folder))
          (if (agent-shell-to-go--start-agent-in-folder folder)
              (agent-shell-to-go--send (format "Agent started in `%s`" folder))
            (agent-shell-to-go--send "Failed to start agent.")))))
     (t
      (agent-shell-to-go--send "Starting new agent…")
      (if (agent-shell-to-go--start-agent-in-folder default-directory)
          (agent-shell-to-go--send "Agent started")
        (agent-shell-to-go--send "Failed to start agent."))))))

(defun agent-shell-to-go--cmd-new-project (args _buffer)
  (let
      ((usage
        "Usage: `!new-project <name>` — name may only contain letters, digits, `-`, `_`, `.`"))
    (cond
     ((not (car args))
      (agent-shell-to-go--send usage))
     ((not (string-match-p "\\`[a-zA-Z0-9_.-]+\\'" (car args)))
      (agent-shell-to-go--send usage))
     (t
      (let ((project-dir
             (expand-file-name (car args) agent-shell-to-go-projects-directory)))
        (if (file-exists-p project-dir)
            (agent-shell-to-go--send
             (format "Project already exists: `%s`" project-dir))
          (agent-shell-to-go--send (format "Creating project: `%s`" project-dir))
          (let ((start-fn
                 (lambda (final-dir)
                   (agent-shell-to-go--send "Starting Claude Code…")
                   (if (agent-shell-to-go--start-agent-in-folder final-dir)
                       (agent-shell-to-go--send
                        (format "Agent started in `%s`" final-dir))
                     (agent-shell-to-go--send "Failed to start agent.")))))
            (if agent-shell-to-go-new-project-function
                (funcall agent-shell-to-go-new-project-function
                         (car args)
                         (expand-file-name agent-shell-to-go-projects-directory)
                         start-fn)
              (make-directory project-dir t)
              (funcall start-fn project-dir)))))))))

(defun agent-shell-to-go--cmd-sessions (_args _buffer)
  (let* ((project-path
          (agent-shell-to-go--resolve-project-from-channel
           agent-shell-to-go--channel-id))
         (tport agent-shell-to-go--transport)
         (chan agent-shell-to-go--channel-id)
         (thrd agent-shell-to-go--thread-id)
         (send
          (lambda (text) (agent-shell-to-go-transport-send-text tport chan thrd text))))
    (if (not project-path)
        (agent-shell-to-go--send "Cannot determine project.")
      (agent-shell-to-go--fetch-sessions
       project-path
       (lambda (sessions)
         (funcall send (agent-shell-to-go--format-session-list project-path sessions)))
       send))))

(defun agent-shell-to-go--cmd-resume (args _buffer)
  (let* ((n (max 1 (string-to-number (car args))))
         (project-path
          (agent-shell-to-go--resolve-project-from-channel
           agent-shell-to-go--channel-id))
         (tport agent-shell-to-go--transport)
         (chan agent-shell-to-go--channel-id)
         (thrd agent-shell-to-go--thread-id)
         (reply
          (lambda (text) (agent-shell-to-go-transport-send-text tport chan thrd text))))
    (if (not project-path)
        (agent-shell-to-go--send "Cannot determine project.")
      (let* ((cached
              (alist-get project-path agent-shell-to-go--session-list-cache
                         nil
                         nil
                         #'equal))
             (sessions (map-elt cached :sessions)))
        (if sessions
            (agent-shell-to-go--do-resume reply project-path sessions n)
          (agent-shell-to-go--fetch-sessions
           project-path
           (lambda (fetched)
             (agent-shell-to-go--do-resume reply project-path fetched n))
           reply))))))

(defun agent-shell-to-go--project-name-regexp ()
  "Return a regexp matching any existing project directory name."
  (let* ((dir (expand-file-name agent-shell-to-go-projects-directory))
         (names
          (and (file-directory-p dir)
               (seq-filter
                (lambda (f)
                  (and (not (string-prefix-p "." f))
                       (file-directory-p (expand-file-name f dir))))
                (directory-files dir)))))
    (if names
        (regexp-opt names)
      "\\`\\'")))

(defconst agent-shell-to-go--commands
  `( ;; mode
    (,#'agent-shell-to-go--cmd-bypass "!yolo")
    (,#'agent-shell-to-go--cmd-bypass "!bypass")
    (,#'agent-shell-to-go--cmd-safe "!safe")
    (,#'agent-shell-to-go--cmd-safe "!accept")
    (,#'agent-shell-to-go--cmd-safe "!acceptedits")
    (,#'agent-shell-to-go--cmd-plan "!plan")
    (,#'agent-shell-to-go--cmd-plan "!planmode")
    ;; info
    (,#'agent-shell-to-go--cmd-mode "!mode")
    (,#'agent-shell-to-go--cmd-help "!help")
    (,#'agent-shell-to-go--cmd-info "!info")
    ;; queue
    (,#'agent-shell-to-go--cmd-queue "!queue")
    (,#'agent-shell-to-go--cmd-clearqueue "!clearqueue")
    ;; control
    (,#'agent-shell-to-go--cmd-stop "!stop")
    (,#'agent-shell-to-go--cmd-restart "!restart")
    ;; projects
    (,#'agent-shell-to-go--cmd-projects "!projects")
    (,#'agent-shell-to-go--cmd-new-agent "!new-agent")
    (,#'agent-shell-to-go--cmd-new-agent
     "!new-agent" ,#'agent-shell-to-go--project-name-regexp)
    (,#'agent-shell-to-go--cmd-new-project "!new-project")
    (,#'agent-shell-to-go--cmd-new-project "!new-project" "[a-zA-Z0-9_.-]+")
    ;; sessions
    (,#'agent-shell-to-go--cmd-sessions "!resume")
    (,#'agent-shell-to-go--cmd-resume "!resume" "[0-9]+"))
  "Alist of (HANDLER CMD ARG-REGEX...) for !-command dispatch.
Each entry builds the regex ^CMD\\s-+ARG1...$ at dispatch time.
HANDLER is called with (ARGS BUFFER) with BUFFER current.")

(defun agent-shell-to-go--dispatch (text buffer)
  "Dispatch TEXT as a !-command in BUFFER, or inject it as a message."
  (if (not (string-prefix-p "!" text))
      (agent-shell-to-go--inject-message text)
    ;; Extract cmd and at most one arg token without splitting the full text.
    (let* ((sep (string-match "\\s-+" text))
           (cmd
            (if sep
                (substring text 0 sep)
              text))
           (arg
            (and sep
                 (let ((arg-str
                        (substring text
                                   (match-end 0)
                                   (string-match "\\s-+" text (match-end 0)))))
                   (and (not (string-empty-p arg-str)) arg-str))))
           (key
            (if arg
                (concat cmd " " arg)
              cmd))
           ;; Phase 1: entries whose command string literally matches cmd.
           (cmd-entries
            (seq-filter (lambda (e) (equal (cadr e) cmd)) agent-shell-to-go--commands))
           ;; Phase 2 (lazy): only among cmd-entries, find one whose full regex
           ;; matches key.  Skipped when cmd-entries is nil.
           (full-entry
            (and cmd-entries
                 (seq-find
                  (lambda (e)
                    (string-match-p
                     (concat
                      "^"
                      (string-join (mapcar
                                    (lambda (p)
                                      (if (functionp p)
                                          (funcall p)
                                        p))
                                    (cdr e))
                                   "\\s-+")
                      "$")
                     key))
                  cmd-entries))))
      (cond
       ;; Unknown command — pass through to agent.
       ((null cmd-entries)
        (agent-shell-to-go--inject-message text))
       ;; Known command, arg (if any) matches pattern — dispatch normally.
       (full-entry
        (funcall (car full-entry) (and arg (list arg)) buffer))
       ;; Known command, arg does not match — call the first handler for this
       ;; cmd with the raw arg; the handler is responsible for the error message.
       (t
        (funcall (caar cmd-entries) (and arg (list arg)) buffer))))))

; Inbound hook handlers (registered on message/reaction hooks)

(cl-defun agent-shell-to-go--bridge-on-message
    (&key transport channel-id thread-id text &allow-other-keys)
  "Handle an inbound message from a transport."
  (when-let* ((buffer
               (agent-shell-to-go--find-buffer-for-transport-channel-thread
                transport channel-id
                thread-id)))
    (with-current-buffer buffer
      (agent-shell-to-go--dispatch text buffer))))

(cl-defun agent-shell-to-go--bridge-on-reaction
    (&key transport channel-id msg-id action added-p &allow-other-keys)
  "Handle permission reactions from a transport.
Presentation reactions are handled by the main dispatcher registered first."
  (when (and added-p
             (memq action '(permission-allow permission-always permission-reject)))
    (let* ((key (list (agent-shell-to-go-transport-name transport) channel-id msg-id))
           (pending (assoc key agent-shell-to-go--pending-permissions #'equal)))
      (when pending
        (let* ((info (cdr pending))
               (respond (map-elt info :respond))
               (options (map-elt info :options))
               (option-id (agent-shell-to-go--find-option-id options action)))
          (when option-id
            (funcall respond option-id)
            (setq agent-shell-to-go--pending-permissions
                  (cl-remove key agent-shell-to-go--pending-permissions
                             :key #'car
                             :test #'equal))))))))


(defun agent-shell-to-go--start-agent-in-folder (folder)
  "Start an agent in FOLDER.  Returns non-nil on success."
  (agent-shell-to-go--debug "starting agent in %s" folder)
  (when (file-directory-p folder)
    (let ((default-directory folder))
      (save-window-excursion
        (condition-case err
            (let ((bufs-before (agent-shell-buffers)))
              ;; Omit :channel-id so bridge-enable derives the correct project
              ;; channel via ensure-project-channel, regardless of which
              ;; channel the command was invoked from.
              (setq agent-shell-to-go--inherit-state
                    (list :transport agent-shell-to-go--transport))
              (funcall agent-shell-to-go-start-agent-function)
              (when-let* ((new-buf
                           (cl-find-if
                            (lambda (b) (not (memq b bufs-before)))
                            (agent-shell-buffers))))
                (with-current-buffer new-buf
                  (agent-shell-to-go-mode 1))
                t))
          (error
           (setq agent-shell-to-go--inherit-state nil)
           (agent-shell-to-go--debug "error starting agent: %s" err)
           nil))))))

; !sessions and !resume helpers 

(defun agent-shell-to-go--resolve-project-from-channel (channel-id)
  "Return project path for CHANNEL-ID by scanning active buffers."
  (when-let* ((buf
               (cl-find-if
                (lambda (b)
                  (and (buffer-live-p b)
                       (equal
                        channel-id
                        (buffer-local-value 'agent-shell-to-go--channel-id b))))
                agent-shell-to-go--active-buffers)))
    (expand-file-name (buffer-local-value 'default-directory buf))))

(defun agent-shell-to-go--format-session-age (updated-at)
  "Format UPDATED-AT (ISO 8601 string or epoch number) as a relative age."
  (condition-case nil
      (let* ((epoch
              (cond
               ((stringp updated-at)
                (float-time (date-to-time updated-at)))
               ((numberp updated-at)
                (float updated-at))
               (t
                (float-time updated-at))))
             (age (max 0 (- (float-time) epoch)))
             (mins (floor (/ age 60)))
             (hours (floor (/ age 3600)))
             (days (floor (/ age 86400))))
        (cond
         ((< age 3600)
          (format "%dm ago" (max 1 mins)))
         ((< age 86400)
          (format "%dh ago" hours))
         ((< age 604800)
          (format "%dd ago" days))
         (t
          (format "%dwk ago" (floor (/ days 7))))))
    (error
     "unknown")))

(defun agent-shell-to-go--format-session-list (project-path sessions)
  "Format SESSIONS for PROJECT-PATH as a reply string."
  (let ((name (file-name-nondirectory (directory-file-name project-path))))
    (if (null sessions)
        (format "No sessions found for *%s*." name)
      (string-join (cons
                    (format "Sessions for *%s*:" name)
                    (cl-loop
                     for session in sessions for i from 1 collect
                     (format "%d. %s  · %s"
                             i (or (map-elt session 'title) "Untitled")
                             (agent-shell-to-go--format-session-age
                              (map-elt session 'updatedAt)))))
                   "\n"))))

(defun agent-shell-to-go--fetch-sessions (project-path on-success on-failure)
  "Fetch ACP sessions for PROJECT-PATH asynchronously.
Calls ON-SUCCESS with the session list or ON-FAILURE with an error string.
Updates `agent-shell-to-go--session-list-cache' on success."
  (let ((ref-buf (cl-find-if #'buffer-live-p agent-shell-to-go--active-buffers)))
    (if (not ref-buf)
        (funcall on-failure "No active session found.")
      (with-current-buffer ref-buf
        (let ((client (map-elt agent-shell--state :client))
              (cwd (agent-shell--resolve-path project-path)))
          (if (not client)
              (funcall on-failure "No active ACP client.")
            (acp-send-request
             :client client
             :request (acp-make-session-list-request :cwd cwd)
             :buffer ref-buf
             :on-success
             (lambda (resp)
               (let ((sessions (append (or (map-elt resp 'sessions) '()) nil)))
                 (setf (alist-get project-path agent-shell-to-go--session-list-cache
                                  nil
                                  nil
                                  #'equal)
                       (list :sessions sessions :project-path project-path))
                 (funcall on-success sessions)))
             :on-failure
             (lambda (_err _raw)
               (funcall on-failure "Failed to fetch session list.")))))))))

(defun agent-shell-to-go--do-resume (reply project-path sessions n)
  "Resume the Nth entry in SESSIONS for PROJECT-PATH, notifying via REPLY."
  (let* ((session (nth (1- n) sessions))
         (sess-id (and session (map-elt session 'sessionId)))
         (title (and session (or (map-elt session 'title) "Untitled"))))
    (if (not sess-id)
        (funcall reply
                 (format "No session #%d. Run `!resume` to list available sessions." n))
      (funcall reply (format "Resuming: _%s_…" title))
      (save-window-excursion
        (condition-case err
            (let ((bufs-before (agent-shell-buffers)))
              (setq agent-shell-to-go--inherit-state
                    (list :transport agent-shell-to-go--transport))
              (let ((default-directory (expand-file-name project-path)))
                (agent-shell-resume-session sess-id))
              (when-let* ((new-buf
                           (cl-find-if
                            (lambda (b) (not (memq b bufs-before)))
                            (agent-shell-buffers))))
                (with-current-buffer new-buf
                  (agent-shell-to-go-mode 1))))
          (error
           (setq agent-shell-to-go--inherit-state nil)
           (funcall reply (format "Failed to resume: %s" err))))))))

; agent shell subscriptions 

;; TODO: use the new session-title-changed event
;; This needs a more recent `agent-shell'
(defun agent-shell-to-go--fetch-session-title (_event)
  "Fetch session title via ACP and update the thread header."
  (when (and (not agent-shell-to-go--thread-title-updated)
             agent-shell-to-go--thread-id
             (boundp 'agent-shell--state)
             agent-shell--state)
    (let* ((session-id (map-nested-elt agent-shell--state '(:session :id)))
           (client (map-elt agent-shell--state :client))
           (cwd (agent-shell--resolve-path default-directory)))
      (when (and session-id client cwd)
        (acp-send-request
         :client client
         :request (acp-make-session-list-request :cwd cwd)
         :buffer (current-buffer)
         :on-success
         (lambda (resp)
           (let* ((sessions (append (or (map-elt resp 'sessions) '()) nil))
                  (current
                   (seq-find
                    (lambda (s) (equal (map-elt s 'sessionId) session-id)) sessions))
                  (title (and current (map-elt current 'title))))
             (when (and title (not (string-empty-p title)))
               (agent-shell-to-go-transport-update-thread-header
                agent-shell-to-go--transport
                agent-shell-to-go--channel-id
                agent-shell-to-go--thread-id
                title)
               (setq agent-shell-to-go--thread-title-updated t)
               (when agent-shell-to-go--turn-complete-subscription
                 (agent-shell-unsubscribe
                  :subscription agent-shell-to-go--turn-complete-subscription)
                 (setq agent-shell-to-go--turn-complete-subscription nil)))))
         :on-failure
         (lambda (_err _raw)
           (agent-shell-to-go--debug "failed to fetch session title")))))))

(defun agent-shell-to-go--on-turn-complete (_event)
  "Flush buffered agent message and send ready signal."
  (when agent-shell-to-go-mode
    (when (and agent-shell-to-go--current-agent-message
               (> (length agent-shell-to-go--current-agent-message) 0))
      (agent-shell-to-go--send
       (agent-shell-to-go-transport-format-agent-message
        agent-shell-to-go--transport agent-shell-to-go--current-agent-message))
      (setq agent-shell-to-go--current-agent-message nil))
    (when agent-shell-to-go--thread-id
      (agent-shell-to-go--send "_Ready for input_"))))

(defun agent-shell-to-go--permission-responder (permission)
  "Handle a PERMISSION request by notifying the remote transport.
Set as `agent-shell-permission-responder-function' when mirroring is on.
Falls back to `agent-shell-to-go--prev-permission-responder' when transport
is not ready.  Returns t to suppress the Emacs permission UI."
  (if (and agent-shell-to-go-mode
           agent-shell-to-go--transport
           agent-shell-to-go--thread-id)
      (let*
          ((tool-call (map-elt permission :tool-call))
           (options (map-elt permission :options))
           (respond (map-elt permission :respond))
           (title (or (map-elt tool-call :title) "Unknown action"))
           (msg-id
            (condition-case err
                (agent-shell-to-go-transport-send-text
                 agent-shell-to-go--transport
                 agent-shell-to-go--channel-id
                 agent-shell-to-go--thread-id
                 (format
                  "*Permission Required*\n`%s`\n\nReact to approve, deny, or always allow."
                  title))
              (error
               (agent-shell-to-go--debug "permission notify error: %s" err)
               nil))))
        (if msg-id
            (progn
              (push (cons
                     (list
                      (agent-shell-to-go-transport-name
                       agent-shell-to-go--transport)
                      agent-shell-to-go--channel-id msg-id)
                     (list :respond respond :options options))
                    agent-shell-to-go--pending-permissions)
              t)
          (when (functionp agent-shell-to-go--prev-permission-responder)
            (funcall agent-shell-to-go--prev-permission-responder permission))))
    (when (functionp agent-shell-to-go--prev-permission-responder)
      (funcall agent-shell-to-go--prev-permission-responder permission))))

(defun agent-shell-to-go--on-send-command (orig-fn &rest args)
  "Advice around `agent-shell--send-command'.  Mirror user prompts."
  (let ((prompt (map-elt args :prompt)))
    (if (and prompt (member prompt agent-shell-to-go--remote-queued))
        (setq agent-shell-to-go--remote-queued
              (delete prompt agent-shell-to-go--remote-queued))
      (when (and agent-shell-to-go-mode agent-shell-to-go--thread-id prompt)
        (agent-shell-to-go--send
         (agent-shell-to-go-transport-format-user-message
          agent-shell-to-go--transport prompt))
        (agent-shell-to-go--send "Processing..."))))
  (setq agent-shell-to-go--current-agent-message nil)
  (apply orig-fn args))

(defun agent-shell-to-go--on-init-client (_event)
  "Handle init-client event.  Send failure notice if client was not created."
  (when (and agent-shell-to-go-mode
             agent-shell-to-go--thread-id
             (not (map-elt agent-shell--state :client)))
    (agent-shell-to-go--send "*Agent failed to start* — check API key / OAuth token")))

(defun agent-shell-to-go--on-init-finished (_event)
  "Handle init-finished event.  Notify remote that the session is connected."
  (when (and agent-shell-to-go-mode agent-shell-to-go--thread-id)
    (agent-shell-to-go--send "_Connected_")))

(defun agent-shell-to-go--on-error (event)
  "Handle error event.  Forward the error message to the remote transport."
  (when (and agent-shell-to-go-mode agent-shell-to-go--thread-id)
    (let* ((data (map-elt event :data))
           (code (map-elt data :code))
           (message (map-elt data :message)))
      (agent-shell-to-go--send (format "*Agent error* — %s: %s" code message)))))

;; there is no event available on notification so we use this
(defun agent-shell-to-go--on-notification (orig-fn &rest args)
  "Advice around `agent-shell--on-notification'.  Accumulate agent message chunks."
  (let* ((state (map-elt args :state))
         (buffer (map-elt state :buffer)))
    (when (and buffer
               (buffer-live-p buffer)
               (buffer-local-value 'agent-shell-to-go-mode buffer))
      (let* ((notification (map-elt args :acp-notification))
             (params (map-elt notification 'params))
             (update (map-elt params 'update))
             (thread-id (buffer-local-value 'agent-shell-to-go--thread-id buffer)))
        (when (and thread-id
                   (equal (map-elt update 'sessionUpdate) "agent_message_chunk"))
          (let ((text (map-elt (map-elt update 'content) 'text)))
            (with-current-buffer buffer
              (setq agent-shell-to-go--current-agent-message
                    (concat agent-shell-to-go--current-agent-message text))))))))
  (apply orig-fn args))

(defun agent-shell-to-go--bridge-on-tool-call-update (event)
  "Handle a tool-call-update EVENT from agent-shell.
Called via `agent-shell-subscribe-to' with the shell buffer current."
  (when (and agent-shell-to-go-mode agent-shell-to-go--thread-id)
    (let* ((data (map-elt event :data))
           (tool-call-id (map-elt data :tool-call-id))
           (tool-call (map-elt data :tool-call))
           (status (map-elt tool-call :status))
           (raw-input (map-elt tool-call :raw-input))
           (content (map-elt tool-call :content))
           (title (map-elt tool-call :title))
           (pseudo-update `((rawInput . ,raw-input) (content . ,content))))
      (if (member status '("completed" "failed"))
          (let* ((content-text
                  (and content
                       (mapconcat (lambda (item)
                                    (or (map-elt (map-elt item 'content) 'text)
                                        (map-elt item 'text)
                                        ""))
                                  (if (vectorp content)
                                      (append content nil)
                                    (if (listp content)
                                        content
                                      nil))
                                  "\n")))
                 (output content-text)
                 (diff
                  (condition-case nil
                      (agent-shell-to-go--extract-diff pseudo-update)
                    (error
                     nil)))
                 (diff-text
                  (and diff
                       (condition-case nil
                           (agent-shell-to-go-transport-format-diff
                            agent-shell-to-go--transport (car diff) (cdr diff))
                         (error
                          nil))))
                 (start-msg-id (map-elt agent-shell-to-go--tool-calls tool-call-id)))
            (agent-shell-to-go--debug
             "tool_call complete: id=%s status=%s start-msg-id=%s"
             tool-call-id
             status
             start-msg-id)
            (let* ((summary
                    (agent-shell-to-go-transport-format-tool-call-result
                     agent-shell-to-go--transport title status nil))
                   (target-channel
                    (or agent-shell-to-go--thread-id agent-shell-to-go--channel-id)))
              (cond
               ((and diff-text (> (length diff-text) 0))
                (let* ((full (format "%s\n%s" summary diff-text))
                       (visible
                        (if agent-shell-to-go-show-tool-output
                            full
                          summary)))
                  (if start-msg-id
                      (progn
                        (agent-shell-to-go-transport-edit-message
                         agent-shell-to-go--transport
                         target-channel
                         start-msg-id
                         visible)
                        (unless agent-shell-to-go-show-tool-output
                          (agent-shell-to-go--save-truncated-message
                           agent-shell-to-go--transport
                           agent-shell-to-go--channel-id
                           start-msg-id
                           full
                           summary))
                        (setf (alist-get tool-call-id agent-shell-to-go--tool-calls)
                              nil))
                    (if agent-shell-to-go-show-tool-output
                        (agent-shell-to-go--send full '(:truncate t))
                      (let ((msg-id (agent-shell-to-go--send summary)))
                        (when msg-id
                          (agent-shell-to-go--save-truncated-message
                           agent-shell-to-go--transport
                           agent-shell-to-go--channel-id
                           msg-id
                           full
                           summary)))))))
               ((and output (stringp output) (> (length output) 0))
                (let* ((full
                        (agent-shell-to-go-transport-format-tool-call-result
                         agent-shell-to-go--transport title status output))
                       (visible
                        (if agent-shell-to-go-show-tool-output
                            full
                          summary)))
                  (if start-msg-id
                      (progn
                        (agent-shell-to-go-transport-edit-message
                         agent-shell-to-go--transport
                         target-channel
                         start-msg-id
                         visible)
                        (unless agent-shell-to-go-show-tool-output
                          (agent-shell-to-go--save-truncated-message
                           agent-shell-to-go--transport
                           agent-shell-to-go--channel-id
                           start-msg-id
                           full
                           summary))
                        (setf (alist-get tool-call-id agent-shell-to-go--tool-calls)
                              nil))
                    (if agent-shell-to-go-show-tool-output
                        (agent-shell-to-go--send full '(:truncate t))
                      (let ((msg-id (agent-shell-to-go--send summary)))
                        (when msg-id
                          (agent-shell-to-go--save-truncated-message
                           agent-shell-to-go--transport
                           agent-shell-to-go--channel-id
                           msg-id
                           full
                           summary)))))))
               (t
                (if start-msg-id
                    (progn
                      (agent-shell-to-go-transport-edit-message
                       agent-shell-to-go--transport target-channel start-msg-id summary)
                      (setf (alist-get tool-call-id agent-shell-to-go--tool-calls) nil))
                  (agent-shell-to-go--send summary))))))
        ;; Tool call started — flush any buffered agent text first, then notify
        (let* ((command (map-elt raw-input 'command))
               (file-path (map-elt raw-input 'file_path))
               (query (map-elt raw-input 'query))
               (url (map-elt raw-input 'url))
               (specific (or command file-path query url))
               (already-sent
                (and tool-call-id
                     (map-elt agent-shell-to-go--tool-calls tool-call-id))))
          (when (and (not already-sent) (or specific title))
            (when (and agent-shell-to-go--current-agent-message
                       (> (length agent-shell-to-go--current-agent-message) 0))
              (agent-shell-to-go--send
               (agent-shell-to-go-transport-format-agent-message
                agent-shell-to-go--transport agent-shell-to-go--current-agent-message))
              (setq agent-shell-to-go--current-agent-message nil))
            (let* ((title-has-specific
                    (and title specific (string-match-p (regexp-quote specific) title)))
                   (display
                    (cond
                     (command
                      command)
                     (title-has-specific
                      title)
                     ((and file-path title)
                      (format "%s: %s" title file-path))
                     ((and query title)
                      (format "%s: %s" title query))
                     ((and url title)
                      (format "%s: %s" title url))
                     (specific
                      specific)
                     (t
                      title)))
                   (diff
                    (condition-case nil
                        (agent-shell-to-go--extract-diff pseudo-update)
                      (error
                       nil)))
                   (diff-text
                    (and diff
                         (condition-case nil
                             (agent-shell-to-go-transport-format-diff
                              agent-shell-to-go--transport (car diff) (cdr diff))
                           (error
                            nil)))))
              (let ((start-msg-id
                     (condition-case err
                         (if (and diff-text (> (length diff-text) 0))
                             (let* ((start-text
                                     (agent-shell-to-go-transport-format-tool-call-start
                                      agent-shell-to-go--transport display))
                                    (full (format "%s\n%s" start-text diff-text)))
                               (if agent-shell-to-go-show-tool-output
                                   (agent-shell-to-go--send full '(:truncate t))
                                 (agent-shell-to-go--send start-text)))
                           (agent-shell-to-go--send
                            (agent-shell-to-go-transport-format-tool-call-start
                             agent-shell-to-go--transport display)
                            '(:truncate t)))
                       (error
                        (agent-shell-to-go--debug "tool_call send error: %s" err)
                        nil))))
                (setf (alist-get tool-call-id agent-shell-to-go--tool-calls)
                      start-msg-id)))))))))


; Hook registration 

(add-hook 'agent-shell-to-go-message-hook #'agent-shell-to-go--bridge-on-message)
(add-hook 'agent-shell-to-go-reaction-hook #'agent-shell-to-go--bridge-on-reaction)

; Enable / disable 

(defun agent-shell-to-go--bridge-enable ()
  "Enable transport mirroring for this buffer."
  ;; `agent-shell-to-go--inherit-state' is set by !restart to carry the old
  ;; session's transport/channel/thread over to the new buffer.  Consumed
  ;; here (set to nil) so it's a one-shot handoff and can't leak elsewhere.
  (let* ((inherited agent-shell-to-go--inherit-state)
         (_ (setq agent-shell-to-go--inherit-state nil))
         (transport
          (or (map-elt inherited :transport) (agent-shell-to-go--get-transport)))
         (project-path (agent-shell-to-go--get-project-path)))
    ;; Load credentials / connect if needed
    (unless (agent-shell-to-go-transport-connected-p transport)
      (agent-shell-to-go-transport-connect transport))
    ;; Resolve channel (reuse inherited or create fresh)
    (setq agent-shell-to-go--transport transport)
    (setq agent-shell-to-go--channel-id
          (or (map-elt inherited :channel-id)
              (agent-shell-to-go-transport-ensure-project-channel
               transport project-path)))
    ;; Start thread (reuse inherited or create fresh)
    (setq agent-shell-to-go--thread-id
          (or (map-elt inherited :thread-id)
              (agent-shell-to-go-transport-start-thread
               transport agent-shell-to-go--channel-id (buffer-name))))
    ;; Track buffer
    (add-to-list 'agent-shell-to-go--active-buffers (current-buffer))

    ;; Save and install permission responder
    (setq agent-shell-to-go--prev-permission-responder
          agent-shell-permission-responder-function)
    (setq-local agent-shell-permission-responder-function
                #'agent-shell-to-go--permission-responder)
    ;; Add advice
    (advice-add 'agent-shell--send-command :around #'agent-shell-to-go--on-send-command)
    (advice-add
     'agent-shell--on-notification
     :around #'agent-shell-to-go--on-notification)
    ;; Subscribe to init-client to detect client creation failure
    (setq agent-shell-to-go--init-client-subscription
          (agent-shell-subscribe-to
           :shell-buffer (current-buffer)
           :event 'init-client
           :on-event #'agent-shell-to-go--on-init-client))
    ;; Subscribe to init-finished to send Ready when a new session connects.
    ;; init-client fires synchronously inside agent-shell-start so it is too
    ;; early; init-finished fires async after the ACP handshake completes.
    (setq agent-shell-to-go--init-finished-subscription
          (agent-shell-subscribe-to
           :shell-buffer (current-buffer)
           :event 'init-finished
           :on-event #'agent-shell-to-go--on-init-finished))
    ;; Subscribe to error events and forward to remote transport
    (setq agent-shell-to-go--error-subscription
          (agent-shell-subscribe-to
           :shell-buffer (current-buffer)
           :event 'error
           :on-event #'agent-shell-to-go--on-error))
    ;; Subscribe to turn-complete for session title
    (setq agent-shell-to-go--turn-complete-subscription
          (agent-shell-subscribe-to
           :shell-buffer (current-buffer)
           :event 'turn-complete
           :on-event #'agent-shell-to-go--fetch-session-title))
    ;; Subscribe to turn-complete for flush + ready signal
    (setq agent-shell-to-go--ready-subscription
          (agent-shell-subscribe-to
           :shell-buffer (current-buffer)
           :event 'turn-complete
           :on-event #'agent-shell-to-go--on-turn-complete))
    ;; Subscribe to tool-call-update for tool call mirroring
    (setq agent-shell-to-go--tool-call-update-subscription
          (agent-shell-subscribe-to
           :shell-buffer (current-buffer)
           :event 'tool-call-update
           :on-event #'agent-shell-to-go--bridge-on-tool-call-update))
    ;; Kill hook
    (add-hook 'kill-buffer-hook #'agent-shell-to-go--on-buffer-kill nil t)
    (agent-shell-to-go--debug
     "bridge enabled, thread=%s" agent-shell-to-go--thread-id)))

(defun agent-shell-to-go--on-buffer-kill ()
  "Hook to run when an agent-shell buffer is killed."
  (when agent-shell-to-go-mode
    (agent-shell-to-go--bridge-disable)))

(defun agent-shell-to-go--bridge-disable ()
  "Disable transport mirroring for this buffer."
  (remove-hook 'kill-buffer-hook #'agent-shell-to-go--on-buffer-kill t)
  (dolist (sub
           (list
            agent-shell-to-go--init-client-subscription
            agent-shell-to-go--init-finished-subscription
            agent-shell-to-go--error-subscription
            agent-shell-to-go--turn-complete-subscription
            agent-shell-to-go--ready-subscription
            agent-shell-to-go--tool-call-update-subscription))
    (when sub
      (ignore-errors
        (agent-shell-unsubscribe :subscription sub))))
  (setq
   agent-shell-to-go--init-client-subscription nil
   agent-shell-to-go--init-finished-subscription nil
   agent-shell-to-go--error-subscription nil
   agent-shell-to-go--turn-complete-subscription nil
   agent-shell-to-go--ready-subscription nil
   agent-shell-to-go--tool-call-update-subscription nil)
  (when (and agent-shell-to-go--thread-id
             agent-shell-to-go--transport
             (not agent-shell-to-go--restarting))
    (when (and agent-shell-to-go-upload-transcript-on-end
               (bound-and-true-p agent-shell--transcript-file)
               (file-exists-p agent-shell--transcript-file))
      (agent-shell-to-go-transport-upload-file
       agent-shell-to-go--transport
       agent-shell-to-go--channel-id
       agent-shell-to-go--thread-id
       agent-shell--transcript-file
       "Session transcript"))
    (agent-shell-to-go--send "_Session ended_"))
  ;; Restore permission responder
  (setq-local agent-shell-permission-responder-function
              agent-shell-to-go--prev-permission-responder)
  (setq agent-shell-to-go--active-buffers
        (delete (current-buffer) agent-shell-to-go--active-buffers))
  (unless agent-shell-to-go--active-buffers
    (advice-remove 'agent-shell--send-command #'agent-shell-to-go--on-send-command)
    (advice-remove 'agent-shell--on-notification #'agent-shell-to-go--on-notification))
  (agent-shell-to-go--debug "bridge disabled"))

(defun agent-shell-to-go--bridge-reconnect-buffer (&optional buffer)
  "Reconnect BUFFER (or current) to its transport with a fresh thread."
  (let ((buf (or buffer (current-buffer))))
    (unless (buffer-live-p buf)
      (user-error "Buffer is not live"))
    (with-current-buffer buf
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      (let* ((transport
              (or agent-shell-to-go--transport (agent-shell-to-go--get-transport)))
             (project-path (agent-shell-to-go--get-project-path)))
        (unless (agent-shell-to-go-transport-connected-p transport)
          (agent-shell-to-go-transport-connect transport))
        (setq agent-shell-to-go--transport transport)
        (setq agent-shell-to-go--channel-id
              (agent-shell-to-go-transport-ensure-project-channel
               transport project-path))
        (setq agent-shell-to-go--thread-id
              (agent-shell-to-go-transport-start-thread
               transport agent-shell-to-go--channel-id (buffer-name)))
        (unless (memq buf agent-shell-to-go--active-buffers)
          (add-to-list 'agent-shell-to-go--active-buffers buf))
        (setq-local agent-shell-permission-responder-function
                    #'agent-shell-to-go--permission-responder)
        (advice-add
         'agent-shell--send-command
         :around #'agent-shell-to-go--on-send-command)
        (advice-add
         'agent-shell--on-notification
         :around #'agent-shell-to-go--on-notification)
        ;; Refresh subscriptions for the new thread
        (dolist (sub
                 (list
                  agent-shell-to-go--turn-complete-subscription
                  agent-shell-to-go--ready-subscription
                  agent-shell-to-go--tool-call-update-subscription))
          (when sub
            (ignore-errors
              (agent-shell-unsubscribe :subscription sub))))
        (setq agent-shell-to-go--turn-complete-subscription
              (agent-shell-subscribe-to
               :shell-buffer buf
               :event 'turn-complete
               :on-event
               (lambda (_event) (agent-shell-to-go--fetch-session-title))))
        (setq agent-shell-to-go--ready-subscription
              (agent-shell-subscribe-to
               :shell-buffer buf
               :event 'turn-complete
               :on-event #'agent-shell-to-go--on-turn-complete))
        (setq agent-shell-to-go--tool-call-update-subscription
              (agent-shell-subscribe-to
               :shell-buffer buf
               :event 'tool-call-update
               :on-event #'agent-shell-to-go--bridge-on-tool-call-update))
        (add-hook 'kill-buffer-hook #'agent-shell-to-go--on-buffer-kill nil t)
        (unless agent-shell-to-go-mode
          (setq agent-shell-to-go-mode t))
        (agent-shell-to-go--debug "reconnected %s (new thread)" (buffer-name buf))))))

(defun agent-shell-to-go--bridge-buffer-connected-p (&optional buffer)
  "Return non-nil if BUFFER has a valid transport connection."
  (let ((buf (or buffer (current-buffer))))
    (and (buffer-live-p buf)
         (buffer-local-value 'agent-shell-to-go--thread-id buf)
         (buffer-local-value 'agent-shell-to-go--channel-id buf)
         (memq buf agent-shell-to-go--active-buffers))))


(provide 'agent-shell-to-go-bridge)
;;; agent-shell-to-go-bridge.el ends here
