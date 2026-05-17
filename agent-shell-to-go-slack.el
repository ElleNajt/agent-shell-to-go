;;; agent-shell-to-go-slack.el --- Slack transport for agent-shell-to-go -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Elle Najt

;; Author: Elle Najt
;; Maintainer: junyi.hou <junyi.yi.hou@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Implements the agent-shell-to-go transport protocol for Slack.
;; Loaded automatically by agent-shell-to-go.el.
;;
;; Configuration:
;;   (setq agent-shell-to-go-slack-bot-token "xoxb-...")
;;   (setq agent-shell-to-go-slack-app-token "xapp-...")
;;   (setq agent-shell-to-go-slack-channel-id "C...")
;;   (setq agent-shell-to-go-slack-authorized-users '("U..."))

;;; Code:

(require 'agent-shell-to-go-core)

; Slack-specific defcustoms 

(defcustom agent-shell-to-go-slack-bot-token nil
  "Slack bot token (xoxb-...)."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-app-token nil
  "Slack app-level token (xapp-...) for Socket Mode."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-channel-id nil
  "Default Slack channel ID.  Used when per-project channels are disabled."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-authorized-users nil
  "List of Slack user IDs allowed to interact with agents.
If nil, NO ONE can interact (secure by default)."
  :type '(repeat string)
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-user-id nil
  "Your Slack user ID for auto-invite to new channels."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-per-project-channels t
  "When non-nil, create a separate Slack channel for each project."
  :type 'boolean
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-channel-prefix ""
  "Prefix string prepended to auto-created project channel names."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-channels-file
  (expand-file-name "agent-shell-to-go-slack-channels.el" user-emacs-directory)
  "File to persist Slack project-to-channel mappings."
  :type 'string
  :group 'agent-shell-to-go)

;; Backward-compatibility aliases for the old unprefixed names.
(defvaralias 'agent-shell-to-go-bot-token 'agent-shell-to-go-slack-bot-token)
(defvaralias 'agent-shell-to-go-app-token 'agent-shell-to-go-slack-app-token)
(defvaralias 'agent-shell-to-go-channel-id 'agent-shell-to-go-slack-channel-id)
(defvaralias 'agent-shell-to-go-authorized-users
             'agent-shell-to-go-slack-authorized-users)
(defvaralias 'agent-shell-to-go-user-id 'agent-shell-to-go-slack-user-id)
(defvaralias 'agent-shell-to-go-per-project-channels
             'agent-shell-to-go-slack-per-project-channels)
(defvaralias 'agent-shell-to-go-channel-prefix 'agent-shell-to-go-slack-channel-prefix)
(defvaralias 'agent-shell-to-go-channels-file 'agent-shell-to-go-slack-channels-file)
(make-obsolete-variable
 'agent-shell-to-go-bot-token 'agent-shell-to-go-slack-bot-token "0.3.0")
(make-obsolete-variable
 'agent-shell-to-go-app-token 'agent-shell-to-go-slack-app-token "0.3.0")
(make-obsolete-variable
 'agent-shell-to-go-channel-id 'agent-shell-to-go-slack-channel-id "0.3.0")
(make-obsolete-variable
 'agent-shell-to-go-authorized-users 'agent-shell-to-go-slack-authorized-users "0.3.0")
(make-obsolete-variable
 'agent-shell-to-go-user-id 'agent-shell-to-go-slack-user-id "0.3.0")
(make-obsolete-variable
 'agent-shell-to-go-per-project-channels
 'agent-shell-to-go-slack-per-project-channels
 "0.3.0")
(make-obsolete-variable
 'agent-shell-to-go-channel-prefix 'agent-shell-to-go-slack-channel-prefix "0.3.0")
(make-obsolete-variable
 'agent-shell-to-go-channels-file 'agent-shell-to-go-slack-channels-file "0.3.0")

; Reaction map (raw Slack emoji → canonical action) 

(defcustom agent-shell-to-go-slack-reaction-map
  '((hide . ("see_no_evil" "no_bell"))
    (expand-truncated . ("eyes"))
    (expand-full . ("book" "open_book"))
    (permission-allow . ("white_check_mark" "+1"))
    (permission-always . ("unlock" "star"))
    (permission-reject . ("x" "-1")))
  "Map canonical action symbols to lists of Slack emoji names.
Used to translate raw Slack reactions to canonical hook actions."
  :type '(alist :key-type symbol :value-type (repeat string))
  :group 'agent-shell-to-go)

(defun agent-shell-to-go--slack-emoji-to-action (emoji)
  "Return the canonical action symbol for Slack EMOJI, or nil."
  (car
   (seq-find
    (lambda (pair) (member emoji (cdr pair))) agent-shell-to-go-slack-reaction-map)))

; Struct 

(cl-defstruct (agent-shell-to-go-slack-transport
               (:include agent-shell-to-go-transport)
               (:constructor agent-shell-to-go--make-slack-transport))
  "Slack transport state."
  ws ; agent-shell-to-go--ws struct
  bot-user-id-cache
  (processed-ts (make-hash-table :test 'equal))
  (project-channels (make-hash-table :test 'equal)))

; Low-level API helpers 

(defun agent-shell-to-go--slack-api (method endpoint &optional data)
  "Make a Slack API call using curl.
METHOD is GET or POST, ENDPOINT is without the base URL, DATA is the payload."
  (let* ((url (concat "https://slack.com/api/" endpoint))
         (token agent-shell-to-go-slack-bot-token)
         (args
          (list
           "-s"
           "-X"
           method
           "-H"
           (concat "Authorization: Bearer " token)
           "-H"
           "Content-Type: application/json; charset=utf-8")))
    (when data
      (setq args
            (append args (list "-d" (encode-coding-string (json-encode data) 'utf-8)))))
    (setq args (append args (list url)))
    (with-temp-buffer
      (apply #'call-process "curl" nil t nil args)
      (goto-char (point-min))
      (json-read))))

; Channel management (internal) 

(defun agent-shell-to-go--slack-load-channels (transport)
  "Load project-to-channel mappings into TRANSPORT from disk."
  (let ((file agent-shell-to-go-slack-channels-file))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((data (read (current-buffer))))
          (clrhash (agent-shell-to-go-slack-transport-project-channels transport))
          (dolist (pair data)
            (puthash
             (car pair)
             (cdr pair)
             (agent-shell-to-go-slack-transport-project-channels transport))))))))

(defun agent-shell-to-go--slack-save-channels (transport)
  "Save TRANSPORT's project-to-channel mappings to disk."
  (let (data)
    (maphash
     (lambda (k v) (push (cons k v) data))
     (agent-shell-to-go-slack-transport-project-channels transport))
    (with-temp-file agent-shell-to-go-slack-channels-file
      (prin1 data (current-buffer)))))

(defun agent-shell-to-go--slack-invite-user (channel-id user-id)
  "Invite USER-ID to CHANNEL-ID."
  (agent-shell-to-go--slack-api "POST" "conversations.invite"
                                `((channel . ,channel-id) (users . ,user-id))))

(defun agent-shell-to-go--slack-create-channel (name)
  "Create a Slack channel named NAME.  Returns channel-id or nil."
  (let* ((resp
          (agent-shell-to-go--slack-api "POST" "conversations.create"
                                        `((name . ,name) (is_private . :json-false))))
         (ok (map-elt resp 'ok))
         (channel (map-elt resp 'channel))
         (channel-id (map-elt channel 'id))
         (err (map-elt resp 'error)))
    (cond
     (ok
      (when agent-shell-to-go-slack-user-id
        (agent-shell-to-go--slack-invite-user
         channel-id agent-shell-to-go-slack-user-id))
      channel-id)
     ((equal err "name_taken")
      (agent-shell-to-go--slack-find-channel-by-name name))
     (t
      (agent-shell-to-go--debug "create-channel %s failed: %s" name err)
      nil))))

(defun agent-shell-to-go--slack-find-channel-by-name (name)
  "Find a channel by NAME.  Return its id or nil."
  (let* ((resp
          (agent-shell-to-go--slack-api
           "GET" "conversations.list?types=public_channel,private_channel&limit=1000"))
         (channels (map-elt resp 'channels)))
    (when channels
      (map-elt (seq-find (lambda (ch) (equal (map-elt ch 'name) name)) channels) 'id))))

(defun agent-shell-to-go--slack-get-or-create-project-channel (transport project-path)
  "Return the Slack channel id for PROJECT-PATH, creating it if necessary."
  (if (not agent-shell-to-go-slack-per-project-channels)
      agent-shell-to-go-slack-channel-id
    (let ((cached
           (gethash
            project-path
            (agent-shell-to-go-slack-transport-project-channels transport))))
      (or cached
          (let* ((project-name
                  (file-name-nondirectory (directory-file-name project-path)))
                 (channel-name
                  (concat
                   agent-shell-to-go-slack-channel-prefix
                   (agent-shell-to-go--sanitize-channel-name project-name)))
                 (channel-id (agent-shell-to-go--slack-create-channel channel-name)))
            (when channel-id
              (puthash
               project-path
               channel-id
               (agent-shell-to-go-slack-transport-project-channels transport))
              (agent-shell-to-go--slack-save-channels transport)
              (agent-shell-to-go--debug "channel %s for %s" channel-name project-path))
            (or channel-id agent-shell-to-go-slack-channel-id))))))

(defun agent-shell-to-go--slack-get-project-for-channel (transport channel-id)
  "Return the project path mapped to CHANNEL-ID in TRANSPORT, or nil."
  (let (candidates)
    (maphash
     (lambda (path ch)
       (when (equal ch channel-id)
         (push path candidates)))
     (agent-shell-to-go-slack-transport-project-channels transport))
    (or (cl-find-if #'file-directory-p candidates) (car candidates))))

; Formatting helpers (Slack mrkdwn) 

(defun agent-shell-to-go--slack-format-table-rows (rows)
  "Format ROWS (list of lists of strings) as an aligned text table."
  (let* ((num-cols (apply #'max (mapcar #'length rows)))
         (col-widths
          (seq-reduce
           (lambda (widths row)
             (seq-mapn (lambda (w cell) (max w (length cell))) widths row))
           rows (make-list num-cols 0))))
    (let ((formatted nil)
          (first t))
      (dolist (row rows)
        (push (concat
               "  "
               (mapconcat #'identity
                          (seq-mapn (lambda (cell width)
                                      (concat
                                       cell (make-string (- width (length cell)) ?\s)))
                                    row
                                    col-widths)
                          "   "))
              formatted)
        (when first
          (push (concat
                 "  " (mapconcat (lambda (w) (make-string w ?─)) col-widths "   "))
                formatted)
          (setq first nil)))
      (mapconcat #'identity (nreverse formatted) "\n"))))

(defun agent-shell-to-go--slack-parse-table-line (line)
  "Parse markdown table LINE into trimmed cell strings."
  (let* ((trimmed (string-trim line))
         (inner
          (if (string-prefix-p "|" trimmed)
              (substring trimmed 1)
            trimmed))
         (inner
          (if (string-suffix-p "|" inner)
              (substring inner 0 -1)
            inner)))
    (mapcar #'string-trim (split-string inner "|"))))

(defun agent-shell-to-go--slack-convert-markdown-table (text)
  "Convert markdown tables in TEXT to aligned code blocks for Slack."
  (let ((lines (split-string text "\n"))
        result
        in-code-block
        in-table
        table-lines)
    (dolist (line lines)
      (cond
       ((string-match-p "^```" line)
        (when in-table
          (push (concat
                 "```\n"
                 (agent-shell-to-go--slack-format-table-rows
                  (mapcar
                   #'agent-shell-to-go--slack-parse-table-line (nreverse table-lines)))
                 "\n```")
                result)
          (setq
           in-table nil
           table-lines nil))
        (setq in-code-block (not in-code-block))
        (push line result))
       (in-code-block
        (push line result))
       ((string-match-p "^|" line)
        (setq in-table t)
        (unless (string-match-p "^|[-| :]+|$" line)
          (push line table-lines)))
       (t
        (when in-table
          (push (concat
                 "```\n"
                 (agent-shell-to-go--slack-format-table-rows
                  (mapcar
                   #'agent-shell-to-go--slack-parse-table-line (nreverse table-lines)))
                 "\n```")
                result)
          (setq
           in-table nil
           table-lines nil))
        (push line result))))
    (when in-table
      (push (concat
             "```\n"
             (agent-shell-to-go--slack-format-table-rows
              (mapcar
               #'agent-shell-to-go--slack-parse-table-line (nreverse table-lines)))
             "\n```")
            result))
    (mapconcat #'identity (nreverse result) "\n")))

(defun agent-shell-to-go--slack-markdown-to-mrkdwn (text)
  "Convert Markdown TEXT to Slack mrkdwn.  Preserves code blocks verbatim."
  (setq text (agent-shell-to-go--slack-convert-markdown-table text))
  (let ((result "")
        (pos 0)
        (len (length text)))
    (while (< pos len)
      (cond
       ((and (<= (+ pos 3) len) (string= (substring text pos (+ pos 3)) "```"))
        (let ((end (string-match "```" text (+ pos 3))))
          (if end
              (progn
                (setq result (concat result (substring text pos (+ end 3))))
                (setq pos (+ end 3)))
            (setq result (concat result (substring text pos)))
            (setq pos len))))
       ((= (aref text pos) ?`)
        (let ((end (string-match "`" text (1+ pos))))
          (if end
              (progn
                (setq result (concat result (substring text pos (1+ end))))
                (setq pos (1+ end)))
            (setq result (concat result "`"))
            (setq pos (1+ pos)))))
       (t
        (let* ((next-code (string-match "`" text pos))
               (chunk-end (or next-code len))
               (chunk (substring text pos chunk-end)))
          (setq chunk (replace-regexp-in-string "^\\( *\\)[*-] " "\\1• " chunk))
          (setq chunk (replace-regexp-in-string "^#\\{1,6\\} +\\(.*\\)$" "*\\1*" chunk))
          (setq chunk
                (replace-regexp-in-string "\\*\\*\\([^*]+\\)\\*\\*" "*\\1*" chunk))
          (setq chunk (replace-regexp-in-string "~~\\([^~]+\\)~~" "~\\1~" chunk))
          (setq chunk
                (replace-regexp-in-string
                 "\\[\\([^]]+\\)\\](\\([^)]+\\))" "<\\2|\\1>" chunk))
          (setq chunk
                (replace-regexp-in-string
                 "!<\\([^>]+\\)|\\([^>]*\\)>" "<\\1|\\2>" chunk))
          (setq result (concat result chunk))
          (setq pos chunk-end)))))
    result))

(defun agent-shell-to-go--slack-format-diff (old-text new-text)
  "Generate a unified diff string between OLD-TEXT and NEW-TEXT."
  (let ((old-file (make-temp-file "astg-old"))
        (new-file (make-temp-file "astg-new")))
    (unwind-protect
        (progn
          (with-temp-file old-file
            (insert (or old-text "")))
          (with-temp-file new-file
            (insert (or new-text "")))
          (with-temp-buffer
            (call-process "diff" nil t nil "-U3" old-file new-file)
            (goto-char (point-min))
            (when (looking-at "^---")
              (delete-region
               (point)
               (progn
                 (forward-line 1)
                 (point))))
            (when (looking-at "^\\+\\+\\+")
              (delete-region
               (point)
               (progn
                 (forward-line 1)
                 (point))))
            (buffer-string)))
      (delete-file old-file)
      (delete-file new-file))))

; Transport method implementations 

(cl-defmethod agent-shell-to-go-transport-connect
    ((transport agent-shell-to-go-slack-transport))
  "Connect the Slack transport via Socket Mode WebSocket."
  (agent-shell-to-go--slack-load-env)
  (unless agent-shell-to-go-slack-bot-token
    (error "agent-shell-to-go-slack-bot-token not set"))
  (unless agent-shell-to-go-slack-app-token
    (error "agent-shell-to-go-slack-app-token not set"))
  (agent-shell-to-go--slack-load-channels transport)
  (let ((ws
         (or (agent-shell-to-go-slack-transport-ws transport)
             (agent-shell-to-go--ws-make
              :name 'slack
              :url-fn
              (lambda () (agent-shell-to-go--slack-get-ws-url))
              :on-frame
              (lambda (frame) (agent-shell-to-go--slack-handle-frame transport frame))
              :get-active-p (lambda () t)))))
    (setf (agent-shell-to-go-slack-transport-ws transport) ws)
    (agent-shell-to-go--ws-connect ws)))

(cl-defmethod agent-shell-to-go-transport-disconnect
    ((transport agent-shell-to-go-slack-transport))
  "Disconnect the Slack WebSocket."
  (when-let* ((ws (agent-shell-to-go-slack-transport-ws transport)))
    (agent-shell-to-go--ws-disconnect ws)))

(cl-defmethod agent-shell-to-go-transport-connected-p
    ((transport agent-shell-to-go-slack-transport))
  "Return non-nil if the Slack WebSocket is open."
  (agent-shell-to-go--ws-connected-p (agent-shell-to-go-slack-transport-ws transport)))

(cl-defmethod agent-shell-to-go-transport-authorized-p
    ((transport agent-shell-to-go-slack-transport) user-id)
  "Return non-nil if USER-ID is in `agent-shell-to-go-slack-authorized-users'."
  (and agent-shell-to-go-slack-authorized-users
       (member user-id agent-shell-to-go-slack-authorized-users)))

(cl-defmethod agent-shell-to-go-transport-bot-user-id
    ((transport agent-shell-to-go-slack-transport))
  "Return the bot's Slack user id, caching the result."
  (or (agent-shell-to-go-slack-transport-bot-user-id-cache transport)
      (let ((uid (map-elt (agent-shell-to-go--slack-api "GET" "auth.test") 'user_id)))
        (setf (agent-shell-to-go-slack-transport-bot-user-id-cache transport) uid)
        uid)))

(cl-defmethod agent-shell-to-go-transport-send-text
    ((transport agent-shell-to-go-slack-transport)
     channel
     thread-id
     text
     &optional
     options)
  "Post TEXT to Slack CHANNEL, optionally in THREAD-ID.
Options plist supports :truncate :ephemeral :user-id."
  (let* ((truncate (map-elt options :truncate))
         (ephemeral (map-elt options :ephemeral))
         (user-id (map-elt options :user-id))
         (clean text)
         (display
          (if truncate
              (agent-shell-to-go--truncate-text clean 500)
            clean))
         (was-truncated (and truncate (not (equal clean display)))))
    (condition-case err
        (let* ((endpoint
                (if ephemeral
                    "chat.postEphemeral"
                  "chat.postMessage"))
               (data `((channel . ,channel) (text . ,display)))
               (_
                (when thread-id
                  (push `(thread_ts . ,thread-id) data)))
               (_
                (when (and ephemeral user-id)
                  (push `(user . ,user-id) data)))
               (resp (agent-shell-to-go--slack-api "POST" endpoint data))
               (msg-ts (map-elt resp 'ts)))
          (when (and was-truncated msg-ts)
            (agent-shell-to-go--save-truncated-message transport channel msg-ts clean))
          msg-ts)
      (error
       (agent-shell-to-go--debug "send-text error: %s, retrying ASCII-only" err)
       (let* ((safe (agent-shell-to-go--strip-non-ascii text))
              (display
               (if truncate
                   (agent-shell-to-go--truncate-text safe 500)
                 safe))
              (data `((channel . ,channel) (text . ,display)))
              (_
               (when thread-id
                 (push `(thread_ts . ,thread-id) data)))
              (resp
               (condition-case nil
                   (agent-shell-to-go--slack-api "POST" "chat.postMessage" data)
                 (error
                  nil))))
         (map-elt resp 'ts))))))

(cl-defmethod agent-shell-to-go-transport-edit-message
    ((transport agent-shell-to-go-slack-transport) channel message-id text)
  "Edit MESSAGE-ID in Slack CHANNEL to TEXT."
  (let ((resp
         (agent-shell-to-go--slack-api "POST" "chat.update"
                                       `((channel . ,channel)
                                         (ts . ,message-id)
                                         (text . ,text)))))
    (eq (map-elt resp 'ok) t)))

(cl-defmethod agent-shell-to-go-transport-upload-file
    ((transport agent-shell-to-go-slack-transport)
     channel
     thread-id
     path
     &optional
     comment)
  "Upload PATH to Slack CHANNEL (optionally in THREAD-ID with COMMENT)."
  (when (and path (file-exists-p path))
    (let* ((filename (file-name-nondirectory path))
           (file-size (file-attribute-size (file-attributes path)))
           (url-resp
            (agent-shell-to-go--slack-api
             "GET"
             (format "files.getUploadURLExternal?filename=%s&length=%d"
                     (url-hexify-string filename) file-size)))
           (upload-url (map-elt url-resp 'upload_url))
           (file-id (map-elt url-resp 'file_id)))
      (when (and upload-url file-id)
        (with-temp-buffer
          (call-process "curl"
                        nil
                        t
                        nil
                        "-s"
                        "-X"
                        "POST"
                        "-F"
                        (format "file=@%s" path)
                        upload-url))
        (let* ((files-data `[((id . ,file-id))])
               (complete-data `((files . ,files-data) (channel_id . ,channel)))
               (_
                (when thread-id
                  (push `(thread_ts . ,thread-id) complete-data)))
               (_
                (when comment
                  (push `(initial_comment . ,comment) complete-data))))
          (agent-shell-to-go--slack-api "POST" "files.completeUploadExternal"
                                        complete-data))))))

(cl-defmethod agent-shell-to-go-transport-get-message-text
    ((transport agent-shell-to-go-slack-transport) channel message-id)
  "Fetch the text of MESSAGE-ID from Slack CHANNEL."
  (let* ((resp
          (agent-shell-to-go--slack-api
           "GET"
           (format "conversations.history?channel=%s&latest=%s&limit=1&inclusive=true"
                   channel message-id)))
         (messages (map-elt resp 'messages))
         (msg (and messages (aref messages 0))))
    (map-elt msg 'text)))

(cl-defmethod agent-shell-to-go-transport-get-reactions
    ((transport agent-shell-to-go-slack-transport) channel message-id)
  "Return list of canonical action symbols for MESSAGE-ID in CHANNEL."
  (let* ((resp
          (agent-shell-to-go--slack-api
           "GET" (format "reactions.get?channel=%s&timestamp=%s" channel message-id)))
         (message (map-elt resp 'message))
         (reactions (map-elt message 'reactions)))
    (when reactions
      (delq
       nil
       (mapcar
        (lambda (r)
          (agent-shell-to-go--slack-emoji-to-action (map-elt r 'name)))
        (append reactions nil))))))

(cl-defmethod agent-shell-to-go-transport-fetch-thread-replies
    ((transport agent-shell-to-go-slack-transport) channel thread-id)
  "Return thread reply plists (:msg-id :user :text) for THREAD-ID in CHANNEL."
  (let* ((resp
          (agent-shell-to-go--slack-api
           "GET" (format "conversations.replies?channel=%s&ts=%s" channel thread-id)))
         (messages (map-elt resp 'messages)))
    (when messages
      (mapcar
       (lambda (msg)
         (list
          :msg-id (map-elt msg 'ts)
          :user (map-elt msg 'user)
          :text (map-elt msg 'text)))
       (append messages nil)))))

(cl-defmethod agent-shell-to-go-transport-start-thread
    ((transport agent-shell-to-go-slack-transport) channel label)
  "Post a new thread-header message in Slack CHANNEL.  Return ts."
  (let* ((resp
          (agent-shell-to-go-transport-send-text
           transport channel nil
           (format ":robot_face: *Agent Shell Session* @ %s\n`%s`\n_%s_"
                   (system-name) label (format-time-string "%Y-%m-%d %H:%M:%S")))))
    ;; resp is the msg-ts string
    resp))

(cl-defmethod agent-shell-to-go-transport-update-thread-header
    ((transport agent-shell-to-go-slack-transport) channel thread-id title)
  "Update the Slack thread header message to TITLE."
  (let* ((truncated
          (if (> (length title) 80)
              (concat (substring title 0 77) "…")
            title))
         (text
          (format ":robot_face: *%s*\n_%s_"
                  truncated
                  (format-time-string "%Y-%m-%d %H:%M:%S"))))
    (agent-shell-to-go--slack-api "POST" "chat.update"
                                  `((channel . ,channel)
                                    (ts . ,thread-id)
                                    (text . ,text)))))

(cl-defmethod agent-shell-to-go-transport-ensure-project-channel
    ((transport agent-shell-to-go-slack-transport) project-path)
  "Return the Slack channel id for PROJECT-PATH."
  (agent-shell-to-go--slack-get-or-create-project-channel transport project-path))

(cl-defmethod agent-shell-to-go-transport-list-threads
    ((transport agent-shell-to-go-slack-transport) channel)
  "Return (:thread-id :last-timestamp) plists for Agent Shell threads in CHANNEL."
  (let* ((resp
          (agent-shell-to-go--slack-api
           "GET" (format "conversations.history?channel=%s&limit=500" channel)))
         (messages (map-elt resp 'messages))
         threads)
    (when messages
      (dolist (msg (append messages nil))
        (let ((text (map-elt msg 'text))
              (ts (map-elt msg 'ts))
              (latest-reply (map-elt msg 'latest_reply)))
          (when (and text (string-match-p "Agent Shell Session" text))
            (push (list
                   :thread-id ts
                   :last-timestamp (string-to-number (or latest-reply ts)))
                  threads)))))
    threads))

(cl-defmethod agent-shell-to-go-transport-delete-message
    ((transport agent-shell-to-go-slack-transport) channel message-id)
  "Delete MESSAGE-ID from Slack CHANNEL."
  (agent-shell-to-go--slack-api "POST" "chat.delete"
                                `((channel . ,channel) (ts . ,message-id))))

(cl-defmethod agent-shell-to-go-transport-delete-thread
    ((transport agent-shell-to-go-slack-transport) channel thread-id)
  "Delete all messages in THREAD-ID from Slack CHANNEL."
  (let ((cursor nil)
        (deleted 0)
        (continue t))
    (while continue
      (let* ((endpoint
              (format "conversations.replies?channel=%s&ts=%s&limit=200%s"
                      channel thread-id
                      (if cursor
                          (format "&cursor=%s" cursor)
                        "")))
             (resp (agent-shell-to-go--slack-api "GET" endpoint))
             (messages (map-elt resp 'messages))
             (meta (map-elt resp 'response_metadata)))
        (when messages
          (dolist (msg (reverse (append messages nil)))
            (agent-shell-to-go-transport-delete-message
             transport channel (map-elt msg 'ts))
            (cl-incf deleted)))
        (setq cursor (map-elt meta 'next_cursor))
        (setq continue (and cursor (not (string-empty-p cursor))))))
    deleted))

;; Semantic formatters

(cl-defmethod agent-shell-to-go-transport-format-tool-call-start
    ((transport agent-shell-to-go-slack-transport) title)
  (format "%s `%s`" (alist-get 'start agent-shell-to-go-tool-call-icons) title))

(cl-defmethod agent-shell-to-go-transport-format-tool-call-result
    ((transport agent-shell-to-go-slack-transport) title status output)
  (let ((icon
         (cond
           ((or (equal status "completed") (eq status 'completed))
            (alist-get 'completed agent-shell-to-go-tool-call-icons))
           ((or (equal status "failed") (eq status 'failed))
            (alist-get 'failed    agent-shell-to-go-tool-call-icons))
           (t
            (alist-get 'start     agent-shell-to-go-tool-call-icons)))))
    (if (and output (stringp output) (not (string-empty-p output)))
        (format "%s `%s`\n```\n%s\n```" icon title output)
      (format "%s `%s`" icon title))))

(cl-defmethod agent-shell-to-go-transport-format-diff
    ((transport agent-shell-to-go-slack-transport) old-text new-text)
  (let ((diff (agent-shell-to-go--slack-format-diff old-text new-text)))
    (if (and diff (> (length diff) 0))
        (format "```diff\n%s\n```" diff)
      "")))

(cl-defmethod agent-shell-to-go-transport-format-user-message
    ((transport agent-shell-to-go-slack-transport) text)
  (format ":bust_in_silhouette: *User*\n%s" text))

(cl-defmethod agent-shell-to-go-transport-format-agent-message
    ((transport agent-shell-to-go-slack-transport) text)
  (format ":robot_face: *Agent*\n%s"
          (agent-shell-to-go--slack-markdown-to-mrkdwn text)))

(cl-defmethod agent-shell-to-go-transport-format-markdown
    ((transport agent-shell-to-go-slack-transport) markdown)
  (agent-shell-to-go--slack-markdown-to-mrkdwn markdown))

; WebSocket / Socket Mode 

(defun agent-shell-to-go--slack-get-ws-url ()
  "Get a fresh Socket Mode WebSocket URL from Slack."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " agent-shell-to-go-slack-app-token))
            ("Content-Type" . "application/x-www-form-urlencoded")))
         (url "https://slack.com/api/apps.connections.open"))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (let ((resp (json-read)))
        (kill-buffer)
        (if (eq (map-elt resp 'ok) t)
            (map-elt resp 'url)
          (error "Slack WebSocket URL failed: %s" (map-elt resp 'error)))))))

(defun agent-shell-to-go--slack-handle-frame (transport frame)
  "Parse Socket Mode FRAME and dispatch to inbound hooks."
  (let* ((payload (websocket-frame-text frame))
         (data (json-read-from-string payload))
         (type (map-elt data 'type))
         (envelope-id (map-elt data 'envelope_id))
         (ws (agent-shell-to-go-slack-transport-ws transport)))
    ;; ACK every envelope
    (when (and envelope-id ws (agent-shell-to-go--ws-websocket ws))
      (websocket-send-text
       (agent-shell-to-go--ws-websocket ws)
       (json-encode `((envelope_id . ,envelope-id)))))
    (agent-shell-to-go--debug "slack ws type: %s" type)
    (pcase type
      ("events_api" (let ((ep (map-elt data 'payload)))
         (agent-shell-to-go--defer #'agent-shell-to-go--slack-dispatch-event transport ep)))
      ("hello" (agent-shell-to-go--debug "slack ws: connected"))
      ("disconnect"
       (agent-shell-to-go--debug "slack ws: disconnect requested, reconnecting")
       (when ws
         (agent-shell-to-go--ws-reconnect ws))))))

; Event dispatch (events_api) 

(defvar agent-shell-to-go--slack-event-log-buf "*Agent Shell Events*"
  "Buffer name for the Slack event log.")

(defun agent-shell-to-go--slack-log-event (type ts text &optional extra)
  "Log event TYPE with TS/TEXT/EXTRA to the event log buffer."
  (let ((buf (get-buffer-create agent-shell-to-go--slack-event-log-buf)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert
       (format "[%s] %s ts=%s %s%s\n"
               (format-time-string "%H:%M:%S")
               type
               (or ts "nil")
               (truncate-string-to-width (or text "") 50)
               (if extra
                   (format " (%s)" extra)
                 "")))
      (when (> (count-lines (point-min) (point-max))
               agent-shell-to-go-event-log-max-entries)
        (goto-char (point-min))
        (forward-line 50)
        (delete-region (point-min) (point))))))

(defun agent-shell-to-go--slack-message-seen-p (transport ts)
  "Return non-nil if message TS was already processed by TRANSPORT."
  (let ((table (agent-shell-to-go-slack-transport-processed-ts transport)))
    (if (gethash ts table)
        t
      (puthash ts (float-time) table)
      ;; Prune entries older than 60 seconds
      (let ((now (float-time)))
        (maphash
         (lambda (k v)
           (when (> (- now v) 60)
             (remhash k table)))
         table))
      nil)))

(defun agent-shell-to-go--slack-dispatch-event (transport payload)
  "Dispatch Slack events_api PAYLOAD through the appropriate inbound hook."
  (let* ((event (map-elt payload 'event))
         (event-type (map-elt event 'type))
         (user (map-elt event 'user))
         (bot-id (map-elt event 'bot_id)))
    ;; Skip bot messages silently (they'll be ignored anyway since this is inbound
    ;; message from slack)
    ;; NOTE: This skips ALL bot messages. If we want agents to message each other in the
    ;; future, we'd need to allowlist specific bot IDs here instead.
    (unless bot-id
      (agent-shell-to-go--debug "slack event: %s from %s" event-type user)
      (if (not (agent-shell-to-go-transport-authorized-p transport user))
          (agent-shell-to-go--debug "unauthorized user %s, ignoring %s" user event-type)
        (pcase event-type
          ("message" (agent-shell-to-go--slack-normalize-message transport event))
          ("reaction_added"
           (agent-shell-to-go--slack-normalize-reaction transport event t))
          ("reaction_removed"
           (agent-shell-to-go--slack-normalize-reaction transport event nil)))))))

(defun agent-shell-to-go--slack-normalize-message (transport event)
  "Normalize Slack message EVENT and run `agent-shell-to-go-message-hook'."
  (let* ((ts (map-elt event 'ts))
         (thread-ts (map-elt event 'thread_ts))
         (channel (map-elt event 'channel))
         (user (map-elt event 'user))
         (text (map-elt event 'text))
         (subtype (map-elt event 'subtype))
         (bot-id (map-elt event 'bot_id)))
    (agent-shell-to-go--slack-log-event "msg-in" ts text)
    (when (and text
               ts
               (not subtype)
               (not bot-id)
               (not (equal user (agent-shell-to-go-transport-bot-user-id transport)))
               (not (agent-shell-to-go--slack-message-seen-p transport ts)))
      (apply #'run-hook-with-args
             'agent-shell-to-go-message-hook
             (list
              :transport transport
              :channel-id channel
              :thread-id thread-ts
              :user user
              :text text
              :msg-id ts)))))

(defun agent-shell-to-go--slack-normalize-reaction (transport event added-p)
  "Normalize Slack reaction EVENT and run `agent-shell-to-go-reaction-hook'.
ADDED-P is t for reaction_added, nil for reaction_removed."
  (let* ((item (map-elt event 'item))
         (msg-id (map-elt item 'ts))
         (channel (map-elt item 'channel))
         (user (map-elt event 'user))
         (emoji (map-elt event 'reaction))
         (action (agent-shell-to-go--slack-emoji-to-action emoji)))
    (agent-shell-to-go--debug
     "slack reaction: %s on %s (%s)" emoji msg-id
     (if added-p
         "added"
       "removed"))
    (apply #'run-hook-with-args
           'agent-shell-to-go-reaction-hook
           (list
            :transport transport
            :channel-id channel
            :thread-id nil
            :msg-id msg-id
            :user user
            :action action
            :raw-emoji emoji
            :added-p added-p))))

; Registration 

(defvar agent-shell-to-go--slack-instance nil
  "The singleton Slack transport struct.")

(defun agent-shell-to-go-slack-get-or-create ()
  "Return (or create) the global Slack transport instance."
  (unless agent-shell-to-go--slack-instance
    (setq agent-shell-to-go--slack-instance
          (agent-shell-to-go--make-slack-transport :name 'slack)))
  agent-shell-to-go--slack-instance)

;; Auto-register on load so `(agent-shell-to-go-get-transport 'slack)' works.
(agent-shell-to-go-register-transport 'slack (agent-shell-to-go-slack-get-or-create))

(provide 'agent-shell-to-go-slack)
;;; agent-shell-to-go-slack.el ends here
