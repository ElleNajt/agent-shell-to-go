;;; agent-shell-to-go-discord.el --- Discord transport for agent-shell-to-go -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Elle Najt

;; Author: Elle Najt
;; Maintainer: junyi.hou <junyi.yi.hou@gmail.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Implements the agent-shell-to-go transport protocol for Discord.
;; Use by setting `agent-shell-to-go-default-transport' to `discord'.
;;
;; Layout: each project maps to a Discord Forum channel (type forum/15); each
;; agent-shell session maps to a forum post (thread) within that forum.
;; Messages within a session go directly to the forum post's thread channel.
;;
;; Configuration:
;;   (setq agent-shell-to-go-discord-bot-token "Bot ...")
;;   (setq agent-shell-to-go-discord-guild-id "12345...")
;;   (setq agent-shell-to-go-discord-authorized-users '("user-id..."))
;;
;; Prerequisites:
;;   1. Create a Discord application at https://discord.com/developers/applications
;;   2. Enable MESSAGE CONTENT, SERVER MEMBERS intents in Bot settings
;;   3. Invite bot with scopes: bot, applications.commands
;;   4. Permissions: Send Messages, Read Message History, Manage Channels,
;;                   Add Reactions, Manage Messages, Manage Threads

;;; Code:

(require 'agent-shell-to-go-core)

; Discord-specific defcustoms 

(defcustom agent-shell-to-go-discord-bot-token nil
  "Discord bot token."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-discord-guild-id nil
  "Discord guild (server) ID for channel and thread management."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-discord-authorized-users nil
  "List of Discord user IDs allowed to interact with agents.
If nil, NO ONE can interact (secure by default)."
  :type '(repeat string)
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-discord-channel-prefix ""
  "Prefix prepended to auto-created project channel names."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-discord-channels-file
  (expand-file-name "var/agent-shell-to-go-discord-channels.el" user-emacs-directory)
  "File to persist Discord project-to-channel mappings."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-discord-reaction-map
  '((hide . ("see_no_evil" "no_bell" "🙈" "🔕"))
    (expand-truncated . ("eyes" "👀"))
    (expand-full . ("open_book" "green_book" "📖" "📗"))
    (permission-allow . ("white_check_mark" "thumbsup" "✅" "👍"))
    (permission-always . ("unlock" "star" "🔓" "⭐"))
    (permission-reject . ("x" "thumbsdown" "❌" "👎")))
  "Map canonical action symbols to Discord emoji names.
Values are matched against emoji.name in Gateway events.  This includes both
custom server emoji names (ASCII shortcodes like \"thumbsup\") and standard
Unicode emoji characters (like \"👍\") — Discord sends the Unicode character
as the name for built-in emoji, and the shortcode for custom server emoji."
  :type '(alist :key-type symbol :value-type (repeat string))
  :group 'agent-shell-to-go)

(defun agent-shell-to-go--discord-emoji-to-action (emoji)
  "Return the canonical action symbol for Discord EMOJI, or nil."
  (car
   (seq-find
    (lambda (pair) (member emoji (cdr pair))) agent-shell-to-go-discord-reaction-map)))

;; Constants
(defconst agent-shell-to-go--discord-api-base "https://discord.com/api/v10"
  "Discord REST API v10 base URL.")

(defconst agent-shell-to-go--discord-gateway-url
  "wss://gateway.discord.gg/?v=10&encoding=json"
  "Discord Gateway WebSocket URL.")

(defconst agent-shell-to-go--discord-max-content-length 2000
  "Discord message content character limit.")

(defconst agent-shell-to-go--discord-channel-type-forum 15
  "Discord channel type for Forum channels.")

;;; Gateway opcodes
(defconst agent-shell-to-go--discord-op-dispatch 0)
(defconst agent-shell-to-go--discord-op-heartbeat 1)
(defconst agent-shell-to-go--discord-op-identify 2)
(defconst agent-shell-to-go--discord-op-invalid-session 9)
(defconst agent-shell-to-go--discord-op-hello 10)
(defconst agent-shell-to-go--discord-op-heartbeat-ack 11)

;; Gateway intents (bit flags)
(defconst agent-shell-to-go--discord-intent-guilds 1)
(defconst agent-shell-to-go--discord-intent-guild-messages 512)
(defconst agent-shell-to-go--discord-intent-guild-message-reactions 1024)
(defconst agent-shell-to-go--discord-intent-direct-messages 4096)
(defconst agent-shell-to-go--discord-intent-message-content 32768)

; Struct 

(cl-defstruct (agent-shell-to-go-discord-transport
               (:include agent-shell-to-go-transport)
               (:constructor agent-shell-to-go--make-discord-transport))
  "Discord transport state."
  ws ; agent-shell-to-go--ws struct
  bot-user-id-cache
  heartbeat-timer
  (sequence nil) ; Gateway sequence number for heartbeat
  (session-id nil) ; Gateway session ID for resume
  (processed-ids (make-hash-table :test 'equal))
  (project-channels (make-hash-table :test 'equal))
  (thread-parents (make-hash-table :test 'equal))) ; thread-channel-id → parent-forum-channel-id

(defun agent-shell-to-go--discord-request (method endpoint extra-curl-args)
  "Make a Discord REST API call via curl.
METHOD is the HTTP method, ENDPOINT is the path without the base URL.
EXTRA-CURL-ARGS are appended after the base headers (body flags, form fields, etc.).
Returns the parsed JSON response or nil."
  (let* ((url (concat agent-shell-to-go--discord-api-base endpoint))
         (token agent-shell-to-go-discord-bot-token)
         (args
          (append
           (list
            "-s"
            "-X"
            method
            "-H"
            (concat "Authorization: Bot " token)
            "-H"
            "User-Agent: DiscordBot (agent-shell-to-go, 0.3.1)")
           extra-curl-args (list url))))
    (with-temp-buffer
      (apply #'call-process "curl" nil t nil args)
      (goto-char (point-min))
      (when (> (buffer-size) 0)
        (condition-case err
            (json-read)
          (error
           (agent-shell-to-go--debug "discord-api json-read error: %s" err)
           nil))))))

(defun agent-shell-to-go--discord-api (method endpoint &optional data)
  "Make a Discord REST API call, encoding DATA as JSON."
  (agent-shell-to-go--discord-request
   method endpoint
   (when data
     (list
      "-H"
      "Content-Type: application/json"
      "-d"
      (encode-coding-string (json-encode data) 'utf-8)))))

(defun agent-shell-to-go--discord-api-upload (channel-id path &optional comment)
  "Upload PATH as a file attachment to Discord CHANNEL-ID with optional COMMENT."
  (let* ((filename (file-name-nondirectory path))
         (extra
          (append
           (list "-F" (format "files[0]=@%s;filename=%s" path filename))
           (when (and comment (not (string-empty-p comment)))
             (list "-F" (format "content=%s" comment))))))
    (agent-shell-to-go--discord-request
     "POST" (format "/channels/%s/messages" channel-id) extra)))

(defun agent-shell-to-go--discord-truncate-content (text)
  "Truncate TEXT to fit Discord's content limit, appending a note if cut."
  (if (> (length text) agent-shell-to-go--discord-max-content-length)
      (concat
       (substring text 0 (- agent-shell-to-go--discord-max-content-length 20))
       "\n_... truncated_")
    text))

; Channel management (internal) 

(defun agent-shell-to-go--discord-load-channels (transport)
  "Load project-to-channel mappings into TRANSPORT from disk."
  (let ((file agent-shell-to-go-discord-channels-file))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((data (read (current-buffer))))
          (clrhash (agent-shell-to-go-discord-transport-project-channels transport))
          (dolist (pair data)
            (puthash
             (car pair)
             (cdr pair)
             (agent-shell-to-go-discord-transport-project-channels transport))))))))

(defun agent-shell-to-go--discord-save-channels (transport)
  "Save TRANSPORT's project-to-channel mappings to disk."
  (let (data)
    (maphash
     (lambda (k v) (push (cons k v) data))
     (agent-shell-to-go-discord-transport-project-channels transport))
    (with-temp-file agent-shell-to-go-discord-channels-file
      (prin1 data (current-buffer)))))

(defun agent-shell-to-go--discord-find-channel-by-name (name)
  "Find a guild forum channel by NAME. Return its ID or nil."
  (when agent-shell-to-go-discord-guild-id
    (let* ((resp
            (agent-shell-to-go--discord-api
             "GET" (format "/guilds/%s/channels" agent-shell-to-go-discord-guild-id)))
           (channels
            (when (vectorp resp)
              (append resp nil))))
      (map-elt
       (seq-find
        (lambda (ch)
          (and (equal (map-elt ch 'name) name)
               (= (or (map-elt ch 'type) -1)
                  agent-shell-to-go--discord-channel-type-forum)))
        channels)
       'id))))

(defun agent-shell-to-go--discord-create-channel (name)
  "Create a Discord forum channel named NAME in the guild.  Return its ID or nil."
  (when agent-shell-to-go-discord-guild-id
    (let* ((resp
            (agent-shell-to-go--discord-api
             "POST" (format "/guilds/%s/channels" agent-shell-to-go-discord-guild-id)
             `((name . ,name) (type . ,agent-shell-to-go--discord-channel-type-forum))))
           (id (map-elt resp 'id)))
      (or id (agent-shell-to-go--discord-find-channel-by-name name)))))

(defun agent-shell-to-go--discord-get-or-create-project-channel (transport project-path)
  "Return the Discord forum channel ID for PROJECT-PATH, creating it if needed."
  (let ((cached
         (gethash
          project-path
          (agent-shell-to-go-discord-transport-project-channels transport))))
    (or cached
        (let* ((project-name
                (file-name-nondirectory (directory-file-name project-path)))
               (channel-name
                (concat
                 agent-shell-to-go-discord-channel-prefix
                 (agent-shell-to-go--sanitize-channel-name project-name)))
               (channel-id
                (or (agent-shell-to-go--discord-find-channel-by-name channel-name)
                    (agent-shell-to-go--discord-create-channel channel-name))))
          (when channel-id
            (puthash
             project-path
             channel-id
             (agent-shell-to-go-discord-transport-project-channels transport))
            (agent-shell-to-go--discord-save-channels transport)
            (agent-shell-to-go--debug
             "discord channel %s for %s" channel-name project-path))
          channel-id))))

; Formatting helpers 

(defun agent-shell-to-go--discord-format-diff (old-text new-text)
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
    ((transport agent-shell-to-go-discord-transport))
  "Connect the Discord transport via Gateway WebSocket."
  (unless agent-shell-to-go-discord-bot-token
    (error "agent-shell-to-go-discord-bot-token not set"))
  (agent-shell-to-go--discord-load-channels transport)
  (let ((ws
         (or (agent-shell-to-go-discord-transport-ws transport)
             (agent-shell-to-go--ws-make
              :name 'discord
              :url-fn
              (lambda () agent-shell-to-go--discord-gateway-url)
              :on-frame
              (lambda (frame) (agent-shell-to-go--discord-handle-frame transport frame))
              :on-close
              (lambda () (agent-shell-to-go--discord-stop-heartbeat transport))
              :get-active-p (lambda () t)))))
    (setf (agent-shell-to-go-discord-transport-ws transport) ws)
    (agent-shell-to-go--ws-connect ws)))

(cl-defmethod agent-shell-to-go-transport-disconnect
    ((transport agent-shell-to-go-discord-transport))
  "Disconnect the Discord WebSocket."
  (agent-shell-to-go--discord-stop-heartbeat transport)
  (when-let* ((ws (agent-shell-to-go-discord-transport-ws transport)))
    (agent-shell-to-go--ws-disconnect ws)))

(cl-defmethod agent-shell-to-go-transport-connected-p
    ((transport agent-shell-to-go-discord-transport))
  "Return non-nil if the Discord Gateway WebSocket is open."
  (agent-shell-to-go--ws-connected-p
   (agent-shell-to-go-discord-transport-ws transport)))

(cl-defmethod agent-shell-to-go-transport-authorized-p
    ((transport agent-shell-to-go-discord-transport) user-id)
  "Return non-nil if USER-ID is in `agent-shell-to-go-discord-authorized-users'."
  (and agent-shell-to-go-discord-authorized-users
       (member user-id agent-shell-to-go-discord-authorized-users)))

(cl-defmethod agent-shell-to-go-transport-bot-user-id
    ((transport agent-shell-to-go-discord-transport))
  "Return the bot's Discord user ID, caching the result."
  (or (agent-shell-to-go-discord-transport-bot-user-id-cache transport)
      (let* ((resp (agent-shell-to-go--discord-api "GET" "/users/@me"))
             (uid (map-elt resp 'id)))
        (setf (agent-shell-to-go-discord-transport-bot-user-id-cache transport) uid)
        uid)))

(cl-defmethod agent-shell-to-go-transport-send-text
    ((transport agent-shell-to-go-discord-transport)
     channel-id
     thread-id
     text
     &optional
     options)
  "Post TEXT to Discord THREAD-ID (or CHANNEL-ID if no thread).
Options plist supports :truncate."
  (let* ((truncate (map-elt options :truncate))
         ;; Thread channel IS the destination in Discord
         (target (or thread-id channel-id))
         (display
          (if truncate
              (agent-shell-to-go--truncate-text text 500)
            text))
         (was-truncated (and truncate (not (equal text display))))
         (safe (agent-shell-to-go--discord-truncate-content display)))
    (condition-case err
        (let* ((resp
                (agent-shell-to-go--discord-api
                 "POST" (format "/channels/%s/messages" target)
                 `((content . ,safe))))
               (msg-id (map-elt resp 'id)))
          (when (and was-truncated msg-id)
            (agent-shell-to-go--save-truncated-message
             transport channel-id msg-id text))
          msg-id)
      (error
       (agent-shell-to-go--debug "discord send-text error: %s" err)
       nil))))

(cl-defmethod agent-shell-to-go-transport-edit-message
    ((transport agent-shell-to-go-discord-transport) channel-id message-id text)
  "Edit MESSAGE-ID in Discord CHANNEL-ID to TEXT."
  (let* ((safe (agent-shell-to-go--discord-truncate-content text))
         (resp
          (agent-shell-to-go--discord-api
           "PATCH" (format "/channels/%s/messages/%s" channel-id message-id)
           `((content . ,safe)))))
    (agent-shell-to-go--debug "edit-message: msg=%s resp=%s" message-id (json-encode resp))
    (map-elt resp 'id)))

(cl-defmethod agent-shell-to-go-transport-upload-file
    ((transport agent-shell-to-go-discord-transport)
     channel-id
     thread-id
     path
     &optional
     comment)
  "Upload PATH to Discord THREAD-ID (or CHANNEL-ID) with optional COMMENT."
  (when (and path (file-exists-p path))
    (agent-shell-to-go--discord-api-upload (or thread-id channel-id) path comment)))

(cl-defmethod agent-shell-to-go-transport-get-message-text
    ((transport agent-shell-to-go-discord-transport) channel-id message-id)
  "Fetch the content of MESSAGE-ID from Discord CHANNEL-ID."
  (let* ((resp
          (agent-shell-to-go--discord-api
           "GET" (format "/channels/%s/messages/%s" channel-id message-id))))
    (map-elt resp 'content)))

(cl-defmethod agent-shell-to-go-transport-get-reactions
    ((transport agent-shell-to-go-discord-transport) channel-id message-id)
  "Return canonical action symbols for reactions on MESSAGE-ID."
  ;; Discord does not provide a single endpoint for all reactions; returns nil.
  ;; Reactions are handled in real-time via the Gateway.
  nil)

(cl-defmethod agent-shell-to-go-transport-fetch-thread-replies
    ((transport agent-shell-to-go-discord-transport) channel-id thread-id)
  "Return reply plists (:msg-id :user :text) for THREAD-ID.
In Discord the thread IS a channel, so THREAD-ID is the channel to query."
  (let* ((target (or thread-id channel-id))
         (resp
          (agent-shell-to-go--discord-api
           "GET" (format "/channels/%s/messages?limit=100" target)))
         (messages
          (when (vectorp resp)
            (append resp nil))))
    (mapcar
     (lambda (msg)
       (list
        :msg-id (map-elt msg 'id)
        :user (map-elt (map-elt msg 'author) 'id)
        :text (map-elt msg 'content)))
     (nreverse messages))))

(cl-defmethod agent-shell-to-go-transport-start-thread
    ((transport agent-shell-to-go-discord-transport) channel-id label)
  "Create a forum post in the Discord forum CHANNEL-ID. Return the post's thread channel ID.
CHANNEL-ID must be a forum channel; LABEL becomes the post title."
  (let* ((post-name
          (if (> (length label) 100)
              (concat (substring label 0 97) "…")
            label))
         (opening
          (format ":robot: **Agent Shell Session** @ %s\n_%s_"
                  (system-name)
                  (format-time-string "%Y-%m-%d %H:%M:%S")))
         ;; POST to the forum channel with an embedded `message' field —
         ;; this is the Discord forum-post creation endpoint.
         (resp
          (agent-shell-to-go--discord-api
           "POST" (format "/channels/%s/threads" channel-id)
           `((name . ,post-name)
             (auto_archive_duration . 10080)
             (message
              .
              ((content . ,(agent-shell-to-go--discord-truncate-content opening)))))))
         (thread-id (map-elt resp 'id)))
    ;; Record parent forum so inbound messages from this post can be routed back.
    (when thread-id
      (puthash
       thread-id
       channel-id
       (agent-shell-to-go-discord-transport-thread-parents transport)))
    thread-id))

(cl-defmethod agent-shell-to-go-transport-update-thread-header
    ((transport agent-shell-to-go-discord-transport) channel-id thread-id title)
  "Update the Discord forum post's title to TITLE."
  (when thread-id
    (let ((name
           (if (> (length title) 100)
               (concat (substring title 0 97) "…")
             title)))
      (agent-shell-to-go--discord-api "PATCH" (format "/channels/%s" thread-id)
                                      `((name . ,name))))))

(cl-defmethod agent-shell-to-go-transport-ensure-project-channel
    ((transport agent-shell-to-go-discord-transport) project-path)
  "Return the Discord forum channel ID for PROJECT-PATH."
  (agent-shell-to-go--discord-get-or-create-project-channel transport project-path))

(cl-defmethod agent-shell-to-go-transport-list-threads
    ((transport agent-shell-to-go-discord-transport) channel-id)
  "Return (:thread-id :last-timestamp) plists for forum posts in forum CHANNEL-ID."
  (let (result)
    ;; Active threads via guild endpoint
    (when agent-shell-to-go-discord-guild-id
      (let* ((resp
              (agent-shell-to-go--discord-api
               "GET"
               (format "/guilds/%s/threads/active" agent-shell-to-go-discord-guild-id)))
             (threads
              (when resp
                (append (map-elt resp 'threads) nil))))
        (dolist (thread threads)
          (when (equal (map-elt thread 'parent_id) channel-id)
            (let* ((tid (map-elt thread 'id))
                   ;; Derive approximate timestamp from Discord snowflake
                   (ts (/ (+ (ash (string-to-number tid) -22) 1420070400000) 1000.0)))
              (push (list :thread-id tid :last-timestamp ts) result))))))
    ;; Archived public threads
    (let* ((resp
            (agent-shell-to-go--discord-api
             "GET"
             (format "/channels/%s/threads/archived/public?limit=100" channel-id)))
           (threads
            (when resp
              (append (map-elt resp 'threads) nil))))
      (dolist (thread threads)
        (let* ((tid (map-elt thread 'id))
               (ts (/ (+ (ash (string-to-number tid) -22) 1420070400000) 1000.0)))
          (push (list :thread-id tid :last-timestamp ts) result))))
    result))

(cl-defmethod agent-shell-to-go-transport-delete-message
    ((transport agent-shell-to-go-discord-transport) channel-id message-id)
  "Delete MESSAGE-ID from Discord CHANNEL-ID."
  (agent-shell-to-go--discord-api
   "DELETE" (format "/channels/%s/messages/%s" channel-id message-id)))

(cl-defmethod agent-shell-to-go-transport-delete-thread
    ((transport agent-shell-to-go-discord-transport) channel-id thread-id)
  "Delete the Discord forum post THREAD-ID (deletes its thread channel)."
  (when thread-id
    (agent-shell-to-go--discord-api "DELETE" (format "/channels/%s" thread-id))))

;; Semantic formatters — Discord uses CommonMark natively so no conversion needed.

(cl-defmethod agent-shell-to-go-transport-format-tool-call-start
    ((transport agent-shell-to-go-discord-transport) title)
  (format "%s `%s`" (alist-get 'start agent-shell-to-go-tool-call-icons) title))

(cl-defmethod agent-shell-to-go-transport-format-tool-call-result
    ((transport agent-shell-to-go-discord-transport) title status output)
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
    ((transport agent-shell-to-go-discord-transport) old-text new-text)
  (let ((diff (agent-shell-to-go--discord-format-diff old-text new-text)))
    (if (and diff (> (length diff) 0))
        (format "```diff\n%s\n```" diff)
      "")))

(cl-defmethod agent-shell-to-go-transport-format-user-message
    ((transport agent-shell-to-go-discord-transport) text)
  (format ":bust_in_silhouette: **User**\n%s" text))

(cl-defmethod agent-shell-to-go-transport-format-agent-message
    ((transport agent-shell-to-go-discord-transport) text)
  (format ":robot: **Agent**\n%s" text))

(cl-defmethod agent-shell-to-go-transport-format-markdown
    ((transport agent-shell-to-go-discord-transport) markdown)
  markdown)

; WebSocket / Gateway 

(defun agent-shell-to-go--discord-stop-heartbeat (transport)
  "Cancel the heartbeat timer for TRANSPORT."
  (when-let* ((timer (agent-shell-to-go-discord-transport-heartbeat-timer transport)))
    (cancel-timer timer)
    (setf (agent-shell-to-go-discord-transport-heartbeat-timer transport) nil)))

(defun agent-shell-to-go--discord-send-heartbeat (transport)
  "Send a Gateway heartbeat for TRANSPORT."
  (let ((ws (agent-shell-to-go-discord-transport-ws transport)))
    (when (agent-shell-to-go--ws-connected-p ws)
      (condition-case err
          (websocket-send-text
           (agent-shell-to-go--ws-websocket ws)
           (json-encode
            `((op . ,agent-shell-to-go--discord-op-heartbeat)
              (d
               .
               ,(or (agent-shell-to-go-discord-transport-sequence transport) :null)))))
        (error
         (agent-shell-to-go--debug "discord heartbeat send error: %s" err))))))

(defun agent-shell-to-go--discord-start-heartbeat (transport interval-ms)
  "Start a repeating heartbeat timer for TRANSPORT every INTERVAL-MS milliseconds."
  (agent-shell-to-go--discord-stop-heartbeat transport)
  (let ((interval-sec (/ interval-ms 1000.0)))
    (setf (agent-shell-to-go-discord-transport-heartbeat-timer transport)
          (run-with-timer
           interval-sec interval-sec #'agent-shell-to-go--discord-send-heartbeat
           transport))))

(defun agent-shell-to-go--discord-send-identify (transport)
  "Send the Gateway Identify payload for TRANSPORT."
  (let ((ws (agent-shell-to-go-discord-transport-ws transport)))
    (when (agent-shell-to-go--ws-connected-p ws)
      (let* ((intents
              (logior
               agent-shell-to-go--discord-intent-guilds
               agent-shell-to-go--discord-intent-guild-messages
               agent-shell-to-go--discord-intent-guild-message-reactions
               agent-shell-to-go--discord-intent-direct-messages
               agent-shell-to-go--discord-intent-message-content))
             (payload
              `((op . ,agent-shell-to-go--discord-op-identify)
                (d
                 .
                 ((token . ,agent-shell-to-go-discord-bot-token)
                  (intents . ,intents)
                  (properties
                   .
                   ((os . "linux")
                    (browser . "agent-shell-to-go")
                    (device . "agent-shell-to-go"))))))))
        (websocket-send-text
         (agent-shell-to-go--ws-websocket ws) (json-encode payload))))))

(defun agent-shell-to-go--discord-handle-frame (transport frame)
  "Parse a Discord Gateway FRAME and dispatch."
  (let* ((payload (websocket-frame-text frame))
         (data
          (condition-case nil
              (json-read-from-string payload)
            (error
             nil))))
    (when data
      (let* ((op (map-elt data 'op))
             (d (map-elt data 'd))
             (s (map-elt data 's))
             (t-event (map-elt data 't)))
        (when s
          (setf (agent-shell-to-go-discord-transport-sequence transport) s))
        (cond
         ((= op agent-shell-to-go--discord-op-hello)
          (let ((interval (map-elt d 'heartbeat_interval)))
            (agent-shell-to-go--debug
             "discord gateway: hello, heartbeat_interval=%dms" interval)
            (agent-shell-to-go--discord-start-heartbeat transport interval)
            (agent-shell-to-go--discord-send-identify transport)))
         ((= op agent-shell-to-go--discord-op-heartbeat-ack)
          (agent-shell-to-go--debug "discord gateway: heartbeat ack"))
         ((= op agent-shell-to-go--discord-op-heartbeat)
          (agent-shell-to-go--discord-send-heartbeat transport))
         ((= op agent-shell-to-go--discord-op-invalid-session)
          (agent-shell-to-go--debug
           "discord gateway: invalid session, re-identifying in 5s")
          (run-with-timer 5 nil #'agent-shell-to-go--discord-send-identify transport))
         ((= op agent-shell-to-go--discord-op-dispatch)
          (agent-shell-to-go--defer
           #'agent-shell-to-go--discord-dispatch-event
           transport
           (format "%s" t-event)
           d)))))))

; Event dispatch 

(defun agent-shell-to-go--discord-message-seen-p (transport msg-id)
  "Return non-nil if MSG-ID was already processed by TRANSPORT."
  (let ((table (agent-shell-to-go-discord-transport-processed-ids transport)))
    (if (gethash msg-id table)
        t
      (puthash msg-id (float-time) table)
      (let ((now (float-time)))
        (maphash
         (lambda (k v)
           (when (> (- now v) 60)
             (remhash k table)))
         table))
      nil)))

(defun agent-shell-to-go--discord-dispatch-event (transport event-type data)
  "Dispatch Discord Gateway event EVENT-TYPE with DATA through inbound hooks."
  (agent-shell-to-go--debug "discord dispatch: %s" event-type)
  (pcase event-type
    ("READY" (let* ((user (map-elt data 'user))
            (uid (map-elt user 'id))
            (sid (map-elt data 'session_id)))
       (setf (agent-shell-to-go-discord-transport-bot-user-id-cache transport) uid)
       (setf (agent-shell-to-go-discord-transport-session-id transport) sid)
       (agent-shell-to-go--debug "discord gateway: ready as user=%s" uid)))
    ("MESSAGE_CREATE" (agent-shell-to-go--discord-normalize-message transport data))
    ("MESSAGE_REACTION_ADD"
     (agent-shell-to-go--discord-normalize-reaction transport data t))
    ("MESSAGE_REACTION_REMOVE"
     (agent-shell-to-go--discord-normalize-reaction transport data nil))))

(defun agent-shell-to-go--discord-resolve-channel (transport channel-id)
  "Return (parent-channel-id . thread-channel-id-or-nil) for CHANNEL-ID.
If CHANNEL-ID is a forum post we created, returns its parent forum channel.
Falls back to a Discord API lookup when the channel is not in the local
thread-parents cache (e.g. after an Emacs restart)."
  (let ((parent
         (gethash
          channel-id (agent-shell-to-go-discord-transport-thread-parents transport))))
    (if parent
        (cons parent channel-id)
      ;; Cache miss: ask Discord whether this channel is a thread (type 11).
      ;; Thread channels have a parent_id pointing to the forum channel.
      (condition-case nil
          (let* ((resp
                  (agent-shell-to-go--discord-api
                   "GET" (format "/channels/%s" channel-id)))
                 (type (map-elt resp 'type))
                 (parent-id (map-elt resp 'parent_id)))
            (if (and parent-id (= type 11))
                (progn
                  (puthash
                   channel-id
                   parent-id
                   (agent-shell-to-go-discord-transport-thread-parents transport))
                  (cons parent-id channel-id))
              (cons channel-id nil)))
        (error
         (cons channel-id nil))))))

(defun agent-shell-to-go--discord-normalize-message (transport data)
  "Normalize a Discord MESSAGE_CREATE payload and fire the message hook."
  (let* ((msg-id (map-elt data 'id))
         (channel-id (map-elt data 'channel_id))
         (author (map-elt data 'author))
         (user-id (map-elt author 'id))
         (bot-p (eq (map-elt author 'bot) t))
         (content (map-elt data 'content))
         (bot-uid (agent-shell-to-go-discord-transport-bot-user-id-cache transport)))
    (unless (or bot-p
                (not content)
                (string-empty-p content)
                (equal user-id bot-uid)
                (agent-shell-to-go--discord-message-seen-p transport msg-id))
      (when (agent-shell-to-go-transport-authorized-p transport user-id)
        (let* ((resolved
                (agent-shell-to-go--discord-resolve-channel transport channel-id))
               (hook-channel (car resolved))
               (hook-thread (cdr resolved)))
          (apply #'run-hook-with-args
                 'agent-shell-to-go-message-hook
                 (list
                  :transport transport
                  :channel-id hook-channel
                  :thread-id hook-thread
                  :user user-id
                  :text content
                  :msg-id msg-id)))))))

(defun agent-shell-to-go--discord-normalize-reaction (transport data added-p)
  "Normalize a Discord reaction event and fire the reaction hook.
ADDED-P is t for MESSAGE_REACTION_ADD, nil for MESSAGE_REACTION_REMOVE."
  (let* ((channel-id (map-elt data 'channel_id))
         (msg-id (map-elt data 'message_id))
         (user-id (map-elt data 'user_id))
         (emoji (map-elt data 'emoji))
         (emoji-name (map-elt emoji 'name))
         (action (agent-shell-to-go--discord-emoji-to-action emoji-name))
         (bot-uid (agent-shell-to-go-discord-transport-bot-user-id-cache transport)))
    (unless (equal user-id bot-uid)
      (agent-shell-to-go--debug
       "discord reaction: %s on %s (%s)" emoji-name msg-id
       (if added-p
           "added"
         "removed"))
      (when (agent-shell-to-go-transport-authorized-p transport user-id)
        (let* ((resolved
                (agent-shell-to-go--discord-resolve-channel transport channel-id))
               (hook-channel (car resolved)))
          (apply #'run-hook-with-args
                 'agent-shell-to-go-reaction-hook
                 (list
                  :transport transport
                  :channel-id hook-channel
                  :thread-id nil
                  :msg-id msg-id
                  :user user-id
                  :action action
                  :raw-emoji emoji-name
                  :added-p added-p)))))))

; Registration 

(defvar agent-shell-to-go--discord-instance nil
  "The singleton Discord transport struct.")

(defun agent-shell-to-go-discord-get-or-create ()
  "Return (or create) the global Discord transport instance."
  (unless agent-shell-to-go--discord-instance
    (setq agent-shell-to-go--discord-instance
          (agent-shell-to-go--make-discord-transport :name 'discord)))
  agent-shell-to-go--discord-instance)

;; Auto-register on load so `(agent-shell-to-go-get-transport 'discord)' works.
(agent-shell-to-go-register-transport
 'discord (agent-shell-to-go-discord-get-or-create))

(provide 'agent-shell-to-go-discord)
;;; agent-shell-to-go-discord.el ends here
