;;; agent-shell-to-go.el --- Take your agent-shell sessions anywhere -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Elle Najt

;; Author: Elle Najt
;; URL: https://github.com/ElleNajt/agent-shell-to-go
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.33.1") (websocket "1.14"))
;; Keywords: convenience, tools, ai

;; This file is not part of GNU Emacs.

;;; Commentary:

;; agent-shell-to-go mirrors your agent-shell conversations to Slack,
;; letting you interact with your AI agents from your phone or any device.
;;
;; Features:
;; - Each agent-shell session gets its own Slack thread
;; - Messages you send from Emacs appear in Slack
;; - Messages you send from Slack get injected back into agent-shell
;; - Real-time via Slack Socket Mode (WebSocket)
;; - Works with any agent-shell agent (Claude, Gemini, etc.)
;;
;; Setup:
;; 1. Create a Slack app with:
;;    - Bot token scopes: chat:write, channels:history, channels:read, reactions:read
;;    - Enable Socket Mode and get an app-level token (xapp-...)
;;
;; 2. Create ~/.doom.d/.env:
;;    SLACK_BOT_TOKEN=xoxb-your-token
;;    SLACK_CHANNEL_ID=C0123456789
;;    SLACK_APP_TOKEN=xapp-your-app-token
;;
;; 3. Add to your config:
;;    (use-package agent-shell-to-go
;;      :after agent-shell
;;      :config
;;      (agent-shell-to-go-setup))

;;; Code:

(require 'json)
(require 'url)
(require 'websocket)

(defgroup agent-shell-to-go nil
  "Take your agent-shell sessions anywhere."
  :group 'agent-shell
  :prefix "agent-shell-to-go-")

(defcustom agent-shell-to-go-env-file "~/.doom.d/.env"
  "Path to .env file containing Slack credentials."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-bot-token nil
  "Slack bot token (xoxb-...). Loaded from .env if nil."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-channel-id nil
  "Default Slack channel ID to post to. Loaded from .env if nil.
When `agent-shell-to-go-per-project-channels' is non-nil, this is used
as a fallback when no project-specific channel exists."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-per-project-channels t
  "When non-nil, create a separate Slack channel for each project.
Channels are named based on the project directory name."
  :type 'boolean
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-channel-prefix ""
  "Prefix for auto-created project channels."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-channels-file
  (expand-file-name "agent-shell-to-go-channels.el" user-emacs-directory)
  "File to persist project-to-channel mappings."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-app-token nil
  "Slack app-level token (xapp-...) for Socket Mode. Loaded from .env if nil."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-default-folder "~/"
  "Default folder for /new-agent command when no folder is specified."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-start-agent-function #'agent-shell
  "Function to call to start a new agent-shell.
Override this if you have a custom agent-shell starter function."
  :type 'function
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-debug nil
  "When non-nil, log debug messages to *Messages*."
  :type 'boolean
  :group 'agent-shell-to-go)

(defun agent-shell-to-go--debug (format-string &rest args)
  "Log a debug message if `agent-shell-to-go-debug' is non-nil."
  (when agent-shell-to-go-debug
    (apply #'message (concat "agent-shell-to-go: " format-string) args)))

(defun agent-shell-to-go--strip-non-ascii (text)
  "Strip non-ASCII characters from TEXT, replacing them with '?'."
  (when text
    (replace-regexp-in-string "[^[:ascii:]]" "?" text)))

(defun agent-shell-to-go--load-env ()
  "Load credentials from .env file if not already set."
  (when (file-exists-p agent-shell-to-go-env-file)
    (with-temp-buffer
      (insert-file-contents (expand-file-name agent-shell-to-go-env-file))
      (goto-char (point-min))
      (while (re-search-forward "^\\([A-Z_]+\\)=\\(.+\\)$" nil t)
        (let ((key (match-string 1))
              (value (match-string 2)))
          (pcase key
            ("SLACK_BOT_TOKEN"
             (unless agent-shell-to-go-bot-token
               (setq agent-shell-to-go-bot-token value)))
            ("SLACK_CHANNEL_ID"
             (unless agent-shell-to-go-channel-id
               (setq agent-shell-to-go-channel-id value)))
            ("SLACK_APP_TOKEN"
             (unless agent-shell-to-go-app-token
               (setq agent-shell-to-go-app-token value)))))))))

(defun agent-shell-to-go--load-channels ()
  "Load project-to-channel mappings from file."
  (when (file-exists-p agent-shell-to-go-channels-file)
    (with-temp-buffer
      (insert-file-contents agent-shell-to-go-channels-file)
      (let ((data (read (current-buffer))))
        (clrhash agent-shell-to-go--project-channels)
        (dolist (pair data)
          (puthash (car pair) (cdr pair) agent-shell-to-go--project-channels))))))

(defun agent-shell-to-go--save-channels ()
  "Save project-to-channel mappings to file."
  (with-temp-file agent-shell-to-go-channels-file
    (let ((data nil))
      (maphash (lambda (k v) (push (cons k v) data))
               agent-shell-to-go--project-channels)
      (prin1 data (current-buffer)))))

(defun agent-shell-to-go--sanitize-channel-name (name)
  "Sanitize NAME for use as a Slack channel name.
Lowercase, replace spaces/underscores with hyphens, max 80 chars."
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
           (when-let ((proj (project-current)))
             (if (fboundp 'project-root)
                 (project-root proj)
               (car (project-roots proj)))))
      default-directory))

(defun agent-shell-to-go--invite-user-to-channel (channel-id user-id)
  "Invite USER-ID to CHANNEL-ID."
  (agent-shell-to-go--api-request
   "POST" "conversations.invite"
   `((channel . ,channel-id)
     (users . ,user-id))))

(defun agent-shell-to-go--get-owner-user-id ()
  "Get the user ID of the workspace owner or first admin.
Falls back to getting the authed user from a recent message."
  ;; Try to get from auth.test - this gives us info about who installed the app
  (let* ((response (agent-shell-to-go--api-request "GET" "auth.test"))
         (user-id (alist-get 'user_id response)))
    ;; The bot's user_id is returned, but we need the installing user
    ;; We'll use the SLACK_USER_ID env var if set, otherwise skip invite
    nil))

(defcustom agent-shell-to-go-user-id nil
  "Your Slack user ID for auto-invite to new channels.
Find this in Slack: click your profile -> three dots -> Copy member ID."
  :type 'string
  :group 'agent-shell-to-go)

(defun agent-shell-to-go--create-channel (name)
  "Create a Slack channel with NAME. Return channel ID or nil on failure."
  (let* ((response (agent-shell-to-go--api-request
                    "POST" "conversations.create"
                    `((name . ,name)
                      (is_private . :json-false))))
         (ok (alist-get 'ok response))
         (channel (alist-get 'channel response))
         (channel-id (alist-get 'id channel))
         (error-msg (alist-get 'error response)))
    (cond
     (ok
      ;; Auto-invite user if configured
      (when agent-shell-to-go-user-id
        (agent-shell-to-go--invite-user-to-channel channel-id agent-shell-to-go-user-id))
      channel-id)
     ((equal error-msg "name_taken")
      ;; Channel exists, try to find it
      (agent-shell-to-go--find-channel-by-name name))
     (t
      (agent-shell-to-go--debug "Failed to create channel %s: %s" name error-msg)
      nil))))

(defun agent-shell-to-go--find-channel-by-name (name)
  "Find a channel by NAME, return its ID or nil."
  (let* ((response (agent-shell-to-go--api-request
                    "GET" (format "conversations.list?types=public_channel,private_channel&limit=1000")))
         (channels (alist-get 'channels response)))
    (when channels
      (cl-loop for channel across channels
               when (equal (alist-get 'name channel) name)
               return (alist-get 'id channel)))))

(defun agent-shell-to-go--get-or-create-project-channel ()
  "Get or create a Slack channel for the current project.
Returns the channel ID."
  (if (not agent-shell-to-go-per-project-channels)
      agent-shell-to-go-channel-id
    (let* ((project-path (agent-shell-to-go--get-project-path))
           (cached-id (gethash project-path agent-shell-to-go--project-channels)))
      (or cached-id
          (let* ((project-name (file-name-nondirectory
                                (directory-file-name project-path)))
                 (channel-name (concat agent-shell-to-go-channel-prefix
                                       (agent-shell-to-go--sanitize-channel-name project-name)))
                 (channel-id (agent-shell-to-go--create-channel channel-name)))
            (when channel-id
              (puthash project-path channel-id agent-shell-to-go--project-channels)
              (agent-shell-to-go--save-channels)
              (agent-shell-to-go--debug "Created/found channel %s for %s" channel-name project-path))
            (or channel-id agent-shell-to-go-channel-id))))))

;; Internal state
(defvar-local agent-shell-to-go--thread-ts nil
  "Slack thread timestamp for this buffer's conversation.")

(defvar agent-shell-to-go--active-buffers nil
  "List of agent-shell buffers with active Slack mirroring.")

(defvar agent-shell-to-go--bot-user-id-cache nil
  "Cached bot user ID.")

(defvar-local agent-shell-to-go--current-agent-message nil
  "Accumulator for streaming agent message chunks.")

(defvar agent-shell-to-go--pending-permissions nil
  "Alist of pending permission requests.
Each entry: (slack-msg-ts . (:request-id id :buffer buffer :options options))")

(defvar-local agent-shell-to-go--from-slack nil
  "Non-nil when the current message originated from Slack (to prevent echo).")

(defvar-local agent-shell-to-go--channel-id nil
  "The Slack channel ID for this buffer (may differ from default if per-project).")

(defvar agent-shell-to-go--project-channels (make-hash-table :test 'equal)
  "Hash table mapping project paths to Slack channel IDs.")

(defvar agent-shell-to-go--websocket nil
  "The WebSocket connection to Slack.")

(defvar agent-shell-to-go--websocket-reconnect-timer nil
  "Timer for reconnecting WebSocket.")

(defconst agent-shell-to-go--reaction-map
  '(("white_check_mark" . allow)
    ("+1" . allow)
    ("unlock" . always)
    ("star" . always)
    ("x" . reject)
    ("-1" . reject))
  "Map Slack reaction names to permission actions.")

(defconst agent-shell-to-go--hide-reactions
  '("no_bell" "see_no_evil")
  "Reactions that trigger hiding a message.")

(defconst agent-shell-to-go--expand-reactions
  '("eyes")
  "Reactions that trigger expanding a truncated message.")

(defconst agent-shell-to-go--heart-reactions
  '("heart" "heart_eyes" "heartpulse" "sparkling_heart" "two_hearts" "revolving_hearts")
  "Reactions that send appreciation to the agent.")

(defcustom agent-shell-to-go-hidden-messages-dir "~/.agent-shell/slack/"
  "Directory to store original content of hidden messages.
Messages are stored as CHANNEL/TIMESTAMP.txt files."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-truncated-messages-dir "~/.agent-shell/slack-truncated/"
  "Directory to store full content of truncated messages.
Messages are stored as CHANNEL/TIMESTAMP.txt files."
  :type 'string
  :group 'agent-shell-to-go)

;;; Slack API

(defun agent-shell-to-go--api-request (method endpoint &optional data)
  "Make a Slack API request using curl.
METHOD is GET or POST, ENDPOINT is the API endpoint, DATA is the payload."
  (let* ((url (concat "https://slack.com/api/" endpoint))
         (args (list "-s" "-X" method
                     "-H" (concat "Authorization: Bearer " agent-shell-to-go-bot-token)
                     "-H" "Content-Type: application/json; charset=utf-8")))
    (when data
      (setq args (append args (list "-d" (encode-coding-string (json-encode data) 'utf-8)))))
    (setq args (append args (list url)))
    (with-temp-buffer
      (apply #'call-process "curl" nil t nil args)
      (goto-char (point-min))
      (json-read))))

(defun agent-shell-to-go--send (text &optional thread-ts channel-id truncate)
  "Send TEXT to Slack, optionally in THREAD-TS thread.
CHANNEL-ID overrides the buffer-local or default channel.
If TRUNCATE is non-nil, truncate long messages and store full text for 👀 expansion."
  (condition-case err
      (let* ((channel (or channel-id
                          agent-shell-to-go--channel-id
                          agent-shell-to-go-channel-id))
             (clean-text text)
             (truncated-text (if truncate
                                 (agent-shell-to-go--truncate-message clean-text 500)
                               clean-text))
             (was-truncated (and truncate (not (equal clean-text truncated-text))))
             (data `((channel . ,channel)
                     (text . ,truncated-text))))
        (when thread-ts
          (push `(thread_ts . ,thread-ts) data))
        (let ((response (agent-shell-to-go--api-request "POST" "chat.postMessage" data)))
          ;; If truncated, save full text for expansion
          (when was-truncated
            (let ((msg-ts (alist-get 'ts response)))
              (when msg-ts
                (agent-shell-to-go--save-truncated-message channel msg-ts clean-text))))
          response))
    (error
     (agent-shell-to-go--debug "send error: %s, retrying with ASCII-only" err)
     ;; Fallback: strip non-ASCII and try again
     (let* ((channel (or channel-id
                         agent-shell-to-go--channel-id
                         agent-shell-to-go-channel-id))
            (safe-text (agent-shell-to-go--strip-non-ascii text))
            (truncated-text (if truncate
                                (agent-shell-to-go--truncate-message safe-text 500)
                              safe-text))
            (data `((channel . ,channel)
                    (text . ,truncated-text))))
       (when thread-ts
         (push `(thread_ts . ,thread-ts) data))
       (condition-case nil
           (agent-shell-to-go--api-request "POST" "chat.postMessage" data)
         (error
          (agent-shell-to-go--debug "send failed even with ASCII fallback")
          nil))))))

(defun agent-shell-to-go--get-reactions (msg-ts)
  "Get reactions on message MSG-TS."
  (let* ((response (agent-shell-to-go--api-request
                    "GET"
                    (format "reactions.get?channel=%s&timestamp=%s"
                            agent-shell-to-go-channel-id msg-ts)))
         (message (alist-get 'message response))
         (reactions (alist-get 'reactions message)))
    (when reactions
      (append reactions nil))))

(defun agent-shell-to-go--get-bot-user-id ()
  "Get the bot's user ID."
  (or agent-shell-to-go--bot-user-id-cache
      (setq agent-shell-to-go--bot-user-id-cache
            (alist-get 'user_id
                       (agent-shell-to-go--api-request "GET" "auth.test")))))

(defun agent-shell-to-go--start-thread (buffer-name)
  "Start a new Slack thread for BUFFER-NAME, return thread_ts."
  (let* ((response (agent-shell-to-go--send
                    (format ":robot_face: *Agent Shell Session*\n`%s`\n_%s_"
                            buffer-name
                            (format-time-string "%Y-%m-%d %H:%M:%S"))))
         (ts (alist-get 'ts response)))
    ts))

;;; Message formatting

(defun agent-shell-to-go--truncate-message (text &optional max-len)
  "Truncate TEXT to MAX-LEN (default 500) for Slack."
  (let ((max-len (or max-len 500)))
    (if (> (length text) max-len)
        (concat (substring text 0 max-len) "\n:eyes: _for more_")
      text)))

(defun agent-shell-to-go--format-user-message (prompt)
  "Format user PROMPT for Slack."
  (format ":bust_in_silhouette: *User*\n%s" prompt))

(defun agent-shell-to-go--format-agent-message (text)
  "Format agent TEXT for Slack."
  (format ":robot_face: *Agent*\n%s" text))

(defun agent-shell-to-go--format-tool-call (title status &optional output)
  "Format tool call with TITLE, STATUS, and optional OUTPUT for Slack."
  (let ((emoji (pcase status
                 ("completed" ":white_check_mark:")
                 ("failed" ":x:")
                 ("pending" ":hourglass:")
                 (_ ":wrench:"))))
    (if (and output (not (string-empty-p output)))
        (format "%s `%s`\n```\n%s\n```" emoji title output)
      (format "%s `%s`" emoji title))))

;;; WebSocket / Socket Mode

(defun agent-shell-to-go--get-websocket-url ()
  "Get WebSocket URL from Slack apps.connections.open API."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " agent-shell-to-go-app-token))
            ("Content-Type" . "application/x-www-form-urlencoded")))
         (url "https://slack.com/api/apps.connections.open"))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (let ((response (json-read)))
        (kill-buffer)
        (if (eq (alist-get 'ok response) t)
            (alist-get 'url response)
          (error "Failed to get WebSocket URL: %s" (alist-get 'error response)))))))

(defun agent-shell-to-go--find-buffer-for-thread (thread-ts &optional channel-id)
  "Find the agent-shell buffer that corresponds to THREAD-TS.
Optionally also match CHANNEL-ID if provided."
  (cl-find-if
   (lambda (buf)
     (and (buffer-live-p buf)
          (equal thread-ts
                 (buffer-local-value 'agent-shell-to-go--thread-ts buf))
          (or (not channel-id)
              (equal channel-id
                     (buffer-local-value 'agent-shell-to-go--channel-id buf)))))
   agent-shell-to-go--active-buffers))

(defun agent-shell-to-go--handle-websocket-message (frame)
  "Handle incoming WebSocket FRAME from Slack."
  (let* ((payload (websocket-frame-text frame))
         (data (json-read-from-string payload))
         (type (alist-get 'type data))
         (envelope-id (alist-get 'envelope_id data)))
    ;; Acknowledge the event
    (when envelope-id
      (websocket-send-text agent-shell-to-go--websocket
                           (json-encode `((envelope_id . ,envelope-id)))))
    ;; Handle different event types
    (agent-shell-to-go--debug "websocket message type: %s" type)
    (pcase type
      ("events_api"
       (agent-shell-to-go--handle-event (alist-get 'payload data)))
      ("slash_commands"
       (agent-shell-to-go--debug "got slash_commands payload: %s" (alist-get 'payload data))
       (agent-shell-to-go--handle-slash-command (alist-get 'payload data)))
      ("hello"
       (agent-shell-to-go--debug "WebSocket connected"))
      ("disconnect"
       (agent-shell-to-go--debug "WebSocket disconnect requested, reconnecting...")
       (agent-shell-to-go--websocket-reconnect)))))

(defun agent-shell-to-go--handle-event (payload)
  "Handle Slack event PAYLOAD."
  (let* ((event (alist-get 'event payload))
         (event-type (alist-get 'type event)))
    (agent-shell-to-go--debug "received event type: %s" event-type)
    (pcase event-type
      ("message"
       (agent-shell-to-go--handle-message-event event))
      ("reaction_added"
       (agent-shell-to-go--debug "reaction event: %s" event)
       (agent-shell-to-go--handle-reaction-event event))
      ("reaction_removed"
       (agent-shell-to-go--debug "reaction removed event: %s" event)
       (agent-shell-to-go--handle-reaction-removed-event event)))))

(defun agent-shell-to-go--handle-message-event (event)
  "Handle a message EVENT from Slack."
  (let* ((thread-ts (alist-get 'thread_ts event))
         (channel (alist-get 'channel event))
         (user (alist-get 'user event))
         (text (alist-get 'text event))
         (subtype (alist-get 'subtype event))
         (bot-id (alist-get 'bot_id event))
         (buffer (and thread-ts (agent-shell-to-go--find-buffer-for-thread thread-ts channel))))
    (agent-shell-to-go--debug "message event: thread=%s channel=%s text=%s buffer=%s"
                              thread-ts channel text buffer)
    ;; Only handle real user messages in threads we're tracking
    (when (and buffer
               text
               (not subtype)
               (not bot-id)
               (not (equal user (agent-shell-to-go--get-bot-user-id))))
      (with-current-buffer buffer
        (if (string-prefix-p "!" text)
            (progn
              (agent-shell-to-go--debug "handling command: %s" text)
              (agent-shell-to-go--handle-command text buffer thread-ts))
          (agent-shell-to-go--debug "received from Slack: %s" text)
          (agent-shell-to-go--inject-message text))))))

(defun agent-shell-to-go--hidden-message-path (channel ts)
  "Return the file path for storing hidden message content.
CHANNEL is the Slack channel ID, TS is the message timestamp."
  (expand-file-name (concat channel "/" ts ".txt")
                    agent-shell-to-go-hidden-messages-dir))

(defun agent-shell-to-go--get-message-text (channel ts &optional thread-ts)
  "Get the text of message at TS in CHANNEL.
If THREAD-TS is provided, look in that thread for the message."
  (if thread-ts
      ;; Look in thread replies
      (let* ((response (agent-shell-to-go--api-request
                        "GET"
                        (format "conversations.replies?channel=%s&ts=%s"
                                channel thread-ts)))
             (messages (alist-get 'messages response)))
        (cl-loop for msg across messages
                 when (equal ts (alist-get 'ts msg))
                 return (alist-get 'text msg)))
    ;; Top-level message
    (let* ((response (agent-shell-to-go--api-request
                      "GET"
                      (format "conversations.history?channel=%s&latest=%s&limit=1&inclusive=true"
                              channel ts)))
           (messages (alist-get 'messages response))
           (message (and messages (aref messages 0))))
      (alist-get 'text message))))

(defun agent-shell-to-go--save-hidden-message (channel ts text)
  "Save TEXT of message at TS in CHANNEL to disk."
  (let ((path (agent-shell-to-go--hidden-message-path channel ts)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert text))))

(defun agent-shell-to-go--load-hidden-message (channel ts)
  "Load the original text of hidden message at TS in CHANNEL."
  (let ((path (agent-shell-to-go--hidden-message-path channel ts)))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string)))))

(defun agent-shell-to-go--delete-hidden-message-file (channel ts)
  "Delete the stored hidden message file for TS in CHANNEL."
  (let ((path (agent-shell-to-go--hidden-message-path channel ts)))
    (when (file-exists-p path)
      (delete-file path))))

;;; Truncated message storage

(defun agent-shell-to-go--truncated-message-path (channel ts)
  "Return the file path for storing truncated message full content.
CHANNEL is the Slack channel ID, TS is the message timestamp."
  (expand-file-name (concat channel "/" ts ".txt")
                    agent-shell-to-go-truncated-messages-dir))

(defun agent-shell-to-go--save-truncated-message (channel ts full-text)
  "Save FULL-TEXT of truncated message at TS in CHANNEL to disk."
  (let ((path (agent-shell-to-go--truncated-message-path channel ts)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert full-text))))

(defun agent-shell-to-go--load-truncated-message (channel ts)
  "Load the full text of truncated message at TS in CHANNEL."
  (let ((path (agent-shell-to-go--truncated-message-path channel ts)))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string)))))

(defun agent-shell-to-go--delete-truncated-message-file (channel ts)
  "Delete the stored truncated message file for TS in CHANNEL."
  (let ((path (agent-shell-to-go--truncated-message-path channel ts)))
    (when (file-exists-p path)
      (delete-file path))))

(defconst agent-shell-to-go--slack-max-length 3800
  "Maximum message length for Slack API (with buffer for truncation note).")

(defconst agent-shell-to-go--truncation-note
  "\n_... (full text too long for Slack)_"
  "Note appended when expanded message still exceeds Slack limit.")

(defun agent-shell-to-go--expand-message (channel ts)
  "Expand truncated message at TS in CHANNEL to show full text.
If the full text exceeds Slack's limit, show as much as possible with a note."
  (let ((full-text (agent-shell-to-go--load-truncated-message channel ts)))
    (when full-text
      (let* ((too-long (> (length full-text) agent-shell-to-go--slack-max-length))
             (display-text (if too-long
                               (concat (substring full-text 0 agent-shell-to-go--slack-max-length)
                                       agent-shell-to-go--truncation-note)
                             full-text)))
        (agent-shell-to-go--api-request
         "POST" "chat.update"
         `((channel . ,channel)
           (ts . ,ts)
           (text . ,display-text)))))))

(defun agent-shell-to-go--find-buffer-for-channel (channel-id)
  "Find an agent-shell buffer associated with CHANNEL-ID."
  (cl-find-if
   (lambda (buf)
     (and (buffer-live-p buf)
          (equal channel-id
                 (buffer-local-value 'agent-shell-to-go--channel-id buf))))
   agent-shell-to-go--active-buffers))

(defun agent-shell-to-go--handle-heart-reaction (channel ts)
  "Handle heart reaction on message at TS in CHANNEL.
Sends the message content to the agent as appreciation feedback."
  (let* ((buffer (agent-shell-to-go--find-buffer-for-channel channel))
         (thread-ts (and buffer (buffer-local-value 'agent-shell-to-go--thread-ts buffer)))
         (message-text (agent-shell-to-go--get-message-text channel ts thread-ts)))
    (when (and buffer message-text)
      (with-current-buffer buffer
        (agent-shell-to-go--inject-message
         (format "The user heart reacted to: %s" message-text))))))

(defun agent-shell-to-go--collapse-message (channel ts)
  "Re-truncate expanded message at TS in CHANNEL."
  (let* ((full-text (agent-shell-to-go--load-truncated-message channel ts))
         (truncated (and full-text
                         (agent-shell-to-go--truncate-message full-text 500))))
    (when truncated
      (agent-shell-to-go--api-request
       "POST" "chat.update"
       `((channel . ,channel)
         (ts . ,ts)
         (text . ,truncated))))))

(defun agent-shell-to-go--hide-message (channel ts)
  "Hide message at TS in CHANNEL by replacing with collapsed text."
  ;; First fetch and save the original message
  (let ((original-text (agent-shell-to-go--get-message-text channel ts)))
    (when original-text
      (agent-shell-to-go--save-hidden-message channel ts original-text)
      (agent-shell-to-go--api-request
       "POST" "chat.update"
       `((channel . ,channel)
         (ts . ,ts)
         (text . ":see_no_evil: _message hidden_"))))))

(defun agent-shell-to-go--unhide-message (channel ts)
  "Restore hidden message at TS in CHANNEL to its original text."
  (let ((original-text (agent-shell-to-go--load-hidden-message channel ts)))
    (when original-text
      (agent-shell-to-go--api-request
       "POST" "chat.update"
       `((channel . ,channel)
         (ts . ,ts)
         (text . ,original-text)))
      (agent-shell-to-go--delete-hidden-message-file channel ts))))

(defun agent-shell-to-go--handle-reaction-removed-event (event)
  "Handle a reaction removed EVENT from Slack."
  (let* ((item (alist-get 'item event))
         (msg-ts (alist-get 'ts item))
         (channel (alist-get 'channel item))
         (reaction (alist-get 'reaction event)))
    ;; Check if it was a hide reaction being removed
    (when (member reaction agent-shell-to-go--hide-reactions)
      (agent-shell-to-go--unhide-message channel msg-ts))
    ;; Check if it was an expand reaction being removed (re-truncate)
    (when (member reaction agent-shell-to-go--expand-reactions)
      (agent-shell-to-go--collapse-message channel msg-ts))))

(defun agent-shell-to-go--handle-reaction-event (event)
  "Handle a reaction EVENT from Slack."
  (let* ((item (alist-get 'item event))
         (msg-ts (alist-get 'ts item))
         (channel (alist-get 'channel item))
         (reaction (alist-get 'reaction event))
         (pending (assoc msg-ts agent-shell-to-go--pending-permissions)))
    ;; Check for hide reactions first
    (when (member reaction agent-shell-to-go--hide-reactions)
      (agent-shell-to-go--hide-message channel msg-ts))
    ;; Check for expand reactions (show full truncated message)
    (when (member reaction agent-shell-to-go--expand-reactions)
      (agent-shell-to-go--expand-message channel msg-ts))
    ;; Check for heart reactions (send appreciation to agent)
    (when (member reaction agent-shell-to-go--heart-reactions)
      (agent-shell-to-go--debug "heart reaction: %s on %s" reaction msg-ts)
      (agent-shell-to-go--handle-heart-reaction channel msg-ts))
    ;; Then check for permission reactions
    (when pending
      (let* ((info (cdr pending))
             (request-id (plist-get info :request-id))
             (buffer (plist-get info :buffer))
             (options (plist-get info :options))
             (action (alist-get reaction agent-shell-to-go--reaction-map nil nil #'string=)))
        (when (and action buffer (buffer-live-p buffer))
          (let ((option-id (agent-shell-to-go--find-option-id options action)))
            (when option-id
              (with-current-buffer buffer
                (let ((state agent-shell--state))
                  (agent-shell--send-permission-response
                   :client (alist-get :client state)
                   :request-id request-id
                   :option-id option-id
                   :state state)))
              ;; Remove from pending
              (setq agent-shell-to-go--pending-permissions
                    (assq-delete-all msg-ts agent-shell-to-go--pending-permissions)))))))))

(defun agent-shell-to-go--start-agent-in-folder (folder &optional use-container)
  "Start a new agent in FOLDER. If USE-CONTAINER is non-nil, pass prefix arg."
  (agent-shell-to-go--debug "starting agent in %s (container: %s)" folder use-container)
  (if (file-directory-p folder)
      (let ((default-directory folder))
        (save-window-excursion
          (condition-case err
              (progn
                ;; Pass '(4) for C-u prefix, nil otherwise
                (funcall agent-shell-to-go-start-agent-function
                         (if use-container '(4) nil))
                (when agent-shell-to-go--thread-ts
                  (agent-shell-to-go--send
                   (format ":rocket: New agent started in `%s`%s"
                           folder
                           (if use-container " (container)" ""))
                   agent-shell-to-go--thread-ts)))
            (error
             (agent-shell-to-go--debug "error starting agent: %s" err)))))
    (agent-shell-to-go--debug "folder does not exist: %s" folder)))

(defun agent-shell-to-go--get-open-projects ()
  "Get list of open projects from Emacs.
Tries projectile first, then project.el, then falls back to buffer directories."
  (delete-dups
   (delq nil
         (cond
          ;; Try projectile
          ((fboundp 'projectile-open-projects)
           (projectile-open-projects))
          ;; Try project.el (Emacs 28+)
          ((fboundp 'project-known-project-roots)
           (project-known-project-roots))
          ;; Fallback: unique directories from file-visiting buffers
          (t
           (mapcar (lambda (buf)
                     (when-let ((file (buffer-file-name buf)))
                       (file-name-directory file)))
                   (buffer-list)))))))

(defun agent-shell-to-go--get-project-for-channel (channel-id)
  "Get the project path associated with CHANNEL-ID, or nil if not found."
  (let ((result nil))
    (maphash (lambda (project-path ch-id)
               (when (equal ch-id channel-id)
                 (setq result project-path)))
             agent-shell-to-go--project-channels)
    result))

(defun agent-shell-to-go--handle-slash-command (payload)
  "Handle a slash command PAYLOAD from Slack."
  (let* ((command (alist-get 'command payload))
         (text (alist-get 'text payload))
         (channel (alist-get 'channel_id payload))
         (channel-project (agent-shell-to-go--get-project-for-channel channel))
         (folder (expand-file-name
                  (cond
                   ;; Explicit folder argument takes priority
                   ((and text (not (string-empty-p text))) text)
                   ;; Use channel's project if available
                   (channel-project channel-project)
                   ;; Fall back to default
                   (t agent-shell-to-go-default-folder)))))
    (agent-shell-to-go--debug "slash command: %s %s (channel project: %s)" command text channel-project)
    (pcase command
      ("/new-agent"
       (agent-shell-to-go--start-agent-in-folder folder nil))
      ("/new-agent-container"
       (agent-shell-to-go--start-agent-in-folder folder t))
      ("/projects"
       (let ((projects (agent-shell-to-go--get-open-projects)))
         (if projects
             (progn
               (agent-shell-to-go--api-request
                "POST" "chat.postMessage"
                `((channel . ,channel)
                  (text . ":file_folder: *Open Projects:*")))
               (dolist (project projects)
                 (agent-shell-to-go--api-request
                  "POST" "chat.postMessage"
                  `((channel . ,channel)
                    (text . ,project)))))
           (agent-shell-to-go--api-request
            "POST" "chat.postMessage"
            `((channel . ,channel)
              (text . ":shrug: No open projects found")))))))))

(defvar agent-shell-to-go--intentional-close nil
  "Non-nil when we're intentionally closing the WebSocket (to prevent reconnect loop).")

(defun agent-shell-to-go--websocket-connect ()
  "Connect to Slack via WebSocket."
  (agent-shell-to-go--load-env)
  (unless agent-shell-to-go-app-token
    (error "agent-shell-to-go-app-token not set"))
  (when agent-shell-to-go--websocket
    (setq agent-shell-to-go--intentional-close t)
    (ignore-errors (websocket-close agent-shell-to-go--websocket))
    (setq agent-shell-to-go--intentional-close nil))
  (let ((ws-url (agent-shell-to-go--get-websocket-url)))
    (setq agent-shell-to-go--websocket
          (websocket-open ws-url
                          :on-message (lambda (_ws frame)
                                        (agent-shell-to-go--handle-websocket-message frame))
                          :on-close (lambda (_ws)
                                      (agent-shell-to-go--debug "WebSocket closed")
                                      (unless agent-shell-to-go--intentional-close
                                        (agent-shell-to-go--websocket-reconnect)))
                          :on-error (lambda (_ws _type err)
                                      (agent-shell-to-go--debug "WebSocket error: %s" err))))))

(defun agent-shell-to-go--websocket-reconnect ()
  "Schedule WebSocket reconnection."
  (when agent-shell-to-go--websocket-reconnect-timer
    (cancel-timer agent-shell-to-go--websocket-reconnect-timer))
  (when agent-shell-to-go--active-buffers
    (setq agent-shell-to-go--websocket-reconnect-timer
          (run-with-timer 5 nil #'agent-shell-to-go--websocket-connect))))

(defun agent-shell-to-go--websocket-disconnect ()
  "Disconnect WebSocket."
  (when agent-shell-to-go--websocket-reconnect-timer
    (cancel-timer agent-shell-to-go--websocket-reconnect-timer)
    (setq agent-shell-to-go--websocket-reconnect-timer nil))
  (when agent-shell-to-go--websocket
    (setq agent-shell-to-go--intentional-close t)
    (ignore-errors (websocket-close agent-shell-to-go--websocket))
    (setq agent-shell-to-go--websocket nil)
    (setq agent-shell-to-go--intentional-close nil)))

;;; Advice functions to hook into agent-shell

(defun agent-shell-to-go--on-request (orig-fn &rest args)
  "Advice for agent-shell--on-request. Notify on permission requests.
ORIG-FN is the original function, ARGS are its arguments."
  (let* ((state (plist-get args :state))
         (request (plist-get args :request))
         (method (alist-get 'method request))
         (buffer (and state (alist-get :buffer state))))
    (when (and buffer
               (buffer-live-p buffer)
               (equal method "session/request_permission"))
      (let* ((thread-ts (buffer-local-value 'agent-shell-to-go--thread-ts buffer))
             (request-id (alist-get 'id request))
             (params (alist-get 'params request))
             (options (alist-get 'options params))
             (tool-call (alist-get 'toolCall params))
             (title (alist-get 'title tool-call))
             (raw-input (alist-get 'rawInput tool-call))
             (command (and raw-input (alist-get 'command raw-input))))
        (when thread-ts
          (condition-case err
              (let* ((response (agent-shell-to-go--send
                                (format ":warning: *Permission Required*\n`%s`\n\nReact: :white_check_mark: Allow | :unlock: Always | :x: Reject"
                                        (or command title "Unknown action"))
                                thread-ts))
                     (msg-ts (alist-get 'ts response)))
                (when msg-ts
                  (push (cons msg-ts
                              (list :request-id request-id
                                    :buffer buffer
                                    :options options
                                    :command (or command title "Unknown")))
                        agent-shell-to-go--pending-permissions)))
            (error (message "agent-shell-to-go permission notify error: %s" err)))))))
  (apply orig-fn args))

(defun agent-shell-to-go--on-send-command (orig-fn &rest args)
  "Advice for agent-shell--send-command. Send user prompt to Slack.
ORIG-FN is the original function, ARGS are its arguments."
  (when (and agent-shell-to-go-mode
             agent-shell-to-go--thread-ts
             (not agent-shell-to-go--from-slack))
    (let ((prompt (plist-get args :prompt)))
      (when prompt
        (agent-shell-to-go--send
         (agent-shell-to-go--format-user-message prompt)
         agent-shell-to-go--thread-ts))))
  ;; Clear the from-slack flag after checking it
  (setq agent-shell-to-go--from-slack nil)
  (setq agent-shell-to-go--current-agent-message nil)
  (apply orig-fn args))

(defun agent-shell-to-go--on-notification (orig-fn &rest args)
  "Advice for agent-shell--on-notification. Mirror updates to Slack.
ORIG-FN is the original function, ARGS are its arguments."
  (let* ((state (plist-get args :state))
         (buffer (alist-get :buffer state)))
    (when (and buffer
               (buffer-live-p buffer)
               (buffer-local-value 'agent-shell-to-go-mode buffer))
      (let* ((notification (plist-get args :notification))
             (params (alist-get 'params notification))
             (update (alist-get 'update params))
             (update-type (alist-get 'sessionUpdate update))
             (thread-ts (buffer-local-value 'agent-shell-to-go--thread-ts buffer)))
        (when thread-ts
          (pcase update-type
            ("agent_message_chunk"
             (let ((text (alist-get 'text (alist-get 'content update))))
               (with-current-buffer buffer
                 (setq agent-shell-to-go--current-agent-message
                       (concat agent-shell-to-go--current-agent-message text)))))
            ("tool_call"
             ;; Tool call starting - show command or title
             ;; First flush any pending agent message so order is preserved
             (with-current-buffer buffer
               (when (and agent-shell-to-go--current-agent-message
                          (> (length agent-shell-to-go--current-agent-message) 0))
                 (agent-shell-to-go--send
                  (agent-shell-to-go--format-agent-message agent-shell-to-go--current-agent-message)
                  thread-ts)
                 (setq agent-shell-to-go--current-agent-message nil)))
             (let* ((title (alist-get 'title update))
                    (raw-input (alist-get 'rawInput update))
                    (command (alist-get 'command raw-input))
                    (file-path (alist-get 'file_path raw-input))
                    (display (or command
                                 file-path
                                 title)))
               (when display
                 (agent-shell-to-go--send
                  (format ":hourglass: `%s`" display)
                  thread-ts nil t))))  ; truncate=t
            ("tool_call_update"
             ;; Tool call completed - show output
             (let* ((status (alist-get 'status update))
                    (output (or (alist-get 'rawOutput update)
                                (alist-get 'output update))))
               (when (and (member status '("completed" "failed")) output)
                 (agent-shell-to-go--send
                  (format "%s\n```\n%s\n```"
                          (if (equal status "completed") ":white_check_mark:" ":x:")
                          output)
                  thread-ts nil t)))))))))  ; truncate=t
  (apply orig-fn args))

(defun agent-shell-to-go--on-heartbeat-stop (orig-fn &rest args)
  "Advice for agent-shell-heartbeat-stop. Flush agent message to Slack.
ORIG-FN is the original function, ARGS are its arguments."
  (when (and agent-shell-to-go-mode
             agent-shell-to-go--thread-ts
             agent-shell-to-go--current-agent-message
             (> (length agent-shell-to-go--current-agent-message) 0))
    (agent-shell-to-go--send
     (agent-shell-to-go--format-agent-message agent-shell-to-go--current-agent-message)
     agent-shell-to-go--thread-ts)
    (setq agent-shell-to-go--current-agent-message nil))
  (apply orig-fn args))

;;; Command handling

(defun agent-shell-to-go--find-option-id (options action)
  "Find option ID in OPTIONS matching ACTION (allow, always, reject)."
  (let ((options-list (append options nil)))
    (cl-loop for opt in options-list
             for id = (or (alist-get 'optionId opt) (alist-get 'id opt))
             for kind = (alist-get 'kind opt)
             when (pcase action
                    ('allow (member kind '("allow" "accept" "allow_once")))
                    ('always (member kind '("always" "alwaysAllow" "allow_always")))
                    ('reject (member kind '("deny" "reject" "reject_once"))))
             return id)))

(defun agent-shell-to-go--set-mode (buffer mode-id thread-ts mode-name emoji)
  "Set MODE-ID in BUFFER, notify THREAD-TS with MODE-NAME and EMOJI."
  (with-current-buffer buffer
    (agent-shell--set-default-session-mode
     :shell nil
     :mode-id mode-id
     :on-mode-changed (lambda ()
                        (agent-shell-to-go--send
                         (format "%s Mode: *%s*" emoji mode-name)
                         thread-ts)))))

(defun agent-shell-to-go--handle-command (text buffer thread-ts)
  "Handle command TEXT in BUFFER, reply to THREAD-TS."
  (let ((cmd (downcase (string-trim text))))
    (pcase cmd
      ((or "!yolo" "!bypass")
       (agent-shell-to-go--set-mode buffer "bypassPermissions" thread-ts
                                    "Bypass Permissions" ":zap:")
       t)
      ((or "!safe" "!accept" "!acceptedits")
       (agent-shell-to-go--set-mode buffer "acceptEdits" thread-ts
                                    "Accept Edits" ":shield:")
       t)
      ((or "!plan" "!planmode")
       (agent-shell-to-go--set-mode buffer "plan" thread-ts
                                    "Plan" ":clipboard:")
       t)
      ("!mode"
       (with-current-buffer buffer
         (let ((mode-id (map-nested-elt agent-shell--state '(:session :mode-id))))
           (agent-shell-to-go--send (format ":gear: Current mode: *%s*" (or mode-id "unknown")) thread-ts)))
       t)
      ("!help"
       (agent-shell-to-go--send
        (concat ":question: *Commands:*\n"
                "`!yolo` - Bypass permissions\n"
                "`!safe` - Accept edits mode\n"
                "`!plan` - Plan mode\n"
                "`!mode` - Show current mode\n"
                "`!stop` - Interrupt the agent")
        thread-ts)
       t)
      ("!stop"
       (condition-case err
           (with-current-buffer buffer
             (agent-shell-interrupt t)  ; force=t to skip y-or-n-p prompt
             (agent-shell-to-go--send ":stop_sign: Agent interrupted" thread-ts))
         (error
          (agent-shell-to-go--debug "!stop error: %s" err)
          (agent-shell-to-go--send (format ":x: Stop failed: %s" err) thread-ts)))
       t)
      (_ nil))))

(defun agent-shell-to-go--inject-message (text)
  "Inject TEXT from Slack into the current agent-shell buffer."
  (when (derived-mode-p 'agent-shell-mode)
    ;; Set flag - it will be cleared by the send-command advice after it skips posting
    (setq agent-shell-to-go--from-slack t)
    (save-excursion
      (goto-char (point-max))
      (insert text))
    (goto-char (point-max))
    (call-interactively #'shell-maker-submit)))

;;; Minor mode

(defun agent-shell-to-go--enable ()
  "Enable Slack mirroring for this buffer."
  (agent-shell-to-go--load-env)
  (agent-shell-to-go--load-channels)

  (unless agent-shell-to-go-bot-token
    (error "agent-shell-to-go-bot-token not set. See agent-shell-to-go-env-file"))
  (unless agent-shell-to-go-channel-id
    (error "agent-shell-to-go-channel-id not set. See agent-shell-to-go-env-file"))
  (unless agent-shell-to-go-app-token
    (error "agent-shell-to-go-app-token not set. See agent-shell-to-go-env-file"))

  ;; Get or create project-specific channel
  (setq agent-shell-to-go--channel-id
        (agent-shell-to-go--get-or-create-project-channel))

  ;; Start a new Slack thread for this session
  (setq agent-shell-to-go--thread-ts
        (agent-shell-to-go--start-thread (buffer-name)))

  ;; Track this buffer
  (add-to-list 'agent-shell-to-go--active-buffers (current-buffer))

  ;; Connect WebSocket if not already connected
  (unless (and agent-shell-to-go--websocket
               (websocket-openp agent-shell-to-go--websocket))
    (agent-shell-to-go--websocket-connect))

  ;; Add advice
  (advice-add 'agent-shell--send-command :around #'agent-shell-to-go--on-send-command)
  (advice-add 'agent-shell--on-notification :around #'agent-shell-to-go--on-notification)
  (advice-add 'agent-shell--on-request :around #'agent-shell-to-go--on-request)
  (advice-add 'agent-shell-heartbeat-stop :around #'agent-shell-to-go--on-heartbeat-stop)

  (agent-shell-to-go--debug "mirroring to Slack thread %s" agent-shell-to-go--thread-ts))

(defun agent-shell-to-go--disable ()
  "Disable Slack mirroring for this buffer."
  (when agent-shell-to-go--thread-ts
    (agent-shell-to-go--send ":wave: Session ended" agent-shell-to-go--thread-ts))

  ;; Untrack this buffer
  (setq agent-shell-to-go--active-buffers
        (delete (current-buffer) agent-shell-to-go--active-buffers))

  ;; Disconnect WebSocket if no more active buffers
  (unless agent-shell-to-go--active-buffers
    (agent-shell-to-go--websocket-disconnect)
    (advice-remove 'agent-shell--send-command #'agent-shell-to-go--on-send-command)
    (advice-remove 'agent-shell--on-notification #'agent-shell-to-go--on-notification)
    (advice-remove 'agent-shell--on-request #'agent-shell-to-go--on-request)
    (advice-remove 'agent-shell-heartbeat-stop #'agent-shell-to-go--on-heartbeat-stop))

  (agent-shell-to-go--debug "mirroring disabled"))

;;;###autoload
(define-minor-mode agent-shell-to-go-mode
  "Mirror agent-shell conversations to Slack.
Take your AI agent sessions anywhere - chat from your phone!"
  :lighter " ToGo"
  :group 'agent-shell-to-go
  (if agent-shell-to-go-mode
      (agent-shell-to-go--enable)
    (agent-shell-to-go--disable)))

;;;###autoload
(defun agent-shell-to-go-auto-enable ()
  "Automatically enable Slack mirroring for agent-shell buffers."
  (when (derived-mode-p 'agent-shell-mode)
    (agent-shell-to-go-mode 1)))

;;;###autoload
(defun agent-shell-to-go-setup ()
  "Set up automatic Slack mirroring for all agent-shell sessions."
  (add-hook 'agent-shell-mode-hook #'agent-shell-to-go-auto-enable))

(provide 'agent-shell-to-go)
;;; agent-shell-to-go.el ends here
