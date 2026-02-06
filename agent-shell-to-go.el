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
;; Quick start:
;;    (use-package agent-shell-to-go
;;      :after agent-shell
;;      :config
;;      (setq agent-shell-to-go-bot-token "xoxb-...")
;;      (setq agent-shell-to-go-channel-id "C...")
;;      (setq agent-shell-to-go-app-token "xapp-...")
;;      (setq agent-shell-to-go-authorized-users '("U..."))  ; REQUIRED
;;      (agent-shell-to-go-setup))
;;
;; See README.md for full setup instructions including:
;; - Slack app creation (with manifest for quick setup)
;; - Secure credential storage (pass, macOS Keychain)
;; - Security configuration (authorized users allowlist)

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

(defcustom agent-shell-to-go-show-tool-output t
  "When non-nil, show tool call outputs in Slack messages.
When nil, only status icons are shown (use 👀 reaction to expand)."
  :type 'boolean
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-authorized-users nil
  "List of Slack user IDs authorized to interact with agents.
REQUIRED: You must set this for Slack integration to work.
If nil, NO ONE can interact (secure by default).
Find your user ID in Slack: click your profile -> three dots -> Copy member ID."
  :type '(repeat string)
  :group 'agent-shell-to-go)

(defun agent-shell-to-go--debug (format-string &rest args)
  "Log a debug message if `agent-shell-to-go-debug' is non-nil."
  (when agent-shell-to-go-debug
    (apply #'message (concat "agent-shell-to-go: " format-string) args)))

(defun agent-shell-to-go--authorized-p (user-id)
  "Return non-nil if USER-ID is authorized to interact with agents.
USER-ID is a Slack user ID (e.g., \"U01234567\").
If `agent-shell-to-go-authorized-users' is nil, NO ONE is authorized (secure by default).
Note: This is Slack-specific; other integrations will need their own auth."
  (and agent-shell-to-go-authorized-users
       (member user-id agent-shell-to-go-authorized-users)))

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

(defvar-local agent-shell-to-go--file-watcher nil
  "Process for fswatch watching image files in this buffer's project.")

(defvar-local agent-shell-to-go--uploaded-images nil
  "Hash table of image paths already uploaded, to avoid duplicates.")

(defvar-local agent-shell-to-go--upload-timestamps nil
  "List of recent upload timestamps for rate limiting.")

(defvar-local agent-shell-to-go--mentioned-files nil
  "Hash table of file paths mentioned in recent tool calls.
Maps file path to timestamp when it was mentioned.
Used to filter image uploads to only files the agent touched.")

(defvar-local agent-shell-to-go--tool-calls nil
  "Hash table tracking tool calls by toolCallId.
Stores the best display info we've seen so far for each tool call,
so we only send to Slack when we have meaningful info.")

(defcustom agent-shell-to-go-image-upload-rate-limit 30
  "Maximum number of images to upload per minute.
Set to nil to disable rate limiting."
  :type '(choice (integer :tag "Max uploads per minute")
          (const :tag "No limit" nil))
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-image-upload-rate-window 60
  "Time window in seconds for rate limiting."
  :type 'integer
  :group 'agent-shell-to-go)

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
  "Reactions that trigger expanding to truncated view (glance).")

(defconst agent-shell-to-go--full-expand-reactions
  '("book" "open_book")
  "Reactions that trigger full expansion (read everything).")

(defconst agent-shell-to-go--heart-reactions
  '("heart" "heart_eyes" "heartpulse" "sparkling_heart" "two_hearts" "revolving_hearts")
  "Reactions that send appreciation to the agent.")

(defconst agent-shell-to-go--bookmark-reactions
  '("bookmark")
  "Reactions that create a TODO from the message.")

(defcustom agent-shell-to-go-todo-directory "~/org/todo/"
  "Directory where bookmark TODOs are saved as org files."
  :type 'string
  :group 'agent-shell-to-go)

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

(defun agent-shell-to-go--image-file-p (path)
  "Return non-nil if PATH is an image file based on extension."
  (when (and path (stringp path))
    (let ((ext (downcase (or (file-name-extension path) ""))))
      (member ext agent-shell-to-go--image-extensions))))

(defun agent-shell-to-go--check-upload-rate-limit ()
  "Check if we're within rate limit. Returns t if upload is allowed."
  (if (not agent-shell-to-go-image-upload-rate-limit)
      t  ; No limit configured
    (let* ((now (float-time))
           (window-start (- now agent-shell-to-go-image-upload-rate-window)))
      ;; Prune old timestamps
      (setq agent-shell-to-go--upload-timestamps
            (cl-remove-if (lambda (ts) (< ts window-start))
                          agent-shell-to-go--upload-timestamps))
      ;; Check if under limit
      (< (length agent-shell-to-go--upload-timestamps)
         agent-shell-to-go-image-upload-rate-limit))))

(defun agent-shell-to-go--record-upload ()
  "Record an upload timestamp for rate limiting."
  (push (float-time) agent-shell-to-go--upload-timestamps))

(defcustom agent-shell-to-go-mentioned-file-ttl 14400
  "Time in seconds to remember mentioned files for image upload filtering.
Files mentioned in tool calls are eligible for upload for this long.
Default is 4 hours (14400 seconds)."
  :type 'integer
  :group 'agent-shell-to-go)

(defun agent-shell-to-go--record-mentioned-file (file-path)
  "Record FILE-PATH as mentioned by the agent."
  (unless agent-shell-to-go--mentioned-files
    (setq agent-shell-to-go--mentioned-files (make-hash-table :test 'equal)))
  (puthash file-path (float-time) agent-shell-to-go--mentioned-files))

(defun agent-shell-to-go--file-was-mentioned-p (file-path)
  "Return non-nil if FILE-PATH was recently mentioned by the agent."
  (when agent-shell-to-go--mentioned-files
    (let ((mentioned-time (gethash file-path agent-shell-to-go--mentioned-files)))
      (and mentioned-time
           (< (- (float-time) mentioned-time) agent-shell-to-go-mentioned-file-ttl)))))

(defun agent-shell-to-go--extract-file-paths-from-update (update)
  "Extract file paths mentioned in tool call UPDATE."
  (let ((paths nil)
        (raw-input (alist-get 'rawInput update))
        (content (alist-get 'content update)))
    ;; Check rawInput for file_path
    (when-let ((fp (alist-get 'file_path raw-input)))
      (push fp paths))
    ;; Check rawInput for path
    (when-let ((p (alist-get 'path raw-input)))
      (push p paths))
    ;; Check content for paths (in diff items)
    (when content
      (let ((content-list (if (vectorp content) (append content nil)
                            (if (listp content) content nil))))
        (dolist (item content-list)
          (when-let ((p (alist-get 'path item)))
            (push p paths)))))
    paths))

(defun agent-shell-to-go--handle-fswatch-output (buffer output)
  "Handle fswatch OUTPUT, uploading new images for BUFFER.
Only uploads images that were recently mentioned by this buffer's agent."
  (when (buffer-live-p buffer)
    (dolist (file-path (split-string output "\n" t))
      (when (and (agent-shell-to-go--image-file-p file-path)
                 (file-exists-p file-path)
                 (> (file-attribute-size (file-attributes file-path)) 0))
        (with-current-buffer buffer
          ;; Only upload if this file was mentioned by this buffer's agent
          (when (agent-shell-to-go--file-was-mentioned-p file-path)
            ;; Initialize hash table if needed
            (unless agent-shell-to-go--uploaded-images
              (setq agent-shell-to-go--uploaded-images (make-hash-table :test 'equal)))
            ;; Track by path + mtime to detect updates
            (let* ((mtime (file-attribute-modification-time (file-attributes file-path)))
                   (mtime-float (float-time mtime))
                   (prev-mtime (gethash file-path agent-shell-to-go--uploaded-images)))
              ;; Upload if new file or modified since last upload
              (when (or (not prev-mtime)
                        (> mtime-float prev-mtime))
                ;; Check rate limit
                (if (not (agent-shell-to-go--check-upload-rate-limit))
                    (agent-shell-to-go--debug "rate limit exceeded, skipping: %s" file-path)
                  ;; Mark as uploaded with current mtime
                  (puthash file-path mtime-float agent-shell-to-go--uploaded-images)
                  ;; Small delay to ensure file is fully written
                  (run-at-time 0.5 nil
                               (lambda ()
                                 (when (and (buffer-live-p buffer)
                                            (file-exists-p file-path))
                                   (with-current-buffer buffer
                                     (agent-shell-to-go--debug "fswatch uploading: %s" file-path)
                                     (agent-shell-to-go--record-upload)
                                     (agent-shell-to-go--upload-file
                                      file-path
                                      agent-shell-to-go--channel-id
                                      agent-shell-to-go--thread-ts
                                      (format ":frame_with_picture: `%s`"
                                              (file-name-nondirectory file-path))))))))))))))))

(defun agent-shell-to-go--start-file-watcher ()
  "Start fswatch for new image files in the project directory (recursive)."
  (agent-shell-to-go--stop-file-watcher)
  (let* ((project-dir (agent-shell-to-go--get-project-path))
         (buffer (current-buffer)))
    (when (and project-dir (file-directory-p project-dir))
      (if (not (executable-find "fswatch"))
          (agent-shell-to-go--debug "fswatch not found, image watching disabled")
        (condition-case err
            (let ((proc (start-process
                         "agent-shell-to-go-fswatch"
                         nil  ; no buffer
                         "fswatch"
                         "-r"  ; recursive
                         "--event" "Created"
                         "--event" "Updated"
                         project-dir)))
              (set-process-filter
               proc
               (lambda (_proc output)
                 (agent-shell-to-go--handle-fswatch-output buffer output)))
              (set-process-query-on-exit-flag proc nil)
              (setq agent-shell-to-go--file-watcher proc)
              (agent-shell-to-go--debug "started fswatch on %s" project-dir))
          (error
           (agent-shell-to-go--debug "failed to start fswatch: %s" err)))))))

(defun agent-shell-to-go--stop-file-watcher ()
  "Stop fswatch process."
  (when (and agent-shell-to-go--file-watcher
             (process-live-p agent-shell-to-go--file-watcher))
    (ignore-errors (kill-process agent-shell-to-go--file-watcher)))
  (setq agent-shell-to-go--file-watcher nil))

(defun agent-shell-to-go--upload-file (file-path channel-id &optional thread-ts comment)
  "Upload FILE-PATH to Slack CHANNEL-ID, optionally in THREAD-TS with COMMENT.
Uses the new Slack files API (getUploadURLExternal + completeUploadExternal)."
  (when (and file-path (file-exists-p file-path))
    (let* ((filename (file-name-nondirectory file-path))
           (file-size (file-attribute-size (file-attributes file-path)))
           ;; Step 1: Get upload URL
           (url-response (agent-shell-to-go--api-request
                          "GET"
                          (format "files.getUploadURLExternal?filename=%s&length=%d"
                                  (url-hexify-string filename)
                                  file-size)))
           (upload-url (alist-get 'upload_url url-response))
           (file-id (alist-get 'file_id url-response)))
      (when (and upload-url file-id)
        ;; Step 2: Upload the file content via curl
        (let ((upload-result
               (with-temp-buffer
                 (call-process "curl" nil t nil
                               "-s" "-X" "POST"
                               "-F" (format "file=@%s" file-path)
                               upload-url)
                 (buffer-string))))
          (agent-shell-to-go--debug "upload result: %s" upload-result)
          ;; Step 3: Complete the upload and share to channel
          (let* ((files-data `[((id . ,file-id))])
                 (complete-data `((files . ,files-data)
                                  (channel_id . ,channel-id)))
                 (_ (when thread-ts
                      (push `(thread_ts . ,thread-ts) complete-data)))
                 (_ (when comment
                      (push `(initial_comment . ,comment) complete-data)))
                 (complete-response (agent-shell-to-go--api-request
                                     "POST" "files.completeUploadExternal"
                                     complete-data)))
            (agent-shell-to-go--debug "complete upload response: %s" complete-response)
            complete-response))))))

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
  "Handle Slack event PAYLOAD.
All events are gated on `agent-shell-to-go-authorized-users'."
  (let* ((event (alist-get 'event payload))
         (event-type (alist-get 'type event))
         (user (alist-get 'user event))
         (bot-id (alist-get 'bot_id event)))
    ;; Skip bot messages silently (they'll be ignored anyway)
    ;; NOTE: This skips ALL bot messages. If we want agents to message each other
    ;; in the future, we'd need to allowlist specific bot IDs here instead.
    (unless bot-id
      (agent-shell-to-go--debug "received event type: %s from user: %s" event-type user)
      ;; Check authorization for all events
      (if (not (agent-shell-to-go--authorized-p user))
          (agent-shell-to-go--debug "unauthorized user %s, ignoring %s event" user event-type)
        (pcase event-type
          ("message"
           (agent-shell-to-go--handle-message-event event))
          ("reaction_added"
           (agent-shell-to-go--debug "reaction event: %s" event)
           (agent-shell-to-go--handle-reaction-event event))
          ("reaction_removed"
           (agent-shell-to-go--debug "reaction removed event: %s" event)
           (agent-shell-to-go--handle-reaction-removed-event event)))))))

(defvar agent-shell-to-go--processed-message-ts (make-hash-table :test 'equal)
  "Hash table of recently processed message timestamps to prevent duplicates.")

(defun agent-shell-to-go--message-already-processed-p (ts)
  "Return non-nil if message TS was already processed. Marks it as processed."
  (if (gethash ts agent-shell-to-go--processed-message-ts)
      t
    ;; Mark as processed, auto-expire after 60 seconds
    (puthash ts (float-time) agent-shell-to-go--processed-message-ts)
    ;; Clean up old entries (older than 60s)
    (let ((now (float-time)))
      (maphash (lambda (k v)
                 (when (> (- now v) 60)
                   (remhash k agent-shell-to-go--processed-message-ts)))
               agent-shell-to-go--processed-message-ts))
    nil))

(defun agent-shell-to-go--handle-message-event (event)
  "Handle a message EVENT from Slack.
Authorization is checked upstream in `agent-shell-to-go--handle-event'."
  (let* ((thread-ts (alist-get 'thread_ts event))
         (channel (alist-get 'channel event))
         (user (alist-get 'user event))
         (text (alist-get 'text event))
         (msg-ts (alist-get 'ts event))
         (subtype (alist-get 'subtype event))
         (bot-id (alist-get 'bot_id event))
         (buffer (and thread-ts (agent-shell-to-go--find-buffer-for-thread thread-ts channel))))
    (agent-shell-to-go--debug "message event: thread=%s channel=%s text=%s buffer=%s"
                              thread-ts channel text buffer)
    ;; Only handle real user messages in threads we're tracking
    ;; Also deduplicate by message timestamp
    (when (and buffer
               text
               msg-ts
               (not (agent-shell-to-go--message-already-processed-p msg-ts))
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

(defun agent-shell-to-go--save-truncated-message (channel ts full-text &optional collapsed-text)
  "Save FULL-TEXT of truncated message at TS in CHANNEL to disk.
If COLLAPSED-TEXT is provided, also save it for restoration when collapsing."
  (let ((path (agent-shell-to-go--truncated-message-path channel ts)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert full-text)))
  ;; Save collapsed form if provided
  (when collapsed-text
    (let ((collapsed-path (concat (agent-shell-to-go--truncated-message-path channel ts) ".collapsed")))
      (with-temp-file collapsed-path
        (insert collapsed-text)))))

(defun agent-shell-to-go--load-truncated-message (channel ts)
  "Load the full text of truncated message at TS in CHANNEL."
  (let ((path (agent-shell-to-go--truncated-message-path channel ts)))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string)))))

(defun agent-shell-to-go--load-collapsed-message (channel ts)
  "Load the collapsed form of message at TS in CHANNEL."
  (let ((path (concat (agent-shell-to-go--truncated-message-path channel ts) ".collapsed")))
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

(defconst agent-shell-to-go--image-extensions
  '("png" "jpg" "jpeg" "gif" "webp" "bmp" "svg")
  "File extensions recognized as images for upload to Slack.")

(defun agent-shell-to-go--parse-unified-diff (diff-string)
  "Parse unified DIFF-STRING into old and new text.
Returns a cons cell (OLD-TEXT . NEW-TEXT)."
  (let (old-lines new-lines in-hunk)
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
    (cons (string-join (nreverse old-lines) "\n")
          (string-join (nreverse new-lines) "\n"))))

(defun agent-shell-to-go--extract-diff (update)
  "Extract diff info from tool call UPDATE.
Returns (old-text . new-text) or nil if no diff found."
  (let* ((content (alist-get 'content update))
         (raw-input (alist-get 'rawInput update))
         ;; Normalize content to a list for searching
         (content-list (cond
                        ((vectorp content) (append content nil))
                        ((and content (listp content) (not (alist-get 'type content))) content)
                        (content (list content))  ; Single item, wrap in list
                        (t nil))))
    (cond
     ;; Search content list for diff item
     ((and content-list
           (seq-find (lambda (item) (equal (alist-get 'type item) "diff")) content-list))
      (let ((diff-item (seq-find (lambda (item) (equal (alist-get 'type item) "diff")) content-list)))
        (cons (or (alist-get 'oldText diff-item) "")
              (alist-get 'newText diff-item))))
     ;; rawInput with new_str/old_str (for Edit tool)
     ((and raw-input (alist-get 'new_str raw-input))
      (cons (or (alist-get 'old_str raw-input) "")
            (alist-get 'new_str raw-input)))
     ;; rawInput with diff string (Copilot style)
     ((and raw-input (alist-get 'diff raw-input))
      (let ((diff-str (alist-get 'diff raw-input)))
        (agent-shell-to-go--parse-unified-diff diff-str))))))

(defun agent-shell-to-go--format-diff-for-slack (old-text new-text)
  "Format a diff between OLD-TEXT and NEW-TEXT for Slack."
  (let ((old-file (make-temp-file "old"))
        (new-file (make-temp-file "new")))
    (unwind-protect
        (progn
          (with-temp-file old-file (insert (or old-text "")))
          (with-temp-file new-file (insert (or new-text "")))
          (with-temp-buffer
            (call-process "diff" nil t nil "-U3" old-file new-file)
            ;; Remove file header lines
            (goto-char (point-min))
            (when (looking-at "^---")
              (delete-region (point) (progn (forward-line 1) (point))))
            (when (looking-at "^\\+\\+\\+")
              (delete-region (point) (progn (forward-line 1) (point))))
            (buffer-string)))
      (delete-file old-file)
      (delete-file new-file))))

(defconst agent-shell-to-go--truncated-view-length 500
  "Length for truncated view (👀 glance).")

(defun agent-shell-to-go--expand-message (channel ts)
  "Expand message at TS in CHANNEL to truncated view (👀 glance).
Shows first ~500 chars of the full output."
  (let ((full-text (agent-shell-to-go--load-truncated-message channel ts)))
    (when full-text
      (let* ((too-long (> (length full-text) agent-shell-to-go--truncated-view-length))
             (display-text (if too-long
                               (concat (substring full-text 0 agent-shell-to-go--truncated-view-length)
                                       "\n_... 📖 for full output_")
                             full-text)))
        (agent-shell-to-go--api-request
         "POST" "chat.update"
         `((channel . ,channel)
           (ts . ,ts)
           (text . ,display-text)))))))

(defun agent-shell-to-go--full-expand-message (channel ts)
  "Fully expand message at TS in CHANNEL (📖 read everything).
Shows full output up to Slack's limit."
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

(defun agent-shell-to-go--handle-bookmark-reaction (channel ts)
  "Handle bookmark reaction on message at TS in CHANNEL.
Creates an org TODO file with the message content."
  ;; Try to find the buffer for this channel to get thread context
  (let* ((buffer (agent-shell-to-go--find-buffer-for-channel channel))
         (thread-ts (and buffer (buffer-local-value 'agent-shell-to-go--thread-ts buffer)))
         ;; For threaded messages, ts might be different from thread-ts
         ;; Try to get message with thread context if available
         (message-text (or (and thread-ts (agent-shell-to-go--get-message-text channel ts thread-ts))
                           (agent-shell-to-go--get-message-text channel ts nil)))
         (project-name (or (and buffer
                                (with-current-buffer buffer
                                  (file-name-nondirectory
                                   (directory-file-name default-directory))))
                           ;; Fallback: get project from channel mapping
                           (let ((project-path (agent-shell-to-go--get-project-for-channel channel)))
                             (and project-path
                                  (file-name-nondirectory
                                   (directory-file-name project-path))))))
         (today (format-time-string "%Y-%m-%d"))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (todo-dir (expand-file-name agent-shell-to-go-todo-directory))
         (todo-file (expand-file-name (format "%s-%s.org" (or project-name "slack") timestamp) todo-dir))
         ;; Truncate message for title (first line, max 60 chars)
         (title-text (if message-text
                         (let ((first-line (car (split-string message-text "\n" t))))
                           (if (> (length first-line) 60)
                               (concat (substring first-line 0 57) "...")
                             first-line))
                       "Bookmarked message")))
    (when message-text
      ;; Ensure directory exists
      (make-directory todo-dir t)
      ;; Write org file
      (with-temp-file todo-file
        (insert (format "* TODO %s\n" title-text))
        (insert (format "SCHEDULED: <%s>\n\n" today))
        (insert (format "Project: %s\n\n" (or project-name "unknown")))
        (insert "** Message\n")
        (insert message-text)
        (insert "\n"))
      ;; Notify in Slack - try to reply in thread if we know it
      (agent-shell-to-go--send
       (format ":bookmark: TODO created: `%s`" (file-name-nondirectory todo-file))
       (or thread-ts ts)
       channel))))

(defun agent-shell-to-go--collapse-message (channel ts)
  "Re-truncate expanded message at TS in CHANNEL.
First tries to restore the saved collapsed form (e.g. status icon).
Falls back to truncating the full text if no collapsed form was saved."
  (let* ((collapsed (agent-shell-to-go--load-collapsed-message channel ts))
         (full-text (agent-shell-to-go--load-truncated-message channel ts))
         (restore-text (or collapsed
                           (and full-text
                                (agent-shell-to-go--truncate-message full-text 500)))))
    (agent-shell-to-go--debug "collapse-message: ts=%s collapsed=%s full-text=%s"
                              ts (and collapsed (substring collapsed 0 (min 50 (length collapsed))))
                              (and full-text (substring full-text 0 (min 50 (length full-text)))))
    (when restore-text
      (agent-shell-to-go--api-request
       "POST" "chat.update"
       `((channel . ,channel)
         (ts . ,ts)
         (text . ,restore-text))))))

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
      (agent-shell-to-go--collapse-message channel msg-ts))
    ;; Check if it was a full-expand reaction being removed (re-truncate)
    (when (member reaction agent-shell-to-go--full-expand-reactions)
      (agent-shell-to-go--collapse-message channel msg-ts))))

(defun agent-shell-to-go--handle-reaction-event (event)
  "Handle a reaction EVENT from Slack.
Authorization is checked upstream in `agent-shell-to-go--handle-event'."
  (let* ((item (alist-get 'item event))
         (msg-ts (alist-get 'ts item))
         (channel (alist-get 'channel item))
         (reaction (alist-get 'reaction event))
         (pending (assoc msg-ts agent-shell-to-go--pending-permissions)))
    ;; Check for hide reactions first
    (when (member reaction agent-shell-to-go--hide-reactions)
      (agent-shell-to-go--hide-message channel msg-ts))
    ;; Check for expand reactions (show truncated glance view)
    (when (member reaction agent-shell-to-go--expand-reactions)
      (agent-shell-to-go--expand-message channel msg-ts))
    ;; Check for full-expand reactions (show full output)
    (when (member reaction agent-shell-to-go--full-expand-reactions)
      (agent-shell-to-go--full-expand-message channel msg-ts))
    ;; Check for heart reactions (send appreciation to agent)
    (when (member reaction agent-shell-to-go--heart-reactions)
      (agent-shell-to-go--debug "heart reaction: %s on %s" reaction msg-ts)
      (agent-shell-to-go--handle-heart-reaction channel msg-ts))
    ;; Check for bookmark reactions (create TODO)
    (when (member reaction agent-shell-to-go--bookmark-reactions)
      (agent-shell-to-go--debug "bookmark reaction: %s on %s" reaction msg-ts)
      (agent-shell-to-go--handle-bookmark-reaction channel msg-ts))
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

(defcustom agent-shell-to-go-projects-directory "~/code/"
  "Directory where /new-project creates new project folders."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-new-project-function nil
  "Function to call to set up a new project.
Called with (PROJECT-NAME BASE-DIR CALLBACK).
CALLBACK is called with PROJECT-DIR when setup is complete.
If nil, just creates the directory and starts the agent immediately."
  :type '(choice (const :tag "Just create directory" nil)
          (function :tag "Custom setup function"))
  :group 'agent-shell-to-go)

(defun agent-shell-to-go--handle-slash-command (payload)
  "Handle a slash command PAYLOAD from Slack."
  (let* ((command (alist-get 'command payload))
         (text (alist-get 'text payload))
         (channel (alist-get 'channel_id payload))
         (user (alist-get 'user_id payload))
         (channel-project (agent-shell-to-go--get-project-for-channel channel))
         (folder (expand-file-name
                  (cond
                   ;; Explicit folder argument takes priority
                   ((and text (not (string-empty-p text))) text)
                   ;; Use channel's project if available
                   (channel-project channel-project)
                   ;; Fall back to default
                   (t agent-shell-to-go-default-folder)))))
    (agent-shell-to-go--debug "slash command: %s %s (channel project: %s, user: %s)" command text channel-project user)
    ;; Check authorization
    (if (not (agent-shell-to-go--authorized-p user))
        (agent-shell-to-go--api-request
         "POST" "chat.postEphemeral"
         `((channel . ,channel)
           (user . ,user)
           (text . ":no_entry: You are not authorized to use this command.")))
      (pcase command
        ("/new-project"
         (if (or (not text) (string-empty-p text))
             (agent-shell-to-go--api-request
              "POST" "chat.postMessage"
              `((channel . ,channel)
                (text . ":x: Usage: `/new-project <project-name>`")))
           (let* ((project-name (string-trim text))
                  (project-dir (expand-file-name project-name agent-shell-to-go-projects-directory)))
             (if (file-exists-p project-dir)
                 (agent-shell-to-go--api-request
                  "POST" "chat.postMessage"
                  `((channel . ,channel)
                    (text . ,(format ":warning: Project already exists: `%s`" project-dir))))
               (agent-shell-to-go--api-request
                "POST" "chat.postMessage"
                `((channel . ,channel)
                  (text . ,(format ":file_folder: Creating project: `%s`" project-dir))))
               ;; Set up project and start agent when done
               (let ((start-agent-fn
                      (lambda (final-project-dir)
                        (agent-shell-to-go--api-request
                         "POST" "chat.postMessage"
                         `((channel . ,channel)
                           (text . ":rocket: Starting Claude Code...")))
                        (agent-shell-to-go--start-agent-in-folder final-project-dir nil))))
                 (if agent-shell-to-go-new-project-function
                     ;; Use custom setup function (PROJECT-NAME BASE-DIR CALLBACK)
                     (funcall agent-shell-to-go-new-project-function
                              project-name
                              (expand-file-name agent-shell-to-go-projects-directory)
                              start-agent-fn)
                   ;; No setup function, just create directory and start the agent
                   (make-directory project-dir t)
                   (funcall start-agent-fn project-dir)))))))
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
                (text . ":shrug: No open projects found"))))))))))

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
         agent-shell-to-go--thread-ts)
        ;; Send processing indicator immediately after user message
        (agent-shell-to-go--send
         ":hourglass_flowing_sand: _Processing..._"
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
                 (setq agent-shell-to-go--current-agent-message nil))
               ;; Record any file paths mentioned for image upload filtering
               (dolist (path (agent-shell-to-go--extract-file-paths-from-update update))
                 (agent-shell-to-go--record-mentioned-file path))
               ;; Initialize tool-calls hash if needed
               (unless agent-shell-to-go--tool-calls
                 (setq agent-shell-to-go--tool-calls (make-hash-table :test 'equal))))
             (let* ((tool-call-id (alist-get 'toolCallId update))
                    (title (alist-get 'title update))
                    (raw-input (alist-get 'rawInput update))
                    (command (alist-get 'command raw-input))
                    (file-path (alist-get 'file_path raw-input))
                    (query (alist-get 'query raw-input))  ; For WebSearch
                    (url (alist-get 'url raw-input))      ; For WebFetch
                    ;; Build display - title often already contains file path
                    (specific-info (or command file-path query url))
                    ;; Check if title already contains the specific info
                    (title-has-info (and title specific-info
                                         (string-match-p (regexp-quote specific-info) title)))
                    (display (cond
                              (command command)  ; Commands are self-explanatory
                              ;; If title already has file path, just use title
                              (title-has-info title)
                              ;; Otherwise prefix with tool name
                              ((and file-path title) (format "%s: %s" title file-path))
                              ((and query title) (format "%s: %s" title query))
                              ((and url title) (format "%s: %s" title url))
                              (specific-info specific-info)
                              (t title)))
                    ;; Extract diff if present
                    (diff (agent-shell-to-go--extract-diff update))
                    (diff-text (and diff
                                    (agent-shell-to-go--format-diff-for-slack
                                     (car diff) (cdr diff))))
                    ;; Check if we already sent for this tool call
                    (already-sent (and tool-call-id
                                       (with-current-buffer buffer
                                         (gethash tool-call-id agent-shell-to-go--tool-calls)))))
               ;; Only send if we have specific info (not just generic title) and haven't sent yet
               (when (and specific-info (not already-sent))
                 (with-current-buffer buffer
                   (puthash tool-call-id t agent-shell-to-go--tool-calls))
                 (condition-case err
                     (if (and diff-text (> (length diff-text) 0))
                         ;; Show file path with diff
                         (agent-shell-to-go--send
                          (format ":hourglass: `%s`\n```diff\n%s\n```" display diff-text)
                          thread-ts nil t)
                       ;; No diff, just show command/title
                       (agent-shell-to-go--send
                        (format ":hourglass: `%s`" display)
                        thread-ts nil t))
                   (error
                    (agent-shell-to-go--debug "tool_call error: %s" err)
                    (agent-shell-to-go--send
                     (format ":hourglass: `%s`" display)
                     thread-ts nil t))))
               ;; If we only have generic title and haven't sent, don't send yet
               ;; (wait for update with more info)
               ))  ; truncate=t
            ("tool_call_update"
             ;; Tool call completed - show output and/or diff
             ;; Record any file paths mentioned for image upload filtering
             (with-current-buffer buffer
               (dolist (path (agent-shell-to-go--extract-file-paths-from-update update))
                 (agent-shell-to-go--record-mentioned-file path)))
             (let* ((status (alist-get 'status update))
                    (content (alist-get 'content update))
                    ;; Extract text from content array - try both structures
                    (content-text (and content
                                       (mapconcat
                                        (lambda (item)
                                          ;; Try nested .content.text first, then direct .text
                                          (or (alist-get 'text (alist-get 'content item))
                                              (alist-get 'text item)
                                              ""))
                                        (if (vectorp content) (append content nil) 
                                          (if (listp content) content nil))
                                        "\n")))
                    (output (or (alist-get 'rawOutput update)
                                (alist-get 'output update)
                                content-text))
                    ;; Try to extract diff
                    (diff (condition-case nil
                              (agent-shell-to-go--extract-diff update)
                            (error nil)))
                    (diff-text (and diff
                                    (condition-case nil
                                        (agent-shell-to-go--format-diff-for-slack
                                         (car diff) (cdr diff))
                                      (error nil)))))
               (when (member status '("completed" "failed"))
                 (let ((status-icon (if (equal status "completed") ":white_check_mark:" ":x:")))
                   (cond
                    ;; Has diff - show diff
                    ((and diff-text (> (length diff-text) 0))
                     (let ((full-text (format "%s\n```diff\n%s\n```" status-icon diff-text)))
                       (if agent-shell-to-go-show-tool-output
                           (agent-shell-to-go--send full-text thread-ts nil t)
                         ;; Hidden mode: show just icon, save full for expansion
                         (let* ((response (agent-shell-to-go--send status-icon thread-ts))
                                (msg-ts (alist-get 'ts response)))
                           (when msg-ts
                             (with-current-buffer buffer
                               (agent-shell-to-go--save-truncated-message
                                agent-shell-to-go--channel-id msg-ts full-text status-icon)))))))
                    ;; Has output - show output
                    ((and output (stringp output) (> (length output) 0))
                     (let ((full-text (format "%s\n```\n%s\n```" status-icon output)))
                       (if agent-shell-to-go-show-tool-output
                           (agent-shell-to-go--send full-text thread-ts nil t)
                         ;; Hidden mode: show just icon, save full for expansion
                         (let* ((response (agent-shell-to-go--send status-icon thread-ts))
                                (msg-ts (alist-get 'ts response)))
                           (when msg-ts
                             (with-current-buffer buffer
                               (agent-shell-to-go--save-truncated-message
                                agent-shell-to-go--channel-id msg-ts full-text status-icon)))))))
                    ;; Neither - just show status
                    (t
                     (agent-shell-to-go--send status-icon thread-ts))))))))))))  ; truncate=t
  (apply orig-fn args))

(defun agent-shell-to-go--on-heartbeat-stop (orig-fn &rest args)
  "Advice for agent-shell-heartbeat-stop. Flush agent message and send ready indicator.
ORIG-FN is the original function, ARGS are its arguments."
  (when (and agent-shell-to-go-mode
             agent-shell-to-go--thread-ts)
    ;; Flush any pending agent message
    (when (and agent-shell-to-go--current-agent-message
               (> (length agent-shell-to-go--current-agent-message) 0))
      (agent-shell-to-go--send
       (agent-shell-to-go--format-agent-message agent-shell-to-go--current-agent-message)
       agent-shell-to-go--thread-ts)
      (setq agent-shell-to-go--current-agent-message nil))
    ;; Send ready for input indicator
    (agent-shell-to-go--send ":speech_balloon: _Ready for input_" agent-shell-to-go--thread-ts))
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
                "`!stop` - Interrupt the agent\n"
                "`!restart` - Kill and restart agent with transcript\n"
                "`!queue` - Show pending queued messages\n"
                "`!clearqueue` - Clear all pending queued messages\n"
                "`!latest` - Jump to bottom of thread")
        thread-ts)
       t)
      ("!queue"
       (with-current-buffer buffer
         (let ((pending (map-elt agent-shell--state :pending-requests)))
           (if (seq-empty-p pending)
               (agent-shell-to-go--send ":inbox_tray: No pending requests" thread-ts)
             (agent-shell-to-go--send
              (format ":inbox_tray: *Pending requests (%d):*\n%s"
                      (length pending)
                      (mapconcat
                       (lambda (req)
                         (format "• %s"
                                 (agent-shell-to-go--truncate-message req 80)))
                       pending
                       "\n"))
              thread-ts))))
       t)
      ("!clearqueue"
       (with-current-buffer buffer
         (let ((count (length (map-elt agent-shell--state :pending-requests))))
           (map-put! agent-shell--state :pending-requests nil)
           (agent-shell-to-go--send
            (format ":wastebasket: Cleared %d pending request%s"
                    count (if (= count 1) "" "s"))
            thread-ts)))
       t)
      ("!latest"
       (agent-shell-to-go--send ":point_down:" thread-ts)
       t)
      ("!debug"
       (with-current-buffer buffer
         (let* ((state agent-shell--state)
                (session-id (map-nested-elt state '(:session :id)))
                (mode-id (map-nested-elt state '(:session :mode-id)))
                (transcript-dir (expand-file-name "transcripts" 
                                                  (or (bound-and-true-p agent-shell-sessions-dir)
                                                      "~/.agent-shell")))
                (transcript-files (and (file-directory-p transcript-dir)
                                       (directory-files transcript-dir nil "\\.md$" t)))
                (latest-transcript (and transcript-files
                                        (car (last (sort transcript-files #'string<)))))
                (truncated-dir (expand-file-name 
                                (buffer-local-value 'agent-shell-to-go--channel-id buffer)
                                agent-shell-to-go-truncated-messages-dir))
                (truncated-count (if (file-directory-p truncated-dir)
                                     (length (directory-files truncated-dir nil "\\.txt$"))
                                   0)))
           (agent-shell-to-go--send
            (format (concat ":bug: *Debug Info*\n"
                            "*Buffer:* `%s`\n"
                            "*Thread:* `%s`\n"
                            "*Channel:* `%s`\n"
                            "*Session ID:* `%s`\n"
                            "*Mode:* `%s`\n"
                            "*Transcript:* `%s`\n"
                            "*Truncated msgs:* %d files in `%s`")
                    (buffer-name buffer)
                    thread-ts
                    agent-shell-to-go--channel-id
                    (or session-id "none")
                    (or mode-id "default")
                    (or latest-transcript "none")
                    truncated-count
                    truncated-dir)
            thread-ts)))
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
      ("!restart"
       (condition-case err
           (with-current-buffer buffer
             (let* ((project-dir default-directory)
                    (state agent-shell--state)
                    (session-id (map-nested-elt state '(:session :id)))
                    (transcript-dir (expand-file-name "transcripts"
                                                      (or (bound-and-true-p agent-shell-sessions-dir)
                                                          "~/.agent-shell")))
                    (transcript-file (and session-id
                                          (expand-file-name (concat session-id ".md")
                                                            transcript-dir)))
                    (transcript-exists (and transcript-file (file-exists-p transcript-file))))
               ;; Notify about restart
               (agent-shell-to-go--send ":arrows_counterclockwise: Restarting agent..." thread-ts)
               ;; Kill current agent
               (ignore-errors (agent-shell-interrupt t))
               ;; Start new agent in same directory
               (run-at-time 1 nil
                            (lambda ()
                              (let ((default-directory project-dir))
                                (save-window-excursion
                                  (funcall agent-shell-to-go-start-agent-function nil)
                                  ;; If transcript exists, tell the new agent about it
                                  (when transcript-exists
                                    (run-at-time 2 nil
                                                 (lambda ()
                                                   (when-let ((new-buf (car agent-shell-to-go--active-buffers)))
                                                     (with-current-buffer new-buf
                                                       (agent-shell-to-go--inject-message
                                                        (format "Continue from previous session. Transcript at: %s"
                                                                transcript-file)))))))))))))
         (error
          (agent-shell-to-go--debug "!restart error: %s" err)
          (agent-shell-to-go--send (format ":x: Restart failed: %s" err) thread-ts)))
       t)
      (_ nil))))

(defun agent-shell-to-go--inject-message (text)
  "Inject TEXT from Slack into the current agent-shell buffer.
If the shell is busy, queue the message for later processing."
  (when (derived-mode-p 'agent-shell-mode)
    (if (shell-maker-busy)
        ;; Shell is busy - queue the request
        (progn
          (agent-shell--enqueue-request :prompt text)
          (agent-shell-to-go--send
           (format ":hourglass: _Queued: %s_"
                   (agent-shell-to-go--truncate-message text 100))
           agent-shell-to-go--thread-ts))
      ;; Shell is ready - inject immediately
      ;; Set flag - it will be cleared by the send-command advice after it skips posting
      (setq agent-shell-to-go--from-slack t)
      (save-excursion
        (goto-char (point-max))
        (insert text))
      (goto-char (point-max))
      (call-interactively #'shell-maker-submit))))

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

  ;; Start file watcher for auto-uploading images
  (agent-shell-to-go--start-file-watcher)

  ;; Add kill-buffer hook to send shutdown message
  (add-hook 'kill-buffer-hook #'agent-shell-to-go--on-buffer-kill nil t)

  (agent-shell-to-go--debug "mirroring to Slack thread %s" agent-shell-to-go--thread-ts))

(defun agent-shell-to-go--on-buffer-kill ()
  "Hook to run when an agent-shell buffer is killed."
  (when agent-shell-to-go-mode
    (agent-shell-to-go--disable)))

(defun agent-shell-to-go--disable ()
  "Disable Slack mirroring for this buffer."
  ;; Remove kill hook to avoid double-firing
  (remove-hook 'kill-buffer-hook #'agent-shell-to-go--on-buffer-kill t)

  ;; Stop file watcher
  (agent-shell-to-go--stop-file-watcher)

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

;;;###autoload
(defun agent-shell-to-go-reconnect-buffer (&optional buffer)
  "Reconnect BUFFER (or current buffer) to Slack with a fresh thread.
Always creates a new Slack thread to ensure clean state.
Use this when a buffer's Slack connection is broken."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (unless (buffer-live-p buf)
      (user-error "Buffer is not live"))
    (with-current-buffer buf
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      ;; Load credentials if needed
      (agent-shell-to-go--load-env)
      (agent-shell-to-go--load-channels)
      ;; Get or create channel
      (setq agent-shell-to-go--channel-id
            (agent-shell-to-go--get-or-create-project-channel))
      ;; Always create a fresh thread
      (setq agent-shell-to-go--thread-ts
            (agent-shell-to-go--start-thread (buffer-name)))
      ;; Ensure buffer is tracked
      (unless (memq buf agent-shell-to-go--active-buffers)
        (add-to-list 'agent-shell-to-go--active-buffers buf))
      ;; Ensure WebSocket is connected
      (unless (and agent-shell-to-go--websocket
                   (websocket-openp agent-shell-to-go--websocket))
        (agent-shell-to-go--websocket-connect))
      ;; Restart file watcher (stops existing one if any)
      (agent-shell-to-go--start-file-watcher)
      ;; Ensure advice is set up
      (advice-add 'agent-shell--send-command :around #'agent-shell-to-go--on-send-command)
      (advice-add 'agent-shell--on-notification :around #'agent-shell-to-go--on-notification)
      (advice-add 'agent-shell--on-request :around #'agent-shell-to-go--on-request)
      (advice-add 'agent-shell-heartbeat-stop :around #'agent-shell-to-go--on-heartbeat-stop)
      ;; Ensure kill hook
      (add-hook 'kill-buffer-hook #'agent-shell-to-go--on-buffer-kill nil t)
      ;; Enable the mode if not already
      (unless agent-shell-to-go-mode
        (setq agent-shell-to-go-mode t))
      (message "Reconnected %s to Slack (new thread)" (buffer-name buf)))))

(defun agent-shell-to-go--buffer-connected-p (&optional buffer)
  "Return non-nil if BUFFER (or current buffer) has a valid Slack connection."
  (let ((buf (or buffer (current-buffer))))
    (and (buffer-live-p buf)
         (buffer-local-value 'agent-shell-to-go--thread-ts buf)
         (buffer-local-value 'agent-shell-to-go--channel-id buf)
         (memq buf agent-shell-to-go--active-buffers))))

;;;###autoload
(defun agent-shell-to-go-ensure-connected (&optional buffer)
  "Ensure BUFFER (or current buffer) is connected to Slack.
Only connects if not already connected (idempotent).
Returns t if was already connected, 'connected if newly connected, nil on failure."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (if (agent-shell-to-go--buffer-connected-p buf)
        t
      (condition-case err
          (progn
            (agent-shell-to-go-reconnect-buffer buf)
            'connected)
        (error
         (message "Failed to connect %s: %s" (buffer-name buf) err)
         nil)))))

;;;###autoload
(defun agent-shell-to-go--websocket-healthy-p ()
  "Return non-nil if the websocket connection is healthy."
  (and agent-shell-to-go--websocket
       (websocket-openp agent-shell-to-go--websocket)))

(defun agent-shell-to-go-ensure-all-connected ()
  "Ensure all agent-shell buffers are connected to Slack.
Also checks websocket health and reconnects if needed.
Only connects buffers that aren't already connected (idempotent)."
  (interactive)
  ;; First ensure websocket is healthy
  (unless (agent-shell-to-go--websocket-healthy-p)
    (message "Slack websocket unhealthy, reconnecting...")
    (agent-shell-to-go--websocket-connect))
  ;; Then check all buffers
  (let ((connected 0)
        (already 0)
        (failed 0)
        (agent-buffers (cl-remove-if-not
                        (lambda (buf)
                          (and (buffer-live-p buf)
                               (with-current-buffer buf
                                 (derived-mode-p 'agent-shell-mode))))
                        (buffer-list))))
    (dolist (buf agent-buffers)
      (pcase (agent-shell-to-go-ensure-connected buf)
        ('t (cl-incf already))
        ('connected (cl-incf connected))
        (_ (cl-incf failed))))
    (when (or (> connected 0) (> failed 0))
      (message "Slack: %d newly connected, %d already connected, %d failed"
               connected already failed))))

(defvar agent-shell-to-go--ensure-timer nil
  "Timer for periodic connection checks.")

;;;###autoload
(defun agent-shell-to-go-start-periodic-check (&optional interval)
  "Start periodic check to ensure all buffers stay connected.
INTERVAL is seconds between checks (default 60)."
  (interactive)
  (agent-shell-to-go-stop-periodic-check)
  (setq agent-shell-to-go--ensure-timer
        (run-with-timer 0 (or interval 60) #'agent-shell-to-go-ensure-all-connected))
  (message "Started periodic Slack connection check (every %ds)" (or interval 60)))

;;;###autoload
(defun agent-shell-to-go-stop-periodic-check ()
  "Stop periodic connection checks."
  (interactive)
  (when agent-shell-to-go--ensure-timer
    (cancel-timer agent-shell-to-go--ensure-timer)
    (setq agent-shell-to-go--ensure-timer nil)
    (message "Stopped periodic Slack connection check")))

;;;###autoload
(defun agent-shell-to-go-reconnect-all ()
  "Reconnect all agent-shell buffers to Slack (creates new threads).
Use `agent-shell-to-go-ensure-all-connected' for idempotent connection."
  (interactive)
  (let ((reconnected 0)
        (agent-buffers (cl-remove-if-not
                        (lambda (buf)
                          (and (buffer-live-p buf)
                               (with-current-buffer buf
                                 (derived-mode-p 'agent-shell-mode))))
                        (buffer-list))))
    (dolist (buf agent-buffers)
      (condition-case err
          (progn
            (agent-shell-to-go-reconnect-buffer buf)
            (cl-incf reconnected))
        (error
         (message "Failed to reconnect %s: %s" (buffer-name buf) err))))
    (message "Reconnected %d/%d agent-shell buffers"
             reconnected (length agent-buffers))))

;;;###autoload
(defun agent-shell-to-go-send-image (file-path &optional comment buffer)
  "Send image at FILE-PATH to an agent-shell Slack thread.
With optional COMMENT to display with the image.
BUFFER specifies which agent-shell buffer to use (defaults to current or most recent).
This is useful for sending images that are outside the project directory."
  (interactive
   (let* ((file (read-file-name "Image file: " nil nil t nil
                                (lambda (f) (or (file-directory-p f)
                                                (agent-shell-to-go--image-file-p f)))))
          (cmt (read-string "Comment (optional): "))
          ;; If multiple buffers, ask which one
          (buf (if (and (not (derived-mode-p 'agent-shell-mode))
                        (> (length agent-shell-to-go--active-buffers) 1))
                   (get-buffer
                    (completing-read "Send to buffer: "
                                     (mapcar #'buffer-name agent-shell-to-go--active-buffers)
                                     nil t))
                 nil)))
     (list file cmt buf)))
  (let ((buffer (or buffer
                    (and (derived-mode-p 'agent-shell-mode)
                         (buffer-local-value 'agent-shell-to-go-mode (current-buffer))
                         (current-buffer))
                    ;; Find most recent buffer by checking if current window has one
                    (cl-find-if (lambda (b)
                                  (and (buffer-live-p b)
                                       (get-buffer-window b)))
                                agent-shell-to-go--active-buffers)
                    (car agent-shell-to-go--active-buffers))))
    (unless buffer
      (user-error "No active agent-shell-to-go session"))
    (unless (file-exists-p file-path)
      (user-error "File does not exist: %s" file-path))
    (unless (agent-shell-to-go--image-file-p file-path)
      (user-error "Not an image file: %s" file-path))
    (with-current-buffer buffer
      (unless agent-shell-to-go--thread-ts
        (user-error "No Slack thread for this session"))
      (let ((comment-text (if (and comment (not (string-empty-p comment)))
                              comment
                            (format ":frame_with_picture: `%s`"
                                    (file-name-nondirectory file-path)))))
        (agent-shell-to-go--upload-file
         (expand-file-name file-path)
         agent-shell-to-go--channel-id
         agent-shell-to-go--thread-ts
         comment-text)
        (message "Sent image to Slack: %s" (file-name-nondirectory file-path))))))

;;; Cleanup functions
;;
;; NOTE: Slack doesn't support bulk message deletion. The only "fast" option
;; would be archiving channels, but bot tokens cannot unarchive channels
;; (requires user token), which breaks the workflow. So we're stuck with
;; deleting messages one by one, which is slow but reliable.
;;
;; If you hit Slack's free tier message limit, consider upgrading to a paid
;; workspace.

(defun agent-shell-to-go--get-channel-messages (channel-id &optional limit)
  "Get recent messages from CHANNEL-ID.
LIMIT defaults to 200. Returns list of messages."
  (let* ((response (agent-shell-to-go--api-request
                    "GET"
                    (format "conversations.history?channel=%s&limit=%d"
                            channel-id (or limit 200))))
         (messages (alist-get 'messages response)))
    (when messages
      (append messages nil))))

(defun agent-shell-to-go--get-thread-replies (channel-id thread-ts)
  "Get replies in thread THREAD-TS from CHANNEL-ID."
  (let* ((response (agent-shell-to-go--api-request
                    "GET"
                    (format "conversations.replies?channel=%s&ts=%s&limit=1"
                            channel-id thread-ts)))
         (messages (alist-get 'messages response)))
    (when messages
      (append messages nil))))

(defun agent-shell-to-go--delete-message (channel-id ts)
  "Delete message at TS in CHANNEL-ID."
  (agent-shell-to-go--api-request
   "POST" "chat.delete"
   `((channel . ,channel-id)
     (ts . ,ts))))

(defun agent-shell-to-go--thread-active-p (thread-ts)
  "Return non-nil if THREAD-TS belongs to an active buffer."
  (cl-some (lambda (buf)
             (and (buffer-live-p buf)
                  (equal thread-ts
                         (buffer-local-value 'agent-shell-to-go--thread-ts buf))))
           agent-shell-to-go--active-buffers))

(defun agent-shell-to-go--get-agent-shell-threads (channel-id)
  "Get all Agent Shell Session threads from CHANNEL-ID.
Returns list of (thread-ts . last-activity-time) for each thread."
  (let ((messages (agent-shell-to-go--get-channel-messages channel-id 500))
        (threads nil))
    (dolist (msg messages)
      (let ((text (alist-get 'text msg))
            (ts (alist-get 'ts msg))
            (reply-count (alist-get 'reply_count msg))
            (latest-reply (alist-get 'latest_reply msg)))
        ;; Match our session start messages
        (when (and text (string-match-p "Agent Shell Session" text))
          (let ((last-activity (if latest-reply
                                   (string-to-number latest-reply)
                                 (string-to-number ts))))
            (push (cons ts last-activity) threads)))))
    threads))

(defun agent-shell-to-go--delete-thread (channel-id thread-ts)
  "Delete all messages in thread THREAD-TS from CHANNEL-ID.
Returns count of deleted messages."
  (let ((deleted 0)
        (cursor nil)
        (continue t))
    ;; Get all replies in the thread
    (while continue
      (let* ((endpoint (format "conversations.replies?channel=%s&ts=%s&limit=200%s"
                               channel-id thread-ts
                               (if cursor (format "&cursor=%s" cursor) "")))
             (response (agent-shell-to-go--api-request "GET" endpoint))
             (messages (alist-get 'messages response))
             (metadata (alist-get 'response_metadata response)))
        (when messages
          ;; Delete each message (in reverse to delete replies before parent)
          (dolist (msg (reverse (append messages nil)))
            (let ((msg-ts (alist-get 'ts msg)))
              (agent-shell-to-go--delete-message channel-id msg-ts)
              (cl-incf deleted))))
        ;; Check for pagination
        (setq cursor (alist-get 'next_cursor metadata))
        (setq continue (and cursor (not (string-empty-p cursor))))))
    deleted))

(defun agent-shell-to-go--cleanup-async (channel-id thread-timestamps)
  "Delete THREAD-TIMESTAMPS from CHANNEL-ID asynchronously.
Spawns a subprocess to do the deletion without blocking Emacs.
Uses parallel requests for speed."
  (let* ((token agent-shell-to-go-bot-token)
         (script (format "
TOKEN='%s'
CHANNEL='%s'

delete_msg() {
  curl -s -X POST 'https://slack.com/api/chat.delete' \
    -H \"Authorization: Bearer $TOKEN\" \
    -H 'Content-Type: application/json' \
    -d \"{\\\"channel\\\":\\\"$CHANNEL\\\",\\\"ts\\\":\\\"$1\\\"}\" > /dev/null
  echo \"Deleted $1\"
}
export -f delete_msg
export TOKEN CHANNEL

for ts in %s; do
  echo \"Processing thread $ts...\"
  cursor=''
  while true; do
    if [ -n \"$cursor\" ]; then
      response=$(curl -s \"https://slack.com/api/conversations.replies?channel=$CHANNEL&ts=$ts&limit=200&cursor=$cursor\" -H \"Authorization: Bearer $TOKEN\")
    else
      response=$(curl -s \"https://slack.com/api/conversations.replies?channel=$CHANNEL&ts=$ts&limit=200\" -H \"Authorization: Bearer $TOKEN\")
    fi
    
    # Extract and delete in parallel (10 at a time)
    echo \"$response\" | grep -o '\"ts\":\"[0-9.]*\"' | sed 's/\"ts\":\"//;s/\"//' | xargs -P 10 -I {} bash -c 'delete_msg \"{}\"'
    
    cursor=$(echo \"$response\" | grep -o '\"next_cursor\":\"[^\"]*\"' | sed 's/\"next_cursor\":\"//;s/\"//' | head -1)
    if [ -z \"$cursor\" ]; then
      break
    fi
  done
  echo \"Finished thread $ts\"
done
echo \"Cleanup complete\"
"
                         token channel-id
                         (mapconcat #'identity thread-timestamps " "))))
    (let ((proc (start-process-shell-command
                 "agent-shell-cleanup"
                 "*Agent Shell Cleanup*"
                 script)))
      (message "Cleanup started in background. See *Agent Shell Cleanup* buffer for progress.")
      proc)))

;;;###autoload
(defun agent-shell-to-go-list-threads (&optional channel-id)
  "List all Agent Shell threads in CHANNEL-ID with their age.
CHANNEL-ID defaults to `agent-shell-to-go-channel-id'."
  (interactive)
  (agent-shell-to-go--load-env)
  (let* ((channel (or channel-id agent-shell-to-go-channel-id))
         (threads (agent-shell-to-go--get-agent-shell-threads channel))
         (now (float-time)))
    (if (not threads)
        (message "No Agent Shell threads found")
      (with-current-buffer (get-buffer-create "*Agent Shell Threads*")
        (erase-buffer)
        (insert (format "Agent Shell Threads in channel %s\n" channel))
        (insert (make-string 60 ?-) "\n\n")
        (dolist (thread (sort threads (lambda (a b) (> (cdr a) (cdr b)))))
          (let* ((ts (car thread))
                 (last-activity (cdr thread))
                 (age-hours (/ (- now last-activity) 3600.0))
                 (active (agent-shell-to-go--thread-active-p ts))
                 (status (cond
                          (active "[ACTIVE]")
                          ((< age-hours agent-shell-to-go-cleanup-age-hours) "[recent]")
                          (t "[old]"))))
            (insert (format "%s %s  %.1fh ago  %s\n"
                            status
                            ts
                            age-hours
                            (format-time-string "%Y-%m-%d %H:%M" last-activity)))))
        (insert (format "\n%d threads total\n" (length threads)))
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

;;;###autoload
(defun agent-shell-to-go-cleanup-old-threads (&optional channel-id dry-run)
  "Delete Agent Shell threads older than `agent-shell-to-go-cleanup-age-hours'.
Skips threads that are currently active (have a live buffer).
CHANNEL-ID defaults to `agent-shell-to-go-channel-id'.
With prefix arg or DRY-RUN non-nil, just report what would be deleted."
  (interactive (list nil current-prefix-arg))
  (agent-shell-to-go--load-env)
  (let* ((channel (or channel-id agent-shell-to-go-channel-id))
         (threads (agent-shell-to-go--get-agent-shell-threads channel))
         (now (float-time))
         (age-threshold (* agent-shell-to-go-cleanup-age-hours 3600))
         (to-delete nil)
         (skipped-active 0)
         (skipped-recent 0))
    ;; Find threads to delete
    (dolist (thread threads)
      (let* ((ts (car thread))
             (last-activity (cdr thread))
             (age (- now last-activity)))
        (cond
         ((agent-shell-to-go--thread-active-p ts)
          (cl-incf skipped-active))
         ((< age age-threshold)
          (cl-incf skipped-recent))
         (t
          (push thread to-delete)))))
    ;; Report or delete
    (if (not to-delete)
        (message "No threads to clean up (skipped %d active, %d recent)"
                 skipped-active skipped-recent)
      (if dry-run
          (progn
            (message "Would delete %d threads (skipping %d active, %d recent)"
                     (length to-delete) skipped-active skipped-recent)
            (agent-shell-to-go-list-threads channel))
        ;; Delete async
        (message "Deleting %d threads (skipping %d active, %d recent)..."
                 (length to-delete) skipped-active skipped-recent)
        (agent-shell-to-go--cleanup-async channel (mapcar #'car to-delete))))))

;;;###autoload
(defun agent-shell-to-go-cleanup-all-channels (&optional dry-run)
  "Clean up old threads in all known project channels.
With prefix arg or DRY-RUN non-nil, just report what would be deleted."
  (interactive "P")
  (agent-shell-to-go--load-env)
  (agent-shell-to-go--load-channels)
  (let ((channels (list agent-shell-to-go-channel-id)))
    ;; Add all project channels
    (maphash (lambda (_k v) (cl-pushnew v channels :test #'equal))
             agent-shell-to-go--project-channels)
    (dolist (channel channels)
      (message "Checking channel %s..." channel)
      (agent-shell-to-go-cleanup-old-threads channel dry-run))))

(provide 'agent-shell-to-go)
;;; agent-shell-to-go.el ends here
