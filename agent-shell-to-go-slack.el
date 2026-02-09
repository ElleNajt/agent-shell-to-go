;;; agent-shell-to-go-slack.el --- Slack transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Elle Najt

;;; Commentary:

;; Slack transport for agent-shell-to-go. Mirrors conversations to Slack
;; threads with real-time WebSocket support for reactions and incoming messages.

;;; Code:

(require 'agent-shell-to-go-core)
(require 'json)
(require 'url)
(require 'websocket)

;;; Configuration

(defcustom agent-shell-to-go-slack-env-file "~/.doom.d/.env"
  "Path to .env file containing Slack credentials."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-bot-token nil
  "Slack bot token (xoxb-...). Loaded from .env if nil."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-channel-id nil
  "Default Slack channel ID. Loaded from .env if nil."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-app-token nil
  "Slack app-level token (xapp-...) for Socket Mode."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-per-project-channels t
  "When non-nil, create a separate Slack channel for each project."
  :type 'boolean
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-channel-prefix ""
  "Prefix for auto-created project channels."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-channels-file
  (expand-file-name "agent-shell-to-go-channels.el" user-emacs-directory)
  "File to persist project-to-channel mappings."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-user-id nil
  "Your Slack user ID for auto-invite to new channels."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-authorized-users nil
  "List of Slack user IDs authorized to interact with agents.
If nil, NO ONE can interact (secure by default)."
  :type '(repeat string)
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-show-tool-output t
  "When non-nil, show tool call outputs in Slack messages."
  :type 'boolean
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-default-folder "~/"
  "Default folder for /new-agent command."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-start-agent-function #'agent-shell
  "Function to call to start a new agent-shell."
  :type 'function
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-hidden-messages-dir "~/.agent-shell/slack/"
  "Directory to store original content of hidden messages."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-truncated-messages-dir "~/.agent-shell/slack-truncated/"
  "Directory to store full content of truncated messages."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-todo-directory "~/org/todo/"
  "Directory where bookmark TODOs are saved."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-projects-directory "~/code/"
  "Directory where /new-project creates new project folders."
  :type 'string
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-new-project-function nil
  "Function to call to set up a new project."
  :type '(choice (const :tag "Just create directory" nil)
          (function :tag "Custom setup function"))
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-image-upload-rate-limit 30
  "Maximum number of images to upload per minute.
Set to nil to disable rate limiting."
  :type '(choice (integer :tag "Max uploads per minute")
          (const :tag "No limit" nil))
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-image-upload-rate-window 60
  "Time window in seconds for rate limiting."
  :type 'integer
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-mentioned-file-ttl 14400
  "Time in seconds to remember mentioned files for image upload filtering.
Files mentioned in tool calls are eligible for upload for this long.
Default is 4 hours (14400 seconds)."
  :type 'integer
  :group 'agent-shell-to-go)

(defcustom agent-shell-to-go-slack-cleanup-age-hours 24
  "Age in hours after which threads are eligible for cleanup."
  :type 'integer
  :group 'agent-shell-to-go)

;;; Buffer-local State (stored in session-data plist)
;; These are stored per-buffer in the transport data:
;; :thread-ts - Slack thread timestamp
;; :channel-id - Slack channel ID
;; :file-watcher - fswatch process
;; :uploaded-images - hash table of uploaded image paths -> mtime
;; :upload-timestamps - list of upload timestamps for rate limiting
;; :mentioned-files - hash table of file paths mentioned in tool calls

;;; Internal State

(defvar agent-shell-to-go-slack--project-channels (make-hash-table :test 'equal)
  "Hash table mapping project paths to Slack channel IDs.")

(defvar agent-shell-to-go-slack--websocket nil
  "The WebSocket connection to Slack.")

(defvar agent-shell-to-go-slack--websocket-reconnect-timer nil
  "Timer for reconnecting WebSocket.")

(defvar agent-shell-to-go-slack--intentional-close nil
  "Non-nil when intentionally closing WebSocket.")

(defvar agent-shell-to-go-slack--bot-user-id-cache nil
  "Cached bot user ID.")

(defvar agent-shell-to-go-slack--pending-permissions nil
  "Alist of pending permission requests.")

(defvar agent-shell-to-go-slack--processed-message-ts (make-hash-table :test 'equal)
  "Hash table of recently processed message timestamps.")

;;; Constants

(defconst agent-shell-to-go-slack--reaction-map
  '(("white_check_mark" . allow)
    ("+1" . allow)
    ("unlock" . always)
    ("star" . always)
    ("x" . reject)
    ("-1" . reject))
  "Map Slack reaction names to permission actions.")

(defconst agent-shell-to-go-slack--hide-reactions
  '("no_bell" "see_no_evil")
  "Reactions that trigger hiding a message.")

(defconst agent-shell-to-go-slack--expand-reactions
  '("eyes")
  "Reactions that trigger expanding to truncated view.")

(defconst agent-shell-to-go-slack--full-expand-reactions
  '("book" "open_book")
  "Reactions that trigger full expansion.")

(defconst agent-shell-to-go-slack--heart-reactions
  '("heart" "heart_eyes" "heartpulse" "sparkling_heart" "two_hearts" "revolving_hearts")
  "Reactions that send appreciation to the agent.")

(defconst agent-shell-to-go-slack--bookmark-reactions
  '("bookmark")
  "Reactions that create a TODO from the message.")

(defconst agent-shell-to-go-slack--slack-max-length 3800
  "Maximum message length for Slack API.")

(defconst agent-shell-to-go-slack--image-extensions
  '("png" "jpg" "jpeg" "gif" "webp" "bmp" "svg")
  "File extensions recognized as images.")

;;; Utilities

(defun agent-shell-to-go-slack--strip-non-ascii (text)
  "Strip non-ASCII characters from TEXT."
  (when text
    (replace-regexp-in-string "[^[:ascii:]]" "?" text)))

(defun agent-shell-to-go-slack--authorized-p (user-id)
  "Return non-nil if USER-ID is authorized."
  (and agent-shell-to-go-slack-authorized-users
       (member user-id agent-shell-to-go-slack-authorized-users)))

(defun agent-shell-to-go-slack--load-env ()
  "Load credentials from .env file if not already set."
  (when (file-exists-p agent-shell-to-go-slack-env-file)
    (with-temp-buffer
      (insert-file-contents (expand-file-name agent-shell-to-go-slack-env-file))
      (goto-char (point-min))
      (while (re-search-forward "^\\([A-Z_]+\\)=\\(.+\\)$" nil t)
        (let ((key (match-string 1))
              (value (match-string 2)))
          (pcase key
            ("SLACK_BOT_TOKEN"
             (unless agent-shell-to-go-slack-bot-token
               (setq agent-shell-to-go-slack-bot-token value)))
            ("SLACK_CHANNEL_ID"
             (unless agent-shell-to-go-slack-channel-id
               (setq agent-shell-to-go-slack-channel-id value)))
            ("SLACK_APP_TOKEN"
             (unless agent-shell-to-go-slack-app-token
               (setq agent-shell-to-go-slack-app-token value)))))))))

(defun agent-shell-to-go-slack--load-channels ()
  "Load project-to-channel mappings from file."
  (when (file-exists-p agent-shell-to-go-slack-channels-file)
    (with-temp-buffer
      (insert-file-contents agent-shell-to-go-slack-channels-file)
      (let ((data (read (current-buffer))))
        (clrhash agent-shell-to-go-slack--project-channels)
        (dolist (pair data)
          (puthash (car pair) (cdr pair) agent-shell-to-go-slack--project-channels))))))

(defun agent-shell-to-go-slack--save-channels ()
  "Save project-to-channel mappings to file."
  (with-temp-file agent-shell-to-go-slack-channels-file
    (let ((data nil))
      (maphash (lambda (k v) (push (cons k v) data))
               agent-shell-to-go-slack--project-channels)
      (prin1 data (current-buffer)))))

;;; Slack API

(defun agent-shell-to-go-slack--api-request (method endpoint &optional data)
  "Make a Slack API request using curl."
  (let* ((url (concat "https://slack.com/api/" endpoint))
         (args (list "-s" "-X" method
                     "-H" (concat "Authorization: Bearer " agent-shell-to-go-slack-bot-token)
                     "-H" "Content-Type: application/json; charset=utf-8")))
    (when data
      (setq args (append args (list "-d" (encode-coding-string (json-encode data) 'utf-8)))))
    (setq args (append args (list url)))
    (with-temp-buffer
      (apply #'call-process "curl" nil t nil args)
      (goto-char (point-min))
      (json-read))))

(defun agent-shell-to-go-slack--get-bot-user-id ()
  "Get the bot's user ID."
  (or agent-shell-to-go-slack--bot-user-id-cache
      (setq agent-shell-to-go-slack--bot-user-id-cache
            (alist-get 'user_id
                       (agent-shell-to-go-slack--api-request "GET" "auth.test")))))

;;; Channel Management

(defun agent-shell-to-go-slack--sanitize-channel-name (name)
  "Sanitize NAME for use as a Slack channel name."
  (let* ((clean (downcase name))
         (clean (replace-regexp-in-string "[^a-z0-9-]" "-" clean))
         (clean (replace-regexp-in-string "-+" "-" clean))
         (clean (replace-regexp-in-string "^-\\|-$" "" clean)))
    (if (> (length clean) 80)
        (substring clean 0 80)
      clean)))

(defun agent-shell-to-go-slack--invite-user-to-channel (channel-id user-id)
  "Invite USER-ID to CHANNEL-ID."
  (agent-shell-to-go-slack--api-request
   "POST" "conversations.invite"
   `((channel . ,channel-id)
     (users . ,user-id))))

(defun agent-shell-to-go-slack--find-channel-by-name (name)
  "Find a channel by NAME, return its ID or nil."
  (let* ((response (agent-shell-to-go-slack--api-request
                    "GET" "conversations.list?types=public_channel,private_channel&limit=1000"))
         (channels (alist-get 'channels response)))
    (when channels
      (cl-loop for channel across channels
               when (equal (alist-get 'name channel) name)
               return (alist-get 'id channel)))))

(defun agent-shell-to-go-slack--create-channel (name)
  "Create a Slack channel with NAME. Return channel ID or nil."
  (let* ((response (agent-shell-to-go-slack--api-request
                    "POST" "conversations.create"
                    `((name . ,name)
                      (is_private . :json-false))))
         (ok (alist-get 'ok response))
         (channel (alist-get 'channel response))
         (channel-id (alist-get 'id channel))
         (error-msg (alist-get 'error response)))
    (cond
     (ok
      (when agent-shell-to-go-slack-user-id
        (agent-shell-to-go-slack--invite-user-to-channel channel-id agent-shell-to-go-slack-user-id))
      channel-id)
     ((equal error-msg "name_taken")
      (agent-shell-to-go-slack--find-channel-by-name name))
     (t
      (agent-shell-to-go--debug "Failed to create channel %s: %s" name error-msg)
      nil))))

(defun agent-shell-to-go-slack--get-or-create-project-channel (buffer)
  "Get or create a Slack channel for BUFFER's project."
  (if (not agent-shell-to-go-slack-per-project-channels)
      agent-shell-to-go-slack-channel-id
    (let* ((project-path (agent-shell-to-go--get-project-path buffer))
           (cached-id (gethash project-path agent-shell-to-go-slack--project-channels)))
      (or cached-id
          (let* ((project-name (file-name-nondirectory (directory-file-name project-path)))
                 (channel-name (concat agent-shell-to-go-slack-channel-prefix
                                       (agent-shell-to-go-slack--sanitize-channel-name project-name)))
                 (channel-id (agent-shell-to-go-slack--create-channel channel-name)))
            (when channel-id
              (puthash project-path channel-id agent-shell-to-go-slack--project-channels)
              (agent-shell-to-go-slack--save-channels)
              (agent-shell-to-go--debug "Created/found channel %s for %s" channel-name project-path))
            (or channel-id agent-shell-to-go-slack-channel-id))))))

;;; Image Upload

(defun agent-shell-to-go-slack--image-file-p (path)
  "Return non-nil if PATH is an image file based on extension."
  (when (and path (stringp path))
    (let ((ext (downcase (or (file-name-extension path) ""))))
      (member ext agent-shell-to-go-slack--image-extensions))))

(defun agent-shell-to-go-slack--check-upload-rate-limit (session-data)
  "Check if we're within rate limit. Returns t if upload is allowed."
  (if (not agent-shell-to-go-slack-image-upload-rate-limit)
      t  ; No limit configured
    (let* ((upload-timestamps (plist-get session-data :upload-timestamps))
           (now (float-time))
           (window-start (- now agent-shell-to-go-slack-image-upload-rate-window))
           ;; Prune old timestamps
           (recent (cl-remove-if (lambda (ts) (< ts window-start)) upload-timestamps)))
      ;; Check if under limit
      (< (length recent) agent-shell-to-go-slack-image-upload-rate-limit))))

(defun agent-shell-to-go-slack--record-upload (buffer)
  "Record an upload timestamp for rate limiting in BUFFER."
  (let* ((session-data (agent-shell-to-go--get-transport-data buffer "slack"))
         (timestamps (plist-get session-data :upload-timestamps)))
    (setq session-data (plist-put session-data :upload-timestamps (cons (float-time) timestamps)))
    (agent-shell-to-go--set-transport-data buffer "slack" session-data)))

(defun agent-shell-to-go-slack--record-mentioned-file (buffer file-path)
  "Record FILE-PATH as mentioned by the agent in BUFFER."
  (let* ((session-data (agent-shell-to-go--get-transport-data buffer "slack"))
         (mentioned-files (or (plist-get session-data :mentioned-files)
                              (make-hash-table :test 'equal))))
    (puthash file-path (float-time) mentioned-files)
    (setq session-data (plist-put session-data :mentioned-files mentioned-files))
    (agent-shell-to-go--set-transport-data buffer "slack" session-data)))

(defun agent-shell-to-go-slack--file-was-mentioned-p (buffer file-path)
  "Return non-nil if FILE-PATH was recently mentioned by the agent in BUFFER."
  (let* ((session-data (agent-shell-to-go--get-transport-data buffer "slack"))
         (mentioned-files (plist-get session-data :mentioned-files)))
    (when mentioned-files
      (let ((mentioned-time (gethash file-path mentioned-files)))
        (and mentioned-time
             (< (- (float-time) mentioned-time) agent-shell-to-go-slack-mentioned-file-ttl))))))

(defun agent-shell-to-go-slack--extract-file-paths-from-update (update)
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

(defun agent-shell-to-go-slack--upload-file (file-path channel-id &optional thread-ts comment)
  "Upload FILE-PATH to Slack CHANNEL-ID, optionally in THREAD-TS with COMMENT."
  (when (and file-path (file-exists-p file-path))
    (let* ((filename (file-name-nondirectory file-path))
           (file-size (file-attribute-size (file-attributes file-path)))
           ;; Step 1: Get upload URL
           (url-response (agent-shell-to-go-slack--api-request
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
                 (complete-response (agent-shell-to-go-slack--api-request
                                     "POST" "files.completeUploadExternal"
                                     complete-data)))
            (agent-shell-to-go--debug "complete upload response: %s" complete-response)
            complete-response))))))

(defun agent-shell-to-go-slack--handle-fswatch-output (buffer output)
  "Handle fswatch OUTPUT, uploading new images for BUFFER."
  (when (buffer-live-p buffer)
    (dolist (file-path (split-string output "\n" t))
      (when (and (agent-shell-to-go-slack--image-file-p file-path)
                 (file-exists-p file-path)
                 (> (file-attribute-size (file-attributes file-path)) 0))
        (let* ((session-data (agent-shell-to-go--get-transport-data buffer "slack"))
               (thread-ts (plist-get session-data :thread-ts))
               (channel-id (plist-get session-data :channel-id)))
          ;; Only upload if this file was mentioned by this buffer's agent
          (when (agent-shell-to-go-slack--file-was-mentioned-p buffer file-path)
            (let* ((uploaded-images (or (plist-get session-data :uploaded-images)
                                        (make-hash-table :test 'equal)))
                   (mtime (file-attribute-modification-time (file-attributes file-path)))
                   (mtime-float (float-time mtime))
                   (prev-mtime (gethash file-path uploaded-images)))
              ;; Upload if new file or modified since last upload
              (when (or (not prev-mtime)
                        (> mtime-float prev-mtime))
                ;; Check rate limit
                (if (not (agent-shell-to-go-slack--check-upload-rate-limit session-data))
                    (agent-shell-to-go--debug "rate limit exceeded, skipping: %s" file-path)
                  ;; Mark as uploaded with current mtime
                  (puthash file-path mtime-float uploaded-images)
                  (setq session-data (plist-put session-data :uploaded-images uploaded-images))
                  (agent-shell-to-go--set-transport-data buffer "slack" session-data)
                  ;; Small delay to ensure file is fully written
                  (run-at-time 0.5 nil
                               (lambda ()
                                 (when (and (buffer-live-p buffer)
                                            (file-exists-p file-path))
                                   (agent-shell-to-go--debug "fswatch uploading: %s" file-path)
                                   (agent-shell-to-go-slack--record-upload buffer)
                                   (agent-shell-to-go-slack--upload-file
                                    file-path
                                    channel-id
                                    thread-ts
                                    (format ":frame_with_picture: `%s`"
                                            (file-name-nondirectory file-path)))))))))))))))

(defun agent-shell-to-go-slack--start-file-watcher (buffer)
  "Start fswatch for new image files in BUFFER's project directory (recursive)."
  (agent-shell-to-go-slack--stop-file-watcher buffer)
  (let ((project-dir (agent-shell-to-go--get-project-path buffer)))
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
                 (agent-shell-to-go-slack--handle-fswatch-output buffer output)))
              (set-process-query-on-exit-flag proc nil)
              ;; Store in session data
              (let ((session-data (agent-shell-to-go--get-transport-data buffer "slack")))
                (setq session-data (plist-put session-data :file-watcher proc))
                (agent-shell-to-go--set-transport-data buffer "slack" session-data))
              (agent-shell-to-go--debug "started fswatch on %s" project-dir))
          (error
           (agent-shell-to-go--debug "failed to start fswatch: %s" err)))))))

(defun agent-shell-to-go-slack--stop-file-watcher (buffer)
  "Stop fswatch process for BUFFER."
  (let* ((session-data (agent-shell-to-go--get-transport-data buffer "slack"))
         (watcher (plist-get session-data :file-watcher)))
    (when (and watcher (process-live-p watcher))
      (ignore-errors (kill-process watcher)))
    (when session-data
      (setq session-data (plist-put session-data :file-watcher nil))
      (agent-shell-to-go--set-transport-data buffer "slack" session-data))))

;;; Diff Formatting

(defun agent-shell-to-go-slack--parse-unified-diff (diff-string)
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

(defun agent-shell-to-go-slack--extract-diff (update)
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
        (agent-shell-to-go-slack--parse-unified-diff diff-str))))))

(defun agent-shell-to-go-slack--format-diff-for-slack (old-text new-text)
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

;;; Message Sending

(defun agent-shell-to-go-slack--truncate-message (text &optional max-len)
  "Truncate TEXT to MAX-LEN (default 500)."
  (let ((max-len (or max-len 500)))
    (if (> (length text) max-len)
        (concat (substring text 0 max-len) "\n:eyes: _for more_")
      text)))

(defun agent-shell-to-go-slack--send (text &optional thread-ts channel-id truncate)
  "Send TEXT to Slack, optionally in THREAD-TS thread."
  (condition-case err
      (let* ((channel (or channel-id agent-shell-to-go-slack-channel-id))
             (truncated-text (if truncate
                                 (agent-shell-to-go-slack--truncate-message text 500)
                               text))
             (was-truncated (and truncate (not (equal text truncated-text))))
             (data `((channel . ,channel)
                     (text . ,truncated-text))))
        (when thread-ts
          (push `(thread_ts . ,thread-ts) data))
        (let ((response (agent-shell-to-go-slack--api-request "POST" "chat.postMessage" data)))
          (when was-truncated
            (let ((msg-ts (alist-get 'ts response)))
              (when msg-ts
                (agent-shell-to-go-slack--save-truncated-message channel msg-ts text))))
          response))
    (error
     (agent-shell-to-go--debug "send error: %s, retrying with ASCII-only" err)
     (let* ((channel (or channel-id agent-shell-to-go-slack-channel-id))
            (safe-text (agent-shell-to-go-slack--strip-non-ascii text))
            (truncated-text (if truncate
                                (agent-shell-to-go-slack--truncate-message safe-text 500)
                              safe-text))
            (data `((channel . ,channel)
                    (text . ,truncated-text))))
       (when thread-ts
         (push `(thread_ts . ,thread-ts) data))
       (condition-case nil
           (agent-shell-to-go-slack--api-request "POST" "chat.postMessage" data)
         (error nil))))))

(defun agent-shell-to-go-slack--start-thread (buffer-name channel-id)
  "Start a new Slack thread for BUFFER-NAME in CHANNEL-ID."
  (let* ((response (agent-shell-to-go-slack--send
                    (format ":robot_face: *Agent Shell Session*\n`%s`\n_%s_"
                            buffer-name
                            (format-time-string "%Y-%m-%d %H:%M:%S"))
                    nil channel-id))
         (ts (alist-get 'ts response)))
    ts))

;;; Message Formatting

(defun agent-shell-to-go-slack--format-user-message (text)
  "Format user TEXT for Slack."
  (format ":bust_in_silhouette: *User*\n%s" text))

(defun agent-shell-to-go-slack--format-agent-message (text)
  "Format agent TEXT for Slack."
  (format ":robot_face: *Agent*\n%s" text))

;;; Truncated/Hidden Message Storage

(defun agent-shell-to-go-slack--truncated-message-path (channel ts)
  "Return file path for truncated message content."
  (expand-file-name (concat channel "/" ts ".txt")
                    agent-shell-to-go-slack-truncated-messages-dir))

(defun agent-shell-to-go-slack--save-truncated-message (channel ts full-text &optional collapsed-text)
  "Save FULL-TEXT of truncated message."
  (let ((path (agent-shell-to-go-slack--truncated-message-path channel ts)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert full-text)))
  (when collapsed-text
    (let ((collapsed-path (concat (agent-shell-to-go-slack--truncated-message-path channel ts) ".collapsed")))
      (with-temp-file collapsed-path
        (insert collapsed-text)))))

(defun agent-shell-to-go-slack--load-truncated-message (channel ts)
  "Load full text of truncated message."
  (let ((path (agent-shell-to-go-slack--truncated-message-path channel ts)))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string)))))

(defun agent-shell-to-go-slack--load-collapsed-message (channel ts)
  "Load collapsed form of message."
  (let ((path (concat (agent-shell-to-go-slack--truncated-message-path channel ts) ".collapsed")))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string)))))

(defun agent-shell-to-go-slack--hidden-message-path (channel ts)
  "Return file path for hidden message content."
  (expand-file-name (concat channel "/" ts ".txt")
                    agent-shell-to-go-slack-hidden-messages-dir))

(defun agent-shell-to-go-slack--save-hidden-message (channel ts text)
  "Save TEXT of hidden message."
  (let ((path (agent-shell-to-go-slack--hidden-message-path channel ts)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert text))))

(defun agent-shell-to-go-slack--load-hidden-message (channel ts)
  "Load original text of hidden message."
  (let ((path (agent-shell-to-go-slack--hidden-message-path channel ts)))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string)))))

(defun agent-shell-to-go-slack--delete-hidden-message-file (channel ts)
  "Delete stored hidden message file."
  (let ((path (agent-shell-to-go-slack--hidden-message-path channel ts)))
    (when (file-exists-p path)
      (delete-file path))))

;;; WebSocket / Socket Mode

(defun agent-shell-to-go-slack--get-websocket-url ()
  "Get WebSocket URL from Slack."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " agent-shell-to-go-slack-app-token))
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

(defun agent-shell-to-go-slack--find-buffer-for-thread (thread-ts &optional channel-id)
  "Find the buffer for THREAD-TS."
  (cl-find-if
   (lambda (buf)
     (when (buffer-live-p buf)
       (let ((data (agent-shell-to-go--get-transport-data buf "slack")))
         (and (equal thread-ts (plist-get data :thread-ts))
              (or (not channel-id)
                  (equal channel-id (plist-get data :channel-id)))))))
   agent-shell-to-go--active-buffers))

(defun agent-shell-to-go-slack--find-buffer-for-channel (channel-id)
  "Find a buffer for CHANNEL-ID."
  (cl-find-if
   (lambda (buf)
     (when (buffer-live-p buf)
       (let ((data (agent-shell-to-go--get-transport-data buf "slack")))
         (equal channel-id (plist-get data :channel-id)))))
   agent-shell-to-go--active-buffers))

(defun agent-shell-to-go-slack--handle-websocket-message (frame)
  "Handle incoming WebSocket FRAME."
  (let* ((payload (websocket-frame-text frame))
         (data (json-read-from-string payload))
         (type (alist-get 'type data))
         (envelope-id (alist-get 'envelope_id data)))
    (when envelope-id
      (websocket-send-text agent-shell-to-go-slack--websocket
                           (json-encode `((envelope_id . ,envelope-id)))))
    (pcase type
      ("events_api"
       (let ((event-payload (alist-get 'payload data)))
         (run-at-time 0 nil #'agent-shell-to-go-slack--handle-event event-payload)))
      ("slash_commands"
       (let ((slash-payload (alist-get 'payload data)))
         (run-at-time 0 nil #'agent-shell-to-go-slack--handle-slash-command slash-payload)))
      ("hello"
       (agent-shell-to-go--debug "Slack WebSocket connected"))
      ("disconnect"
       (agent-shell-to-go--debug "Slack WebSocket disconnect, reconnecting...")
       (agent-shell-to-go-slack--websocket-reconnect)))))

(defun agent-shell-to-go-slack--handle-event (payload)
  "Handle Slack event PAYLOAD."
  (let* ((event (alist-get 'event payload))
         (event-type (alist-get 'type event))
         (user (alist-get 'user event))
         (bot-id (alist-get 'bot_id event)))
    (unless bot-id
      (if (not (agent-shell-to-go-slack--authorized-p user))
          (agent-shell-to-go--debug "unauthorized user %s" user)
        (pcase event-type
          ("message"
           (agent-shell-to-go-slack--handle-message-event event))
          ("reaction_added"
           (agent-shell-to-go-slack--handle-reaction-event event))
          ("reaction_removed"
           (agent-shell-to-go-slack--handle-reaction-removed-event event)))))))

(defun agent-shell-to-go-slack--handle-message-event (event)
  "Handle a message EVENT from Slack."
  (let* ((thread-ts (alist-get 'thread_ts event))
         (channel (alist-get 'channel event))
         (user (alist-get 'user event))
         (text (alist-get 'text event))
         (msg-ts (alist-get 'ts event))
         (subtype (alist-get 'subtype event))
         (bot-id (alist-get 'bot_id event))
         (buffer (and thread-ts (agent-shell-to-go-slack--find-buffer-for-thread thread-ts channel))))
    (when (and buffer text msg-ts
               (not (gethash msg-ts agent-shell-to-go-slack--processed-message-ts))
               (not subtype)
               (not bot-id)
               (not (equal user (agent-shell-to-go-slack--get-bot-user-id))))
      (puthash msg-ts (float-time) agent-shell-to-go-slack--processed-message-ts)
      (if (string-prefix-p "!" text)
          (agent-shell-to-go-slack--handle-command text buffer thread-ts)
        (agent-shell-to-go-inject-message buffer text "slack")))))

;;; Reaction Handlers

(defun agent-shell-to-go-slack--hide-message (channel ts)
  "Hide message at TS in CHANNEL by replacing with collapsed text."
  (let ((original-text (agent-shell-to-go-slack--get-message-text channel ts nil)))
    (when original-text
      (agent-shell-to-go-slack--save-hidden-message channel ts original-text)
      (agent-shell-to-go-slack--api-request
       "POST" "chat.update"
       `((channel . ,channel)
         (ts . ,ts)
         (text . ":see_no_evil: _message hidden_"))))))

(defun agent-shell-to-go-slack--unhide-message (channel ts)
  "Restore hidden message at TS in CHANNEL to its original text."
  (let ((original-text (agent-shell-to-go-slack--load-hidden-message channel ts)))
    (when original-text
      (agent-shell-to-go-slack--api-request
       "POST" "chat.update"
       `((channel . ,channel)
         (ts . ,ts)
         (text . ,original-text)))
      (agent-shell-to-go-slack--delete-hidden-message-file channel ts))))

(defun agent-shell-to-go-slack--get-message-text (channel ts thread-ts)
  "Get the text of message at TS in CHANNEL."
  (if thread-ts
      (let* ((response (agent-shell-to-go-slack--api-request
                        "GET"
                        (format "conversations.replies?channel=%s&ts=%s" channel thread-ts)))
             (messages (alist-get 'messages response)))
        (cl-loop for msg across messages
                 when (equal ts (alist-get 'ts msg))
                 return (alist-get 'text msg)))
    (let* ((response (agent-shell-to-go-slack--api-request
                      "GET"
                      (format "conversations.history?channel=%s&latest=%s&limit=1&inclusive=true"
                              channel ts)))
           (messages (alist-get 'messages response))
           (message (and messages (aref messages 0))))
      (alist-get 'text message))))

(defun agent-shell-to-go-slack--expand-message (channel ts)
  "Expand message at TS in CHANNEL to truncated view (glance)."
  (let ((full-text (agent-shell-to-go-slack--load-truncated-message channel ts)))
    (when full-text
      (let* ((max-len 500)
             (too-long (> (length full-text) max-len))
             (display-text (if too-long
                               (concat (substring full-text 0 max-len)
                                       "\n_... 📖 for full output_")
                             full-text)))
        (agent-shell-to-go-slack--api-request
         "POST" "chat.update"
         `((channel . ,channel)
           (ts . ,ts)
           (text . ,display-text)))))))

(defun agent-shell-to-go-slack--full-expand-message (channel ts)
  "Fully expand message at TS in CHANNEL (full output)."
  (let ((full-text (agent-shell-to-go-slack--load-truncated-message channel ts)))
    (when full-text
      (let* ((too-long (> (length full-text) agent-shell-to-go-slack--slack-max-length))
             (display-text (if too-long
                               (concat (substring full-text 0 agent-shell-to-go-slack--slack-max-length)
                                       "\n_... (full text too long for Slack)_")
                             full-text)))
        (agent-shell-to-go-slack--api-request
         "POST" "chat.update"
         `((channel . ,channel)
           (ts . ,ts)
           (text . ,display-text)))))))

(defun agent-shell-to-go-slack--collapse-message (channel ts)
  "Re-truncate expanded message at TS in CHANNEL."
  (let* ((collapsed (agent-shell-to-go-slack--load-collapsed-message channel ts))
         (full-text (agent-shell-to-go-slack--load-truncated-message channel ts))
         (restore-text (or collapsed
                           (and full-text
                                (agent-shell-to-go-slack--truncate-message full-text 500)))))
    (when restore-text
      (agent-shell-to-go-slack--api-request
       "POST" "chat.update"
       `((channel . ,channel)
         (ts . ,ts)
         (text . ,restore-text))))))

(defun agent-shell-to-go-slack--handle-heart-reaction (channel ts)
  "Handle heart reaction on message at TS in CHANNEL."
  (let* ((buffer (agent-shell-to-go-slack--find-buffer-for-channel channel))
         (session-data (and buffer (agent-shell-to-go--get-transport-data buffer "slack")))
         (thread-ts (and session-data (plist-get session-data :thread-ts)))
         (message-text (agent-shell-to-go-slack--get-message-text channel ts thread-ts)))
    (when (and buffer message-text)
      (agent-shell-to-go-inject-message
       buffer
       (format "The user heart reacted to: %s" message-text)
       "slack"))))

(defun agent-shell-to-go-slack--handle-bookmark-reaction (channel ts)
  "Handle bookmark reaction on message at TS in CHANNEL."
  (let* ((buffer (agent-shell-to-go-slack--find-buffer-for-channel channel))
         (session-data (and buffer (agent-shell-to-go--get-transport-data buffer "slack")))
         (thread-ts (and session-data (plist-get session-data :thread-ts)))
         (message-text (or (and thread-ts (agent-shell-to-go-slack--get-message-text channel ts thread-ts))
                           (agent-shell-to-go-slack--get-message-text channel ts nil)))
         (project-name (or (and buffer
                                (with-current-buffer buffer
                                  (file-name-nondirectory
                                   (directory-file-name default-directory))))
                           "slack"))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (todo-dir (expand-file-name agent-shell-to-go-slack-todo-directory))
         (todo-file (expand-file-name (format "%s-%s.org" project-name timestamp) todo-dir))
         (title-text (if message-text
                         (let ((first-line (car (split-string message-text "\n" t))))
                           (if (> (length first-line) 60)
                               (concat (substring first-line 0 57) "...")
                             first-line))
                       "Bookmarked message")))
    (when message-text
      (make-directory todo-dir t)
      (with-temp-file todo-file
        (insert (format "* TODO %s\n" title-text))
        (insert (format "SCHEDULED: <%s>\n\n" (format-time-string "%Y-%m-%d")))
        (insert (format "Project: %s\n\n" project-name))
        (insert "** Message\n")
        (insert message-text)
        (insert "\n"))
      (agent-shell-to-go-slack--send
       (format ":bookmark: TODO created: `%s`" (file-name-nondirectory todo-file))
       (or thread-ts ts) channel))))

(defun agent-shell-to-go-slack--find-option-id (options action)
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

(defun agent-shell-to-go-slack--handle-reaction-event (event)
  "Handle a reaction EVENT from Slack."
  (let* ((item (alist-get 'item event))
         (msg-ts (alist-get 'ts item))
         (channel (alist-get 'channel item))
         (reaction (alist-get 'reaction event))
         (pending (assoc msg-ts agent-shell-to-go-slack--pending-permissions)))
    ;; Check for hide reactions
    (when (member reaction agent-shell-to-go-slack--hide-reactions)
      (agent-shell-to-go-slack--hide-message channel msg-ts))
    ;; Check for expand reactions
    (when (member reaction agent-shell-to-go-slack--expand-reactions)
      (agent-shell-to-go-slack--expand-message channel msg-ts))
    ;; Check for full-expand reactions
    (when (member reaction agent-shell-to-go-slack--full-expand-reactions)
      (agent-shell-to-go-slack--full-expand-message channel msg-ts))
    ;; Check for heart reactions
    (when (member reaction agent-shell-to-go-slack--heart-reactions)
      (agent-shell-to-go--debug "heart reaction: %s on %s" reaction msg-ts)
      (agent-shell-to-go-slack--handle-heart-reaction channel msg-ts))
    ;; Check for bookmark reactions
    (when (member reaction agent-shell-to-go-slack--bookmark-reactions)
      (agent-shell-to-go--debug "bookmark reaction: %s on %s" reaction msg-ts)
      (agent-shell-to-go-slack--handle-bookmark-reaction channel msg-ts))
    ;; Check for permission reactions
    (when pending
      (let* ((info (cdr pending))
             (request-id (plist-get info :request-id))
             (buffer (plist-get info :buffer))
             (options (plist-get info :options))
             (action (alist-get reaction agent-shell-to-go-slack--reaction-map nil nil #'string=)))
        (when (and action buffer (buffer-live-p buffer))
          (let ((option-id (agent-shell-to-go-slack--find-option-id options action)))
            (when option-id
              (with-current-buffer buffer
                (when (boundp 'agent-shell--state)
                  (let ((state agent-shell--state))
                    (agent-shell--send-permission-response
                     :client (alist-get :client state)
                     :request-id request-id
                     :option-id option-id
                     :state state))))
              ;; Remove from pending
              (setq agent-shell-to-go-slack--pending-permissions
                    (assq-delete-all msg-ts agent-shell-to-go-slack--pending-permissions)))))))))

(defun agent-shell-to-go-slack--handle-reaction-removed-event (event)
  "Handle a reaction removed EVENT from Slack."
  (let* ((item (alist-get 'item event))
         (msg-ts (alist-get 'ts item))
         (channel (alist-get 'channel item))
         (reaction (alist-get 'reaction event)))
    ;; Check if it was a hide reaction being removed
    (when (member reaction agent-shell-to-go-slack--hide-reactions)
      (agent-shell-to-go-slack--unhide-message channel msg-ts))
    ;; Check if it was an expand reaction being removed (re-truncate)
    (when (or (member reaction agent-shell-to-go-slack--expand-reactions)
              (member reaction agent-shell-to-go-slack--full-expand-reactions))
      (agent-shell-to-go-slack--collapse-message channel msg-ts))))

;;; Command Handling

(defun agent-shell-to-go-slack--set-mode (buffer mode-id thread-ts channel-id mode-name emoji)
  "Set MODE-ID in BUFFER, notify THREAD-TS in CHANNEL-ID with MODE-NAME and EMOJI."
  (with-current-buffer buffer
    (agent-shell--set-default-session-mode
     :shell nil
     :mode-id mode-id
     :on-mode-changed (lambda ()
                        (agent-shell-to-go-slack--send
                         (format "%s Mode: *%s*" emoji mode-name)
                         thread-ts channel-id)))))

(defun agent-shell-to-go-slack--handle-command (text buffer thread-ts)
  "Handle command TEXT in BUFFER, reply to THREAD-TS."
  (let* ((session-data (agent-shell-to-go--get-transport-data buffer "slack"))
         (channel-id (plist-get session-data :channel-id))
         (cmd (downcase (string-trim text))))
    (pcase cmd
      ("!help"
       (agent-shell-to-go-slack--send
        (concat ":question: *Commands:*\n"
                "`!yolo` - Bypass permissions\n"
                "`!safe` - Accept edits mode\n"
                "`!plan` - Plan mode\n"
                "`!mode` - Show current mode\n"
                "`!stop` - Interrupt the agent\n"
                "`!restart` - Kill and restart agent with transcript\n"
                "`!queue` - Show pending queued messages\n"
                "`!clearqueue` - Clear all pending queued messages\n"
                "`!latest` - Jump to bottom of thread\n"
                "`!debug` - Show session info")
        thread-ts channel-id)
       t)
      ((or "!yolo" "!bypass")
       (agent-shell-to-go-slack--set-mode buffer "bypassPermissions" thread-ts channel-id
                                          "Bypass Permissions" ":zap:")
       t)
      ((or "!safe" "!accept" "!acceptedits")
       (agent-shell-to-go-slack--set-mode buffer "acceptEdits" thread-ts channel-id
                                          "Accept Edits" ":shield:")
       t)
      ((or "!plan" "!planmode")
       (agent-shell-to-go-slack--set-mode buffer "plan" thread-ts channel-id
                                          "Plan" ":clipboard:")
       t)
      ("!mode"
       (with-current-buffer buffer
         (when (boundp 'agent-shell--state)
           (let ((mode-id (map-nested-elt agent-shell--state '(:session :mode-id))))
             (agent-shell-to-go-slack--send
              (format ":gear: Current mode: *%s*" (or mode-id "unknown"))
              thread-ts channel-id))))
       t)
      ("!queue"
       (with-current-buffer buffer
         (let ((pending (map-elt agent-shell--state :pending-requests)))
           (if (seq-empty-p pending)
               (agent-shell-to-go-slack--send ":inbox_tray: No pending requests" thread-ts channel-id)
             (agent-shell-to-go-slack--send
              (format ":inbox_tray: *Pending requests (%d):*\n%s"
                      (length pending)
                      (mapconcat
                       (lambda (req)
                         (format "• %s"
                                 (agent-shell-to-go--truncate req 80)))
                       pending
                       "\n"))
              thread-ts channel-id))))
       t)
      ("!clearqueue"
       (with-current-buffer buffer
         (let ((count (length (map-elt agent-shell--state :pending-requests))))
           (map-put! agent-shell--state :pending-requests nil)
           (agent-shell-to-go-slack--send
            (format ":wastebasket: Cleared %d pending request%s"
                    count (if (= count 1) "" "s"))
            thread-ts channel-id)))
       t)
      ("!latest"
       (agent-shell-to-go-slack--send ":point_down:" thread-ts channel-id)
       t)
      ("!stop"
       (condition-case err
           (with-current-buffer buffer
             (agent-shell-interrupt t)
             (agent-shell-to-go-slack--send ":stop_sign: Agent interrupted" thread-ts channel-id))
         (error
          (agent-shell-to-go--debug "!stop error: %s" err)
          (agent-shell-to-go-slack--send (format ":x: Stop failed: %s" err) thread-ts channel-id)))
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
               (agent-shell-to-go-slack--send ":arrows_counterclockwise: Restarting agent..." thread-ts channel-id)
               ;; Kill current agent
               (ignore-errors (agent-shell-interrupt t))
               ;; Start new agent in same directory
               (run-at-time 1 nil
                            (lambda ()
                              (let ((default-directory project-dir))
                                (save-window-excursion
                                  (funcall agent-shell-to-go-slack-start-agent-function nil)
                                  ;; If transcript exists, tell the new agent about it
                                  (when transcript-exists
                                    (run-at-time 2 nil
                                                 (lambda ()
                                                   (when-let ((new-buf (car agent-shell-to-go--active-buffers)))
                                                     (with-current-buffer new-buf
                                                       (agent-shell-to-go-inject-message
                                                        new-buf
                                                        (format "Continue from previous session. Transcript at: %s"
                                                                transcript-file)
                                                        "slack"))))))))))))
         (error
          (agent-shell-to-go--debug "!restart error: %s" err)
          (agent-shell-to-go-slack--send (format ":x: Restart failed: %s" err) thread-ts channel-id)))
       t)
      ("!debug"
       (with-current-buffer buffer
         (when (boundp 'agent-shell--state)
           (let* ((session-id (map-nested-elt agent-shell--state '(:session :id)))
                  (mode-id (map-nested-elt agent-shell--state '(:session :mode-id)))
                  (transcript-dir (expand-file-name "transcripts"
                                                    (or (bound-and-true-p agent-shell-sessions-dir)
                                                        "~/.agent-shell")))
                  (transcript-files (and (file-directory-p transcript-dir)
                                         (directory-files transcript-dir nil "\\.md$" t)))
                  (latest-transcript (and transcript-files
                                          (car (last (sort transcript-files #'string<)))))
                  (truncated-dir (expand-file-name channel-id
                                                   agent-shell-to-go-slack-truncated-messages-dir))
                  (truncated-count (if (file-directory-p truncated-dir)
                                       (length (directory-files truncated-dir nil "\\.txt$"))
                                     0)))
             (agent-shell-to-go-slack--send
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
                      channel-id
                      (or session-id "none")
                      (or mode-id "default")
                      (or latest-transcript "none")
                      truncated-count
                      truncated-dir)
              thread-ts channel-id))))
       t)
      (_ nil))))

(defun agent-shell-to-go-slack--get-project-for-channel (channel-id)
  "Get the project path associated with CHANNEL-ID, or nil if not found."
  (let (candidates)
    (maphash (lambda (project-path ch-id)
               (when (equal ch-id channel-id)
                 (push project-path candidates)))
             agent-shell-to-go-slack--project-channels)
    (or (cl-find-if #'file-directory-p candidates)
        (car candidates))))

(defun agent-shell-to-go-slack--get-open-projects ()
  "Get list of open projects from Emacs."
  (delete-dups
   (delq nil
         (cond
          ((fboundp 'projectile-open-projects)
           (projectile-open-projects))
          ((fboundp 'project-known-project-roots)
           (project-known-project-roots))
          (t
           (mapcar (lambda (buf)
                     (when-let ((file (buffer-file-name buf)))
                       (file-name-directory file)))
                   (buffer-list)))))))

(defun agent-shell-to-go-slack--start-agent-in-folder (folder channel &optional use-container)
  "Start a new agent in FOLDER. If USE-CONTAINER is non-nil, pass prefix arg.
Notify CHANNEL on success."
  (agent-shell-to-go--debug "starting agent in %s (container: %s)" folder use-container)
  (if (file-directory-p folder)
      (let ((default-directory folder))
        (save-window-excursion
          (condition-case err
              (progn
                (funcall agent-shell-to-go-slack-start-agent-function
                         (if use-container '(4) nil))
                (agent-shell-to-go-slack--api-request
                 "POST" "chat.postMessage"
                 `((channel . ,channel)
                   (text . ,(format ":rocket: New agent started in `%s`%s"
                                    folder
                                    (if use-container " (container)" ""))))))
            (error
             (agent-shell-to-go--debug "error starting agent: %s" err)))))
    (agent-shell-to-go--debug "folder does not exist: %s" folder)))

(defun agent-shell-to-go-slack--handle-slash-command (payload)
  "Handle slash command PAYLOAD from Slack."
  (let* ((command (alist-get 'command payload))
         (text (alist-get 'text payload))
         (channel (alist-get 'channel_id payload))
         (user (alist-get 'user_id payload))
         (channel-project (agent-shell-to-go-slack--get-project-for-channel channel))
         (folder (expand-file-name
                  (cond
                   ((and text (not (string-empty-p text))) text)
                   (channel-project channel-project)
                   (t agent-shell-to-go-slack-default-folder)))))
    (agent-shell-to-go--debug "slash command: %s %s (channel project: %s, user: %s)"
                              command text channel-project user)
    (if (not (agent-shell-to-go-slack--authorized-p user))
        (agent-shell-to-go-slack--api-request
         "POST" "chat.postEphemeral"
         `((channel . ,channel)
           (user . ,user)
           (text . ":no_entry: You are not authorized to use this command.")))
      (pcase command
        ("/new-project"
         (if (or (not text) (string-empty-p text))
             (agent-shell-to-go-slack--api-request
              "POST" "chat.postMessage"
              `((channel . ,channel)
                (text . ":x: Usage: `/new-project <project-name>`")))
           (let* ((project-name (string-trim text))
                  (project-dir (expand-file-name project-name agent-shell-to-go-slack-projects-directory)))
             (if (file-exists-p project-dir)
                 (agent-shell-to-go-slack--api-request
                  "POST" "chat.postMessage"
                  `((channel . ,channel)
                    (text . ,(format ":warning: Project already exists: `%s`" project-dir))))
               (agent-shell-to-go-slack--api-request
                "POST" "chat.postMessage"
                `((channel . ,channel)
                  (text . ,(format ":file_folder: Creating project: `%s`" project-dir))))
               (let ((start-agent-fn
                      (lambda (final-project-dir)
                        (agent-shell-to-go-slack--api-request
                         "POST" "chat.postMessage"
                         `((channel . ,channel)
                           (text . ":rocket: Starting Claude Code...")))
                        (agent-shell-to-go-slack--start-agent-in-folder final-project-dir channel nil))))
                 (if agent-shell-to-go-slack-new-project-function
                     (funcall agent-shell-to-go-slack-new-project-function
                              project-name
                              (expand-file-name agent-shell-to-go-slack-projects-directory)
                              start-agent-fn)
                   (make-directory project-dir t)
                   (funcall start-agent-fn project-dir)))))))
        ("/new-agent"
         (agent-shell-to-go-slack--start-agent-in-folder folder channel nil))
        ("/new-agent-container"
         (agent-shell-to-go-slack--start-agent-in-folder folder channel t))
        ("/projects"
         (let ((projects (agent-shell-to-go-slack--get-open-projects)))
           (if projects
               (progn
                 (agent-shell-to-go-slack--api-request
                  "POST" "chat.postMessage"
                  `((channel . ,channel)
                    (text . ":file_folder: *Open Projects:*")))
                 (dolist (project projects)
                   (agent-shell-to-go-slack--api-request
                    "POST" "chat.postMessage"
                    `((channel . ,channel)
                      (text . ,project)))))
             (agent-shell-to-go-slack--api-request
              "POST" "chat.postMessage"
              `((channel . ,channel)
                (text . ":shrug: No open projects found"))))))))))

;;; Transport Handlers

(defun agent-shell-to-go-slack--on-session-start (buffer)
  "Handle session start for BUFFER. Return session data."
  (agent-shell-to-go-slack--load-env)
  (agent-shell-to-go-slack--load-channels)
  
  ;; Check credentials
  (if (not (and agent-shell-to-go-slack-bot-token
                agent-shell-to-go-slack-channel-id
                agent-shell-to-go-slack-app-token))
      (progn
        (agent-shell-to-go--debug "Slack credentials missing")
        nil)
    (condition-case err
        (let* ((channel-id (agent-shell-to-go-slack--get-or-create-project-channel buffer))
               (thread-ts (agent-shell-to-go-slack--start-thread (buffer-name buffer) channel-id))
               (session-data `(:thread-ts ,thread-ts
                               :channel-id ,channel-id
                               :current-agent-message ""
                               :uploaded-images ,(make-hash-table :test 'equal)
                               :mentioned-files ,(make-hash-table :test 'equal)
                               :upload-timestamps nil)))
          (when thread-ts
            ;; Store session data first so file watcher can access it
            (agent-shell-to-go--set-transport-data buffer "slack" session-data)
            ;; Start file watcher for auto-uploading images
            (agent-shell-to-go-slack--start-file-watcher buffer)
            ;; Return session data
            session-data))
      (error
       (agent-shell-to-go--debug "Slack session start failed: %s" err)
       nil))))

(defun agent-shell-to-go-slack--on-session-end (buffer session-data)
  "Handle session end for BUFFER with SESSION-DATA."
  ;; Stop file watcher
  (agent-shell-to-go-slack--stop-file-watcher buffer)
  ;; Send farewell message
  (when-let ((thread-ts (plist-get session-data :thread-ts))
             (channel-id (plist-get session-data :channel-id)))
    (agent-shell-to-go-slack--send ":wave: Session ended" thread-ts channel-id)))

(defun agent-shell-to-go-slack--on-user-message (buffer session-data text injecting)
  "Handle user message TEXT in BUFFER."
  (unless injecting  ; Don't echo messages that came from Slack
    (when-let ((thread-ts (plist-get session-data :thread-ts))
               (channel-id (plist-get session-data :channel-id)))
      (agent-shell-to-go-slack--send
       (agent-shell-to-go-slack--format-user-message text)
       thread-ts channel-id)
      (agent-shell-to-go-slack--send
       ":hourglass_flowing_sand: _Processing..._"
       thread-ts channel-id))))

(defun agent-shell-to-go-slack--on-agent-message (buffer session-data text)
  "Handle agent message TEXT in BUFFER."
  (when-let ((thread-ts (plist-get session-data :thread-ts))
             (channel-id (plist-get session-data :channel-id)))
    (agent-shell-to-go-slack--send
     (agent-shell-to-go-slack--format-agent-message text)
     thread-ts channel-id)))

(defun agent-shell-to-go-slack--on-tool-call (buffer session-data update)
  "Handle tool call UPDATE in BUFFER."
  ;; Record any file paths mentioned for image upload filtering
  (dolist (path (agent-shell-to-go-slack--extract-file-paths-from-update update))
    (agent-shell-to-go-slack--record-mentioned-file buffer path))
  
  (when-let ((thread-ts (plist-get session-data :thread-ts))
             (channel-id (plist-get session-data :channel-id)))
    (let* ((title (alist-get 'title update))
           (raw-input (alist-get 'rawInput update))
           (command (alist-get 'command raw-input))
           (file-path (alist-get 'file_path raw-input))
           (query (alist-get 'query raw-input))
           (url (alist-get 'url raw-input))
           ;; Build display - title often already contains file path
           (specific-info (or command file-path query url))
           (title-has-info (and title specific-info
                                (string-match-p (regexp-quote specific-info) title)))
           (display (cond
                     (command command)
                     (title-has-info title)
                     ((and file-path title) (format "%s: %s" title file-path))
                     ((and query title) (format "%s: %s" title query))
                     ((and url title) (format "%s: %s" title url))
                     (specific-info specific-info)
                     (t title)))
           ;; Extract diff if present
           (diff (agent-shell-to-go-slack--extract-diff update))
           (diff-text (and diff
                           (agent-shell-to-go-slack--format-diff-for-slack
                            (car diff) (cdr diff)))))
      (when (and specific-info display)
        (condition-case err
            (if (and diff-text (> (length diff-text) 0))
                (agent-shell-to-go-slack--send
                 (format ":hourglass: `%s`\n```diff\n%s\n```" display diff-text)
                 thread-ts channel-id nil t)
              (agent-shell-to-go-slack--send
               (format ":hourglass: `%s`" display)
               thread-ts channel-id nil t))
          (error
           (agent-shell-to-go--debug "tool_call error: %s" err)
           (agent-shell-to-go-slack--send
            (format ":hourglass: `%s`" display)
            thread-ts channel-id nil t)))))))

(defun agent-shell-to-go-slack--on-tool-result (buffer session-data update)
  "Handle tool result UPDATE in BUFFER."
  ;; Record any file paths mentioned for image upload filtering
  (dolist (path (agent-shell-to-go-slack--extract-file-paths-from-update update))
    (agent-shell-to-go-slack--record-mentioned-file buffer path))
  
  (when-let ((thread-ts (plist-get session-data :thread-ts))
             (channel-id (plist-get session-data :channel-id)))
    (let* ((status (alist-get 'status update))
           (content (alist-get 'content update))
           (content-text (and content
                              (mapconcat
                               (lambda (item)
                                 (or (alist-get 'text (alist-get 'content item))
                                     (alist-get 'text item)
                                     ""))
                               (if (vectorp content) (append content nil)
                                 (if (listp content) content nil))
                               "\n")))
           (output (or (alist-get 'rawOutput update)
                       (alist-get 'output update)
                       content-text))
           (diff (condition-case nil
                     (agent-shell-to-go-slack--extract-diff update)
                   (error nil)))
           (diff-text (and diff
                           (condition-case nil
                               (agent-shell-to-go-slack--format-diff-for-slack
                                (car diff) (cdr diff))
                             (error nil)))))
      (when (member status '("completed" "failed"))
        (let ((status-icon (if (equal status "completed") ":white_check_mark:" ":x:")))
          (cond
           ;; Has diff - show diff
           ((and diff-text (> (length diff-text) 0))
            (let ((full-text (format "%s\n```diff\n%s\n```" status-icon diff-text)))
              (if agent-shell-to-go-slack-show-tool-output
                  (agent-shell-to-go-slack--send full-text thread-ts channel-id nil t)
                ;; Hidden mode: show just icon, save full for expansion
                (let* ((response (agent-shell-to-go-slack--send status-icon thread-ts channel-id))
                       (msg-ts (alist-get 'ts response)))
                  (when msg-ts
                    (agent-shell-to-go-slack--save-truncated-message
                     channel-id msg-ts full-text status-icon))))))
           ;; Has output - show output
           ((and output (stringp output) (> (length output) 0))
            (let ((full-text (format "%s\n```\n%s\n```" status-icon output)))
              (if agent-shell-to-go-slack-show-tool-output
                  (agent-shell-to-go-slack--send full-text thread-ts channel-id nil t)
                ;; Hidden mode: show just icon, save full for expansion
                (let* ((response (agent-shell-to-go-slack--send status-icon thread-ts channel-id))
                       (msg-ts (alist-get 'ts response)))
                  (when msg-ts
                    (agent-shell-to-go-slack--save-truncated-message
                     channel-id msg-ts full-text status-icon))))))
           ;; Neither - just show status
           (t
            (agent-shell-to-go-slack--send status-icon thread-ts channel-id))))))))

(defun agent-shell-to-go-slack--on-status-change (buffer session-data status _detail)
  "Handle status change to STATUS in BUFFER."
  (when (equal status "ready")
    (when-let ((thread-ts (plist-get session-data :thread-ts))
               (channel-id (plist-get session-data :channel-id)))
      (agent-shell-to-go-slack--send ":speech_balloon: _Ready for input_" thread-ts channel-id))))

(defun agent-shell-to-go-slack--on-permission-request (buffer session-data request)
  "Handle permission REQUEST in BUFFER."
  (when-let ((thread-ts (plist-get session-data :thread-ts))
             (channel-id (plist-get session-data :channel-id)))
    (let* ((request-id (alist-get 'id request))
           (params (alist-get 'params request))
           (options (alist-get 'options params))
           (tool-call (alist-get 'toolCall params))
           (title (alist-get 'title tool-call))
           (raw-input (alist-get 'rawInput tool-call))
           (command (and raw-input (alist-get 'command raw-input))))
      (let* ((response (agent-shell-to-go-slack--send
                        (format ":warning: *Permission Required*\n`%s`\n\nReact: :white_check_mark: Allow | :unlock: Always | :x: Reject"
                                (or command title "Unknown action"))
                        thread-ts channel-id))
             (msg-ts (alist-get 'ts response)))
        (when msg-ts
          (push (cons msg-ts
                      (list :request-id request-id
                            :buffer buffer
                            :options options))
                agent-shell-to-go-slack--pending-permissions))))))

;;; WebSocket Management

(defun agent-shell-to-go-slack--websocket-connect ()
  "Connect to Slack via WebSocket."
  (when agent-shell-to-go-slack--websocket
    (setq agent-shell-to-go-slack--intentional-close t)
    (ignore-errors (websocket-close agent-shell-to-go-slack--websocket))
    (setq agent-shell-to-go-slack--intentional-close nil))
  (let ((ws-url (agent-shell-to-go-slack--get-websocket-url)))
    (setq agent-shell-to-go-slack--websocket
          (websocket-open ws-url
                          :on-message (lambda (_ws frame)
                                        (agent-shell-to-go-slack--handle-websocket-message frame))
                          :on-close (lambda (_ws)
                                      (agent-shell-to-go--debug "Slack WebSocket closed")
                                      (unless agent-shell-to-go-slack--intentional-close
                                        (agent-shell-to-go-slack--websocket-reconnect)))
                          :on-error (lambda (_ws _type err)
                                      (agent-shell-to-go--debug "Slack WebSocket error: %s" err))))))

(defun agent-shell-to-go-slack--websocket-reconnect ()
  "Schedule WebSocket reconnection."
  (when agent-shell-to-go-slack--websocket-reconnect-timer
    (cancel-timer agent-shell-to-go-slack--websocket-reconnect-timer))
  (when agent-shell-to-go--active-buffers
    (setq agent-shell-to-go-slack--websocket-reconnect-timer
          (run-with-timer 5 nil #'agent-shell-to-go-slack--websocket-connect))))

(defun agent-shell-to-go-slack--websocket-disconnect ()
  "Disconnect WebSocket."
  (when agent-shell-to-go-slack--websocket-reconnect-timer
    (cancel-timer agent-shell-to-go-slack--websocket-reconnect-timer)
    (setq agent-shell-to-go-slack--websocket-reconnect-timer nil))
  (when agent-shell-to-go-slack--websocket
    (setq agent-shell-to-go-slack--intentional-close t)
    (ignore-errors (websocket-close agent-shell-to-go-slack--websocket))
    (setq agent-shell-to-go-slack--websocket nil)
    (setq agent-shell-to-go-slack--intentional-close nil)))

;;; Cleanup Functions

(defun agent-shell-to-go-slack--get-channel-messages (channel-id &optional limit)
  "Get recent messages from CHANNEL-ID.
LIMIT defaults to 200. Returns list of messages."
  (let* ((response (agent-shell-to-go-slack--api-request
                    "GET"
                    (format "conversations.history?channel=%s&limit=%d"
                            channel-id (or limit 200))))
         (messages (alist-get 'messages response)))
    (when messages
      (append messages nil))))

(defun agent-shell-to-go-slack--delete-message (channel-id ts)
  "Delete message at TS in CHANNEL-ID."
  (agent-shell-to-go-slack--api-request
   "POST" "chat.delete"
   `((channel . ,channel-id)
     (ts . ,ts))))

(defun agent-shell-to-go-slack--thread-active-p (thread-ts)
  "Return non-nil if THREAD-TS belongs to an active buffer."
  (cl-some (lambda (buf)
             (when (buffer-live-p buf)
               (let ((session-data (agent-shell-to-go--get-transport-data buf "slack")))
                 (equal thread-ts (plist-get session-data :thread-ts)))))
           agent-shell-to-go--active-buffers))

(defun agent-shell-to-go-slack--get-agent-shell-threads (channel-id)
  "Get all Agent Shell Session threads from CHANNEL-ID.
Returns list of (thread-ts . last-activity-time) for each thread."
  (let ((messages (agent-shell-to-go-slack--get-channel-messages channel-id 500))
        (threads nil))
    (dolist (msg messages)
      (let ((text (alist-get 'text msg))
            (ts (alist-get 'ts msg))
            (latest-reply (alist-get 'latest_reply msg)))
        (when (and text (string-match-p "Agent Shell Session" text))
          (let ((last-activity (if latest-reply
                                   (string-to-number latest-reply)
                                 (string-to-number ts))))
            (push (cons ts last-activity) threads)))))
    threads))

(defun agent-shell-to-go-slack--cleanup-async (channel-id thread-timestamps)
  "Delete THREAD-TIMESTAMPS from CHANNEL-ID asynchronously."
  (let* ((token agent-shell-to-go-slack-bot-token)
         (script (format "
TOKEN='%s'
CHANNEL='%s'

delete_msg() {
  curl -s -X POST 'https://slack.com/api/chat.delete' \\
    -H \"Authorization: Bearer $TOKEN\" \\
    -H 'Content-Type: application/json' \\
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
(defun agent-shell-to-go-slack-list-threads (&optional channel-id)
  "List all Agent Shell threads in CHANNEL-ID with their age."
  (interactive)
  (agent-shell-to-go-slack--load-env)
  (let* ((channel (or channel-id agent-shell-to-go-slack-channel-id))
         (threads (agent-shell-to-go-slack--get-agent-shell-threads channel))
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
                 (active (agent-shell-to-go-slack--thread-active-p ts))
                 (status (cond
                          (active "[ACTIVE]")
                          ((< age-hours agent-shell-to-go-slack-cleanup-age-hours) "[recent]")
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
(defun agent-shell-to-go-slack-cleanup-old-threads (&optional channel-id dry-run)
  "Delete Agent Shell threads older than `agent-shell-to-go-slack-cleanup-age-hours'.
Skips threads that are currently active.
CHANNEL-ID defaults to `agent-shell-to-go-slack-channel-id'.
With prefix arg or DRY-RUN non-nil, just report what would be deleted."
  (interactive (list nil current-prefix-arg))
  (agent-shell-to-go-slack--load-env)
  (let* ((channel (or channel-id agent-shell-to-go-slack-channel-id))
         (threads (agent-shell-to-go-slack--get-agent-shell-threads channel))
         (now (float-time))
         (age-threshold (* agent-shell-to-go-slack-cleanup-age-hours 3600))
         (to-delete nil)
         (skipped-active 0)
         (skipped-recent 0))
    (dolist (thread threads)
      (let* ((ts (car thread))
             (last-activity (cdr thread))
             (age (- now last-activity)))
        (cond
         ((agent-shell-to-go-slack--thread-active-p ts)
          (cl-incf skipped-active))
         ((< age age-threshold)
          (cl-incf skipped-recent))
         (t
          (push thread to-delete)))))
    (if (not to-delete)
        (message "No threads to clean up (skipped %d active, %d recent)"
                 skipped-active skipped-recent)
      (if dry-run
          (progn
            (message "Would delete %d threads (skipping %d active, %d recent)"
                     (length to-delete) skipped-active skipped-recent)
            (agent-shell-to-go-slack-list-threads channel))
        (message "Deleting %d threads (skipping %d active, %d recent)..."
                 (length to-delete) skipped-active skipped-recent)
        (agent-shell-to-go-slack--cleanup-async channel (mapcar #'car to-delete))))))

;;;###autoload
(defun agent-shell-to-go-slack-cleanup-all-channels (&optional dry-run)
  "Clean up old threads in all known project channels.
With prefix arg or DRY-RUN non-nil, just report what would be deleted."
  (interactive "P")
  (agent-shell-to-go-slack--load-env)
  (agent-shell-to-go-slack--load-channels)
  (let ((channels (list agent-shell-to-go-slack-channel-id)))
    (maphash (lambda (_k v) (cl-pushnew v channels :test #'equal))
             agent-shell-to-go-slack--project-channels)
    (dolist (channel channels)
      (message "Checking channel %s..." channel)
      (agent-shell-to-go-slack-cleanup-old-threads channel dry-run))))

;;; Transport Definition

(defvar agent-shell-to-go-slack--transport
  '(:name "slack"
    :init agent-shell-to-go-slack--init
    :cleanup agent-shell-to-go-slack--cleanup
    :on-session-start agent-shell-to-go-slack--on-session-start
    :on-session-end agent-shell-to-go-slack--on-session-end
    :on-user-message agent-shell-to-go-slack--on-user-message
    :on-agent-message agent-shell-to-go-slack--on-agent-message
    :on-tool-call agent-shell-to-go-slack--on-tool-call
    :on-tool-result agent-shell-to-go-slack--on-tool-result
    :on-status-change agent-shell-to-go-slack--on-status-change
    :on-permission-request agent-shell-to-go-slack--on-permission-request)
  "Slack transport definition.")

(defun agent-shell-to-go-slack--init ()
  "Initialize Slack transport."
  (agent-shell-to-go-slack--load-env)
  ;; Auto-populate authorized-users from user-id if not set
  (when (and agent-shell-to-go-slack-user-id
             (not agent-shell-to-go-slack-authorized-users))
    (setq agent-shell-to-go-slack-authorized-users
          (list agent-shell-to-go-slack-user-id)))
  (when (and agent-shell-to-go-slack-bot-token
             agent-shell-to-go-slack-app-token)
    (condition-case err
        (agent-shell-to-go-slack--websocket-connect)
      (error
       (agent-shell-to-go--debug "Slack WebSocket init failed: %s" err)))))

(defun agent-shell-to-go-slack--cleanup ()
  "Cleanup Slack transport."
  (agent-shell-to-go-slack--websocket-disconnect))

;;;###autoload
(defun agent-shell-to-go-slack-setup ()
  "Enable the Slack transport."
  (interactive)
  (agent-shell-to-go-register-transport agent-shell-to-go-slack--transport)
  (message "agent-shell-to-go: Slack transport enabled"))

(provide 'agent-shell-to-go-slack)

;;; agent-shell-to-go-slack.el ends here
