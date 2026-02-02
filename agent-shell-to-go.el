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
  "Slack channel ID to post to. Loaded from .env if nil."
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

;;; Slack API

(defun agent-shell-to-go--api-request (method endpoint &optional data)
  "Make a Slack API request.
METHOD is GET or POST, ENDPOINT is the API endpoint, DATA is the payload."
  (let* ((url-request-method method)
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " agent-shell-to-go-bot-token))
            ("Content-Type" . "application/json")))
         (url-request-data (when data (encode-coding-string (json-encode data) 'utf-8)))
         (url (concat "https://slack.com/api/" endpoint)))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (let ((response (json-read)))
        (kill-buffer)
        response))))

(defun agent-shell-to-go--send (text &optional thread-ts)
  "Send TEXT to Slack, optionally in THREAD-TS thread."
  (let ((data `((channel . ,agent-shell-to-go-channel-id)
                (text . ,text))))
    (when thread-ts
      (push `(thread_ts . ,thread-ts) data))
    (agent-shell-to-go--api-request "POST" "chat.postMessage" data)))

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
  "Truncate TEXT to MAX-LEN (default 3000) for Slack."
  (let ((max-len (or max-len 3000)))
    (if (> (length text) max-len)
        (concat (substring text 0 max-len) "\n... (truncated)")
      text)))

(defun agent-shell-to-go--format-user-message (prompt)
  "Format user PROMPT for Slack."
  (format ":bust_in_silhouette: *User*\n%s"
          (agent-shell-to-go--truncate-message prompt)))

(defun agent-shell-to-go--format-agent-message (text)
  "Format agent TEXT for Slack."
  (format ":robot_face: *Agent*\n%s"
          (agent-shell-to-go--truncate-message text)))

(defun agent-shell-to-go--format-tool-call (title status &optional output)
  "Format tool call with TITLE, STATUS, and optional OUTPUT for Slack."
  (let ((emoji (pcase status
                 ("completed" ":white_check_mark:")
                 ("failed" ":x:")
                 ("pending" ":hourglass:")
                 (_ ":wrench:"))))
    (if (and output (not (string-empty-p output)))
        (format "%s `%s`\n```\n%s\n```"
                emoji
                (agent-shell-to-go--truncate-message title 200)
                (agent-shell-to-go--truncate-message output 1500))
      (format "%s `%s`" emoji (agent-shell-to-go--truncate-message title 200)))))

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

(defun agent-shell-to-go--find-buffer-for-thread (thread-ts)
  "Find the agent-shell buffer that corresponds to THREAD-TS."
  (cl-find-if
   (lambda (buf)
     (and (buffer-live-p buf)
          (equal thread-ts
                 (buffer-local-value 'agent-shell-to-go--thread-ts buf))))
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
    (message "agent-shell-to-go: websocket message type: %s" type)
    (pcase type
      ("events_api"
       (agent-shell-to-go--handle-event (alist-get 'payload data)))
      ("slash_commands"
       (message "agent-shell-to-go: got slash_commands payload: %s" (alist-get 'payload data))
       (agent-shell-to-go--handle-slash-command (alist-get 'payload data)))
      ("hello"
       (message "agent-shell-to-go: WebSocket connected"))
      ("disconnect"
       (message "agent-shell-to-go: WebSocket disconnect requested, reconnecting...")
       (agent-shell-to-go--websocket-reconnect)))))

(defun agent-shell-to-go--handle-event (payload)
  "Handle Slack event PAYLOAD."
  (let* ((event (alist-get 'event payload))
         (event-type (alist-get 'type event)))
    (message "agent-shell-to-go: received event type: %s" event-type)
    (pcase event-type
      ("message"
       (agent-shell-to-go--handle-message-event event))
      ("reaction_added"
       (message "agent-shell-to-go: reaction event: %s" event)
       (agent-shell-to-go--handle-reaction-event event)))))

(defun agent-shell-to-go--handle-message-event (event)
  "Handle a message EVENT from Slack."
  (let* ((thread-ts (alist-get 'thread_ts event))
         (user (alist-get 'user event))
         (text (alist-get 'text event))
         (subtype (alist-get 'subtype event))
         (bot-id (alist-get 'bot_id event))
         (buffer (and thread-ts (agent-shell-to-go--find-buffer-for-thread thread-ts))))
    ;; Only handle real user messages in threads we're tracking
    (when (and buffer
               text
               (not subtype)
               (not bot-id)
               (not (equal user (agent-shell-to-go--get-bot-user-id))))
      (with-current-buffer buffer
        (if (string-prefix-p "!" text)
            (agent-shell-to-go--handle-command text buffer thread-ts)
          (message "agent-shell-to-go: received from Slack: %s" text)
          (agent-shell-to-go--inject-message text))))))

(defun agent-shell-to-go--handle-reaction-event (event)
  "Handle a reaction EVENT from Slack."
  (let* ((item (alist-get 'item event))
         (msg-ts (alist-get 'ts item))
         (reaction (alist-get 'reaction event))
         (pending (assoc msg-ts agent-shell-to-go--pending-permissions)))
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
  (message "agent-shell-to-go: starting agent in %s (container: %s)" folder use-container)
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
             (message "agent-shell-to-go: error starting agent: %s" err)))))
    (message "agent-shell-to-go: folder does not exist: %s" folder)))

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

(defun agent-shell-to-go--handle-slash-command (payload)
  "Handle a slash command PAYLOAD from Slack."
  (let* ((command (alist-get 'command payload))
         (text (alist-get 'text payload))
         (channel (alist-get 'channel_id payload))
         (folder (expand-file-name
                  (if (and text (not (string-empty-p text)))
                      text
                    agent-shell-to-go-default-folder))))
    (message "agent-shell-to-go: slash command: %s %s" command text)
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
                                      (message "agent-shell-to-go: WebSocket closed")
                                      (unless agent-shell-to-go--intentional-close
                                        (agent-shell-to-go--websocket-reconnect)))
                          :on-error (lambda (_ws _type err)
                                      (message "agent-shell-to-go: WebSocket error: %s" err))))))

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
             ;; Tool call starting - show the command
             (let* ((title (alist-get 'title update))
                    (raw-input (alist-get 'rawInput update))
                    (command (alist-get 'command raw-input))
                    (display-title (or command title)))
               (when display-title
                 (agent-shell-to-go--send
                  (format ":hourglass: `%s`" 
                          (agent-shell-to-go--truncate-message display-title 500))
                  thread-ts))))
            ("tool_call_update"
             ;; Tool call completed - show output
             (let* ((status (alist-get 'status update))
                    (output (or (alist-get 'rawOutput update)
                                (alist-get 'output update))))
               (when (and (member status '("completed" "failed")) output)
                 (agent-shell-to-go--send
                  (format "%s\n```\n%s\n```"
                          (if (equal status "completed") ":white_check_mark:" ":x:")
                          (agent-shell-to-go--truncate-message output 1500))
                  thread-ts))))))))
    (apply orig-fn args)))

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
       (with-current-buffer buffer
         (agent-shell-interrupt))
       (agent-shell-to-go--send ":stop_sign: Agent interrupted" thread-ts)
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

  (unless agent-shell-to-go-bot-token
    (error "agent-shell-to-go-bot-token not set. See agent-shell-to-go-env-file"))
  (unless agent-shell-to-go-channel-id
    (error "agent-shell-to-go-channel-id not set. See agent-shell-to-go-env-file"))
  (unless agent-shell-to-go-app-token
    (error "agent-shell-to-go-app-token not set. See agent-shell-to-go-env-file"))

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

  (message "agent-shell-to-go: mirroring to Slack thread %s" agent-shell-to-go--thread-ts))

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

  (message "agent-shell-to-go: mirroring disabled"))

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
