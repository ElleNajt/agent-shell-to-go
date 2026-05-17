;;; agent-shell-to-go-discord-test.el --- Tests for agent-shell-to-go-discord.el -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for the Discord transport implementation (agent-shell-to-go-discord.el).
;; Uses a mocked Discord REST API and a fake WebSocket — no real Discord backend required.
;;
;; Run:
;;   make test TEST=agent-shell-to-go-test-discord.el
;;
;; APIs under test:
;;
;;   agent-shell-to-go--discord-emoji-to-action
;;     - emoji-to-action-known: registered emoji names map to canonical actions
;;     - emoji-to-action-unknown: unknown or nil emoji names return nil
;;
;;   agent-shell-to-go--discord-truncate-content
;;     - truncate-content-short: short text passes through unchanged
;;     - truncate-content-long: text over 2000 chars is cut with a note
;;     - truncate-content-at-limit: text at exactly the limit is not truncated
;;
;;   agent-shell-to-go--discord-message-seen-p
;;     - message-seen-first-time: first call returns nil
;;     - message-seen-second-time: second call for the same ID returns t
;;     - message-seen-independent-ids: different IDs are tracked independently
;;
;;   agent-shell-to-go--discord-resolve-channel
;;     - resolve-channel-top-level: channel with no parent resolves as (channel . nil)
;;     - resolve-channel-forum-post: registered thread resolves as (parent . thread)
;;
;;   agent-shell-to-go-transport-authorized-p
;;     - authorized-in-list: users in the list are authorized
;;     - authorized-not-in-list: users not in the list are denied
;;     - authorized-empty-list: empty list denies everyone
;;
;;   agent-shell-to-go-transport-format-tool-call-start
;;     - format-tool-call-start: output contains the tool name
;;
;;   agent-shell-to-go-transport-format-tool-call-result
;;     - format-tool-call-result-completed: includes tool name and output in a code block
;;     - format-tool-call-result-failed: includes the failed icon
;;     - format-tool-call-result-no-output: nil output omits the code block
;;
;;   agent-shell-to-go-transport-format-diff
;;     - format-diff-empty: identical text yields empty string
;;     - format-diff-has-changes: changed text yields a ```diff block
;;
;;   agent-shell-to-go-transport-format-user-message
;;     - format-user-message: output contains the message text
;;
;;   agent-shell-to-go-transport-format-agent-message
;;     - format-agent-message: output contains the message text
;;
;;   agent-shell-to-go-transport-format-markdown
;;     - format-markdown-passthrough: returns input unchanged (Discord uses CommonMark natively)
;;
;;   agent-shell-to-go--discord-dispatch-event
;;     - dispatch-ready-sets-state: READY caches bot user ID and session ID
;;     - dispatch-message-fires-hook: MESSAGE_CREATE fires message hook for authorized users
;;     - dispatch-reaction-add-fires-hook: MESSAGE_REACTION_ADD fires reaction hook with added-p t
;;     - dispatch-reaction-remove-fires-hook: MESSAGE_REACTION_REMOVE fires hook with added-p nil
;;
;;   agent-shell-to-go--discord-normalize-message
;;     - normalize-message-ignores-bot: author.bot = t or user-id matches bot-uid is silently dropped
;;     - normalize-message-ignores-unauthorized: unauthorized users are dropped
;;     - normalize-message-deduplicates: same message ID fires hook only once
;;     - normalize-message-resolves-thread: thread channel reports parent forum as :channel-id
;;
;;   agent-shell-to-go--discord-normalize-reaction
;;     - normalize-reaction-ignores-own-bot: own bot's reactions are dropped
;;     - normalize-reaction-unknown-emoji-still-fires: unknown emoji fires hook with nil :action
;;
;;   agent-shell-to-go--discord-handle-frame
;;     - gateway-hello: op 10 starts heartbeat with correct interval and sends identify
;;     - gateway-heartbeat-ack: op 11 produces no sends
;;     - gateway-heartbeat-request: op 1 sends heartbeat payload back
;;     - gateway-invalid-session: op 9 schedules re-identify after 5s
;;     - gateway-dispatch-calls-defer: op 0 defers dispatch-event call
;;     - gateway-dispatch-updates-sequence: op 0 updates sequence number
;;
;;   agent-shell-to-go-transport-send-text
;;     - send-text-returns-message-id: returns the message ID from the API
;;     - send-text-uses-thread-id-as-target: posts to thread-id when provided
;;     - send-text-uses-channel-when-no-thread: posts to channel-id when thread is nil
;;     - send-text-truncated-saves-full-text: :truncate saves full text for later expansion
;;
;;   agent-shell-to-go-transport-edit-message
;;     - edit-message: sends PATCH and returns the message ID
;;
;;   agent-shell-to-go-transport-start-thread
;;     - start-thread-returns-id: returns the thread ID from the API
;;     - start-thread-records-parent: registers thread→forum mapping for inbound routing
;;
;;   agent-shell-to-go-transport-update-thread-header
;;     - update-thread-header: sends PATCH to the thread channel endpoint
;;     - update-thread-header-truncates-long-title: titles over 100 chars are truncated
;;
;;   agent-shell-to-go-transport-delete-message
;;     - delete-message: sends DELETE to the message endpoint
;;
;;   agent-shell-to-go-transport-delete-thread
;;     - delete-thread: sends DELETE to the thread channel endpoint
;;
;;   agent-shell-to-go-transport-fetch-thread-replies
;;     - fetch-thread-replies: returns plists in chronological order
;;
;;   agent-shell-to-go-transport-get-message-text
;;     - get-message-text: returns the content field from the API
;;
;;   agent-shell-to-go-transport-get-reactions
;;     - get-reactions-returns-nil: always nil; reactions arrive via Gateway
;;
;;   agent-shell-to-go-transport-upload-file
;;     - upload-file-skips-missing-file: does nothing when the path does not exist
;;     - upload-file-uses-thread-target: posts to thread-id when provided
;;     - upload-file-uses-channel-fallback: posts to channel-id when thread is nil
;;
;;   agent-shell-to-go--discord-save-channels / agent-shell-to-go--discord-load-channels
;;     - save-channels: writes project→channel map to disk as an alist
;;     - load-channels: reads alist from disk into the transport hash
;;     - channels-round-trip: save+load in a fresh transport preserves all mappings
;;
;;   agent-shell-to-go--discord-get-or-create-project-channel
;;     - get-or-create-channel-cache-hit: cached ID returned without any API call
;;     - get-or-create-channel-found-by-name: finds existing forum channel by name
;;     - get-or-create-channel-creates-new: creates forum channel when none is found

;;; Code:

(require 'ert)

(require 'agent-shell-to-go-discord)
(require 'gateway-helpers)

; Test helpers

(defun agent-shell-to-go-test-discord--make ()
  "Return a fresh Discord transport with a known bot-user-id cached."
  (let ((tr (agent-shell-to-go--make-discord-transport :name 'discord)))
    (setf (agent-shell-to-go-discord-transport-bot-user-id-cache tr) "BOT123")
    tr))

(defun agent-shell-to-go-test-discord--make-with-ws ()
  "Return a fresh Discord transport with a fake WebSocket wired up.
The fake socket satisfies websocket-openp when stubbed."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (dummy-socket (list 'fake-discord-ws))
         (ws
          (agent-shell-to-go--ws-make
           :name 'discord-test
           :url-fn (lambda () "wss://test")
           :on-frame (lambda (_) nil))))
    (setf (agent-shell-to-go--ws-websocket ws) dummy-socket)
    (setf (agent-shell-to-go-discord-transport-ws tr) ws)
    tr))

(defmacro with-mocked-discord-api (responses &rest body)
  "Execute BODY with `agent-shell-to-go--discord-request' mocked.
RESPONSES is an alist keyed by (METHOD . ENDPOINT); unmatched calls return nil."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-shell-to-go--discord-request)
              (lambda (method endpoint &rest _extra)
                (cdr (assoc (cons method endpoint) ,responses)))))
     ,@body))

(defmacro with-discord-temp-storage (&rest body)
  "Execute BODY with `agent-shell-to-go-storage-base-dir' bound to a temp dir."
  (declare (indent 0))
  `(let* ((tmpdir (make-temp-file "astg-discord-storage" t))
          (agent-shell-to-go-storage-base-dir tmpdir))
     (unwind-protect
         (progn
           ,@body)
       (delete-directory tmpdir t))))

; 1. Pure helpers

;; Emoji-to-action mapping

(ert-deftest agent-shell-to-go-test-discord-emoji-to-action-known ()
  "Registered emoji names map to the correct canonical action."
  (should (eq 'hide (agent-shell-to-go--discord-emoji-to-action "see_no_evil")))
  (should (eq 'hide (agent-shell-to-go--discord-emoji-to-action "no_bell")))
  (should (eq 'expand-truncated (agent-shell-to-go--discord-emoji-to-action "eyes")))
  (should (eq 'expand-full (agent-shell-to-go--discord-emoji-to-action "open_book")))
  (should (eq 'expand-full (agent-shell-to-go--discord-emoji-to-action "green_book")))
  (should
   (eq
    'permission-allow (agent-shell-to-go--discord-emoji-to-action "white_check_mark")))
  (should
   (eq 'permission-allow (agent-shell-to-go--discord-emoji-to-action "thumbsup")))
  (should (eq 'permission-always (agent-shell-to-go--discord-emoji-to-action "unlock")))
  (should (eq 'permission-always (agent-shell-to-go--discord-emoji-to-action "star")))
  (should (eq 'permission-reject (agent-shell-to-go--discord-emoji-to-action "x")))
  (should
   (eq 'permission-reject (agent-shell-to-go--discord-emoji-to-action "thumbsdown"))))

(ert-deftest agent-shell-to-go-test-discord-emoji-to-action-unknown ()
  "Unknown or nil emoji names return nil."
  (should (null (agent-shell-to-go--discord-emoji-to-action "unknown_emoji")))
  (should (null (agent-shell-to-go--discord-emoji-to-action "")))
  (should (null (agent-shell-to-go--discord-emoji-to-action nil))))

;; Content truncation

(ert-deftest agent-shell-to-go-test-discord-truncate-content-short ()
  "Short text passes through unchanged."
  (should (equal "hello" (agent-shell-to-go--discord-truncate-content "hello")))
  (should (equal "" (agent-shell-to-go--discord-truncate-content ""))))

(ert-deftest agent-shell-to-go-test-discord-truncate-content-long ()
  "Text over 2000 chars is cut and a truncation note appended."
  (let* ((text (make-string 2100 ?a))
         (result (agent-shell-to-go--discord-truncate-content text)))
    (should (<= (length result) agent-shell-to-go--discord-max-content-length))
    (should (string-match-p "truncated" result))))

(ert-deftest agent-shell-to-go-test-discord-truncate-content-at-limit ()
  "Text exactly at the limit is not truncated."
  (let ((text (make-string agent-shell-to-go--discord-max-content-length ?b)))
    (should (equal text (agent-shell-to-go--discord-truncate-content text)))))

;; Deduplication

(ert-deftest agent-shell-to-go-test-discord-message-seen-first-time ()
  "A message ID is not seen on first call."
  (let ((tr (agent-shell-to-go-test-discord--make)))
    (should (null (agent-shell-to-go--discord-message-seen-p tr "msg-1")))))

(ert-deftest agent-shell-to-go-test-discord-message-seen-second-time ()
  "The same message ID returns t on the second call."
  (let ((tr (agent-shell-to-go-test-discord--make)))
    (agent-shell-to-go--discord-message-seen-p tr "msg-dup")
    (should (eq t (agent-shell-to-go--discord-message-seen-p tr "msg-dup")))))

(ert-deftest agent-shell-to-go-test-discord-message-seen-independent-ids ()
  "Different IDs are tracked independently."
  (let ((tr (agent-shell-to-go-test-discord--make)))
    (agent-shell-to-go--discord-message-seen-p tr "msg-a")
    (should (null (agent-shell-to-go--discord-message-seen-p tr "msg-b")))
    (should (eq t (agent-shell-to-go--discord-message-seen-p tr "msg-a")))))

;; Channel resolution

(ert-deftest agent-shell-to-go-test-discord-resolve-channel-top-level ()
  "A channel with no registered parent resolves as (channel . nil)."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (result (agent-shell-to-go--discord-resolve-channel tr "CHAN1")))
    (should (equal "CHAN1" (car result)))
    (should (null (cdr result)))))

(ert-deftest agent-shell-to-go-test-discord-resolve-channel-forum-post ()
  "A thread channel with a registered parent resolves as (parent . thread)."
  (let ((tr (agent-shell-to-go-test-discord--make)))
    (puthash "THREAD1" "FORUM1" (agent-shell-to-go-discord-transport-thread-parents tr))
    (let ((result (agent-shell-to-go--discord-resolve-channel tr "THREAD1")))
      (should (equal "FORUM1" (car result)))
      (should (equal "THREAD1" (cdr result))))))

(ert-deftest agent-shell-to-go-test-discord-resolve-channel-api-fallback-thread ()
  "A thread not in thread-parents falls back to the API and resolves as (parent . thread)."
  (let ((tr (agent-shell-to-go-test-discord--make)))
    (with-mocked-discord-api
        (list (cons '("GET" . "/channels/THREAD1")
                    '((type . 11) (parent_id . "FORUM1"))))
      (let ((result (agent-shell-to-go--discord-resolve-channel tr "THREAD1")))
        (should (equal "FORUM1" (car result)))
        (should (equal "THREAD1" (cdr result)))
        ;; Result should be cached in thread-parents for subsequent calls
        (should (equal "FORUM1"
                       (gethash "THREAD1"
                                (agent-shell-to-go-discord-transport-thread-parents tr))))))))

(ert-deftest agent-shell-to-go-test-discord-resolve-channel-api-fallback-non-thread ()
  "A non-thread channel not in thread-parents falls back to the API and resolves as (channel . nil)."
  (let ((tr (agent-shell-to-go-test-discord--make)))
    (with-mocked-discord-api
        (list (cons '("GET" . "/channels/CHAN1")
                    '((type . 0) (parent_id . nil))))
      (let ((result (agent-shell-to-go--discord-resolve-channel tr "CHAN1")))
        (should (equal "CHAN1" (car result)))
        (should (null (cdr result)))))))

;; Authorization

(ert-deftest agent-shell-to-go-test-discord-authorized-in-list ()
  "Users in the authorized list are authorized."
  (let ((agent-shell-to-go-discord-authorized-users '("U1" "U2"))
        (tr (agent-shell-to-go-test-discord--make)))
    (should (agent-shell-to-go-transport-authorized-p tr "U1"))
    (should (agent-shell-to-go-transport-authorized-p tr "U2"))))

(ert-deftest agent-shell-to-go-test-discord-authorized-not-in-list ()
  "A user not in the authorized list is not authorized."
  (let ((agent-shell-to-go-discord-authorized-users '("U1"))
        (tr (agent-shell-to-go-test-discord--make)))
    (should (null (agent-shell-to-go-transport-authorized-p tr "STRANGER")))))

(ert-deftest agent-shell-to-go-test-discord-authorized-empty-list ()
  "When the authorized list is nil, no one is authorized."
  (let ((agent-shell-to-go-discord-authorized-users nil)
        (tr (agent-shell-to-go-test-discord--make)))
    (should (null (agent-shell-to-go-transport-authorized-p tr "U1")))))

; 2. Formatting

(ert-deftest agent-shell-to-go-test-discord-format-tool-call-start ()
  "Tool call start contains the title."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (s (agent-shell-to-go-transport-format-tool-call-start tr "read_file")))
    (should (string-match-p "read_file" s))))

(ert-deftest agent-shell-to-go-test-discord-format-tool-call-result-completed ()
  "Completed result includes tool name and output in a code block."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (s
          (agent-shell-to-go-transport-format-tool-call-result
           tr "bash" 'completed "output here")))
    (should (string-match-p "bash" s))
    (should (string-match-p "output here" s))
    (should (string-match-p "```" s))))

(ert-deftest agent-shell-to-go-test-discord-format-tool-call-result-failed ()
  "Failed result includes the X emoji shortcode."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (s
          (agent-shell-to-go-transport-format-tool-call-result
           tr "bash" 'failed "err")))
    (should (string-match-p "❌" s))
    (should (string-match-p "err" s))))

(ert-deftest agent-shell-to-go-test-discord-format-tool-call-result-no-output ()
  "Result with nil output omits the code block."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (s
          (agent-shell-to-go-transport-format-tool-call-result
           tr "bash" 'completed nil)))
    (should (string-match-p "bash" s))
    (should (not (string-match-p "```" s)))))

(ert-deftest agent-shell-to-go-test-discord-format-diff-empty ()
  "Identical old and new text yields an empty string."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (s (agent-shell-to-go-transport-format-diff tr "same" "same")))
    (should (equal "" s))))

(ert-deftest agent-shell-to-go-test-discord-format-diff-has-changes ()
  "Different old and new text yields a ```diff fenced block."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (s (agent-shell-to-go-transport-format-diff tr "old line" "new line")))
    (should (string-match-p "```diff" s))))

(ert-deftest agent-shell-to-go-test-discord-format-user-message ()
  "User message format contains the text."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (s (agent-shell-to-go-transport-format-user-message tr "hello there")))
    (should (string-match-p "hello there" s))))

(ert-deftest agent-shell-to-go-test-discord-format-agent-message ()
  "Agent message format contains the text."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (s (agent-shell-to-go-transport-format-agent-message tr "I am a robot")))
    (should (string-match-p "I am a robot" s))))

(ert-deftest agent-shell-to-go-test-discord-format-markdown-passthrough ()
  "Markdown formatter returns its input unchanged (Discord uses CommonMark natively)."
  (let ((tr (agent-shell-to-go-test-discord--make)))
    (should
     (equal
      "**bold** _italic_"
      (agent-shell-to-go-transport-format-markdown tr "**bold** _italic_")))))

; 3. Normalization (via dispatch-event and normalize-*)

;; READY

(ert-deftest agent-shell-to-go-test-discord-dispatch-ready-sets-state ()
  "READY event caches the bot user ID and session ID on the transport."
  (let ((tr (agent-shell-to-go-test-discord--make)))
    (agent-shell-to-go--discord-dispatch-event
     tr "READY"
     '((user . ((id . "NEW-BOT"))) (session_id . "SID123")))
    (should
     (equal "NEW-BOT" (agent-shell-to-go-discord-transport-bot-user-id-cache tr)))
    (should (equal "SID123" (agent-shell-to-go-discord-transport-session-id tr)))))

;; MESSAGE_CREATE

(ert-deftest agent-shell-to-go-test-discord-dispatch-message-fires-hook ()
  "MESSAGE_CREATE fires the message hook for authorized users."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (agent-shell-to-go-discord-authorized-users '("USER1"))
         (received nil)
         (agent-shell-to-go-message-hook
          (list (lambda (&rest plist) (setq received plist)))))
    (agent-shell-to-go--discord-dispatch-event
     tr "MESSAGE_CREATE"
     '((id . "M1")
       (channel_id . "C1")
       (author . ((id . "USER1") (bot . :json-false)))
       (content . "hello")))
    (should received)
    (should (equal "hello" (plist-get received :text)))
    (should (equal "USER1" (plist-get received :user)))))

(ert-deftest agent-shell-to-go-test-discord-normalize-message-ignores-bot ()
  "Messages from bots or the cached bot user ID are silently dropped."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (agent-shell-to-go-discord-authorized-users '("USER1" "BOT123"))
         (received nil)
         (agent-shell-to-go-message-hook (list (lambda (plist) (setq received plist)))))
    (ert-info ("author.bot = t")
      (agent-shell-to-go--discord-normalize-message
       tr
       '((id . "M1")
         (channel_id . "C1")
         (author . ((id . "USER1") (bot . t)))
         (content . "bot msg")))
      (should (null received)))
    (ert-info ("user-id matches cached bot-uid")
      (agent-shell-to-go--discord-normalize-message
       tr
       '((id . "M2")
         (channel_id . "C1")
         (author . ((id . "BOT123") (bot . :json-false)))
         (content . "echo")))
      (should (null received)))))

(ert-deftest agent-shell-to-go-test-discord-normalize-message-ignores-unauthorized ()
  "Messages from unauthorized users are dropped."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (agent-shell-to-go-discord-authorized-users '("ALLOWED"))
         (received nil)
         (agent-shell-to-go-message-hook (list (lambda (plist) (setq received plist)))))
    (agent-shell-to-go--discord-normalize-message
     tr
     '((id . "M1")
       (channel_id . "C1")
       (author . ((id . "STRANGER") (bot . :json-false)))
       (content . "intruder")))
    (should (null received))))

(ert-deftest agent-shell-to-go-test-discord-normalize-message-deduplicates ()
  "The message hook fires only once for a given message ID."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (agent-shell-to-go-discord-authorized-users '("USER1"))
         (count 0)
         (agent-shell-to-go-message-hook
          (list (lambda (&rest _plist) (setq count (1+ count)))))
         (payload
          '((id . "DUP-ID")
            (channel_id . "C1")
            (author . ((id . "USER1") (bot . :json-false)))
            (content . "dup"))))
    (agent-shell-to-go--discord-normalize-message tr payload)
    (agent-shell-to-go--discord-normalize-message tr payload)
    (should (= 1 count))))

(ert-deftest agent-shell-to-go-test-discord-normalize-message-resolves-thread ()
  "Messages in a registered thread channel report the parent forum as :channel-id."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (agent-shell-to-go-discord-authorized-users '("USER1"))
         (received nil)
         (agent-shell-to-go-message-hook
          (list (lambda (&rest plist) (setq received plist)))))
    (puthash "THREAD1" "FORUM1" (agent-shell-to-go-discord-transport-thread-parents tr))
    (agent-shell-to-go--discord-normalize-message
     tr
     '((id . "M2")
       (channel_id . "THREAD1")
       (author . ((id . "USER1") (bot . :json-false)))
       (content . "in thread")))
    (should (equal "FORUM1" (plist-get received :channel-id)))
    (should (equal "THREAD1" (plist-get received :thread-id)))))

;; MESSAGE_REACTION_ADD / REMOVE

(ert-deftest agent-shell-to-go-test-discord-dispatch-reaction-add-fires-hook ()
  "MESSAGE_REACTION_ADD fires the reaction hook with added-p t."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (agent-shell-to-go-discord-authorized-users '("USER1"))
         (received nil)
         (agent-shell-to-go-reaction-hook
          (list (lambda (&rest plist) (setq received plist)))))
    (agent-shell-to-go--discord-dispatch-event
     tr "MESSAGE_REACTION_ADD"
     '((channel_id . "C1")
       (message_id . "M1")
       (user_id . "USER1")
       (emoji . ((name . "eyes")))))
    (should received)
    (should (eq 'expand-truncated (plist-get received :action)))
    (should (eq t (plist-get received :added-p)))))

(ert-deftest agent-shell-to-go-test-discord-dispatch-reaction-remove-fires-hook ()
  "MESSAGE_REACTION_REMOVE fires the reaction hook with added-p nil."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (agent-shell-to-go-discord-authorized-users '("USER1"))
         (received nil)
         (agent-shell-to-go-reaction-hook
          (list (lambda (&rest plist) (setq received plist)))))
    (agent-shell-to-go--discord-dispatch-event
     tr "MESSAGE_REACTION_REMOVE"
     '((channel_id . "C1")
       (message_id . "M1")
       (user_id . "USER1")
       (emoji . ((name . "eyes")))))
    (should received)
    (should (null (plist-get received :added-p)))))

(ert-deftest agent-shell-to-go-test-discord-normalize-reaction-ignores-own-bot ()
  "Reactions from the transport's own bot user ID are dropped."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (agent-shell-to-go-discord-authorized-users '("BOT123"))
         (received nil)
         (agent-shell-to-go-reaction-hook
          (list (lambda (plist) (setq received plist)))))
    (agent-shell-to-go--discord-normalize-reaction
     tr
     '((channel_id . "C1")
       (message_id . "M1")
       (user_id . "BOT123")
       (emoji . ((name . "heart"))))
     t)
    (should (null received))))

(ert-deftest agent-shell-to-go-test-discord-normalize-reaction-unknown-emoji-still-fires
    ()
  "Unknown emoji still fires the hook with nil :action and raw-emoji set."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (agent-shell-to-go-discord-authorized-users '("USER1"))
         (received nil)
         (agent-shell-to-go-reaction-hook
          (list (lambda (&rest plist) (setq received plist)))))
    (agent-shell-to-go--discord-normalize-reaction
     tr
     '((channel_id . "C1")
       (message_id . "M1")
       (user_id . "USER1")
       (emoji . ((name . "dancing_penguin"))))
     t)
    (should received)
    (should (null (plist-get received :action)))
    (should (equal "dancing_penguin" (plist-get received :raw-emoji)))))


; 4. Gateway (handle-frame opcode routing)

(ert-deftest agent-shell-to-go-test-discord-gateway-hello ()
  "Opcode 10 (hello): starts heartbeat with the correct interval and sends identify."
  (let* ((tr (agent-shell-to-go-test-discord--make-with-ws))
         (agent-shell-to-go-discord-bot-token "Bot test-token")
         (captured-interval nil)
         (ws-sends nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--discord-start-heartbeat)
               (lambda (_tr interval) (setq captured-interval interval)))
              ((symbol-function 'websocket-openp) (lambda (_) t))
              ((symbol-function 'websocket-send-text)
               (lambda (_ws text) (push text ws-sends))))
      (agent-shell-to-go--discord-handle-frame
       tr
       (agent-shell-to-go-test--make-fake-frame
        (json-encode
         `((op . ,agent-shell-to-go--discord-op-hello)
           (d . ((heartbeat_interval . 41250))))))))
    (should (= 41250 captured-interval))
    (should (= 1 (length ws-sends)))
    (let ((identify (json-read-from-string (car ws-sends))))
      (should (= agent-shell-to-go--discord-op-identify (map-elt identify 'op))))))

(ert-deftest agent-shell-to-go-test-discord-gateway-heartbeat-ack ()
  "Opcode 11 (heartbeat-ack): no sends, no errors."
  (let* ((tr (agent-shell-to-go-test-discord--make-with-ws))
         (ws-sends
          (agent-shell-to-go-test--with-captured-ws-sends
           (agent-shell-to-go--discord-handle-frame
            tr
            (agent-shell-to-go-test--make-fake-frame
             (json-encode
              `((op . ,agent-shell-to-go--discord-op-heartbeat-ack) (d . :null))))))))
    (should (null ws-sends))))

(ert-deftest agent-shell-to-go-test-discord-gateway-heartbeat-request ()
  "Opcode 1 (heartbeat request from server): sends a heartbeat payload back."
  (let* ((tr (agent-shell-to-go-test-discord--make-with-ws))
         (ws-sends
          (agent-shell-to-go-test--with-captured-ws-sends
           (agent-shell-to-go--discord-handle-frame
            tr
            (agent-shell-to-go-test--make-fake-frame
             (json-encode
              `((op . ,agent-shell-to-go--discord-op-heartbeat) (d . :null))))))))
    (should (= 1 (length ws-sends)))
    (let ((hb (json-read-from-string (car ws-sends))))
      (should (= agent-shell-to-go--discord-op-heartbeat (map-elt hb 'op))))))

(ert-deftest agent-shell-to-go-test-discord-gateway-invalid-session ()
  "Opcode 9 (invalid-session): schedules re-identify after a 5-second delay."
  (let* ((tr (agent-shell-to-go-test-discord--make-with-ws))
         (timer-delay nil))
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (delay _repeat _fn &rest _args)
                 (setq timer-delay delay)
                 (make-symbol "fake-timer"))))
      (agent-shell-to-go--discord-handle-frame
       tr
       (agent-shell-to-go-test--make-fake-frame
        (json-encode
         `((op . ,agent-shell-to-go--discord-op-invalid-session) (d . :json-false))))))
    (should (= 5 timer-delay))))

(ert-deftest agent-shell-to-go-test-discord-gateway-dispatch-calls-defer ()
  "Opcode 0 (dispatch): calls agent-shell-to-go--defer with dispatch-event and event data."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (deferred-fn nil)
         (deferred-args nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--defer)
               (lambda (fn &rest args)
                 (setq
                  deferred-fn fn
                  deferred-args args))))
      (agent-shell-to-go--discord-handle-frame
       tr
       (agent-shell-to-go-test--make-fake-frame
        (json-encode
         `((op . ,agent-shell-to-go--discord-op-dispatch)
           (t . "MESSAGE_CREATE")
           (s . 7)
           (d . ((id . "M1"))))))))
    (should (eq #'agent-shell-to-go--discord-dispatch-event deferred-fn))
    (should (eq tr (nth 0 deferred-args)))
    (should (equal "MESSAGE_CREATE" (nth 1 deferred-args)))))

(ert-deftest agent-shell-to-go-test-discord-gateway-dispatch-updates-sequence ()
  "Dispatch frame with a sequence number updates the transport's sequence slot."
  (let ((tr (agent-shell-to-go-test-discord--make)))
    (cl-letf (((symbol-function 'agent-shell-to-go--defer) (lambda (&rest _) nil)))
      (agent-shell-to-go--discord-handle-frame
       tr
       (agent-shell-to-go-test--make-fake-frame
        (json-encode
         `((op . ,agent-shell-to-go--discord-op-dispatch)
           (t . "MESSAGE_CREATE")
           (s . 42)
           (d . nil))))))
    (should (= 42 (agent-shell-to-go-discord-transport-sequence tr)))))

; 5. REST transport methods

;; send-text

(ert-deftest agent-shell-to-go-test-discord-send-text-returns-message-id ()
  "send-text returns the message ID from the API response."
  (let ((tr (agent-shell-to-go-test-discord--make)))
    (with-mocked-discord-api `((("POST" . "/channels/C1/messages") . ((id . "M1"))))
      (should
       (equal "M1" (agent-shell-to-go-transport-send-text tr "C1" nil "hello"))))))

(ert-deftest agent-shell-to-go-test-discord-send-text-uses-thread-id-as-target ()
  "send-text posts to thread-id when provided, not to channel-id."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (called-endpoint nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--discord-request)
               (lambda (_method endpoint &rest _)
                 (setq called-endpoint endpoint)
                 '((id . "M1")))))
      (agent-shell-to-go-transport-send-text tr "FORUM1" "THREAD1" "hi"))
    (should (string-match-p "THREAD1" called-endpoint))
    (should (not (string-match-p "FORUM1" called-endpoint)))))

(ert-deftest agent-shell-to-go-test-discord-send-text-uses-channel-when-no-thread ()
  "send-text posts to channel-id when thread-id is nil."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (called-endpoint nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--discord-request)
               (lambda (_method endpoint &rest _)
                 (setq called-endpoint endpoint)
                 '((id . "M1")))))
      (agent-shell-to-go-transport-send-text tr "CHAN1" nil "hi"))
    (should (string-match-p "CHAN1" called-endpoint))))

(ert-deftest agent-shell-to-go-test-discord-send-text-truncated-saves-full-text ()
  "send-text with :truncate saves the full text to storage for later expansion."
  (with-discord-temp-storage
    (let* ((tr (agent-shell-to-go-test-discord--make))
           (long-text (make-string 600 ?a)))
      (with-mocked-discord-api `((("POST" . "/channels/C1/messages") . ((id . "M1"))))
        (agent-shell-to-go-transport-send-text tr "C1" nil long-text '(:truncate t)))
      (should
       (equal long-text (agent-shell-to-go--load-truncated-message tr "C1" "M1"))))))

;; edit-message

(ert-deftest agent-shell-to-go-test-discord-edit-message ()
  "edit-message sends PATCH to the message endpoint and returns the message ID."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (called-method nil)
         (called-endpoint nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--discord-request)
               (lambda (method endpoint &rest _)
                 (setq
                  called-method method
                  called-endpoint endpoint)
                 '((id . "M1")))))
      (let ((result (agent-shell-to-go-transport-edit-message tr "C1" "M1" "updated")))
        (should (equal "M1" result))
        (should (equal "PATCH" called-method))
        (should (string-match-p "M1" called-endpoint))
        (should (string-match-p "C1" called-endpoint))))))

;; start-thread

(ert-deftest agent-shell-to-go-test-discord-start-thread-returns-id ()
  "start-thread returns the thread (forum post) ID from the API."
  (let ((tr (agent-shell-to-go-test-discord--make)))
    (with-mocked-discord-api `((("POST" . "/channels/FORUM1/threads")
                                .
                                ((id . "THREAD1"))))
      (should
       (equal
        "THREAD1" (agent-shell-to-go-transport-start-thread tr "FORUM1" "Session"))))))

(ert-deftest agent-shell-to-go-test-discord-start-thread-records-parent ()
  "start-thread registers the thread→parent-forum mapping for inbound routing."
  (let ((tr (agent-shell-to-go-test-discord--make)))
    (with-mocked-discord-api `((("POST" . "/channels/FORUM1/threads")
                                .
                                ((id . "THREAD1"))))
      (agent-shell-to-go-transport-start-thread tr "FORUM1" "Session"))
    (should
     (equal
      "FORUM1"
      (gethash "THREAD1" (agent-shell-to-go-discord-transport-thread-parents tr))))))

;; update-thread-header

(ert-deftest agent-shell-to-go-test-discord-update-thread-header ()
  "update-thread-header sends PATCH to the thread channel endpoint."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (called-endpoint nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--discord-request)
               (lambda (_method endpoint &rest _)
                 (setq called-endpoint endpoint)
                 nil)))
      (agent-shell-to-go-transport-update-thread-header tr "FORUM1" "THREAD1" "Title"))
    (should (string-match-p "THREAD1" called-endpoint))))

(ert-deftest agent-shell-to-go-test-discord-update-thread-header-truncates-long-title ()
  "Titles over 100 characters are truncated before sending."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (long-title (make-string 150 ?a))
         (sent-data nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--discord-api)
               (lambda (_method _endpoint data)
                 (setq sent-data data)
                 nil)))
      (agent-shell-to-go-transport-update-thread-header
       tr "FORUM1" "THREAD1" long-title))
    (should sent-data)
    (should (<= (length (map-elt sent-data 'name)) 100))))

;; delete-message / delete-thread

(ert-deftest agent-shell-to-go-test-discord-delete-message ()
  "delete-message sends DELETE to the correct message endpoint."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (called-method nil)
         (called-endpoint nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--discord-request)
               (lambda (method endpoint &rest _)
                 (setq
                  called-method method
                  called-endpoint endpoint)
                 nil)))
      (agent-shell-to-go-transport-delete-message tr "C1" "M1"))
    (should (equal "DELETE" called-method))
    (should (string-match-p "M1" called-endpoint))))

(ert-deftest agent-shell-to-go-test-discord-delete-thread ()
  "delete-thread sends DELETE to the thread channel endpoint."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (called-method nil)
         (called-endpoint nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--discord-request)
               (lambda (method endpoint &rest _)
                 (setq
                  called-method method
                  called-endpoint endpoint)
                 nil)))
      (agent-shell-to-go-transport-delete-thread tr "FORUM1" "THREAD1"))
    (should (equal "DELETE" called-method))
    (should (string-match-p "THREAD1" called-endpoint))))

;; fetch-thread-replies

(ert-deftest agent-shell-to-go-test-discord-fetch-thread-replies ()
  "fetch-thread-replies returns plists in chronological order."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (msgs
          (vector
           '((id . "M2") (author . ((id . "U2"))) (content . "second"))
           '((id . "M1") (author . ((id . "U1"))) (content . "first")))))
    (with-mocked-discord-api `((("GET" . "/channels/THREAD1/messages?limit=100")
                                .
                                ,msgs))
      (let ((replies
             (agent-shell-to-go-transport-fetch-thread-replies tr "C1" "THREAD1")))
        (should (= 2 (length replies)))
        (should (equal "M1" (plist-get (car replies) :msg-id)))
        (should (equal "first" (plist-get (car replies) :text)))
        (should (equal "M2" (plist-get (cadr replies) :msg-id)))))))

;; get-message-text

(ert-deftest agent-shell-to-go-test-discord-get-message-text ()
  "get-message-text returns the content field from the API response."
  (let ((tr (agent-shell-to-go-test-discord--make)))
    (with-mocked-discord-api `((("GET" . "/channels/C1/messages/M1")
                                .
                                ((content . "fetched text"))))
      (should
       (equal
        "fetched text" (agent-shell-to-go-transport-get-message-text tr "C1" "M1"))))))

;; get-reactions

(ert-deftest agent-shell-to-go-test-discord-get-reactions-returns-nil ()
  "get-reactions always returns nil (reactions arrive via Gateway, not polling)."
  (let ((tr (agent-shell-to-go-test-discord--make)))
    (should (null (agent-shell-to-go-transport-get-reactions tr "C1" "M1")))))

;; upload-file

(ert-deftest agent-shell-to-go-test-discord-upload-file-skips-missing-file ()
  "upload-file does nothing when the path does not exist on disk."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (api-called nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--discord-request)
               (lambda (&rest _)
                 (setq api-called t)
                 nil)))
      (agent-shell-to-go-transport-upload-file tr "C1" nil "/no/such/file.txt"))
    (should (null api-called))))

(ert-deftest agent-shell-to-go-test-discord-upload-file-uses-thread-target ()
  "upload-file posts to thread-id when provided."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (called-endpoint nil)
         (tmpfile (make-temp-file "astg-upload")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-to-go--discord-request)
                   (lambda (_method endpoint &rest _)
                     (setq called-endpoint endpoint)
                     nil)))
          (agent-shell-to-go-transport-upload-file tr "FORUM1" "THREAD1" tmpfile))
      (delete-file tmpfile))
    (should (string-match-p "THREAD1" called-endpoint))
    (should (not (string-match-p "FORUM1" called-endpoint)))))

(ert-deftest agent-shell-to-go-test-discord-upload-file-uses-channel-fallback ()
  "upload-file posts to channel-id when thread-id is nil."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (called-endpoint nil)
         (tmpfile (make-temp-file "astg-upload")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-to-go--discord-request)
                   (lambda (_method endpoint &rest _)
                     (setq called-endpoint endpoint)
                     nil)))
          (agent-shell-to-go-transport-upload-file tr "CHAN1" nil tmpfile))
      (delete-file tmpfile))
    (should (string-match-p "CHAN1" called-endpoint))))

; Channel management

;; Persistence

(ert-deftest agent-shell-to-go-test-discord-save-channels ()
  "save-channels writes the project-to-channel map to disk as an alist."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (tmpfile (make-temp-file "astg-chans")))
    (unwind-protect
        (let ((agent-shell-to-go-discord-channels-file tmpfile))
          (puthash
           "/proj1" "CHAN1" (agent-shell-to-go-discord-transport-project-channels tr))
          (agent-shell-to-go--discord-save-channels tr)
          (with-temp-buffer
            (insert-file-contents tmpfile)
            (let ((data (read (current-buffer))))
              (should (equal "CHAN1" (cdr (assoc "/proj1" data)))))))
      (delete-file tmpfile))))

(ert-deftest agent-shell-to-go-test-discord-load-channels ()
  "load-channels reads the alist from disk into the transport's hash table."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (tmpfile (make-temp-file "astg-chans")))
    (unwind-protect
        (let ((agent-shell-to-go-discord-channels-file tmpfile))
          (with-temp-file tmpfile
            (insert "((\"/proj1\" . \"CHAN1\") (\"/proj2\" . \"CHAN2\"))"))
          (agent-shell-to-go--discord-load-channels tr)
          (let ((table (agent-shell-to-go-discord-transport-project-channels tr)))
            (should (equal "CHAN1" (gethash "/proj1" table)))
            (should (equal "CHAN2" (gethash "/proj2" table)))))
      (delete-file tmpfile))))

(ert-deftest agent-shell-to-go-test-discord-channels-round-trip ()
  "Saving then loading channels in a fresh transport preserves all mappings."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (tr2 (agent-shell-to-go-test-discord--make))
         (tmpfile (make-temp-file "astg-chans")))
    (unwind-protect
        (let ((agent-shell-to-go-discord-channels-file tmpfile))
          (puthash
           "/proj" "CHAN-X" (agent-shell-to-go-discord-transport-project-channels tr))
          (agent-shell-to-go--discord-save-channels tr)
          (agent-shell-to-go--discord-load-channels tr2)
          (should
           (equal
            "CHAN-X"
            (gethash
             "/proj" (agent-shell-to-go-discord-transport-project-channels tr2)))))
      (delete-file tmpfile))))

;; get-or-create-project-channel

(ert-deftest agent-shell-to-go-test-discord-get-or-create-channel-cache-hit ()
  "Cache hit returns the cached ID without making any API call."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (api-called nil))
    (puthash
     "/proj" "CACHED-ID" (agent-shell-to-go-discord-transport-project-channels tr))
    (cl-letf (((symbol-function 'agent-shell-to-go--discord-request)
               (lambda (&rest _)
                 (setq api-called t)
                 nil)))
      (let ((id (agent-shell-to-go--discord-get-or-create-project-channel tr "/proj")))
        (should (equal "CACHED-ID" id))
        (should (null api-called))))))

(ert-deftest agent-shell-to-go-test-discord-get-or-create-channel-found-by-name ()
  "Cache miss: finds an existing forum channel by name and caches it."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (agent-shell-to-go-discord-guild-id "GUILD1")
         (agent-shell-to-go-discord-channel-prefix "")
         (tmpfile (make-temp-file "astg-chans")))
    (unwind-protect
        (let ((agent-shell-to-go-discord-channels-file tmpfile))
          (with-mocked-discord-api
              `((("GET" . "/guilds/GUILD1/channels")
                 .
                 ,(vector
                   `((id . "FOUND-CHAN")
                     (name . "myproject")
                     (type . ,agent-shell-to-go--discord-channel-type-forum)))))
            (let ((id
                   (agent-shell-to-go--discord-get-or-create-project-channel
                    tr "/path/to/myproject")))
              (should (equal "FOUND-CHAN" id))
              (should
               (equal
                "FOUND-CHAN"
                (gethash
                 "/path/to/myproject"
                 (agent-shell-to-go-discord-transport-project-channels tr)))))))
      (delete-file tmpfile))))

(ert-deftest agent-shell-to-go-test-discord-get-or-create-channel-creates-new ()
  "Cache miss: creates a new forum channel when none is found by name."
  (let* ((tr (agent-shell-to-go-test-discord--make))
         (agent-shell-to-go-discord-guild-id "GUILD1")
         (agent-shell-to-go-discord-channel-prefix "")
         (tmpfile (make-temp-file "astg-chans")))
    (unwind-protect
        (let ((agent-shell-to-go-discord-channels-file tmpfile))
          (with-mocked-discord-api `((("GET" . "/guilds/GUILD1/channels") . ,(vector))
                                     (("POST" . "/guilds/GUILD1/channels")
                                      .
                                      ((id . "NEW-CHAN"))))
            (let ((id
                   (agent-shell-to-go--discord-get-or-create-project-channel
                    tr "/path/to/newproject")))
              (should (equal "NEW-CHAN" id)))))
      (delete-file tmpfile))))

(provide 'agent-shell-to-go-discord-test)
;;; agent-shell-to-go-discord-test.el ends here
