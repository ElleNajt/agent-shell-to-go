;;; agent-shell-to-go-slack-test.el --- Tests for agent-shell-to-go-slack.el -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for the Slack transport implementation (agent-shell-to-go-slack.el).
;; Uses a mocked Slack REST API and a fake WebSocket — no real Slack backend required.
;;
;; Run:
;;   make test TEST=agent-shell-to-go-slack-test.el
;;
;; APIs under test:
;;
;;   agent-shell-to-go--slack-emoji-to-action
;;     - emoji-to-action-known: registered emoji names map to canonical actions
;;     - emoji-to-action-unknown: unknown or nil emoji names return nil
;;
;;   agent-shell-to-go--slack-message-seen-p
;;     - message-seen-first-time: first call returns nil
;;     - message-seen-second-time: second call for the same ts returns t
;;     - message-seen-independent-ids: different timestamps are tracked independently
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
;;     - format-tool-call-result-failed: includes the X emoji
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
;;     - format-markdown-converts-bold: **bold** becomes *bold* (mrkdwn)
;;
;;   agent-shell-to-go--slack-handle-frame
;;     - handle-frame-hello: "hello" type produces no sends
;;     - handle-frame-events-api-acks: events_api with envelope_id ACKs back
;;     - handle-frame-events-api-calls-defer: events_api calls agent-shell-to-go--defer
;;     - handle-frame-disconnect-reconnects: "disconnect" calls ws-reconnect
;;
;;   agent-shell-to-go--slack-dispatch-event
;;     - dispatch-event-message-fires-hook: "message" fires message hook for authorized user
;;     - dispatch-event-reaction-added-fires-hook: "reaction_added" fires reaction hook with added-p t
;;     - dispatch-event-reaction-removed-fires-hook: "reaction_removed" fires hook with added-p nil
;;     - dispatch-event-skips-bot-id: events with bot_id set are ignored
;;     - dispatch-event-skips-unauthorized: unauthorized users are ignored
;;
;;   agent-shell-to-go--slack-normalize-message
;;     - normalize-message-ignores-own-bot: user matching bot-user-id is dropped
;;     - normalize-message-ignores-subtype: messages with subtype are dropped
;;     - normalize-message-deduplicates: same ts fires hook only once
;;     - normalize-message-thread: thread_ts is forwarded as :thread-id
;;
;;   agent-shell-to-go--slack-normalize-reaction
;;     - normalize-reaction-known-emoji: known emoji maps to canonical action
;;     - normalize-reaction-unknown-emoji-fires: unknown emoji fires hook with nil :action
;;
;;   agent-shell-to-go-transport-send-text
;;     - send-text-returns-ts: returns the message timestamp from the API
;;     - send-text-uses-thread-ts: posts with thread_ts when thread-id provided
;;     - send-text-uses-channel-when-no-thread: posts to channel only when thread is nil
;;     - send-text-truncated-saves-full-text: :truncate saves full text for later expansion
;;
;;   agent-shell-to-go-transport-edit-message
;;     - edit-message-calls-chat-update: calls chat.update and returns t on success
;;
;;   agent-shell-to-go-transport-start-thread
;;     - start-thread-returns-ts: returns the message ts from send-text
;;
;;   agent-shell-to-go-transport-update-thread-header
;;     - update-thread-header-calls-chat-update: calls chat.update for the thread
;;     - update-thread-header-truncates-long-title: titles over 80 chars are truncated
;;
;;   agent-shell-to-go-transport-delete-message
;;     - delete-message-calls-chat-delete: calls chat.delete with channel and ts
;;
;;   agent-shell-to-go-transport-delete-thread
;;     - delete-thread-deletes-all-messages: deletes each message in the thread
;;
;;   agent-shell-to-go-transport-fetch-thread-replies
;;     - fetch-thread-replies: returns plists in API order
;;
;;   agent-shell-to-go-transport-get-message-text
;;     - get-message-text: returns the text field from the history API
;;
;;   agent-shell-to-go-transport-get-reactions
;;     - get-reactions-returns-actions: maps known emoji to canonical action symbols
;;
;;   agent-shell-to-go-transport-upload-file
;;     - upload-file-skips-missing-file: does nothing when the path does not exist
;;     - upload-file-uses-thread-ts: posts with thread_ts when thread-id provided
;;     - upload-file-uses-channel-fallback: posts to channel only when thread is nil
;;
;;   agent-shell-to-go--slack-save-channels / agent-shell-to-go--slack-load-channels
;;     - save-channels: writes project→channel map to disk as an alist
;;     - load-channels: reads alist from disk into the transport hash
;;     - channels-round-trip: save+load in a fresh transport preserves all mappings
;;
;;   agent-shell-to-go--slack-get-or-create-project-channel
;;     - get-or-create-channel-cache-hit: cached ID returned without any API call
;;     - get-or-create-channel-creates-new: creates channel when cache misses

;;; Code:

(require 'ert)

(require 'agent-shell-to-go-slack)
(require 'gateway-helpers)

; Test helpers

(defun agent-shell-to-go-test-slack--make ()
  "Return a fresh Slack transport with a known bot-user-id cached."
  (let ((tr (agent-shell-to-go--make-slack-transport :name 'slack)))
    (setf (agent-shell-to-go-slack-transport-bot-user-id-cache tr) "BOT123")
    tr))

(defun agent-shell-to-go-test-slack--make-with-ws ()
  "Return a fresh Slack transport with a fake WebSocket wired up."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (dummy-socket (list 'fake-slack-ws))
         (ws
          (agent-shell-to-go--ws-make
           :name 'slack-test
           :url-fn (lambda () "wss://test")
           :on-frame (lambda (_) nil))))
    (setf (agent-shell-to-go--ws-websocket ws) dummy-socket)
    (setf (agent-shell-to-go-slack-transport-ws tr) ws)
    tr))

(defmacro with-mocked-slack-api (responses &rest body)
  "Execute BODY with `agent-shell-to-go--slack-api' mocked.
RESPONSES is an alist keyed by (METHOD . ENDPOINT); unmatched calls return nil."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'agent-shell-to-go--slack-api)
              (lambda (method endpoint &rest _data)
                (cdr (assoc (cons method endpoint) ,responses)))))
     ,@body))

(defmacro with-slack-temp-storage (&rest body)
  "Execute BODY with `agent-shell-to-go-storage-base-dir' bound to a temp dir."
  (declare (indent 0))
  `(let* ((tmpdir (make-temp-file "astg-slack-storage" t))
          (agent-shell-to-go-storage-base-dir tmpdir))
     (unwind-protect
         (progn
           ,@body)
       (delete-directory tmpdir t))))

; 1. Pure helpers

;; Emoji-to-action mapping

(ert-deftest agent-shell-to-go-test-slack-emoji-to-action-known ()
  "Registered Slack emoji names map to the correct canonical action."
  (should (eq 'hide (agent-shell-to-go--slack-emoji-to-action "see_no_evil")))
  (should (eq 'hide (agent-shell-to-go--slack-emoji-to-action "no_bell")))
  (should (eq 'expand-truncated (agent-shell-to-go--slack-emoji-to-action "eyes")))
  (should (eq 'expand-full (agent-shell-to-go--slack-emoji-to-action "book")))
  (should (eq 'expand-full (agent-shell-to-go--slack-emoji-to-action "open_book")))
  (should
   (eq 'permission-allow (agent-shell-to-go--slack-emoji-to-action "white_check_mark")))
  (should (eq 'permission-allow (agent-shell-to-go--slack-emoji-to-action "+1")))
  (should (eq 'permission-always (agent-shell-to-go--slack-emoji-to-action "unlock")))
  (should (eq 'permission-always (agent-shell-to-go--slack-emoji-to-action "star")))
  (should (eq 'permission-reject (agent-shell-to-go--slack-emoji-to-action "x")))
  (should (eq 'permission-reject (agent-shell-to-go--slack-emoji-to-action "-1"))))

(ert-deftest agent-shell-to-go-test-slack-emoji-to-action-unknown ()
  "Unknown or nil emoji names return nil."
  (should (null (agent-shell-to-go--slack-emoji-to-action "unknown_emoji")))
  (should (null (agent-shell-to-go--slack-emoji-to-action "")))
  (should (null (agent-shell-to-go--slack-emoji-to-action nil))))

;; Deduplication

(ert-deftest agent-shell-to-go-test-slack-message-seen-first-time ()
  "A message ts is not seen on the first call."
  (let ((tr (agent-shell-to-go-test-slack--make)))
    (should (null (agent-shell-to-go--slack-message-seen-p tr "1234.5678")))))

(ert-deftest agent-shell-to-go-test-slack-message-seen-second-time ()
  "The same ts returns t on the second call."
  (let ((tr (agent-shell-to-go-test-slack--make)))
    (agent-shell-to-go--slack-message-seen-p tr "1234.5678")
    (should (eq t (agent-shell-to-go--slack-message-seen-p tr "1234.5678")))))

(ert-deftest agent-shell-to-go-test-slack-message-seen-independent-ids ()
  "Different timestamps are tracked independently."
  (let ((tr (agent-shell-to-go-test-slack--make)))
    (agent-shell-to-go--slack-message-seen-p tr "TS-A")
    (should (null (agent-shell-to-go--slack-message-seen-p tr "TS-B")))
    (should (eq t (agent-shell-to-go--slack-message-seen-p tr "TS-A")))))

;; Authorization

(ert-deftest agent-shell-to-go-test-slack-authorized-in-list ()
  "Users in the authorized list are authorized."
  (let ((agent-shell-to-go-slack-authorized-users '("U1" "U2"))
        (tr (agent-shell-to-go-test-slack--make)))
    (should (agent-shell-to-go-transport-authorized-p tr "U1"))
    (should (agent-shell-to-go-transport-authorized-p tr "U2"))))

(ert-deftest agent-shell-to-go-test-slack-authorized-not-in-list ()
  "A user not in the authorized list is not authorized."
  (let ((agent-shell-to-go-slack-authorized-users '("U1"))
        (tr (agent-shell-to-go-test-slack--make)))
    (should (null (agent-shell-to-go-transport-authorized-p tr "STRANGER")))))

(ert-deftest agent-shell-to-go-test-slack-authorized-empty-list ()
  "When the authorized list is nil, no one is authorized."
  (let ((agent-shell-to-go-slack-authorized-users nil)
        (tr (agent-shell-to-go-test-slack--make)))
    (should (null (agent-shell-to-go-transport-authorized-p tr "U1")))))

; 2. Formatting

(ert-deftest agent-shell-to-go-test-slack-format-tool-call-start ()
  "Tool call start contains the title."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (s (agent-shell-to-go-transport-format-tool-call-start tr "read_file")))
    (should (string-match-p "read_file" s))))

(ert-deftest agent-shell-to-go-test-slack-format-tool-call-result-completed ()
  "Completed result includes tool name and output in a code block."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (s
          (agent-shell-to-go-transport-format-tool-call-result
           tr "bash" 'completed "output here")))
    (should (string-match-p "bash" s))
    (should (string-match-p "output here" s))
    (should (string-match-p "```" s))))

(ert-deftest agent-shell-to-go-test-slack-format-tool-call-result-failed ()
  "Failed result includes the failure icon."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (s
          (agent-shell-to-go-transport-format-tool-call-result
           tr "bash" 'failed "err")))
    (should (string-match-p "❌" s))
    (should (string-match-p "err" s))))

(ert-deftest agent-shell-to-go-test-slack-format-tool-call-result-no-output ()
  "Result with nil output omits the code block."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (s
          (agent-shell-to-go-transport-format-tool-call-result
           tr "bash" 'completed nil)))
    (should (string-match-p "bash" s))
    (should (not (string-match-p "```" s)))))

(ert-deftest agent-shell-to-go-test-slack-format-diff-empty ()
  "Identical old and new text yields an empty string."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (s (agent-shell-to-go-transport-format-diff tr "same" "same")))
    (should (equal "" s))))

(ert-deftest agent-shell-to-go-test-slack-format-diff-has-changes ()
  "Different old and new text yields a ```diff fenced block."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (s (agent-shell-to-go-transport-format-diff tr "old line" "new line")))
    (should (string-match-p "```diff" s))))

(ert-deftest agent-shell-to-go-test-slack-format-user-message ()
  "User message format contains the text."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (s (agent-shell-to-go-transport-format-user-message tr "hello there")))
    (should (string-match-p "hello there" s))))

(ert-deftest agent-shell-to-go-test-slack-format-agent-message ()
  "Agent message format contains the text."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (s (agent-shell-to-go-transport-format-agent-message tr "I am a robot")))
    (should (string-match-p "I am a robot" s))))

(ert-deftest agent-shell-to-go-test-slack-format-markdown-converts-bold ()
  "Markdown **bold** is converted to Slack mrkdwn *bold*."
  (let ((tr (agent-shell-to-go-test-slack--make)))
    (should
     (equal
      "*bold*"
      (agent-shell-to-go-transport-format-markdown tr "**bold**")))))

; 3. Normalization (via dispatch-event and normalize-*)

;; dispatch-event

(ert-deftest agent-shell-to-go-test-slack-dispatch-event-message-fires-hook ()
  "A message event from an authorized user fires the message hook."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (agent-shell-to-go-slack-authorized-users '("U1"))
         (received nil)
         (agent-shell-to-go-message-hook
          (list (lambda (&rest plist) (setq received plist)))))
    (agent-shell-to-go--slack-dispatch-event
     tr
     `((event . ((type . "message")
                 (ts . "TS1")
                 (channel . "C1")
                 (user . "U1")
                 (text . "hello")))))
    (should received)
    (should (equal "hello" (plist-get received :text)))
    (should (equal "U1" (plist-get received :user)))))

(ert-deftest agent-shell-to-go-test-slack-dispatch-event-reaction-added-fires-hook ()
  "A reaction_added event fires the reaction hook with added-p t."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (agent-shell-to-go-slack-authorized-users '("U1"))
         (received nil)
         (agent-shell-to-go-reaction-hook
          (list (lambda (&rest plist) (setq received plist)))))
    (agent-shell-to-go--slack-dispatch-event
     tr
     `((event . ((type . "reaction_added")
                 (user . "U1")
                 (reaction . "eyes")
                 (item . ((ts . "TS1") (channel . "C1")))))))
    (should received)
    (should (eq 'expand-truncated (plist-get received :action)))
    (should (eq t (plist-get received :added-p)))))

(ert-deftest agent-shell-to-go-test-slack-dispatch-event-reaction-removed-fires-hook ()
  "A reaction_removed event fires the reaction hook with added-p nil."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (agent-shell-to-go-slack-authorized-users '("U1"))
         (received nil)
         (agent-shell-to-go-reaction-hook
          (list (lambda (&rest plist) (setq received plist)))))
    (agent-shell-to-go--slack-dispatch-event
     tr
     `((event . ((type . "reaction_removed")
                 (user . "U1")
                 (reaction . "eyes")
                 (item . ((ts . "TS1") (channel . "C1")))))))
    (should received)
    (should (null (plist-get received :added-p)))))

(ert-deftest agent-shell-to-go-test-slack-dispatch-event-skips-bot-id ()
  "Events with bot_id set are silently ignored."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (agent-shell-to-go-slack-authorized-users '("U1"))
         (received nil)
         (agent-shell-to-go-message-hook
          (list (lambda (&rest plist) (setq received plist)))))
    (agent-shell-to-go--slack-dispatch-event
     tr
     `((event . ((type . "message")
                 (ts . "TS1")
                 (channel . "C1")
                 (user . "U1")
                 (bot_id . "B1")
                 (text . "from bot")))))
    (should (null received))))

(ert-deftest agent-shell-to-go-test-slack-dispatch-event-skips-unauthorized ()
  "Events from unauthorized users are ignored."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (agent-shell-to-go-slack-authorized-users '("ALLOWED"))
         (received nil)
         (agent-shell-to-go-message-hook
          (list (lambda (&rest plist) (setq received plist)))))
    (agent-shell-to-go--slack-dispatch-event
     tr
     `((event . ((type . "message")
                 (ts . "TS1")
                 (channel . "C1")
                 (user . "STRANGER")
                 (text . "intruder")))))
    (should (null received))))

;; normalize-message

(ert-deftest agent-shell-to-go-test-slack-normalize-message-ignores-own-bot ()
  "Messages where user matches the bot's own user ID are dropped."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (received nil)
         (agent-shell-to-go-message-hook
          (list (lambda (&rest plist) (setq received plist)))))
    (agent-shell-to-go--slack-normalize-message
     tr
     '((ts . "TS1") (channel . "C1") (user . "BOT123") (text . "echo")))
    (should (null received))))

(ert-deftest agent-shell-to-go-test-slack-normalize-message-ignores-subtype ()
  "Messages with a subtype (e.g. bot_message) are dropped."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (received nil)
         (agent-shell-to-go-message-hook
          (list (lambda (&rest plist) (setq received plist)))))
    (agent-shell-to-go--slack-normalize-message
     tr
     '((ts . "TS1")
       (channel . "C1")
       (user . "U1")
       (text . "hi")
       (subtype . "bot_message")))
    (should (null received))))

(ert-deftest agent-shell-to-go-test-slack-normalize-message-deduplicates ()
  "The message hook fires only once for a given ts."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (count 0)
         (agent-shell-to-go-message-hook
          (list (lambda (&rest _plist) (setq count (1+ count)))))
         (event '((ts . "DUP-TS")
                  (channel . "C1")
                  (user . "U1")
                  (text . "dup"))))
    (agent-shell-to-go--slack-normalize-message tr event)
    (agent-shell-to-go--slack-normalize-message tr event)
    (should (= 1 count))))

(ert-deftest agent-shell-to-go-test-slack-normalize-message-thread ()
  "Messages in a thread forward thread_ts as :thread-id."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (received nil)
         (agent-shell-to-go-message-hook
          (list (lambda (&rest plist) (setq received plist)))))
    (agent-shell-to-go--slack-normalize-message
     tr
     '((ts . "TS2")
       (thread_ts . "TS1")
       (channel . "C1")
       (user . "U1")
       (text . "reply")))
    (should (equal "TS1" (plist-get received :thread-id)))
    (should (equal "C1" (plist-get received :channel-id)))))

;; normalize-reaction

(ert-deftest agent-shell-to-go-test-slack-normalize-reaction-known-emoji ()
  "Known emoji fires the hook with the correct canonical action."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (received nil)
         (agent-shell-to-go-reaction-hook
          (list (lambda (&rest plist) (setq received plist)))))
    (agent-shell-to-go--slack-normalize-reaction
     tr
     '((reaction . "white_check_mark")
       (user . "U1")
       (item . ((ts . "TS1") (channel . "C1"))))
     t)
    (should received)
    (should (eq 'permission-allow (plist-get received :action)))
    (should (eq t (plist-get received :added-p)))))

(ert-deftest agent-shell-to-go-test-slack-normalize-reaction-unknown-emoji-fires ()
  "Unknown emoji still fires the hook with nil :action and raw-emoji set."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (received nil)
         (agent-shell-to-go-reaction-hook
          (list (lambda (&rest plist) (setq received plist)))))
    (agent-shell-to-go--slack-normalize-reaction
     tr
     '((reaction . "dancing_parrot")
       (user . "U1")
       (item . ((ts . "TS1") (channel . "C1"))))
     t)
    (should received)
    (should (null (plist-get received :action)))
    (should (equal "dancing_parrot" (plist-get received :raw-emoji)))))

; 4. WebSocket (handle-frame)

(ert-deftest agent-shell-to-go-test-slack-handle-frame-hello ()
  "The 'hello' frame type produces no WebSocket sends."
  (let* ((tr (agent-shell-to-go-test-slack--make-with-ws))
         (ws-sends
          (agent-shell-to-go-test--with-captured-ws-sends
           (agent-shell-to-go--slack-handle-frame
            tr
            (agent-shell-to-go-test--make-fake-frame
             (json-encode '((type . "hello"))))))))
    (should (null ws-sends))))

(ert-deftest agent-shell-to-go-test-slack-handle-frame-events-api-acks ()
  "An events_api frame with an envelope_id sends an ACK back."
  (let* ((tr (agent-shell-to-go-test-slack--make-with-ws))
         (ws-sends nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--defer) (lambda (&rest _) nil))
              ((symbol-function 'websocket-send-text)
               (lambda (_ws text) (push text ws-sends))))
      (agent-shell-to-go--slack-handle-frame
       tr
       (agent-shell-to-go-test--make-fake-frame
        (json-encode
         '((type . "events_api")
           (envelope_id . "EID1")
           (payload . ((event . ((type . "message"))))))))))
    (should (= 1 (length ws-sends)))
    (let ((ack (json-read-from-string (car ws-sends))))
      (should (equal "EID1" (map-elt ack 'envelope_id))))))

(ert-deftest agent-shell-to-go-test-slack-handle-frame-events-api-calls-defer ()
  "An events_api frame defers a call to agent-shell-to-go--slack-dispatch-event."
  (let* ((tr (agent-shell-to-go-test-slack--make-with-ws))
         (deferred-fn nil)
         (deferred-args nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--defer)
               (lambda (fn &rest args)
                 (setq deferred-fn fn deferred-args args)))
              ((symbol-function 'websocket-send-text) (lambda (&rest _) nil)))
      (agent-shell-to-go--slack-handle-frame
       tr
       (agent-shell-to-go-test--make-fake-frame
        (json-encode
         '((type . "events_api")
           (envelope_id . "EID1")
           (payload . ((event . ((type . "message"))))))))))
    (should (eq #'agent-shell-to-go--slack-dispatch-event deferred-fn))
    (should (eq tr (nth 0 deferred-args)))))

(ert-deftest agent-shell-to-go-test-slack-handle-frame-disconnect-reconnects ()
  "A 'disconnect' frame triggers ws-reconnect."
  (let* ((tr (agent-shell-to-go-test-slack--make-with-ws))
         (reconnect-called nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--ws-reconnect)
               (lambda (_ws) (setq reconnect-called t))))
      (agent-shell-to-go--slack-handle-frame
       tr
       (agent-shell-to-go-test--make-fake-frame
        (json-encode '((type . "disconnect"))))))
    (should reconnect-called)))

; 5. REST transport methods

;; send-text

(ert-deftest agent-shell-to-go-test-slack-send-text-returns-ts ()
  "send-text returns the message timestamp from the API response."
  (let ((tr (agent-shell-to-go-test-slack--make)))
    (with-mocked-slack-api
        `((("POST" . "chat.postMessage") . ((ok . t) (ts . "1234.5678"))))
      (should
       (equal "1234.5678" (agent-shell-to-go-transport-send-text tr "C1" nil "hello"))))))

(ert-deftest agent-shell-to-go-test-slack-send-text-uses-thread-ts ()
  "send-text includes thread_ts in the payload when thread-id is provided."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (sent-data nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--slack-api)
               (lambda (_method _endpoint &optional data)
                 (setq sent-data data)
                 '((ok . t) (ts . "TS1")))))
      (agent-shell-to-go-transport-send-text tr "C1" "THREAD-TS" "hi"))
    (should (equal "THREAD-TS" (map-elt sent-data 'thread_ts)))))

(ert-deftest agent-shell-to-go-test-slack-send-text-uses-channel-when-no-thread ()
  "send-text posts to channel only when thread-id is nil."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (sent-data nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--slack-api)
               (lambda (_method _endpoint &optional data)
                 (setq sent-data data)
                 '((ok . t) (ts . "TS1")))))
      (agent-shell-to-go-transport-send-text tr "C1" nil "hi"))
    (should (equal "C1" (map-elt sent-data 'channel)))
    (should (null (map-elt sent-data 'thread_ts)))))

(ert-deftest agent-shell-to-go-test-slack-send-text-truncated-saves-full-text ()
  "send-text with :truncate saves the full text to storage for later expansion."
  (with-slack-temp-storage
    (let* ((tr (agent-shell-to-go-test-slack--make))
           (long-text (make-string 600 ?a)))
      (with-mocked-slack-api
          `((("POST" . "chat.postMessage") . ((ok . t) (ts . "TS1"))))
        (agent-shell-to-go-transport-send-text tr "C1" nil long-text '(:truncate t)))
      (should
       (equal long-text (agent-shell-to-go--load-truncated-message tr "C1" "TS1"))))))

;; edit-message

(ert-deftest agent-shell-to-go-test-slack-edit-message-calls-chat-update ()
  "edit-message calls chat.update and returns t on success."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (called-endpoint nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--slack-api)
               (lambda (_method endpoint &rest _)
                 (setq called-endpoint endpoint)
                 '((ok . t)))))
      (let ((result
             (agent-shell-to-go-transport-edit-message tr "C1" "TS1" "updated")))
        (should (eq t result))
        (should (equal "chat.update" called-endpoint))))))

;; start-thread

(ert-deftest agent-shell-to-go-test-slack-start-thread-returns-ts ()
  "start-thread returns the message ts."
  (let ((tr (agent-shell-to-go-test-slack--make)))
    (with-mocked-slack-api
        `((("POST" . "chat.postMessage") . ((ok . t) (ts . "ROOT-TS"))))
      (should
       (equal "ROOT-TS" (agent-shell-to-go-transport-start-thread tr "C1" "Session"))))))

;; update-thread-header

(ert-deftest agent-shell-to-go-test-slack-update-thread-header-calls-chat-update ()
  "update-thread-header calls chat.update for the given thread."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (called-endpoint nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--slack-api)
               (lambda (_method endpoint &rest _)
                 (setq called-endpoint endpoint)
                 nil)))
      (agent-shell-to-go-transport-update-thread-header tr "C1" "TS1" "Title"))
    (should (equal "chat.update" called-endpoint))))

(ert-deftest agent-shell-to-go-test-slack-update-thread-header-truncates-long-title ()
  "Titles over 80 chars are truncated before appearing in the message text."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (long-title (make-string 90 ?a))
         (sent-text nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--slack-api)
               (lambda (_method _endpoint &optional data)
                 (setq sent-text (map-elt data 'text))
                 nil)))
      (agent-shell-to-go-transport-update-thread-header tr "C1" "TS1" long-title))
    (should sent-text)
    (should (not (string-match-p long-title sent-text)))))

;; delete-message

(ert-deftest agent-shell-to-go-test-slack-delete-message-calls-chat-delete ()
  "delete-message calls chat.delete with the correct channel and ts."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (called-endpoint nil)
         (sent-data nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--slack-api)
               (lambda (_method endpoint &optional data)
                 (setq called-endpoint endpoint sent-data data)
                 nil)))
      (agent-shell-to-go-transport-delete-message tr "C1" "TS1"))
    (should (equal "chat.delete" called-endpoint))
    (should (equal "C1" (map-elt sent-data 'channel)))
    (should (equal "TS1" (map-elt sent-data 'ts)))))

;; delete-thread

(ert-deftest agent-shell-to-go-test-slack-delete-thread-deletes-all-messages ()
  "delete-thread deletes every message returned by conversations.replies."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (deleted-ts nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--slack-api)
               (lambda (method endpoint &optional data)
                 (cond
                  ((equal method "GET")
                   `((ok . t)
                     (messages . [((ts . "TS1")) ((ts . "TS2"))])
                     (response_metadata . ((next_cursor . "")))))
                  ((equal method "POST")
                   (push (map-elt data 'ts) deleted-ts)
                   '((ok . t)))))))
      (agent-shell-to-go-transport-delete-thread tr "C1" "ROOT-TS"))
    (should (= 2 (length deleted-ts)))))

;; fetch-thread-replies

(ert-deftest agent-shell-to-go-test-slack-fetch-thread-replies ()
  "fetch-thread-replies returns plists in API order."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (msgs
          (vector
           '((ts . "TS1") (user . "U1") (text . "first"))
           '((ts . "TS2") (user . "U2") (text . "second")))))
    (with-mocked-slack-api
        `((("GET" . "conversations.replies?channel=C1&ts=ROOT-TS")
           .
           ((ok . t) (messages . ,msgs))))
      (let ((replies
             (agent-shell-to-go-transport-fetch-thread-replies tr "C1" "ROOT-TS")))
        (should (= 2 (length replies)))
        (should (equal "TS1" (plist-get (car replies) :msg-id)))
        (should (equal "first" (plist-get (car replies) :text)))
        (should (equal "TS2" (plist-get (cadr replies) :msg-id)))))))

;; get-message-text

(ert-deftest agent-shell-to-go-test-slack-get-message-text ()
  "get-message-text returns the text field from the history API."
  (let ((tr (agent-shell-to-go-test-slack--make)))
    (with-mocked-slack-api
        `((("GET"
            .
            "conversations.history?channel=C1&latest=TS1&limit=1&inclusive=true")
           .
           ((ok . t) (messages . [((text . "fetched text"))]))))
      (should
       (equal
        "fetched text" (agent-shell-to-go-transport-get-message-text tr "C1" "TS1"))))))

;; get-reactions

(ert-deftest agent-shell-to-go-test-slack-get-reactions-returns-actions ()
  "get-reactions maps known emoji to canonical action symbols."
  (let ((tr (agent-shell-to-go-test-slack--make)))
    (with-mocked-slack-api
        `((("GET" . "reactions.get?channel=C1&timestamp=TS1")
           .
           ((ok . t)
            (message
             .
             ((reactions
               .
               [((name . "eyes") (count . 1)) ((name . "x") (count . 1))]))))))
      (let ((result (agent-shell-to-go-transport-get-reactions tr "C1" "TS1")))
        (should (member 'expand-truncated result))
        (should (member 'permission-reject result))))))

;; upload-file

(ert-deftest agent-shell-to-go-test-slack-upload-file-skips-missing-file ()
  "upload-file does nothing when the path does not exist on disk."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (api-called nil))
    (cl-letf (((symbol-function 'agent-shell-to-go--slack-api)
               (lambda (&rest _)
                 (setq api-called t)
                 nil)))
      (agent-shell-to-go-transport-upload-file tr "C1" nil "/no/such/file.txt"))
    (should (null api-called))))

(ert-deftest agent-shell-to-go-test-slack-upload-file-uses-thread-ts ()
  "upload-file includes thread_ts in the complete-upload payload when thread-id provided."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (complete-data nil)
         (tmpfile (make-temp-file "astg-slack-upload")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-to-go--slack-api)
                   (lambda (method endpoint &optional data)
                     (cond
                      ((string-prefix-p "GET" method)
                       '((upload_url . "https://files.slack.com/upload/v1/x")
                         (file_id . "FID1")))
                      ((equal endpoint "files.completeUploadExternal")
                       (setq complete-data data)
                       nil))))
                  ((symbol-function 'call-process) (lambda (&rest _) 0)))
          (agent-shell-to-go-transport-upload-file tr "C1" "THREAD-TS" tmpfile))
      (delete-file tmpfile))
    (should complete-data)
    (should (equal "THREAD-TS" (map-elt complete-data 'thread_ts)))))

(ert-deftest agent-shell-to-go-test-slack-upload-file-uses-channel-fallback ()
  "upload-file posts to channel_id only when thread-id is nil."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (complete-data nil)
         (tmpfile (make-temp-file "astg-slack-upload")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-to-go--slack-api)
                   (lambda (method endpoint &optional data)
                     (cond
                      ((string-prefix-p "GET" method)
                       '((upload_url . "https://files.slack.com/upload/v1/x")
                         (file_id . "FID1")))
                      ((equal endpoint "files.completeUploadExternal")
                       (setq complete-data data)
                       nil))))
                  ((symbol-function 'call-process) (lambda (&rest _) 0)))
          (agent-shell-to-go-transport-upload-file tr "C1" nil tmpfile))
      (delete-file tmpfile))
    (should complete-data)
    (should (equal "C1" (map-elt complete-data 'channel_id)))
    (should (null (map-elt complete-data 'thread_ts)))))

; 6. Channel management

;; Persistence

(ert-deftest agent-shell-to-go-test-slack-save-channels ()
  "save-channels writes the project-to-channel map to disk as an alist."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (tmpfile (make-temp-file "astg-slack-chans")))
    (unwind-protect
        (let ((agent-shell-to-go-slack-channels-file tmpfile))
          (puthash
           "/proj1" "C1" (agent-shell-to-go-slack-transport-project-channels tr))
          (agent-shell-to-go--slack-save-channels tr)
          (with-temp-buffer
            (insert-file-contents tmpfile)
            (let ((data (read (current-buffer))))
              (should (equal "C1" (cdr (assoc "/proj1" data)))))))
      (delete-file tmpfile))))

(ert-deftest agent-shell-to-go-test-slack-load-channels ()
  "load-channels reads the alist from disk into the transport's hash table."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (tmpfile (make-temp-file "astg-slack-chans")))
    (unwind-protect
        (let ((agent-shell-to-go-slack-channels-file tmpfile))
          (with-temp-file tmpfile
            (insert "((\"/proj1\" . \"C1\") (\"/proj2\" . \"C2\"))"))
          (agent-shell-to-go--slack-load-channels tr)
          (let ((table (agent-shell-to-go-slack-transport-project-channels tr)))
            (should (equal "C1" (gethash "/proj1" table)))
            (should (equal "C2" (gethash "/proj2" table)))))
      (delete-file tmpfile))))

(ert-deftest agent-shell-to-go-test-slack-channels-round-trip ()
  "Saving then loading channels in a fresh transport preserves all mappings."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (tr2 (agent-shell-to-go-test-slack--make))
         (tmpfile (make-temp-file "astg-slack-chans")))
    (unwind-protect
        (let ((agent-shell-to-go-slack-channels-file tmpfile))
          (puthash
           "/proj" "C-X" (agent-shell-to-go-slack-transport-project-channels tr))
          (agent-shell-to-go--slack-save-channels tr)
          (agent-shell-to-go--slack-load-channels tr2)
          (should
           (equal
            "C-X"
            (gethash "/proj" (agent-shell-to-go-slack-transport-project-channels tr2)))))
      (delete-file tmpfile))))

;; get-or-create-project-channel

(ert-deftest agent-shell-to-go-test-slack-get-or-create-channel-cache-hit ()
  "Cache hit returns the cached ID without making any API call."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (api-called nil)
         (agent-shell-to-go-slack-per-project-channels t))
    (puthash
     "/proj" "CACHED-ID" (agent-shell-to-go-slack-transport-project-channels tr))
    (cl-letf (((symbol-function 'agent-shell-to-go--slack-api)
               (lambda (&rest _)
                 (setq api-called t)
                 nil)))
      (let ((id
             (agent-shell-to-go--slack-get-or-create-project-channel tr "/proj")))
        (should (equal "CACHED-ID" id))
        (should (null api-called))))))

(ert-deftest agent-shell-to-go-test-slack-get-or-create-channel-creates-new ()
  "Cache miss: creates a new channel via conversations.create."
  (let* ((tr (agent-shell-to-go-test-slack--make))
         (agent-shell-to-go-slack-per-project-channels t)
         (agent-shell-to-go-slack-channel-prefix "")
         (agent-shell-to-go-slack-user-id nil)
         (tmpfile (make-temp-file "astg-slack-chans")))
    (unwind-protect
        (let ((agent-shell-to-go-slack-channels-file tmpfile))
          (with-mocked-slack-api
              `((("POST" . "conversations.create")
                 .
                 ((ok . t) (channel . ((id . "NEW-CHAN"))))))
            (let ((id
                   (agent-shell-to-go--slack-get-or-create-project-channel
                    tr "/path/to/myproject")))
              (should (equal "NEW-CHAN" id)))))
      (delete-file tmpfile))))

(provide 'agent-shell-to-go-slack-test)
;;; agent-shell-to-go-slack-test.el ends here
