;;; agent-shell-to-go-bridge-test.el --- Unit tests for agent-shell-to-go-bridge.el -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for agent-shell-to-go-bridge.el.  Each test exercises one
;; bridge behaviour in isolation using a real agent-shell session backed by
;; mock-acp and a mock transport — no Slack/Discord credentials required.
;;
;; The full pipeline under test:
;;
;;   ACP protocol → agent-shell → bridge → mock transport
;;
;; Prerequisites:
;;   - Run `make test' from the project root; it clones and builds all deps automatically.
;;
;; Run:
;;   make test TEST=agent-shell-to-go-test-bridge.el
;;
;; APIs under test:
;;
;;   agent-shell-to-go--on-send-command
;;     - user-message-echoed: user prompt echo to transport
;;   agent-shell-to-go--on-input-submitted
;;     - no-spinner-on-local-prompt: local prompts are echoed but produce no spinner
;;     - spinner-sent-on-remote-prompt: spinner verb sent when remote prompt is submitted
;;   agent-shell-to-go--on-turn-complete
;;     - agent-message-forwarded: agent message forwarding on turn end
;;     - agent-message-multiple-chunks-forwarded: multiple chunks accumulated into one send
;;     - remote-message-not-echoed: remote-injected messages produce no [user] echo
;;   agent-shell-to-go--bridge-on-tool-call-update
;;     - tool-call-forwarded: tool call forwarding (generic)
;;     - tool-call-output-shown: show-tool-output t: full text output
;;     - tool-call-output-hidden: show-tool-output nil: [ok] icon only
;;     - tool-call-diff-shown: diff branch, show-tool-output t
;;     - tool-call-diff-hidden: diff branch, show-tool-output nil
;;   agent-shell-to-go--permission-responder
;;     - permission-forwarded: permission request forwarding
;;   agent-shell-to-go--bridge-on-reaction
;;     - permission-allow: permission-allow clears pending entry
;;     - permission-reject: permission-reject clears pending entry
;;   agent-shell-to-go--dispatch / agent-shell-to-go--commands
;;     - help-command: !help command response
;;     - info-command: !info command response
;;     - bypass-permissions command (!yolo, !bypass): sets bypassPermissions mode
;;     - accept-edits command (!safe, !accept, !acceptedits): sets acceptEdits mode
;;     - plan command (!plan, !planmode): sets plan mode
;;     - mode-command: !mode returns current mode name
;;     - stop-command: !stop interrupts a long-running agent
;;     - kill-command: !kill kills the buffer without restarting; "Session ended" is sent
;;     - restart-command: !restart synchronously kills buffer, spawns new one with mode re-enabled
;;     - new-agent-unknown-project: !new-agent with unknown name replies with usage error
;;     - new-agent-absolute-path: !new-agent with absolute path replies with usage error
;;     - new-agent-success: !new-agent with valid project name starts agent in projects-directory/name
;;     - new-agent-no-arg: !new-agent with no arg starts a new agent in the current project
;;     - new-project-invalid-name: !new-project with disallowed chars replies with usage error
;;     - new-project-missing-arg: !new-project with no arg replies with usage message
;;     - new-project-existing-dir: !new-project with existing directory replies with error
;;     - new-project-success: !new-project creates directory and starts agent
;;     - resume-lists-sessions: !resume with no arg returns numbered session list
;;     - resume-empty-sessions: !resume with no arg reports no sessions
;;     - resume-fetch-failure: !resume with no arg reports ACP fetch failure
;;     - resume-default-first: !resume 1 resumes first session
;;     - resume-nth: !resume 2 resumes second session
;;     - resume-uses-cache: !resume 1 after !resume uses cached session list
;;     - resume-out-of-range: !resume N > count replies with error
;;     - resume-no-project: !resume when no project resolves replies with cannot-determine
;;     - resume-session-error: !resume 1 reports error when resume-session signals
;;   agent-shell-to-go--on-init-client
;;     - on-init-client-nil-client: failure branch when :client is nil
;;   agent-shell-to-go--on-input-submitted (see spinner-sent-on-* above)
;;   agent-shell-to-go--on-error
;;     - on-error-server-init-error: ACP init error forwarding
;;     - on-error-auth-failure: ACP auth error forwarding
;;     - on-error-prompt-failure: prompt error forwarding

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'agent-shell)
(require 'agent-shell-mock-agent)
(require 'agent-shell-to-go)
(require 'mock-transport)

;;; Configuration

(defconst agent-shell-to-go-test-bridge--mock-acp-root
  (expand-file-name "deps/mock-acp"
                    (file-name-directory (or load-file-name buffer-file-name ".")))
  "Root directory of the mock-acp project.")

(defconst agent-shell-to-go-test-bridge--python
  (expand-file-name ".venv/bin/python" agent-shell-to-go-test-bridge--mock-acp-root)
  "Python interpreter in mock-acp's virtual environment.")

(defconst agent-shell-to-go-test-bridge--mock-acp-main
  (expand-file-name "src/main.py" agent-shell-to-go-test-bridge--mock-acp-root)
  "Absolute path to mock-acp main.py.")

(defconst agent-shell-to-go-test-bridge--mock-acp-init-error-server
  (expand-file-name "src/init_error_server.py" agent-shell-to-go-test-bridge--mock-acp-root)
  "Absolute path to mock-acp init error server.")

(defconst agent-shell-to-go-test-bridge--mock-acp-auth-error-server
  (expand-file-name "src/auth_error_server.py" agent-shell-to-go-test-bridge--mock-acp-root)
  "Absolute path to mock-acp auth error server.")

;;; Helpers

(defun agent-shell-to-go-test-bridge--wait-until (pred &optional timeout-sec)
  "Poll PRED every 100ms for up to TIMEOUT-SEC seconds (default 10).
Pumps process output each iteration.  Returns truthy on success, nil on timeout."
  (let ((deadline (+ (float-time) (or timeout-sec 10))))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (funcall pred)))

(defun agent-shell-to-go-test-bridge--session-id (buf)
  "Return the ACP session ID for BUF, or nil if not yet established."
  (and (buffer-live-p buf)
       (with-current-buffer buf
         (map-nested-elt agent-shell--state '(:session :id)))))

(defun agent-shell-to-go-test-bridge--sent-texts (transport)
  "Return all text payloads sent via TRANSPORT in call order."
  (mapcar
   (lambda (c) (nth 3 c)) (agent-shell-to-go-test-outbound-calls transport 'send-text)))

(defun agent-shell-to-go-test-bridge--edited-texts (transport)
  "Return all text payloads edited via TRANSPORT in call order."
  (mapcar
   (lambda (c) (nth 3 c))
   (agent-shell-to-go-test-outbound-calls transport 'edit-message)))

(defun agent-shell-to-go-test-bridge--wait-for-ready (transport &optional timeout)
  "Wait until TRANSPORT has received a Ready signal from the bridge.
Returns non-nil on success, nil on timeout."
  (agent-shell-to-go-test-bridge--wait-until
   (lambda ()
     (cl-some
      (lambda (text) (string-match-p "Ready" text))
      (agent-shell-to-go-test-bridge--sent-texts transport)))
   (or timeout 15)))

(defun agent-shell-to-go-test-bridge--start-session ()
  "Start an agent-shell session backed by mock-acp and return the buffer.

Configures `agent-shell-mock-agent-acp-command' to point at the local
mock-acp Python environment and calls `agent-shell-start'.  The caller
is responsible for cleanup (kill the buffer when done)."
  (let* ((default-directory agent-shell-to-go-test-bridge--mock-acp-root)
         (agent-shell-mock-agent-acp-command
          (list
           agent-shell-to-go-test-bridge--python
           agent-shell-to-go-test-bridge--mock-acp-main)))
    (agent-shell-start :config (agent-shell-mock-agent-make-agent-config))))

(defmacro agent-shell-to-go-test-bridge--with-session (transport buf &rest body)
  "Evaluate BODY with TRANSPORT bound to a mock transport and BUF to a live agent-shell buffer.

Starts an agent-shell session connected to mock-acp, waits for the ACP
session to be established, enables `agent-shell-to-go-mode' on BUF using
TRANSPORT, waits for the bridge to initialize, then evaluates BODY.  Always kills BUF on exit."
  (declare (indent 2))
  `(let* ((,transport (agent-shell-to-go-test-make))
          (,buf (agent-shell-to-go-test-bridge--start-session)))
     (unwind-protect
         (progn
           (unless (agent-shell-to-go-test-bridge--wait-until
                    (lambda () (agent-shell-to-go-test-bridge--session-id ,buf))
                    15)
             (error "Timed out waiting for mock-acp session to establish"))
           (with-current-buffer ,buf
             (cl-letf (((symbol-function 'agent-shell-to-go--get-transport)
                        (lambda () ,transport)))
               (agent-shell-to-go-mode 1)))
           ,@body)
       (setq agent-shell-to-go--pending-permissions nil)
       (when (buffer-live-p ,buf)
         (kill-buffer ,buf)))))

(defun agent-shell-to-go-test-bridge--start-session-with-server
    (server &optional config-fn)
  "Like `agent-shell-to-go-test-bridge--start-session' but using SERVER script.
SERVER is a path relative to the mock-acp root (e.g. \"src/init_error_server.py\").
CONFIG-FN, if provided, is called with the base config alist and must return
the final config (use it to add extra keys such as :authenticate-request-maker)."
  (let ((agent-shell-mock-agent-acp-command
         (list agent-shell-to-go-test-bridge--python server))
        (default-directory agent-shell-to-go-test-bridge--mock-acp-root))
    (let ((config (agent-shell-mock-agent-make-agent-config)))
      (agent-shell-start
       :config
       (if config-fn
           (funcall config-fn config)
         config)))))

(defmacro agent-shell-to-go-test-bridge--with-error-session
    (transport buf server config-fn &rest body)
  "Like `agent-shell-to-go-test-bridge--with-session' but for error-path testing.

Starts a session using SERVER script (path relative to mock-acp root), applies
CONFIG-FN (or nil) to the base config, enables `agent-shell-to-go-mode'
immediately (without waiting for session establishment), then evaluates BODY.
Use this when the server is expected to fail during ACP initialization so the
normal session-ID wait would time out."
  (declare (indent 4))
  `(let* ((,transport (agent-shell-to-go-test-make))
          (,buf
           (agent-shell-to-go-test-bridge--start-session-with-server ,server
                                                                     ,config-fn)))
     (unwind-protect
         (progn
           (with-current-buffer ,buf
             (cl-letf (((symbol-function 'agent-shell-to-go--get-transport)
                        (lambda () ,transport)))
               (agent-shell-to-go-mode 1)))
           ,@body)
       (setq agent-shell-to-go--pending-permissions nil)
       (when (buffer-live-p ,buf)
         (kill-buffer ,buf)))))

(defmacro agent-shell-to-go-test-bridge--with-mode-stub (&rest body)
  "Evaluate BODY with `agent-shell--send-request' stubbed to call on-success immediately."
  `(cl-letf (((symbol-function 'agent-shell--send-request)
              (lambda (&rest args)
                (when-let* ((on-success (plist-get args :on-success)))
                  (funcall on-success nil)))))
     ,@body))

(defun agent-shell-to-go-test-bridge--send-prompt (buf text)
  "Send TEXT as a prompt in the agent-shell buffer BUF."
  (agent-shell-insert :text text :submit t :no-focus t :shell-buffer buf))

;;; Tests

(ert-deftest agent-shell-to-go-test-bridge-user-message-echoed ()
  "The user prompt is echoed to the transport."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (agent-shell-to-go-test-bridge--send-prompt buf "test agent_message")
    (should (agent-shell-to-go-test-bridge--wait-for-ready tr))
    (let ((texts (agent-shell-to-go-test-bridge--sent-texts tr)))
      (should (cl-some (lambda (s) (string-match-p "test agent_message" s)) texts)))))

(ert-deftest agent-shell-to-go-test-bridge-agent-message-forwarded ()
  "Agent message chunks are accumulated and forwarded to the transport on turn-complete."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (agent-shell-to-go-test-bridge--send-prompt buf "test agent_message")
    (should (agent-shell-to-go-test-bridge--wait-for-ready tr))
    (let ((texts (agent-shell-to-go-test-bridge--sent-texts tr)))
      (should (cl-some (lambda (t) (string-match-p "Paris" t)) texts)))))

(ert-deftest agent-shell-to-go-test-bridge-agent-message-multiple-chunks-forwarded ()
  "Multiple agent message chunks are accumulated into a single transport send on turn-complete."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (agent-shell-to-go-test-bridge--send-prompt buf "test long_running")
    (should (agent-shell-to-go-test-bridge--wait-for-ready tr 20))
    (let* ((texts (agent-shell-to-go-test-bridge--sent-texts tr))
           (agent-texts
            (cl-remove-if-not (lambda (t) (string-match-p "Paris" t)) texts)))
      ;; All three chunks must be concatenated into one forwarded message,
      ;; not sent as three separate messages.
      (should (= 1 (length agent-texts)))
      (should (= 4 (length (split-string (car agent-texts) "Paris")))))))

(ert-deftest agent-shell-to-go-test-bridge-tool-call-forwarded ()
  "Tool call results are forwarded to the transport."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (agent-shell-to-go-test-bridge--send-prompt buf "test tool_call")
    (should (agent-shell-to-go-test-bridge--wait-for-ready tr))
    (let ((texts (agent-shell-to-go-test-bridge--sent-texts tr)))
      ;; Bridge forwards tool call results — either [ok]/[fail] icon or full
      ;; formatted output depending on agent-shell-to-go-show-tool-output.
      (should
       (cl-some
        (lambda (t)
          (string-match-p "\\[ok\\]\\|\\[fail\\]\\|\\[completed\\]\\|\\[running\\]" t))
        texts)))))

;;; tool-call-update branching

(ert-deftest agent-shell-to-go-test-bridge-tool-call-output-shown ()
  "With show-tool-output t, completed tool call edits the start message with full text output."
  (let ((agent-shell-to-go-show-tool-output t))
    (agent-shell-to-go-test-bridge--with-session tr buf
      (agent-shell-to-go-test-bridge--send-prompt buf "test tool_call_read")
      (should (agent-shell-to-go-test-bridge--wait-for-ready tr))
      (should
       (cl-some
        (lambda (text) (string-match-p "File read successfully" text))
        (agent-shell-to-go-test-bridge--edited-texts tr))))))

(ert-deftest agent-shell-to-go-test-bridge-tool-call-output-hidden ()
  "With show-tool-output nil, completed tool call edits the start message with icon and title."
  (let ((agent-shell-to-go-show-tool-output nil))
    (agent-shell-to-go-test-bridge--with-session tr buf
      (agent-shell-to-go-test-bridge--send-prompt buf "test tool_call_read")
      (should (agent-shell-to-go-test-bridge--wait-for-ready tr))
      (let ((sent (agent-shell-to-go-test-bridge--sent-texts tr))
            (edited (agent-shell-to-go-test-bridge--edited-texts tr)))
        (should-not
         (cl-some (lambda (text) (string-match-p "File read successfully" text)) sent))
        (should (cl-some (lambda (text) (string-match-p "completed" text)) edited))))))

(ert-deftest agent-shell-to-go-test-bridge-tool-call-diff-shown ()
  "With show-tool-output t, start message shows diff and completion edits the start message."
  (let ((agent-shell-to-go-show-tool-output t))
    (agent-shell-to-go-test-bridge--with-session tr buf
      (agent-shell-to-go-test-bridge--send-prompt buf "test tool_call_diff")
      (should (agent-shell-to-go-test-bridge--wait-for-ready tr))
      (should
       (cl-some
        (lambda (text)
          (string-match-p "diff omitted in test transport" text))
        (agent-shell-to-go-test-bridge--sent-texts tr)))
      (should (agent-shell-to-go-test-outbound-calls tr 'edit-message)))))

(ert-deftest agent-shell-to-go-test-bridge-tool-call-diff-hidden ()
  "With show-tool-output nil, completion edits the start message with icon and title."
  (let ((agent-shell-to-go-show-tool-output nil))
    (agent-shell-to-go-test-bridge--with-session tr buf
      (agent-shell-to-go-test-bridge--send-prompt buf "test tool_call_diff")
      (should (agent-shell-to-go-test-bridge--wait-for-ready tr))
      (let ((sent (agent-shell-to-go-test-bridge--sent-texts tr))
            (edited (agent-shell-to-go-test-bridge--edited-texts tr)))
        (should-not
         (cl-some
          (lambda (text) (string-match-p "diff omitted in test transport" text)) sent))
        (should (cl-some (lambda (text) (string-match-p "completed" text)) edited))))))

;;; permission responder + reaction

(ert-deftest agent-shell-to-go-test-bridge-permission-forwarded ()
  "Permission request from agent sends 'Permission Required' message to transport."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (agent-shell-to-go-test-bridge--send-prompt buf "test request_permission")
    (should
     (agent-shell-to-go-test-bridge--wait-until
      (lambda ()
        (cl-some
         (lambda (text) (string-match-p "Permission Required" text))
         (agent-shell-to-go-test-bridge--sent-texts tr)))))
    ;; Respond to unblock the mock-acp so it can send PromptResponse.
    (let* ((key (caar agent-shell-to-go--pending-permissions))
           (msg-id (nth 2 key))
           (channel (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (agent-shell-to-go-test-inbound-reaction
       tr channel msg-id "testuser" 'permission-allow t))
    (should (agent-shell-to-go-test-bridge--wait-for-ready tr))))

(ert-deftest agent-shell-to-go-test-bridge-permission-allow ()
  "permission-allow reaction removes the pending permission entry."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (agent-shell-to-go-test-bridge--send-prompt buf "test request_permission")
    (should
     (agent-shell-to-go-test-bridge--wait-until
      (lambda ()
        (and agent-shell-to-go--pending-permissions
             (cl-some
              (lambda (text) (string-match-p "Permission Required" text))
              (agent-shell-to-go-test-bridge--sent-texts tr))))))
    (let* ((key (caar agent-shell-to-go--pending-permissions))
           (msg-id (nth 2 key))
           (channel (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (agent-shell-to-go-test-inbound-reaction
       tr channel msg-id "testuser" 'permission-allow t)
      (should (null agent-shell-to-go--pending-permissions))
      (should (agent-shell-to-go-test-bridge--wait-for-ready tr)))))

(ert-deftest agent-shell-to-go-test-bridge-permission-reject ()
  "permission-reject reaction removes the pending permission entry."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (agent-shell-to-go-test-bridge--send-prompt buf "test request_permission")
    (should
     (agent-shell-to-go-test-bridge--wait-until
      (lambda ()
        (and agent-shell-to-go--pending-permissions
             (cl-some
              (lambda (text) (string-match-p "Permission Required" text))
              (agent-shell-to-go-test-bridge--sent-texts tr))))))
    (let* ((key (caar agent-shell-to-go--pending-permissions))
           (msg-id (nth 2 key))
           (channel (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (agent-shell-to-go-test-inbound-reaction
       tr channel msg-id "testuser" 'permission-reject t)
      (should (null agent-shell-to-go--pending-permissions))
      (should (agent-shell-to-go-test-bridge--wait-for-ready tr)))))

;;; inbound hook handling

(ert-deftest agent-shell-to-go-test-bridge-remote-message-not-echoed ()
  "Remote-injected messages produce no [user] echo from `--on-send-command'.
Exercises the `agent-shell-to-go--remote-queued' suppression: `--on-send-command'
takes the dequeue branch and does not echo the message back."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((thread-id (buffer-local-value 'agent-shell-to-go--thread-id buf))
          (channel-id (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (agent-shell-to-go-test-inbound-message
       tr channel-id thread-id "testuser" "test agent_message")
      (should (agent-shell-to-go-test-bridge--wait-for-ready tr))
      (let ((texts (agent-shell-to-go-test-bridge--sent-texts tr)))
        (should-not
         (cl-some
          (lambda (s) (string-match-p "\\[user\\].*test agent_message" s)) texts))))))

(ert-deftest agent-shell-to-go-test-bridge-spinner-sent-on-remote-prompt ()
  "Remote-injected prompts receive a spinner verb via `--on-input-submitted'."
  (let ((agent-shell-to-go-spinner-verbs '("Tinkering")))
    (agent-shell-to-go-test-bridge--with-session tr buf
      (let ((thread-id (buffer-local-value 'agent-shell-to-go--thread-id buf))
            (channel-id (buffer-local-value 'agent-shell-to-go--channel-id buf)))
        (agent-shell-to-go-test-inbound-message
         tr channel-id thread-id "testuser" "first remote message")
        (should (agent-shell-to-go-test-bridge--wait-for-ready tr))
        (let ((texts (agent-shell-to-go-test-bridge--sent-texts tr)))
          (should
           (cl-some (lambda (s) (string-match-p "Tinkering\\.\\.\\." s)) texts)))))))

(ert-deftest agent-shell-to-go-test-bridge-no-spinner-on-local-prompt ()
  "Local prompts are echoed to the transport but do not produce a spinner verb."
  (let ((agent-shell-to-go-spinner-verbs '("Tinkering")))
    (agent-shell-to-go-test-bridge--with-session tr buf
      (agent-shell-to-go-test-bridge--send-prompt buf "test agent_message")
      (should (agent-shell-to-go-test-bridge--wait-for-ready tr))
      (let ((texts (agent-shell-to-go-test-bridge--sent-texts tr)))
        (should-not
         (cl-some (lambda (s) (string-match-p "Tinkering\\.\\.\\." s)) texts))))))

(ert-deftest agent-shell-to-go-test-bridge-help-command ()
  "!help sends a command reference synchronously without touching agent-shell.
Exercises `agent-shell-to-go--dispatch' via the message hook."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((thread-id (buffer-local-value 'agent-shell-to-go--thread-id buf))
          (channel-id (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (agent-shell-to-go-test-inbound-message
       tr channel-id thread-id "testuser" "!help")
      (should
       (cl-some
        (lambda (text) (string-match-p "Commands" text))
        (agent-shell-to-go-test-bridge--sent-texts tr))))))

(ert-deftest agent-shell-to-go-test-bridge-info-command ()
  "!info sends buffer/thread/channel/session info synchronously.
Exercises `agent-shell-to-go--dispatch' via the message hook."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((thread-id (buffer-local-value 'agent-shell-to-go--thread-id buf))
          (channel-id (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (agent-shell-to-go-test-inbound-message
       tr channel-id thread-id "testuser" "!info")
      (let ((texts (agent-shell-to-go-test-bridge--sent-texts tr)))
        (should (cl-some (lambda (text) (string-match-p "Thread" text)) texts))
        (should (cl-some (lambda (text) (string-match-p "Channel" text)) texts))
        (should (cl-some (lambda (text) (string-match-p "Session" text)) texts))))))

;;; mode commands

(ert-deftest agent-shell-to-go-test-bridge-yolo-command ()
  "!yolo sets bypassPermissions mode and notifies the transport."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((thread-id (buffer-local-value 'agent-shell-to-go--thread-id buf))
          (channel-id (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (agent-shell-to-go-test-bridge--with-mode-stub
       (agent-shell-to-go-test-inbound-message
        tr channel-id thread-id "testuser" "!yolo"))
      (should
       (cl-some
        (lambda (text) (string-match-p "Bypass Permissions" text))
        (agent-shell-to-go-test-bridge--sent-texts tr)))
      (should
       (equal
        "bypassPermissions"
        (with-current-buffer buf
          (map-nested-elt agent-shell--state '(:session :mode-id))))))))

(ert-deftest agent-shell-to-go-test-bridge-bypass-command ()
  "!bypass is an alias for !yolo — sets bypassPermissions mode."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((thread-id (buffer-local-value 'agent-shell-to-go--thread-id buf))
          (channel-id (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (agent-shell-to-go-test-bridge--with-mode-stub
       (agent-shell-to-go-test-inbound-message
        tr channel-id thread-id "testuser" "!bypass"))
      (should
       (cl-some
        (lambda (text) (string-match-p "Bypass Permissions" text))
        (agent-shell-to-go-test-bridge--sent-texts tr)))
      (should
       (equal
        "bypassPermissions"
        (with-current-buffer buf
          (map-nested-elt agent-shell--state '(:session :mode-id))))))))

(ert-deftest agent-shell-to-go-test-bridge-safe-command ()
  "!safe sets acceptEdits mode and notifies the transport."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((thread-id (buffer-local-value 'agent-shell-to-go--thread-id buf))
          (channel-id (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (agent-shell-to-go-test-bridge--with-mode-stub
       (agent-shell-to-go-test-inbound-message
        tr channel-id thread-id "testuser" "!safe"))
      (should
       (cl-some
        (lambda (text) (string-match-p "Accept Edits" text))
        (agent-shell-to-go-test-bridge--sent-texts tr)))
      (should
       (equal
        "acceptEdits"
        (with-current-buffer buf
          (map-nested-elt agent-shell--state '(:session :mode-id))))))))

(ert-deftest agent-shell-to-go-test-bridge-accept-command ()
  "!accept is an alias for !safe — sets acceptEdits mode."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((thread-id (buffer-local-value 'agent-shell-to-go--thread-id buf))
          (channel-id (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (agent-shell-to-go-test-bridge--with-mode-stub
       (agent-shell-to-go-test-inbound-message
        tr channel-id thread-id "testuser" "!accept"))
      (should
       (cl-some
        (lambda (text) (string-match-p "Accept Edits" text))
        (agent-shell-to-go-test-bridge--sent-texts tr)))
      (should
       (equal
        "acceptEdits"
        (with-current-buffer buf
          (map-nested-elt agent-shell--state '(:session :mode-id))))))))

(ert-deftest agent-shell-to-go-test-bridge-acceptedits-command ()
  "!acceptedits is an alias for !safe — sets acceptEdits mode."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((thread-id (buffer-local-value 'agent-shell-to-go--thread-id buf))
          (channel-id (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (agent-shell-to-go-test-bridge--with-mode-stub
       (agent-shell-to-go-test-inbound-message
        tr channel-id thread-id "testuser" "!acceptedits"))
      (should
       (cl-some
        (lambda (text) (string-match-p "Accept Edits" text))
        (agent-shell-to-go-test-bridge--sent-texts tr)))
      (should
       (equal
        "acceptEdits"
        (with-current-buffer buf
          (map-nested-elt agent-shell--state '(:session :mode-id))))))))

(ert-deftest agent-shell-to-go-test-bridge-plan-command ()
  "!plan sets plan mode and notifies the transport."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((thread-id (buffer-local-value 'agent-shell-to-go--thread-id buf))
          (channel-id (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (agent-shell-to-go-test-bridge--with-mode-stub
       (agent-shell-to-go-test-inbound-message
        tr channel-id thread-id "testuser" "!plan"))
      (should
       (cl-some
        (lambda (text) (string-match-p "Plan" text))
        (agent-shell-to-go-test-bridge--sent-texts tr)))
      (should
       (equal
        "plan"
        (with-current-buffer buf
          (map-nested-elt agent-shell--state '(:session :mode-id))))))))

(ert-deftest agent-shell-to-go-test-bridge-planmode-command ()
  "!planmode is an alias for !plan — sets plan mode."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((thread-id (buffer-local-value 'agent-shell-to-go--thread-id buf))
          (channel-id (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (agent-shell-to-go-test-bridge--with-mode-stub
       (agent-shell-to-go-test-inbound-message
        tr channel-id thread-id "testuser" "!planmode"))
      (should
       (cl-some
        (lambda (text) (string-match-p "Plan" text))
        (agent-shell-to-go-test-bridge--sent-texts tr)))
      (should
       (equal
        "plan"
        (with-current-buffer buf
          (map-nested-elt agent-shell--state '(:session :mode-id))))))))

(ert-deftest agent-shell-to-go-test-bridge-mode-command ()
  "!mode returns the current session mode-id."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((thread-id (buffer-local-value 'agent-shell-to-go--thread-id buf))
          (channel-id (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (with-current-buffer buf
        (let ((session (map-elt agent-shell--state :session)))
          (map-put! session :mode-id "bypassPermissions")
          (map-put! agent-shell--state :session session)))
      (agent-shell-to-go-test-inbound-message
       tr channel-id thread-id "testuser" "!mode")
      (should
       (cl-some
        (lambda (text) (string-match-p "bypassPermissions" text))
        (agent-shell-to-go-test-bridge--sent-texts tr))))))

(ert-deftest agent-shell-to-go-test-bridge-stop-command ()
  "!stop interrupts a long_running scenario and notifies the transport.
The long_running fixture sends 6 steps at 1.5 s each (~9 s total); the test
verifies the session becomes idle well before that deadline."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((thread-id (buffer-local-value 'agent-shell-to-go--thread-id buf))
          (channel-id (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (agent-shell-to-go-test-bridge--send-prompt buf "test long_running")
      ;; Wait until mock-acp has started producing output
      (should
       (agent-shell-to-go-test-bridge--wait-until (lambda ()
                                                    (with-current-buffer buf
                                                      (shell-maker-busy)))
                                                  5))
      ;; !stop fires agent-shell-interrupt synchronously then sends the notice
      (agent-shell-to-go-test-inbound-message
       tr channel-id thread-id "testuser" "!stop")
      (should
       (cl-some
        (lambda (text) (string-match-p "Agent interrupted" text))
        (agent-shell-to-go-test-bridge--sent-texts tr)))
      ;; Session must become idle well before the uninterrupted 9 s window
      (should
       (agent-shell-to-go-test-bridge--wait-until (lambda ()
                                                    (with-current-buffer buf
                                                      (not (shell-maker-busy))))
                                                  8)))))

(ert-deftest agent-shell-to-go-test-bridge-kill-command ()
  "!kill kills the agent-shell buffer and notifies the transport.
No replacement buffer is spawned, and bridge-disable sends \"Session ended\"."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((channel-id (buffer-local-value 'agent-shell-to-go--channel-id buf))
          (thread-id (buffer-local-value 'agent-shell-to-go--thread-id buf)))
      (agent-shell-to-go-test-inbound-message
       tr channel-id thread-id "testuser" "!kill")
      (should
       (cl-some
        (lambda (text) (string-match-p "Killing agent" text))
        (agent-shell-to-go-test-bridge--sent-texts tr)))
      (should (not (buffer-live-p buf)))
      (should
       (cl-some
        (lambda (text) (string-match-p "Session ended" text))
        (agent-shell-to-go-test-bridge--sent-texts tr)))
      (should-not
       (agent-shell-to-go--find-buffer-for-transport-channel-thread
        tr channel-id thread-id)))))

(ert-deftest agent-shell-to-go-test-bridge-restart-command ()
  "!restart kills the old buffer and spawns a new one with mode re-enabled.
Verifies inherit-state carries transport/channel/thread to the new buffer."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let* ( ;; Make sure that we are using the right mock-acp server
           (agent-shell-mock-agent-acp-command
            (list
             agent-shell-to-go-test-bridge--python
             agent-shell-to-go-test-bridge--mock-acp-main))
           (old-channel-id (buffer-local-value 'agent-shell-to-go--channel-id buf))
           (old-thread-id (buffer-local-value 'agent-shell-to-go--thread-id buf)))
      (agent-shell-to-go-test-inbound-message
       tr old-channel-id old-thread-id "testuser" "!restart")
      (should
       (cl-some
        (lambda (text) (string-match-p "Restarting agent" text))
        (agent-shell-to-go-test-bridge--sent-texts tr)))
      ;; Restart is synchronous — old buffer is dead and new one exists immediately
      (should (not (buffer-live-p buf)))
      ;; No "Session ended" — suppressed by --restarting flag
      (should-not
       (cl-some
        (lambda (text) (string-match-p "Session ended" text))
        (agent-shell-to-go-test-bridge--sent-texts tr)))
      (let ((new-buf
             (agent-shell-to-go--find-buffer-for-transport-channel-thread
              tr old-channel-id old-thread-id)))
        (should new-buf)
        (unwind-protect
            (with-current-buffer new-buf
              (should agent-shell-to-go-mode)
              (should (equal agent-shell-to-go--channel-id old-channel-id))
              (should (equal agent-shell-to-go--thread-id old-thread-id))
              (should
               (agent-shell-to-go-test-bridge--wait-until
                (lambda () (agent-shell-to-go-test-bridge--session-id new-buf))
                15)))
          (when (buffer-live-p new-buf)
            (kill-buffer new-buf)))))))

;;; init-client and error event handling

;;; !-command handling

(ert-deftest agent-shell-to-go-test-bridge-command-new-agent-unknown-project ()
  "!new-agent with a name not in projects-directory replies with a usage error."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let* ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf))
           (projects-dir (make-temp-file "ag2g-test-projects" t)))
      (unwind-protect
          (let ((agent-shell-to-go-projects-directory projects-dir))
            (agent-shell-to-go-test-inbound-message
             tr channel nil "testuser" "!new-agent no-such-project")
            (should
             (cl-some
              (lambda (text) (string-match-p "Usage" text))
              (agent-shell-to-go-test-bridge--sent-texts tr))))
        (delete-directory projects-dir t)))))

(ert-deftest agent-shell-to-go-test-bridge-command-new-agent-absolute-path ()
  "!new-agent with an absolute path replies with a usage error."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (agent-shell-to-go-test-inbound-message
       tr channel nil "testuser" "!new-agent /tmp/some-folder")
      (should
       (cl-some
        (lambda (text) (string-match-p "Usage" text))
        (agent-shell-to-go-test-bridge--sent-texts tr))))))

(ert-deftest agent-shell-to-go-test-bridge-command-new-agent-success ()
  "!new-agent with a project name starts agent in projects-directory/name.
Verifies bridge mode is enabled with inherited transport/channel."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let* ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf))
           (projects-dir (make-temp-file "ag2g-test-projects" t))
           (project-name "myproject")
           (project-dir (expand-file-name project-name projects-dir))
           (new-buf nil))
      (make-directory project-dir)
      (unwind-protect
          (let ((agent-shell-to-go-projects-directory projects-dir)
                (agent-shell-to-go-start-agent-function
                 (lambda ()
                   (let ((agent-shell-mock-agent-acp-command
                          (list
                           agent-shell-to-go-test-bridge--python
                           agent-shell-to-go-test-bridge--mock-acp-main))
                         (default-directory
                          agent-shell-to-go-test-bridge--mock-acp-root))
                     (setq new-buf
                           (agent-shell-start
                            :config (agent-shell-mock-agent-make-agent-config)))))))
            (agent-shell-to-go-test-inbound-message
             tr channel nil "testuser" (format "!new-agent %s" project-name))
            (should
             (cl-some
              (lambda (text) (string-match-p "Agent started in" text))
              (agent-shell-to-go-test-bridge--sent-texts tr)))
            (should (buffer-live-p new-buf))
            (with-current-buffer new-buf
              (should agent-shell-to-go-mode)
              (should (eq agent-shell-to-go--transport tr))
              (should (equal agent-shell-to-go--channel-id channel))
              (should agent-shell-to-go--thread-id)))
        (delete-directory projects-dir t)
        (when (and new-buf (buffer-live-p new-buf))
          (kill-buffer new-buf))))))


(ert-deftest agent-shell-to-go-test-bridge-command-new-agent-no-arg ()
  "!new-agent with no arg starts a new agent in the current project directory."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf))
          (new-buf nil))
      (let ((agent-shell-to-go-start-agent-function
             (lambda ()
               (let ((agent-shell-mock-agent-acp-command
                      (list
                       agent-shell-to-go-test-bridge--python
                       agent-shell-to-go-test-bridge--mock-acp-main))
                     (default-directory agent-shell-to-go-test-bridge--mock-acp-root))
                 (setq new-buf
                       (agent-shell-start
                        :config (agent-shell-mock-agent-make-agent-config)))))))
        (agent-shell-to-go-test-inbound-message tr channel nil "testuser" "!new-agent")
        (should
         (cl-some
          (lambda (text) (string-match-p "Starting new agent" text))
          (agent-shell-to-go-test-bridge--sent-texts tr)))
        (should
         (cl-some
          (lambda (text) (string-match-p "Agent started" text))
          (agent-shell-to-go-test-bridge--sent-texts tr))))
      (when (and new-buf (buffer-live-p new-buf))
        (kill-buffer new-buf)))))

(ert-deftest agent-shell-to-go-test-bridge-command-new-project-invalid-name ()
  "!new-project with a name containing disallowed chars replies with a usage error."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (agent-shell-to-go-test-inbound-message
       tr channel nil "testuser" "!new-project foo/bar")
      (should
       (cl-some
        (lambda (text) (string-match-p "Usage" text))
        (agent-shell-to-go-test-bridge--sent-texts tr))))))

(ert-deftest agent-shell-to-go-test-bridge-command-new-project-missing-arg ()
  "!new-project with no :project-name replies with usage message."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf)))
      (agent-shell-to-go-test-inbound-message tr channel nil "testuser" "!new-project")
      (should
       (cl-some
        (lambda (text) (string-match-p "Usage" text))
        (agent-shell-to-go-test-bridge--sent-texts tr))))))

(ert-deftest agent-shell-to-go-test-bridge-command-new-project-existing-dir ()
  "!new-project with an already-existing directory replies with an error."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let* ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf))
           (existing (make-temp-file "ag2g-test-proj" t)))
      (unwind-protect
          (let ((agent-shell-to-go-projects-directory
                 (file-name-parent-directory existing)))
            (agent-shell-to-go-test-inbound-message
             tr
             channel
             nil
             "testuser"
             (format "!new-project %s" (file-name-nondirectory existing)))
            (should
             (cl-some
              (lambda (text) (string-match-p "already exists" text))
              (agent-shell-to-go-test-bridge--sent-texts tr))))
        (delete-directory existing t)))))

(ert-deftest agent-shell-to-go-test-bridge-command-new-project-success ()
  "!new-project creates the directory and sends Creating/Starting/Started confirmations."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let* ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf))
           (projects-dir (make-temp-file "ag2g-test-projects-dir" t))
           (new-buf nil))
      (unwind-protect
          (let ((agent-shell-to-go-projects-directory projects-dir)
                (agent-shell-to-go-new-project-function nil)
                (agent-shell-to-go-start-agent-function
                 (lambda ()
                   (let ((agent-shell-mock-agent-acp-command
                          (list
                           agent-shell-to-go-test-bridge--python
                           agent-shell-to-go-test-bridge--mock-acp-main))
                         (default-directory
                          agent-shell-to-go-test-bridge--mock-acp-root))
                     (setq new-buf
                           (agent-shell-start
                            :config (agent-shell-mock-agent-make-agent-config)))))))
            (agent-shell-to-go-test-inbound-message
             tr channel nil "testuser" "!new-project my-proj")
            (let ((texts (agent-shell-to-go-test-bridge--sent-texts tr)))
              (should
               (cl-some (lambda (t) (string-match-p "Creating project" t)) texts))
              (should (cl-some (lambda (t) (string-match-p "Starting Claude" t)) texts))
              (should (cl-some (lambda (t) (string-match-p "Agent started" t)) texts))
              (should (file-directory-p (expand-file-name "my-proj" projects-dir)))))
        (delete-directory projects-dir t)
        (when (and new-buf (buffer-live-p new-buf))
          (kill-buffer new-buf))))))

(defmacro agent-shell-to-go-test-bridge--with-mock-acp-sessions (sessions &rest body)
  "Evaluate BODY with `acp-send-request' mocked to call :on-success with SESSIONS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'acp-send-request)
              (lambda (&rest args)
                (funcall (plist-get args :on-success)
                         (list (cons 'sessions (vconcat ,sessions)))))))
     ,@body))

(ert-deftest agent-shell-to-go-test-bridge-command-resume-lists-sessions ()
  "!resume with no arg returns a numbered list of session titles from ACP."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let* ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf))
           (agent-shell-to-go--session-list-cache nil))
      (agent-shell-to-go-test-bridge--with-mock-acp-sessions
          (list
           '((sessionId . "S1")
             (title . "Build the thing")
             (updatedAt . "2024-01-01T00:00:00Z"))
           '((sessionId . "S2")
             (title . "Fix the bug")
             (updatedAt . "2024-01-01T00:00:00Z")))
        (agent-shell-to-go-test-inbound-message tr channel nil "testuser" "!resume")
        (let ((texts (agent-shell-to-go-test-bridge--sent-texts tr)))
          (should (cl-some (lambda (t) (string-match-p "1\\." t)) texts))
          (should (cl-some (lambda (t) (string-match-p "Build the thing" t)) texts))
          (should (cl-some (lambda (t) (string-match-p "2\\." t)) texts))
          (should (cl-some (lambda (t) (string-match-p "Fix the bug" t)) texts)))))))

(ert-deftest agent-shell-to-go-test-bridge-command-resume-empty-sessions ()
  "!resume with no arg replies with no-sessions message when ACP returns an empty list."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let* ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf))
           (agent-shell-to-go--session-list-cache nil))
      (agent-shell-to-go-test-bridge--with-mock-acp-sessions nil
        (agent-shell-to-go-test-inbound-message tr channel nil "testuser" "!resume")
        (should
         (cl-some
          (lambda (text) (string-match-p "No sessions found" text))
          (agent-shell-to-go-test-bridge--sent-texts tr)))))))


(ert-deftest agent-shell-to-go-test-bridge-command-resume-fetch-failure ()
  "!resume with no arg replies with failure message when acp-send-request calls :on-failure."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let* ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf))
           (agent-shell-to-go--session-list-cache nil))
      (cl-letf (((symbol-function 'acp-send-request)
                 (lambda (&rest args)
                   (funcall (plist-get args :on-failure) "acp error" nil))))
        (agent-shell-to-go-test-inbound-message tr channel nil "testuser" "!resume")
        (should
         (cl-some
          (lambda (text)
            (string-match-p "Failed to fetch session list" text))
          (agent-shell-to-go-test-bridge--sent-texts tr)))))))

(ert-deftest agent-shell-to-go-test-bridge-command-resume-default-first ()
  "!resume 1 resumes the first session inferred from the channel."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let* ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf))
           (agent-shell-to-go--session-list-cache nil)
           (agent-shell-to-go--inherit-state nil)
           (resumed-id nil))
      (agent-shell-to-go-test-bridge--with-mock-acp-sessions
          (list
           '((sessionId . "S1") (title . "First") (updatedAt . "2024-01-01T00:00:00Z"))
           '((sessionId . "S2")
             (title . "Second")
             (updatedAt . "2024-01-01T00:00:00Z")))
        (cl-letf (((symbol-function 'agent-shell-resume-session)
                   (lambda (id) (setq resumed-id id))))
          (agent-shell-to-go-test-inbound-message tr channel nil "testuser" "!resume 1")
          (should (equal "S1" resumed-id))
          (should
           (cl-some
            (lambda (text) (string-match-p "Resuming" text))
            (agent-shell-to-go-test-bridge--sent-texts tr))))))))

(ert-deftest agent-shell-to-go-test-bridge-command-resume-nth ()
  "!resume 2 resumes the second session in the list."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let* ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf))
           (agent-shell-to-go--session-list-cache nil)
           (agent-shell-to-go--inherit-state nil)
           (resumed-id nil))
      (agent-shell-to-go-test-bridge--with-mock-acp-sessions
          (list
           '((sessionId . "S1") (title . "First") (updatedAt . "2024-01-01T00:00:00Z"))
           '((sessionId . "S2")
             (title . "Second")
             (updatedAt . "2024-01-01T00:00:00Z")))
        (cl-letf (((symbol-function 'agent-shell-resume-session)
                   (lambda (id) (setq resumed-id id))))
          (agent-shell-to-go-test-inbound-message tr channel nil "testuser" "!resume 2")
          (should (equal "S2" resumed-id)))))))

(ert-deftest agent-shell-to-go-test-bridge-command-resume-uses-cache ()
  "!resume 1 after !sessions uses the cached list without a second ACP call."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let* ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf))
           (agent-shell-to-go--session-list-cache nil)
           (agent-shell-to-go--inherit-state nil)
           (acp-call-count 0)
           (resumed-id nil))
      (cl-letf (((symbol-function 'acp-send-request)
                 (lambda (&rest args)
                   (cl-incf acp-call-count)
                   (funcall (plist-get args :on-success)
                            '((sessions
                               .
                               [((sessionId . "S1")
                                 (title . "Cached")
                                 (updatedAt . "2024-01-01T00:00:00Z"))])))))
                ((symbol-function 'agent-shell-resume-session)
                 (lambda (id) (setq resumed-id id))))
        (agent-shell-to-go-test-inbound-message tr channel nil "testuser" "!resume")
        (agent-shell-to-go-test-inbound-message tr channel nil "testuser" "!resume 1")
        (should (equal "S1" resumed-id))
        (should (= 1 acp-call-count))))))

(ert-deftest agent-shell-to-go-test-bridge-command-resume-out-of-range ()
  "!resume N where N exceeds the session count replies with an error."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let* ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf))
           (agent-shell-to-go--session-list-cache nil)
           (agent-shell-to-go--inherit-state nil))
      (agent-shell-to-go-test-bridge--with-mock-acp-sessions
          (list
           '((sessionId . "S1")
             (title . "Only one")
             (updatedAt . "2024-01-01T00:00:00Z")))
        (cl-letf (((symbol-function 'agent-shell-resume-session) (lambda (_id) nil)))
          (agent-shell-to-go-test-inbound-message tr channel nil "testuser" "!resume 5")
          (should
           (cl-some
            (lambda (text) (string-match-p "No session #5" text))
            (agent-shell-to-go-test-bridge--sent-texts tr))))))))

(ert-deftest agent-shell-to-go-test-bridge-command-resume-no-project ()
  "!resume without :project-name when no project resolves replies with cannot-determine."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf))
          (agent-shell-to-go--session-list-cache nil))
      (cl-letf (((symbol-function 'agent-shell-to-go--resolve-project-from-channel)
                 (lambda (_) nil)))
        (agent-shell-to-go-test-inbound-message tr channel nil "testuser" "!resume")
        (should
         (cl-some
          (lambda (text)
            (string-match-p "Cannot determine project" text))
          (agent-shell-to-go-test-bridge--sent-texts tr)))))))

(ert-deftest agent-shell-to-go-test-bridge-command-resume-session-error ()
  "/resume replies with failure message when agent-shell-resume-session signals an error."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let* ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf))
           (agent-shell-to-go--session-list-cache nil)
           (agent-shell-to-go--inherit-state nil))
      (agent-shell-to-go-test-bridge--with-mock-acp-sessions
          (list
           '((sessionId . "S1")
             (title . "Error session")
             (updatedAt . "2024-01-01T00:00:00Z")))
        (cl-letf (((symbol-function 'agent-shell-resume-session)
                   (lambda (_id) (error "Resume failed for testing"))))
          (agent-shell-to-go-test-inbound-message tr channel nil "testuser" "!resume 1")
          (should
           (cl-some
            (lambda (text) (string-match-p "Failed to resume" text))
            (agent-shell-to-go-test-bridge--sent-texts tr))))))))

(ert-deftest agent-shell-to-go-test-bridge-command-project-lists-subdirs ()
  "/project lists non-hidden subdirectories under agent-shell-to-go-projects-directory."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let* ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf))
           (tmpdir (make-temp-file "ag2g-test-projects" t)))
      (unwind-protect
          (let ((agent-shell-to-go-projects-directory tmpdir))
            (make-directory (expand-file-name "alpha" tmpdir))
            (make-directory (expand-file-name "beta" tmpdir))
            (make-directory (expand-file-name ".hidden" tmpdir))
            (agent-shell-to-go-test-inbound-message
             tr channel nil "testuser" "!projects")
            (let ((texts (agent-shell-to-go-test-bridge--sent-texts tr)))
              (should (cl-some (lambda (t) (string-match-p "alpha" t)) texts))
              (should (cl-some (lambda (t) (string-match-p "beta" t)) texts))
              (should (cl-notany (lambda (t) (string-match-p "hidden" t)) texts))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-shell-to-go-test-bridge-command-project-empty-dir ()
  "/project with an empty directory replies with a no-projects message."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let* ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf))
           (tmpdir (make-temp-file "ag2g-test-projects-empty" t)))
      (unwind-protect
          (let ((agent-shell-to-go-projects-directory tmpdir))
            (agent-shell-to-go-test-inbound-message
             tr channel nil "testuser" "!projects")
            (should
             (cl-some
              (lambda (text) (string-match-p "No projects found" text))
              (agent-shell-to-go-test-bridge--sent-texts tr))))
        (delete-directory tmpdir t)))))

(ert-deftest agent-shell-to-go-test-bridge-command-projects-nonexistent-dir ()
  "/projects with a non-existent directory replies with no-projects message."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (let* ((channel (buffer-local-value 'agent-shell-to-go--channel-id buf))
           (agent-shell-to-go-projects-directory "/nonexistent-ag2g-test-dir-xyz"))
      (agent-shell-to-go-test-inbound-message tr channel nil "testuser" "!projects")
      (should
       (cl-some
        (lambda (text) (string-match-p "No projects found" text))
        (agent-shell-to-go-test-bridge--sent-texts tr))))))


(ert-deftest agent-shell-to-go-test-bridge-on-init-client-nil-client ()
  "When init-client fires with :client nil in agent-shell state, failure notice is sent.
Exercises the failure branch of `agent-shell-to-go--on-init-client'."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (with-current-buffer buf
      (let ((saved-client (map-elt agent-shell--state :client)))
        (map-put! agent-shell--state :client nil)
        (unwind-protect
            (agent-shell-to-go--on-init-client nil)
          (map-put! agent-shell--state :client saved-client))))
    (should
     (cl-some
      (lambda (text) (string-match-p "Agent failed to start" text))
      (agent-shell-to-go-test-bridge--sent-texts tr)))))

(ert-deftest agent-shell-to-go-test-bridge-on-error-server-init-error ()
  "When the ACP server raises an error on initialize, --on-error forwards it to the transport.
Also verifies --on-init-client does not fire a false \"failed to start\" notice,
since the client struct was created successfully before the RPC failed."
  (agent-shell-to-go-test-bridge--with-error-session
      tr buf
      agent-shell-to-go-test-bridge--mock-acp-init-error-server
      nil
    (should
     (agent-shell-to-go-test-bridge--wait-until
      (lambda ()
        (cl-some
         (lambda (text) (string-match-p "Agent error" text))
         (agent-shell-to-go-test-bridge--sent-texts tr)))))
    (should-not
     (cl-some
      (lambda (text) (string-match-p "Agent failed to start" text))
      (agent-shell-to-go-test-bridge--sent-texts tr)))))

(ert-deftest agent-shell-to-go-test-bridge-on-error-auth-failure ()
  "When the ACP server raises an auth-required error, --on-error forwards it to the transport."
  (agent-shell-to-go-test-bridge--with-error-session
      tr buf
      agent-shell-to-go-test-bridge--mock-acp-auth-error-server
      (lambda (config)
        (map-put! config :needs-authentication t)
        (map-put!
         config
         :authenticate-request-maker
         (lambda () (acp-make-authenticate-request :method-id "token" :method "token")))
        config)
    (should
     (agent-shell-to-go-test-bridge--wait-until
      (lambda ()
        (cl-some
         (lambda (text) (string-match-p "Agent error" text))
         (agent-shell-to-go-test-bridge--sent-texts tr)))))))

(ert-deftest agent-shell-to-go-test-bridge-on-error-prompt-failure ()
  "When the agent raises an error in response to a prompt, --on-error forwards it to the transport."
  (agent-shell-to-go-test-bridge--with-session tr buf
    (agent-shell-to-go-test-bridge--send-prompt buf "test error")
    (should
     (agent-shell-to-go-test-bridge--wait-until
      (lambda ()
        (cl-some
         (lambda (text) (string-match-p "Agent error" text))
         (agent-shell-to-go-test-bridge--sent-texts tr)))))))

(provide 'agent-shell-to-go-bridge-test)
;;; agent-shell-to-go-bridge-test.el ends here
