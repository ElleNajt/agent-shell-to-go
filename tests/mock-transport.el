;;; mock-transport.el --- Mock transport for testing -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A second transport implementation that validates the protocol without
;; requiring a real Discord (or Slack) backend.
;;
;; Records all outbound method calls and supports replaying scripted
;; inbound events to exercise the hook-based dispatch path.
;;
;; Usage:
;;   (require 'agent-shell-to-go-test-transport)
;;   (agent-shell-to-go-register-transport 'test (agent-shell-to-go-test-make))
;;   ;; Then trigger hooks manually:
;;   (agent-shell-to-go-test-inbound-message my-transport "C1" "T1" "U1" "hello!")

;;; Code:

(require 'agent-shell-to-go-core)

; Struct 

(cl-defstruct (agent-shell-to-go-test-transport
               (:include agent-shell-to-go-transport)
               (:constructor agent-shell-to-go-test-make-internal))
  "Mock transport for protocol validation."
  (connected-p nil)
  (calls nil)                      ; reversed list of (method . args)
  (messages (make-hash-table :test 'equal)) ; message-id → text
  (next-id-counter 0)
  (authorized-users '("testuser")))

(defun agent-shell-to-go-test-make (&rest slots)
  "Create a test transport.  SLOTS override defaults."
  (apply #'agent-shell-to-go-test-make-internal :name 'test slots))

(defun agent-shell-to-go-test--record (transport method &rest args)
  "Record a METHOD call with ARGS on TRANSPORT."
  (push (cons method args)
        (agent-shell-to-go-test-transport-calls transport)))

(defun agent-shell-to-go-test--next-id (transport)
  "Return a fresh message id string for TRANSPORT."
  (let ((n (agent-shell-to-go-test-transport-next-id-counter transport)))
    (setf (agent-shell-to-go-test-transport-next-id-counter transport) (1+ n))
    (format "test-msg-%d" n)))

; Method implementations 

(cl-defmethod agent-shell-to-go-transport-connect
  ((transport agent-shell-to-go-test-transport))
  (agent-shell-to-go-test--record transport 'connect)
  (setf (agent-shell-to-go-test-transport-connected-p transport) t))

(cl-defmethod agent-shell-to-go-transport-disconnect
  ((transport agent-shell-to-go-test-transport))
  (agent-shell-to-go-test--record transport 'disconnect)
  (setf (agent-shell-to-go-test-transport-connected-p transport) nil))

(cl-defmethod agent-shell-to-go-transport-connected-p
  ((transport agent-shell-to-go-test-transport))
  (agent-shell-to-go-test-transport-connected-p transport))

(cl-defmethod agent-shell-to-go-transport-authorized-p
  ((transport agent-shell-to-go-test-transport) user-id)
  (member user-id (agent-shell-to-go-test-transport-authorized-users transport)))

(cl-defmethod agent-shell-to-go-transport-bot-user-id
  ((transport agent-shell-to-go-test-transport))
  "test-bot")

(cl-defmethod agent-shell-to-go-transport-send-text
  ((transport agent-shell-to-go-test-transport) channel thread-id text &optional options)
  (let ((id (agent-shell-to-go-test--next-id transport)))
    (agent-shell-to-go-test--record transport 'send-text channel thread-id text options)
    (puthash id text (agent-shell-to-go-test-transport-messages transport))
    id))

(cl-defmethod agent-shell-to-go-transport-edit-message
  ((transport agent-shell-to-go-test-transport) channel message-id text)
  (agent-shell-to-go-test--record transport 'edit-message channel message-id text)
  (when (gethash message-id (agent-shell-to-go-test-transport-messages transport))
    (puthash message-id text (agent-shell-to-go-test-transport-messages transport))
    t))

(cl-defmethod agent-shell-to-go-transport-upload-file
  ((transport agent-shell-to-go-test-transport) channel thread-id path &optional comment)
  (agent-shell-to-go-test--record transport 'upload-file channel thread-id path comment)
  (agent-shell-to-go-test--next-id transport))

(cl-defmethod agent-shell-to-go-transport-get-message-text
  ((transport agent-shell-to-go-test-transport) _channel message-id)
  (gethash message-id (agent-shell-to-go-test-transport-messages transport)))

(cl-defmethod agent-shell-to-go-transport-get-reactions
  ((transport agent-shell-to-go-test-transport) _channel _message-id)
  nil)

(cl-defmethod agent-shell-to-go-transport-fetch-thread-replies
  ((transport agent-shell-to-go-test-transport) _channel _thread-id)
  nil)

(cl-defmethod agent-shell-to-go-transport-start-thread
  ((transport agent-shell-to-go-test-transport) _channel label)
  (let ((id (agent-shell-to-go-test--next-id transport)))
    (agent-shell-to-go-test--record transport 'start-thread label)
    id))

(cl-defmethod agent-shell-to-go-transport-update-thread-header
  ((transport agent-shell-to-go-test-transport) _channel _thread-id title)
  (agent-shell-to-go-test--record transport 'update-thread-header title))

(cl-defmethod agent-shell-to-go-transport-ensure-project-channel
  ((transport agent-shell-to-go-test-transport) _project-path)
  "test-channel")

(cl-defmethod agent-shell-to-go-transport-list-threads
  ((transport agent-shell-to-go-test-transport) _channel)
  nil)

(cl-defmethod agent-shell-to-go-transport-delete-message
  ((transport agent-shell-to-go-test-transport) _channel message-id)
  (agent-shell-to-go-test--record transport 'delete-message message-id)
  (remhash message-id (agent-shell-to-go-test-transport-messages transport)))

(cl-defmethod agent-shell-to-go-transport-delete-thread
  ((transport agent-shell-to-go-test-transport) _channel _thread-id)
  (agent-shell-to-go-test--record transport 'delete-thread))

(cl-defmethod agent-shell-to-go-transport-format-tool-call-start
  ((transport agent-shell-to-go-test-transport) title)
  (format "[running] %s" title))

(cl-defmethod agent-shell-to-go-transport-format-tool-call-result
  ((transport agent-shell-to-go-test-transport) title status output)
  (format "[%s] %s: %s" status title (or output "")))

(cl-defmethod agent-shell-to-go-transport-format-diff
  ((transport agent-shell-to-go-test-transport) _old _new)
  "[diff omitted in test transport]")

(cl-defmethod agent-shell-to-go-transport-format-user-message
  ((transport agent-shell-to-go-test-transport) text)
  (format "[user] %s" text))

(cl-defmethod agent-shell-to-go-transport-format-agent-message
  ((transport agent-shell-to-go-test-transport) text)
  (format "[agent] %s" text))

(cl-defmethod agent-shell-to-go-transport-format-markdown
  ((transport agent-shell-to-go-test-transport) markdown)
  markdown)

; Scripted inbound event helpers 

(defun agent-shell-to-go-test-inbound-message (transport channel thread-id user text)
  "Fire a message inbound event on TRANSPORT."
  (apply #'run-hook-with-args
         'agent-shell-to-go-message-hook
         (list :transport transport
               :channel-id channel
               :thread-id thread-id
               :user user
               :text text
               :msg-id (agent-shell-to-go-test--next-id transport))))

(defun agent-shell-to-go-test-inbound-reaction
    (transport channel msg-id user action added-p &optional thread-id)
  "Fire a reaction inbound event on TRANSPORT."
  (apply #'run-hook-with-args
         'agent-shell-to-go-reaction-hook
         (list :transport transport
               :channel-id channel
               :thread-id thread-id
               :msg-id msg-id
               :user user
               :action action
               :raw-emoji (symbol-name action)
               :added-p added-p)))

(defun agent-shell-to-go-test-outbound-calls (transport &optional method)
  "Return recorded calls on TRANSPORT, optionally filtered by METHOD."
  (let ((calls (nreverse (copy-sequence
                          (agent-shell-to-go-test-transport-calls transport)))))
    (if method
        (cl-remove-if-not (lambda (c) (eq (car c) method)) calls)
      calls)))

(provide 'mock-transport)
;;; mock-transport.el ends here
