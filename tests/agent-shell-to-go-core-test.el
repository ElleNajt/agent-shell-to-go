;;; agent-shell-to-go-core-test.el --- Tests for agent-shell-to-go.el -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for agent-shell-to-go-core.el.  Each test exercises one
;; core behaviour in isolation using a mock transport — no Slack/Discord
;; credentials required.
;;
;; Run:
;;   make test TEST=agent-shell-to-go-test-core.el
;;
;; APIs under test:
;;
;;   agent-shell-to-go--handle-presentation-reaction
;;     - presentation-hide-expand: hide/edit reactions on cached tool call messages
;;     - presentation-cache-miss: reactions on non-cached messages append a note
;;
;;   agent-shell-to-go--tool-call-cache
;;     - cache-put-get: put entry then retrieve it
;;     - cache-persistence: save session to disk and load it back
;;
;;   agent-shell-to-go-register-transport / agent-shell-to-go-get-transport
;;     - transport-registry: transports registered and retrieved by name
;;
;;   agent-shell-to-go--all-transport-objects
;;     - all-transport-objects: default and alist transports collected, deduplicated
;;
;;   agent-shell-to-go--get-transport
;;     - default-transport-prefix-match: longest alist prefix wins; falls back to default

;;; Code:

(require 'ert)

(require 'mock-transport)

;;; Presentation reaction tests

(ert-deftest agent-shell-to-go-test-core-presentation-hide-add ()
  "hide reaction on a cached tool call message edits to '_message hidden_'."
  (let* ((tr (agent-shell-to-go-test-make))
         (id (agent-shell-to-go-transport-send-text tr "C1" "T1" "✅ edit completed"))
         (agent-shell-to-go--tool-call-cache (make-hash-table :test #'equal)))
    (agent-shell-to-go--cache-put-entry tr "C1" "T1" id "✅ edit completed" "the diff output")
    (agent-shell-to-go-test-inbound-reaction tr "C1" id "testuser" 'hide t "T1")
    (should
     (string-match-p
      "hidden" (or (agent-shell-to-go-transport-get-message-text tr "C1" id) "")))))

(ert-deftest agent-shell-to-go-test-core-presentation-hide-remove ()
  "Removing hide restores the collapsed title form from cache."
  (let* ((tr (agent-shell-to-go-test-make))
         (id (agent-shell-to-go-transport-send-text tr "C1" "T1" "_message hidden_"))
         (agent-shell-to-go--tool-call-cache (make-hash-table :test #'equal)))
    (agent-shell-to-go--cache-put-entry tr "C1" "T1" id "✅ edit completed" "the diff output")
    (agent-shell-to-go-test-inbound-reaction tr "C1" id "testuser" 'hide nil "T1")
    (should
     (equal
      "✅ edit completed" (agent-shell-to-go-transport-get-message-text tr "C1" id)))))

(ert-deftest agent-shell-to-go-test-core-presentation-expand-add ()
  "expand reaction on a cached tool call message shows title + output."
  (let* ((tr (agent-shell-to-go-test-make))
         (id (agent-shell-to-go-transport-send-text tr "C1" "T1" "✅ edit completed"))
         (agent-shell-to-go--tool-call-cache (make-hash-table :test #'equal)))
    (agent-shell-to-go--cache-put-entry tr "C1" "T1" id "✅ edit completed" "the diff output")
    (agent-shell-to-go-test-inbound-reaction tr "C1" id "testuser" 'expand t "T1")
    (should
     (string-match-p
      "the diff output" (or (agent-shell-to-go-transport-get-message-text tr "C1" id) "")))))

(ert-deftest agent-shell-to-go-test-core-presentation-expand-remove ()
  "Removing expand restores the collapsed title form from cache."
  (let* ((tr (agent-shell-to-go-test-make))
         (id (agent-shell-to-go-transport-send-text tr "C1" "T1" "✅ edit completed\nthe diff output"))
         (agent-shell-to-go--tool-call-cache (make-hash-table :test #'equal)))
    (agent-shell-to-go--cache-put-entry tr "C1" "T1" id "✅ edit completed" "the diff output")
    (agent-shell-to-go-test-inbound-reaction tr "C1" id "testuser" 'expand nil "T1")
    (should
     (equal
      "✅ edit completed" (agent-shell-to-go-transport-get-message-text tr "C1" id)))))

(ert-deftest agent-shell-to-go-test-core-presentation-cache-miss ()
  "Reaction on a non-cached message appends an ignored note."
  (let* ((tr (agent-shell-to-go-test-make))
         (id (agent-shell-to-go-transport-send-text tr "C1" "T1" "some random message"))
         (agent-shell-to-go--tool-call-cache (make-hash-table :test #'equal)))
    (agent-shell-to-go-test-inbound-reaction tr "C1" id "testuser" 'hide t "T1")
    (should
     (string-match-p
      "no cache entry"
      (or (agent-shell-to-go-transport-get-message-text tr "C1" id) "")))))

;;; Cache infrastructure

(ert-deftest agent-shell-to-go-test-core-cache-put-get ()
  "Cache put-entry followed by get-entry returns the stored data."
  (let* ((tr (agent-shell-to-go-test-make))
         (agent-shell-to-go--tool-call-cache (make-hash-table :test #'equal)))
    (agent-shell-to-go--cache-put-entry tr "C1" "T1" "msg-1" "✅ title" "body")
    (let ((entry (agent-shell-to-go--cache-get-entry tr "C1" "T1" "msg-1")))
      (should entry)
      (should (equal "✅ title" (nth 0 entry)))
      (should (equal "body" (nth 1 entry)))
      (should (null (nth 2 entry))))))  ;; expanded-p defaults to nil

(ert-deftest agent-shell-to-go-test-core-cache-persistence ()
  "Session cache survives save/load cycle."
  (let* ((tr (agent-shell-to-go-test-make))
         (tmpdir (make-temp-file "astg-cache-test-" t))
         (agent-shell-to-go--tool-call-cache (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          ;; Stub storage-root so we control where the file goes
          (cl-letf (((symbol-function 'agent-shell-to-go-transport-storage-root)
                     (lambda (_) tmpdir)))
            (agent-shell-to-go--cache-put-entry tr "C1" "T1" "msg-1" "title" "output")
            (agent-shell-to-go--cache-save-session tr "C1" "T1")
            ;; Verify file exists
            (should (file-exists-p (expand-file-name "sessions/T1.el" tmpdir)))
            ;; Reset cache and load back
            (setq agent-shell-to-go--tool-call-cache (make-hash-table :test #'equal))
            (agent-shell-to-go--cache-load-session tr "C1" "T1")
            (let ((entry (agent-shell-to-go--cache-get-entry tr "C1" "T1" "msg-1")))
              (should entry)
              (should (equal "title" (nth 0 entry)))
              (should (equal "output" (nth 1 entry))))))
      (delete-directory tmpdir t))))

;;; Transport registry

(ert-deftest agent-shell-to-go-test-core-transport-registry ()
  "Transports can be registered and retrieved by name."
  (let ((tr (agent-shell-to-go-test-make))
        (agent-shell-to-go--transports nil))
    (should (null (agent-shell-to-go-get-transport 'mytest)))
    (agent-shell-to-go-register-transport 'mytest tr)
    (should (eq tr (agent-shell-to-go-get-transport 'mytest)))))

(ert-deftest agent-shell-to-go-test-core-all-transport-objects ()
  "All-transport list includes default and alist transports, deduplicated."
  (let* ((tr1 (agent-shell-to-go-test-make))
         (tr2 (agent-shell-to-go-test-make))
         (agent-shell-to-go--transports nil)
         (agent-shell-to-go-default-transport 'tr1)
         (agent-shell-to-go-project-transport-alist
          (list (cons "/work/acme/" 'tr2) (cons "/work/other/" 'tr1))))
    (agent-shell-to-go-register-transport 'tr1 tr1)
    (agent-shell-to-go-register-transport 'tr2 tr2)
    (let ((objs (agent-shell-to-go--all-transport-objects)))
      (should (= 2 (length objs)))
      (should (memq tr1 objs))
      (should (memq tr2 objs)))))

(ert-deftest agent-shell-to-go-test-core-default-transport-prefix-match ()
  "Longest prefix in alist wins; falls back to default when no match."
  (let* ((tr-default (agent-shell-to-go-test-make))
         (tr-work (agent-shell-to-go-test-make))
         (tr-acme (agent-shell-to-go-test-make))
         (agent-shell-to-go--transports nil)
         (agent-shell-to-go-default-transport 'default)
         (agent-shell-to-go-project-transport-alist
          (list (cons "/work/" 'work) (cons "/work/acme/" 'acme))))
    (agent-shell-to-go-register-transport 'default tr-default)
    (agent-shell-to-go-register-transport 'work tr-work)
    (agent-shell-to-go-register-transport 'acme tr-acme)
    (let ((default-directory "/home/user/"))
      (should (eq tr-default (agent-shell-to-go--get-transport))))
    (let ((default-directory "/work/other/"))
      (should (eq tr-work (agent-shell-to-go--get-transport))))
    (let ((default-directory "/work/acme/myproject/"))
      (should (eq tr-acme (agent-shell-to-go--get-transport))))))

(provide 'agent-shell-to-go-core-test)
;;; agent-shell-to-go-core-test.el ends here
