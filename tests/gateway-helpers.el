;;; gateway-helpers.el --- Shared Gateway test utilities -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Code:

(require 'websocket)

(defun agent-shell-to-go-test--make-fake-frame (json-string)
  "Return a websocket-frame with JSON-STRING as its payload.
`websocket-frame-text' will decode this back to JSON-STRING."
  (make-websocket-frame :opcode 'text
                        :payload (encode-coding-string json-string 'utf-8)
                        :completep t))

(defmacro agent-shell-to-go-test--with-captured-ws-sends (&rest body)
  "Execute BODY with websocket send functions stubbed.
`websocket-openp' returns t for any argument.
`websocket-send-text' captures each text argument.
Returns the list of captured sends in chronological order."
  (let ((var (gensym "ws-sends")))
    `(let ((,var nil))
       (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                 ((symbol-function 'websocket-send-text)
                  (lambda (_sock text) (push text ,var))))
         ,@body)
       (nreverse ,var))))

(provide 'gateway-helpers)
;;; gateway-helpers.el ends here
