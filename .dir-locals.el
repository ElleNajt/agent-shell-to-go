;;; Directory Local Variables -*- no-byte-compile: t -*-

((emacs-lisp-mode
  (elisp-autofmt-load-packages-local . ("cl-lib" "cl-seq" "cl-macs")))
 (nil
  (gatsby>project-tests-command . ("make" "test"))
  (eval . (setq-local gatsby>get-individual-test-function (lambda ()
                                                            (let* ((default-directory
                                                                    (or (and (project-current) (project-root (project-current))) default-directory))
                                                                   (test-file
                                                                    (completing-read
                                                                     "Run test: " (directory-files "tests" nil ".+-test\\.el"))))
                                                              `("make" "test" ,(format "TEST=%s" (file-name-nondirectory test-file)))))))
  ))
