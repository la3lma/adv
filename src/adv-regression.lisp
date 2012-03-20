;; -*-LISP-*-

(asdf:operate 'asdf:load-op :lisp-unit)

(defpackage :adv-regression
  (:use :common-lisp :lisp-unit :adv))

(in-package :adv-regression)
;;


;; Documentation can be found her
;; file:///Users/rmz2/quicklisp/dists/quicklisp/software/lisp-unit-20120107-git/documentation/lisp-unit.html#overview

(define-test test-quit-cmd
  (let* ((inputstream (make-string-input-stream (format nil "quit~%")))
         (outputstream (make-string-output-stream)))
    
    (catch 'adv::escape-from-game
      (adv::inner-game-repl
       :input inputstream
       :output outputstream))
    
    ;; XXX Should get the output from the stream and print it,
    ;;     perhaps even look for some particular output
    (let ((the-output (GET-OUTPUT-STREAM-STRING outputstream)))
      (format *standard-output* "~% The output from the test-run was ~S" the-output)
      (assert-true       (SYSTEM::SEARCH-STRING-EQUAL "Lgtm" the-output))
      )))


