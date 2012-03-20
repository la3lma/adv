;; -*-LISP-*-

(asdf:operate 'asdf:load-op :lisp-unit)

(defpackage :adv-regression
  (:use :common-lisp :lisp-unit :adv))

(in-package :adv-regression)

;; Documentation can be found her
;; file:///Users/rmz2/quicklisp/dists/quicklisp/software/lisp-unit-20120107-git/documentation/lisp-unit.html#overview


(defun run-command-oneliner (inputstring expected-output)
  (let* ((inputstream (make-string-input-stream (format nil "quit~%")))
         (outputstream (make-string-output-stream)))

    (catch 'adv::escape-from-game
      (adv::inner-game-repl
       :input inputstream
       :output outputstream))
    
    ;; XXX Should get the output from the stream and print it,
    ;;     perhaps even look for some particular output
    (let ((the-output (get-output-stream-string outputstream)))
      (assert-true (system::search-string-equal expected-output the-output))
      the-output)))

(define-test test-quit-cmd
  (run-command-oneliner "quit" "Ttfn"))

(define-test test-quit-emptyness
  (run-command-oneliner "" ""))


