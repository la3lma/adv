;; -*-LISP-*-

;; This is a hack, the ordinary require etc. should be used instead, but how?
(asdf:operate 'asdf:load-op :lisp-unit)

(defpackage :adv-regression
  (:use :common-lisp :lisp-unit :adv)
  (:export :run-command-oneliner :run-testgame))

(in-package :adv-regression)

(require 'adv) ;; ???

;; Documentation can be found here
;; file:///Users/rmz2/quicklisp/dists/quicklisp/software/lisp-unit-20120107-git/documentation/lisp-unit.html#overview


(defun run-testgame (initializer)
  "Run a test game interactively"
  (let* ((my-world     (funcall initializer))
	 (player       (adv:find-player "player" my-world)))
    (game-repl player)))

(defun run-command-oneliner (initializer inputstring expected-output &key (tracep nil))
  "Run a sequence of game commands encoded in the inputstream and return the output from the game as a string"
  (let* ((inputstream (make-string-input-stream (format nil "~a ~%quit~%" inputstring)))
         (outputstream (make-string-output-stream))
	 (my-world     (funcall initializer :input inputstream :output outputstream))
	 (player       (adv:find-player "player" my-world)))

    (catch 'adv::escape-from-game
      (adv::inner-game-repl
       player
       :input inputstream
       :output outputstream))
    
    (let* ((the-output (get-output-stream-string outputstream)))
      (when tracep
	(format *standard-output* "~% The input we gave is   ~s" inputstring)
	(format *standard-output* "~%    The output are expecting is  ~s" expected-output)
	(format *standard-output* "~%    The output we got was ~s" the-output))
      
      (assert-true (search expected-output the-output))
      the-output)))


