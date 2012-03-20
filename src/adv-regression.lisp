;; -*-LISP-*-

(asdf:operate 'asdf:load-op :lisp-unit)

(defpackage :adv-regression
  (:use :common-lisp :lisp-unit :adv))

(in-package :adv-regression)

;; Documentation can be found her
;; file:///Users/rmz2/quicklisp/dists/quicklisp/software/lisp-unit-20120107-git/documentation/lisp-unit.html#overview

;;
;; The actual game objects. For testing, not playing (obviously)
;; This should be put into the regression test stuff, not in the
;; game itself.
;;

(defvar *initial-location* nil)
(defvar *goal-location*    nil)
(defvar *initial-item*     nil)
(defvar *current-player*   nil)
(defvar *first-monster*    nil)
(defvar *sword*            nil)

;;
;; The actual game objects. For testing, not playing (obviously)
;; This should be put into the regression test stuff, not in the
;; game itself.
;;

(defun initialize-fixture ()

  (setf *initial-location* (make-instance 'adv::Location :description "The start"))
  (setf *goal-location*    (make-instance 'adv::Location :description "The goal"))
  (setf *initial-item*     (make-instance 'adv::Item     :description "An item"))
  (setf *current-player*   (make-instance 'adv::Player
                                                  :description "The player"
                                                  :location *initial-location*))
  (setf *first-monster*    (make-instance 'adv::Monster
                                                  :health       30
                                                  :description "Green little qutie monster"))
  
  (setf *sword* (make-instance 'adv::Weapon :description "The sword of generic strikes"))
  
  (adv::move-object *sword*        nil  *initial-location*)
  (adv::move-object *initial-item* nil  *initial-location*)
  (adv::move-object *first-monster* nil *initial-location*)
  
  (adv::set-navigation *initial-location* *goal-location*    adv::*north*)
  (adv::set-navigation *goal-location*    *initial-location* adv::*south*))


(defun run-testgame ()
  "Run a test game interactively"
  (initialize-fixture)
  (game-repl *current-player*))


(defun run-command-oneliner (inputstring expected-output)
  "Run a sequence of game commands encoded in the inputstream and return the output from the game as a string"
  (initialize-fixture)
  (let* ((inputstream (make-string-input-stream (format nil "quit~%")))
         (outputstream (make-string-output-stream)))

    (catch 'adv::escape-from-game
      (adv::inner-game-repl
       *current-player*
       :input inputstream
       :output outputstream))

    (let ((the-output (get-output-stream-string outputstream)))
      (assert-true (system::search-string-equal expected-output the-output))
      the-output)))

(define-test test-quit-emptyness
  (run-command-oneliner "" ""))

(define-test test-quit-cmd
  (run-command-oneliner "quit" "Ttfn"))


