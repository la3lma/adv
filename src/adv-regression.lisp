;; -*-LISP-*-

(asdf:operate 'asdf:load-op :lisp-unit)

(defpackage :adv-regression
  (:use :common-lisp :lisp-unit :adv))

(in-package :adv-regression)

(require 'adv) ;; ???

;; Documentation can be found her
;; file:///Users/rmz2/quicklisp/dists/quicklisp/software/lisp-unit-20120107-git/documentation/lisp-unit.html#overview

;;
;; The actual game objects. For testing, not playing (ogbviously)
;; This should be put into the regression test stuff, not in the
;; game itself.
;;

(defvar *current-world*    nil)
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

(defun initialize-fixture (&key (input *standard-input*) (output *standard-output*))
  
  (setf *current-world*  (make-instance 'adv::GameWorld :description "The game we play"))
  
  (setf *initial-location* (make-instance 'adv::Location :description "The start"))
  (setf *goal-location*    (make-instance 'adv::Location :description "The goal"))
  (setf *initial-item*     (make-instance 'adv::Item     :description "An item"))
  (setf *current-player*   (make-instance 'adv::Player
                                          :description "The player"
                                          :in-stream input
                                          :out-stream output
                                                  :location *initial-location*))
  (setf *first-monster*    (make-instance 'adv::Monster
                                          :health       30
                                          :in-stream input
                                          :out-stream output
                                          :description "Green little qutie monster"))
  
  (setf *sword*   (make-instance 'adv::Weapon :description "The sword of generic strikes"))
  (setf *hammer*  (make-instance 'adv::Weapon :description "The hammer of serious blows"))
  (setf *feather* (make-instance 'adv::Weapon :description "The feather of fiendish ticles" :strength 0.1))
  
  (adv::move-object *sword*         nil *initial-location*)
  (adv::move-object *initial-item*  nil *initial-location*)
  (adv::move-object *first-monster* nil *initial-location*)

  ;; Give the monster a hammer and a feather
  (adv::move-object *feather* nil *first-monster*)
  (adv::move-object *hammer*  nil *first-monster*)
  
  (adv::set-navigation *initial-location* *goal-location*    adv::*north*)
  (adv::set-navigation *goal-location*    *initial-location* adv::*south*)

  (adv::add-all-to-inventory
   *current-world*
   (list
    *current-player*
    *first-monster*
    *sword*
    *hammer*
    *feather*
    *initial-location*
    *goal-location*
    )))


(defun run-testgame ()
  "Run a test game interactively"
  (initialize-fixture)
  (game-repl *current-player*))


(defun run-command-oneliner (inputstring expected-output &key (tracep nil))
  "Run a sequence of game commands encoded in the inputstream and return the output from the game as a string"
  (let* ((inputstream (make-string-input-stream (format nil "~a ~%quit~%" inputstring)))
         (outputstream (make-string-output-stream)))
    
    (initialize-fixture :input inputstream :output outputstream)
    
    (catch 'adv::escape-from-game
      (adv::inner-game-repl
       *current-player*
       :input inputstream
       :output outputstream))
    
    (let* ((the-output (get-output-stream-string outputstream)))
      (when tracep
        (format *standard-output* "~% The input we gave is   ~s" inputstring)
        (format *standard-output* "~%    The output are expecting is  ~s" expected-output)
        (format *standard-output* "~%    The output we got was ~s" the-output))
      
      (assert-true (search expected-output the-output))
      the-output)))

(define-test test-quit-emptyness
  (run-command-oneliner "" ""))

(define-test test-quit-cmd
  (run-command-oneliner "quit" "Ttfn"))

(define-test test-look-for-sword
  (run-command-oneliner "look" "Sword of generic strikes"))

(define-test test-look-for-the-start
  (run-command-oneliner "look" "The start"))

(define-test test-take-sword
  (run-command-oneliner "take sword" "Got it"))

(define-test test-take-sword-then-strike-monster
  (run-command-oneliner "take sword
kill monster" "You are dead"))

