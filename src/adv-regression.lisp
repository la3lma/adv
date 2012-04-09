;; -*-LISP-*-

;; This is a hack, the ordinary require etc. should be used instead, but how?
(asdf:operate 'asdf:load-op :lisp-unit)

(defpackage :adv-regression
  (:use :common-lisp :lisp-unit :adv))

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

;;
;;  THE FIXTURE
;;

(defun initialize-fixture (&key (input *standard-input*) (output *standard-output*))
  "Set up a gameworld, and return that gameworld as the result"
  (let* ((current-world   (make-instance 'adv::GameWorld :description "The game we play"))
	(initial-location (make-instance 'adv::Location :description "The start"))
	(goal-location    (make-instance 'adv::Location :description "The goal"))
	(initial-item     (make-instance 'adv::Item     :description "An item"))
	(current-player   (make-instance 'adv::Player
					 :description "The player"
					 :in-stream input
					 :out-stream output
					 :location initial-location))
	(first-monster    (make-instance 'adv::Monster
					 :health       30
					 :in-stream input
					 :out-stream output
					 :description "Green little qutie monster"))
	
	(sword   (make-instance 'adv::Weapon :description "The sword of generic strikes"))
	(hammer  (make-instance 'adv::Weapon :description "The hammer of serious blows"))
	(feather (make-instance 'adv::Weapon :description "The feather of fiendish ticles" :strength 0.1)))
    
    (adv::move-object sword         nil initial-location)
    (adv::move-object initial-item  nil initial-location)
    (adv::move-object first-monster nil initial-location)
    
    ;; Give the monster a hammer and a feather
    (adv::move-object feather nil first-monster)
    (adv::move-object hammer  nil first-monster)
    
    (adv::set-navigation initial-location goal-location    adv::*north*)
    (adv::set-navigation goal-location    initial-location adv::*south*)

    (adv::add-all-to-inventory
     current-world
     (list
      current-player
      first-monster
      sword
      hammer
      feather
      initial-location
      goal-location
      ))   
    current-world))
  
;;
;;  THE TESTS
;;

(defun rco (input expected)
  (run-command-oneliner #'initialize-fixture input expected))

(define-test test-quit-emptyness
  (rco "" ""))

(define-test test-quit-cmd
  (rco  "quit" "Ttfn"))

(define-test test-look-for-sword
  (rco  "look" "sword of generic strikes"))

(define-test test-look-for-the-start
  (rco "look" "The start"))

(define-test test-take-sword
  (rco "take sword" "Got it"))

(define-test test-take-sword-then-strike-monster
  (rco "take sword
kill monster" "You are dead"))

