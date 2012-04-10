;; -*-LISP-*-

;; This is a hack, the ordinary require etc. should be used instead, but how?
(asdf:operate 'asdf:load-op :lisp-unit)

(defpackage :adv-regression-scenario1
  (:use :common-lisp :lisp-unit :adv-regression :adv))

(in-package :adv-regression-scenario1)

;;
;;  THE FIXTURE
;;

(defun initialize-fixture (&key (input *standard-input*) (output *standard-output*))
  "Set up a gameworld, and return that gameworld as the result"
  
  (format *standard-output* "~% Initializing fixture" )
  
  (adv:defworld "The game we play"
    (let* ((initial-location 
	    (adv:stash (new-location  "The start")
		       (adv:new-item   "An item")
		       (adv:new-weapon "The sword of generic strikes")
		       (adv:new-player   "The player"
					 :in-stream input
					 :out-stream output)
		       (adv:stash (adv:new-monster "Green little cutie monster"
						   :health       30
						   :in-stream input
						   :out-stream output
						   )
				  (adv:new-weapon "The hammer of serious blows")
				  (adv:new-weapon "The feather of fiendish ticles" :strength 0.1)
				  )))
	   (goal-location    (adv:new-location "The goal")) )
      
      (navigation-path initial-location adv::*north* goal-location))))
  
;;
;;  THE TESTS
;;

(defun rco (input expected)
  (adv-regression:run-command-oneliner #'initialize-fixture input expected))

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

