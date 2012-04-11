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
  
  (defworld "The game we play"
    (let* ((initial-location 
	    (stash (new-location  "The start")
		   (new-item   "An item")
		   (new-readable "Ancient looking dusty scroll"
				 :content "Fooxxxy")
		   (new-weapon "The sword of generic strikes")
		   (new-player   "The player"
				 :in-stream input
				 :out-stream output)
		   (stash (new-monster "Green little cutie monster"
				       :health       30
				       :in-stream input
				       :out-stream output
				       )
			  (new-weapon "The hammer of serious blows")
			  (new-weapon "The feather of fiendish ticles" :strength 0.1)
			  )))
	   (goal-location    (stash (new-location "The goal")
				    (stash 
				     (new-monster "Norbert the Norwegian ridgeback")
				     (new-weapon "A tail with fierce looking horns"
						 :strength    50
						 :reliability 0.2)
				     ))))
      
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

(define-test test-take-scroll-then-read-it
  (rco "take scroll
read scroll" "Fooxxxy"))

