;; -*-LISP-*-

;; This is a hack, the ordinary require etc. should be used instead, but how?
(asdf:operate 'asdf:load-op :lisp-unit)

(defpackage :adv-regression-scenario2
  (:use :common-lisp :lisp-unit :adv-regression :adv)
  (:export :initialize-fixture))

(in-package :adv-regression-scenario2)

;;
;;  THE FIXTURE
;;

(defvar *traced-value* nil)

(defun traceval (title value)
  (setq *traced-value* value)
  (format *standard-output* "~%The ~a has value ~s" title value)
  value)


(defun initialize-fixture (&key (input *standard-input*) (output *standard-output*))
  "Set up a gameworld, and return that gameworld as the result"
  
  (format *standard-output* "~% Initializing fixture")
  
  (defworld "The game we play"
    (let* ((initial-location 
	    (stash (new-location  "The start")
		   (new-item   "An item")
		   (stash (new-container   "A stone table that seems to grow out of the ground" :is-fixture-p t :is-transparent-p t)
			  (new-item "a torch"))
		   (new-weapon "A knife")
		   (new-readable "Ancient looking dusty scroll"
				 :content "Your mission is to find the coin and drop it in the well")
		   (new-player   "The player"
				 :in-stream  input
				 :out-stream output)))

	   (goal-location    (stash (new-location "The wellhouse")
				    (stash 
				     (new-item   "A well" :is-fixture-p t)    
				     ))))
      
      ;; We start out with a simple linear topology.   When we get
      ;; that working we'll progress go more interesting topologies.

      (navigation-path initial-location 
		       adv::*north* (new-location "A narrow path")
		       adv::*north* (new-location "A troll operated bridge")
		       adv::*north* (new-location "The foot of a mountain with many paths going in many directions")
		       adv::*north* (new-location "The top of the mountain")
		       adv::*north* (new-location "A small hut with gingerbread roof and walls and candy glued to its walls and roof")
		       adv::*north* (new-location "A narrow canyon with dangerous-looking boulders on all sides")
		       adv::*north* (new-location "A lush green valley with slightly weird-looking houses spread around")
		       adv::*north* (stash (new-location "A desert")
					   (new-item "A gold coin with an angel on one side and a devil on the other"))
		       adv::*north* (new-location "An oasis")
		       adv::*north* (new-location "A desert")
		       adv::*north* (new-location "A palace")
		       adv::*north* (new-location "A cave")
		       adv::*north* (new-location "A small boathouse")
		       adv::*north* (new-location "An uninteresting stretch of water")
		       adv::*north* (new-location "A small island")
		       adv::*north* goal-location))))

;;
;;  THE TESTS
;;

;; Tests are added when bugs are detected.  The procedure is to
;; play the game until a bug is detected, then to add a test
;; that fails due to the newly detected bug, then to fix the bug
;; and then to commit code that fixes the bug along with the test
;; that fails unless the bug is fixed.  Rinse&repeat.

(defun rco (input expected)
  (adv-regression:run-command-oneliner #'initialize-fixture input expected))

(define-test test-go-north-until-bridge
  (rco (format nil"~%go north~%go north~%look") "A troll operated bridge"))

