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

(defun initialize-fixture (&key (input *standard-input*) (output *standard-output*))
  "Set up a gameworld, and return that gameworld as the result"
  
  (format *standard-output* "~% Initializing fixture" )
  
  (defworld "The game we play"
    (let* ((initial-location 
	    (stash (new-location  "The start")
		   (new-item   "An item")
		   (new-readable "Ancient looking dusty scroll"
				 :content "Fooxxxy")
		   (new-player   "The player"
				 :in-stream input
				 :out-stream output)))

	   (goal-location    (stash (new-location "The wellhouse")
				    (stash 
				     (new-item   "A well")    
				     ))))
      
      (navigation-path initial-location 
		       adv::*north* (new-location "A narrow path")
		       adv::*north* (new-location "A troll operated bridge")
		       adv::*north* (new-location "The foot of a mountain with many paths going in many directions")
		       adv::*north* (new-location "The top of the mountain")
		       adv::*north* (new-location "A small hut with gingerbread roof and walls and candy glued to its walls and roof")
		       adv::*north* (new-location "A narrow canyon with dangerous-looking boulders on all sides")
		       adv::*north* (new-location "A lush green valley with slightly weird-looking houses spread around")
		       adv::*north* (new-location "A desert")
		       adv::*north* (new-location "An oasis")
		       adv::*north* (new-location "A desert")
		       adv::*north* (new-location "A palace")
		       adv::*north* (new-location "A cave")
		       adv::*north* (new-location "A small boathouse")
		       adv::*north* goal-location))))
  
;;
;;  THE TESTS
;;

;; So far none