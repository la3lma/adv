;; -*-LISP-*-

;; This is a hack, the ordinary require etc. should be used instead, but how?
(asdf:operate 'asdf:load-op :lisp-unit)

(defpackage :adv-regression-scenario1
  (:use :common-lisp :lisp-unit :adv-regression :adv))

(in-package :adv-regression-scenario1)

;;
;;  THE FIXTURE
;;



;; XXX This is a work in progress.   When done, it will make the 
;;      world
(defmacro defworld (gameworld-description &body defworld-body)
  (let ((world-var (gensym))
	(tmp (gensym)))
    `(let ((,world-var (make-instance 'adv::GameWorld :description "The game we play")))
       (labels ((current-world () 
			       ,world-var)
		(internalize-item (item)
				  (adv:add-to-inventory item ,world-var)
				  item)
		(create-internalized-item (class description params)
		  (internalize-item (apply #'make-instance (cons class (cons :description (cons description  params)))))))
	 ;; XXX  This flet could be made much simpler!
	 (flet ((new-location (description &rest location-body)
			(create-internalized-item 'adv::Location description location-body))
		(new-item (description &rest description-body)
			(create-internalized-item 'adv::Item description description-body))
		(new-player (description &rest player-body)
			(create-internalized-item 'adv::Player description player-body))
		(new-monster (description &rest monster-body)
			(create-internalized-item 'adv::Monster description monster-body))
		(new-weapon (description &rest weapon-body)
			(create-internalized-item 'adv::Weapon description weapon-body))
		(stash (recipient &rest items)
		       (dolist (item items)
			 (adv:move-object item nil  recipient))
		       recipient))
		  ,@defworld-body)))))

(defun initialize-fixture (&key (input *standard-input*) (output *standard-output*))
  "Set up a gameworld, and return that gameworld as the result"
  
  (format *standard-output* "~% Initializing fixture" )
  
  (defworld "The game we play"
    (let* ((initial-location (stash (new-location "The start")
				    (new-item     "An item")
				    (new-weapon "The sword of generic strikes")
				    ))
	   (goal-location    (new-location "The goal")) 
	   (current-player   (new-player   "The player"
					    :in-stream input
					    :out-stream output
					    :location initial-location))
	   (first-monster    (stash (new-monster "Green little cutie monster"
						 :health       30
						 :in-stream input
						 :out-stream output
						 )
				    (new-weapon "The hammer of serious blows")
				    (new-weapon "The feather of fiendish ticles" :strength 0.1)
				    )))
    
    ;; Put items in their various locations
     (adv::move-object first-monster nil initial-location)


    ;; Add navigation to locations
    (adv::set-navigation initial-location goal-location    adv::*north*)
    (adv::set-navigation goal-location    initial-location adv::*south*)

    ;; Finally return the gameworld
    (current-world))))

  
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

