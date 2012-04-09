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
    
    ;; Put items in their various locations
    (adv::move-object sword         nil initial-location)
    (adv::move-object initial-item  nil initial-location)
    (adv::move-object first-monster nil initial-location)
    
    ;; Give the monster a hammer and a feather
    (adv::move-object feather nil first-monster)
    (adv::move-object hammer  nil first-monster)
    
    ;; Add navigation to locations
    (adv::set-navigation initial-location goal-location    adv::*north*)
    (adv::set-navigation goal-location    initial-location adv::*south*)

    ;; Add all the items, players, monsters etc. to
    ;; the gameworld's inventory
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

    ;; Finally return the gameworld
    current-world))
  
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

