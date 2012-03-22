
;;
;;   This file represents a take on how I right now would like to
;;   describe and interact with gameworlds.  It is currently just
;;   a sketch, but I might decide to make it 
;;

(defclass GameWorld (Describable Inventory)
  (
   ;; The location where new players are teleported to when they start
   ;; to play the game
   (initial-location :accessor initial-location :initarg :initial-location  :initform '()))
  (:documentation "A set of objects and actors that defines a game,
 both players and locations are part of the inventory")))


(defun new-instance  (&key (target nil) &rest argv)
  (let ((instance   (apply #'make-instance argv)))
    ;;; This is instance we should care about, but it doesn't tell
    ;;; us where it should go

    ;; XXX Perhaps we should also look dynamically for
    ;;     a context to add the instance to?
    (if (not (null target))
        (add-to-inventory instance target))))

;;; This is a thin veneer over the existing builder used in
;;; adv.lisp

(defgame SampleGame ()
  (let ((initial-location (new-instance 'Location :description "The start"))
        (goal-location    (new-instance 'Location :description "The goal"))
        (initial-item     (new-instance 'Item     :description "An item"))
        (player           (new-instance 'Player
                                        :description "The player"
                                        :location initial-location)) ;; XXX  Bogus!
        (first-monster    (new-instance 'Monster
                                        :health       30
                                        :description "Green little qutie monster"))
        (sword (new-instance 'Weapon :description "The sword of generic strikes")))

    (add-all-to-inventory
     (list
      initial-location
      goal-location
      initial-item
      player
      first-monster
      sword
      ) (this-game))
    
    (move-object sword         nil initial-location)
    (move-object initial-item  nil initial-location)
    (move-object first-monster nil initial-location)
    
    (set-navigation initial-location goal-location    north)
    (set-navigation goal-location    initial-location south)))


;;; Applying some syntactic sugar we get:
(defgame SampleGame ()
  (instance-let 
   ((initial-location 'Location :description "The start")
    (goal-location  'Location :description "The goal")
    (initial-item      'Item     :description "An item")
    (player              'Player    :description "The player"
                         :location initial-location)
    (first-monster    'Monster
                      :health       30
                      :description "Green little qutie monster")
    (sword 'Weapon :description "The sword of generic strikes"))
   
   (move-object sword         nil initial-location)
   (move-object initial-item  nil initial-location)
   (move-object first-monster nil initial-location)
   
   (set-navigation initial-location goal-location    north)
   (set-navigation goal-location    initial-location south)))


;; Pouring on more sugar we get this:


(defgame SampleGame ()

  ;; The body of this definition must in fact be interpreted as a
  ;; progn.  First the deflocations is run, then the defitems, then
  ;; there may be more and nested deflocations and defitems run.  This
  ;; way the world can be build using all of the tools available in
  ;; Common Lisp, but still be defined in a very succinct manner.


  ;; In order to this to play, the deflocations macro has to
  ;; first define the items, then run through the navigation stuff
  ;; to update the navigation structures in the locations.
  (with-locations
    ((initial-location ((goal-location north))    'Location :description "The start")
     (goal-location    ((initial-lcoation south)) 'Location :description "The goal")))
  
  (set-initial-location initial-location)
  
  (with-items
    (initial-item   initial-location
                    'Item     :description "An item")
    (player         initial-location
                    'Player    :description "The player")
    (first-monster  initial-location   'Monster
                      :health       30
                      :description "Green little qutie monster")
    (sword initial-location 'Weapon :description "The sword of generic strikes")))
   
;; We should have serialization/deserialization, but at present it's
;; ok to picle/unpicke the entire world, so

(save-game game file)     ; ....should be ok.   Similarly
(restore-game file)  ; should get a game object

;; How we should model players is  bit tricky, but I think that at
;; present we should assume that players are strongly associated with
;; particular games, up to and including authentication information.
