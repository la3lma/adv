;; -*-LISP-*-

;;
;; Copyright 2012 Bjørn Remseth (rmz@rmz.no)
;;
;;  Licensed under the Apache License, Version 2.0 (the "License");
;;  you may not use this file except in compliance with the License.
;;  You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;  Unless required by applicable law or agreed to in writing, software
;;  distributed under the License is distributed on an "AS IS" BASIS,
;;  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;  See the License for the specific language governing permissions and
;;  limitations under the License.
;;

;; This is a small unpretentious implementation of a seventies
;; style text adventure.  It's written just for fun and to
;; get to play a bit with my old friend Common Lisp again.

(defpackage :adv
  (:use :common-lisp)
  (:export  :add-to-inventory :inner-game-repl :game-repl 
	    :find-player :inventory :move-object :reverse-direction 
	    :set-navigation  :defworld :navigation-path :stash :new-weapon :new-item :new-monster :new-location :new-player :new-readable))
  
(in-package :adv)


;;;
;;; THE GAME MODEL
;;;

(defclass Describable ()
  ((description :accessor description :initarg :description :initform ""))
  (:documentation "Something that is describable for a user"))

(defclass Inventory ()
  ((inventory :accessor inventory :initarg :inventory :initform '()))
  (:documentation "Something that can contain other objects"))

(defclass Inhabitant ()
  ((world :accessor world :initarg :world)))

(defclass GameWorld (Describable Inventory)
  (
   ;; The location where new players are teleported to when they start
   ;; to play the game
   (initial-location :accessor initial-location :initarg :initial-location  :initform '()))
  (:documentation "A set of objects and actors that defines a game,
 both players and locations are part of the inventory"))

(defclass Location (Describable Inventory)
  ((navigation  :accessor navigation   :initarg :navigation :initform '())))


(defclass  Located ()
  ((location    :accessor location    :initarg :location)))

(defclass Player (Describable Inventory Located Fighter Inhabitant)
  ((out-stream :accessor out-stream :initarg :out-stream :initform *standard-output*)
   (in-stream  :accessor in-stream  :initarg :in-stream  :initform *standard-output*)))
 
(defclass Item (Describable Located Inhabitant)
  ())

(defclass Navigation ()
  ((names  :accessor names   :initarg :names)
   (destination :accessor destination :initarg :destination)))

(defgeneric describe-for-user (stream describable)
  (:documentation "Describe something for a user")
  (:method ((stream t) (l Location))
           (format stream "~% Location: ~A" (description l))
           (format stream "~% With inventory:~{~%  ~a~}." (mapcar #'description (inventory l)))
           (let ((directions (mapcar #'car  (mapcar #'names  (navigation l)))))
             (format stream "~%      exit~p:~{ ~a~^, ~}." (length directions) directions)))

  (:method ((stream t)(i Inventory))
           (format stream "Inventory for ~s: ~{~%  ~a~}." (description i) (mapcar #'description (inventory i)))))

(defgeneric move (Player List)
  (:documentation "Move the player somewhere based on some description of a direction")
  (:method ((p Player) (l list))
           (let* ((direction  (first l))
                  (location   (location p))
                  (navigation (navigation location)))

             (dolist (nav  navigation)
               (when
                   (find direction (names nav) :test #'string-equal)
                 (move-object p location (destination nav))
                 (return-from move)))
             (if (not (null direction))
                 (format (out-stream p) "Don't know how to go ~{~s~^ ~}" l)))))


;;;
;;; Informational items
;;;

(defclass Readable (Item)
  ((content     :accessor content     :initarg :content))
  (:documentation "Something that can be read by players/monsters"))
  

;;;
;;;  A BATTLE SYSTEM
;;;

(defclass Fighter ()
  ((strength :accessor strength :initarg :strength :initform 1.0)
   (health   :accessor health   :initarg :health   :initform 1.0))
  (:documentation "Base class for fighter.  Used to define the fighting system"))

(defclass Weapon (Item)
  ((strength     :accessor strength     :initarg :strength    :initform 3.0)
   (reliability  :accessor reliability  :initarg :reliability :initform 1.0))
  (:documentation "Base class for weapon.  Used to define the fighting system"))

(defclass Monster (Player)
  ())

(defgeneric extract-damage-points (weapon)
  (:method ((w Weapon))
	   (if (<= (random 1) (reliability weapon))
	       (strength w)
	     0))

  (:method ((f Fighter))
           (* (strength f) (health f))
           )
  (:documentation "Get some hitpoints from the weapon, possibly change the weapon's state. Return the hitpoints"))

(defun use-weapon (user weapon attacked)
  "Determine how many damage points the attacked  should be inflicted by the weapon
   then apply that damage to the attacked"
  (let ((effect (extract-damage-points weapon)))
    (msg user "~% effect of weapon ~s is ~s" weapon effect)
    (inflict-damage user effect attacked)))


;; XXX This is the core of the battle system, this is where
;;     the attack/counterattack system should be injected.
;;     Also, sound/graphics should be focused at this point.



(defun is-alive-p (actor)
  (not (is-dead-p actor)))

(defgeneric is-dead-p (actor)
  (:method-combination and)
  (:method  and  ((actor T))
            t)
  (:method and ((fighter Fighter))
           (<= (health fighter) 0.0000001)))



(defgeneric msg-stream (recipient)
  (:method ((r T))
           *standard-output*)
  (:method ((l Location))
           (apply #'make-broadcast-stream (mapcar #'msg-stream (find-players l))))
  (:method ((l GameWorld))
           (apply #'make-broadcast-stream (mapcar #'msg-stream (find-players l))))
  (:method ((p Player))
           (out-stream p)))
 
(defun msg (recipient &rest format-args)
     (apply #'format (cons  (msg-stream recipient) format-args)))

(defgeneric health-reaction (actor newHealth)
  (:method ((fighter Fighter)(newHealth Number))
           (setf (health fighter) newHealth)
           ;; XXX Set state of player as well.  Dead players shouldn't
           ;;     be allowed to act, but may be allowed to react, and may
           ;;     perhaps be reanimated using a healing spell (not yet defined)
           (when (is-dead-p fighter)
             (msg fighter "~% ~a dies" (description fighter)))))

(defgeneric inflict-damage  (user damage attacked)
  (:method ((user T) (damage Number) (attacked Fighter))
           (msg user "~%   Inflicting damage ~f hp to fighter ~a with initial health ~f"
                   damage (description attacked) (health attacked))

           (health-reaction
            attacked 
            (max 0 (- (health attacked) damage)))))
    
(defgeneric attack (attacker attacked weapon)
  (:method ((attacker Fighter) (attacked Fighter) (weapon T))
           ;; If we don't have a weapon, let the attacker be the
           ;; weapon.
           (msg attacker  "~% ~@(~a~) attacks ~a with his own hands" (description attacker) (description attacked))
           (use-weapon attacker attacker attacked))
  
  (:method ((attacker Fighter) (attacked Fighter) (weapon Weapon))
           (msg attacker "~% ~@(~a~) attacks ~a with ~a" (description attacker) (description attacked) (description weapon))
           (use-weapon attacker weapon attacked))

  (:documentation "..."))

;;;
;;; COMMAND LINE PARSER
;;;


(defun inner-game-repl (player &key
                        (input *standard-input* )
                        (output *standard-output*))
  "The inner part of the main loop, it will throw an 'escape-from-game exception when the user wishes to quit"
  (if (null player)
      (error "player can't be null"))
  
  (setf (out-stream player) output)
  (setf (in-stream  player) input)
  (loop
   (if (is-dead-p player)
       (format (out-stream player) "~% You are dead"))
   (format (out-stream player) "~% Adv>")
   (finish-output (out-stream player))
   (parse-wordlist (split-string-to-words (read-command-from-user :input input))
                   player))
  (move-all-monsters (world  player)))

(defun move-all-monsters (world)
  "Should move all monsters within a world"
  (declare (ignore world))
  (format *standard-output* "~% Placeholder for monster movement"))

(defun game-repl (player &key
                   (input *standard-input* )
                   (output *standard-output*))
  "The outer part of the main loop, it will terminate when the user terminates through quit or otherwise"
  (catch 'escape-from-game
      (inner-game-repl player :input input :output output )))


(defun read-command-from-user (&key (input *standard-input*))
  "Read a simple line from the command line"
  (read-line input))


(defun split-by-one-space (string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(defun split-string-to-words (line)
  #+clisp
  (REGEXP:REGEXP-SPLIT " "  line)
  #+sbcl
  (split-by-one-space line)
  )

(defun split-on-word (input splitlist)
  (let ((pre  '())
        (post '())
        (post-marker nil))

    (dolist (i input)
      (cond  (post-marker
              (setf post (append post (list i))))
             ((find i splitlist :test #'string-equal)
              (setf post-marker t))
             (t 
              (setf pre (append pre (list i))))))
    (values pre post)))

;;
;;  A command interpreter
;;

(defclass Command ()
  ((names :accessor names :initarg :names)))

(defclass InventoryCmd (Command) ())
(defclass LookCmd      (Command) ())
(defclass GoCmd        (Command) ())
(defclass FightCmd     (Command) ())
(defclass HelpCmd      (Command) ())
(defclass QuitCmd      (Command) ())
(defclass TakeCmd      (Command) ())
(defclass DropCmd      (Command) ())
(defclass ReadCmd      (Command) ())

(defgeneric command-available-for-player-p (command player)
  (:documentation "Return t if the command is available to the user, nil otherwise")

  ;; By default commands are only available for living players.
  (:method ((c Command) (p Player))
           (is-alive-p p))
  
  ;; However, a couple of commands will always be available
  (:method ((c QuitCmd) (p Player))  t)
  (:method ((c HelpCmd) (p Player))  t))


(defun add-to-inventory (ob destination)
  (add-all-to-inventory destination (list ob)))

(defgeneric add-backpointer (ob destination)
  (:method-combination progn)
  (:method  progn ((o T)(d T)))
  (:method  progn ((o Inhabitant) (d Gameworld))
           (setf (world o) d)))


(defun add-all-to-inventory (destination obs)
  (setf (inventory destination)
        (union obs (inventory destination)))
  (dolist (ob obs)
    (add-backpointer ob destination))
  (inventory destination))


(defun remove-from-inventory (ob source)
  (setf (inventory source)
        (remove ob (inventory source))))

(defgeneric movement-ok (ob source destination)
  (:method-combination and)
  (:method and ((ob T) (source T) (destination T))
           t)
  (:method and ((ob Monster) (source T) (destination Player))
           nil))


(defgeneric move-object (ob source destination)
  (:method-combination progn))

(defmethod move-object progn ((ob Located) (source t) (destination Location))
  (setf (location ob) destination) )

(defmethod move-object  progn ((ob t) (source Inventory) (destination t))
  (remove-from-inventory ob source))

(defmethod move-object  progn ((ob t) (source t) (destination Inventory))
  (add-to-inventory ob destination))


(defun find-and-move (stream query source destination ack)
  (let ((objects  (identify query (inventory source))))
    (cond ((= 1 (length objects))
           (if (movement-ok (first objects) source destination)
               (progn (move-object (first objects) source destination)
                      (format stream  "~% ~a" ack))
             (format stream "~% no can do")))
          ((null objects)
           (format stream "~% Couldn't find anything like that"))
          (t
           (format stream  "~% Hmmm. More than one thing can be described that way. Please be more specific.")))))

;;
;; Finding objects of various types
;;

;; XXX Perhaps  this function should be called "collect" intead, since
;; it does not, as the common lisp "find" function find only the first
;; matching element, it collects all the matching elements.  Think
;; about it.

(defun find-objects-of-type (container query-type)
  "Find objects in an inventory-class container that matches the query type"
  (loop for item in (inventory container)
        when (subtypep (type-of item) query-type)
        collect item))

(defun find-objects-with-typename (container query-typename)
  "Find objects in an inventory-class container that matches the query typename"
  (find-objects-of-type container (find-class query-typename)))

(defun weapons-available-for-player (player)
  (find-objects-with-typename player 'Weapon))

(defun find-players (container)
  (find-objects-with-typename container 'Player))

(defun find-readables (container)
  (find-objects-with-typename container 'Readable))


(defun find-player (playerid gameworld)
  (let ((players (identify (list playerid) (find-players gameworld))))
    (cond ((= (length players) 1) (car players))
	  ((null players) 
	   nil)
	  (t (error "player is not unique")))))

(defun pick-best-weapon (attacker attacked)
  "Today's simple heuristic is simply to get the strongest weapon available no matter what"
  (declare (ignore attacker)
           (ignore attacked))

  #'(lambda (old new)
      (if (> (strength old)
             (strength new))
          old
        new)))


;; Btw, this is a really horrible heuristic.
(defun pick-most-appropriate-weapon (attacker attacked)
  "Pick the most appropriate for the attacker to attack the attacked,
if no weapon can be found, nil is returned"
  (let ((weapons (weapons-available-for-player attacker)))
    (cond ((null weapons)
           nil)
          ((null (cdr weapons))
           (car weapons)
           )
          (t  (reduce (pick-best-weapon attacker attacked)
                      weapons)))))


(defun counterattack (defender attacker)
  "The defender counterattacks against the attacker, if possible"
  (attack defender attacker (pick-most-appropriate-weapon defender attacker)))

(defgeneric applyCmd (Command Player List)
  (:documentation "Apply a command to a player with some input parameters")

  (:method ((c TakeCmd) (p Player) (query List))
           (find-and-move (out-stream p) query (location p) p "Got it"))

  (:method ((c DropCmd) (p Player) (query List))
           (find-and-move (out-stream p) query p (location p) "Dropped"))

  (:method ((c FightCmd) (p Player) (query List))
           
           ;; Is the query on the format <target> (<with> <weapon>)?
           (multiple-value-bind (target-desc weapon-desc)
               (split-on-word (cdr query) '("with" "using"))
             (let ((target (identify target-desc (inventory (location p))))
                   (weapon (identify weapon-desc (inventory p))))
               (let ((tgt (if (null target) nil (car target)))
                     (wpn (if (null weapon) p   (car weapon))))
                 (attack p  tgt  wpn)
                 (counterattack tgt p)))))
  
  (:method ((c InventoryCmd) (p Player) (l List))
           (describe-for-user (out-stream p) p))

  (:method ((c ReadCmd) (p Player) (l List))
	   (let ((items (identify l (find-readables p))))

	     (cond ((null items)
		    (format (out-stream p) "~% Couldn't find anything like that"))
		   ((= 1 (length items))
		    (format (out-stream p) "~% You read: ~A." (content (first items))))
		   (t 
		    (format (out-stream p) "~% Hmmm, that isn't specific enough")))))
  
  (:method ((c LookCmd) (p Player) (l List))
           (describe-for-user  (out-stream p) (location p)))

  (:method ((c GoCmd) (p Player) (l List))
           (cond ((null l)
                  (format *standard-output* "~% You must say where you want to go"))
                 (t 
                  (move p (rest l)))))

  (:method ((c HelpCmd) (p Player) (l List))
           (let ((available-commands (commands-available-for-player p)))
             (format (out-stream p) "~% Available commands are: ~{~s~^, ~}."
                     (map-commands-to-command-names available-commands))))
  
  (:method ((c QuitCmd) (p Player) (l List))
           (format (out-stream p) "~% Ttfn~2%")
           (throw 'escape-from-game 'user-quit)))

(defparameter *commands*
  (list
   (make-instance 'InventoryCmd :names '("inventory" "inv" "list"))
   (make-instance 'LookCmd      :names '("look" "peek" "see" "glance"))
   (make-instance 'GoCmd        :names '("go" "move" "run" "jump" "crawl"))
   (make-instance 'FightCmd     :names '("fight" "kill" "strike" "slash" "slab" "attack" "stab" "maim" "hit"))
   (make-instance 'TakeCmd      :names '("take" "grab" "pick"))
   (make-instance 'DropCmd      :names '("drop" "leave" "stash"))
   (make-instance 'ReadCmd      :names '("read"))
   (make-instance 'HelpCmd      :names '("?" "help" "what"))
   (make-instance 'QuitCmd      :names '("quit" "bye" "q"))))

(defun commands-available-for-player (player &optional (commands *commands*))
  (loop for command in commands
        when (command-available-for-player-p command player)
        collect command))


(defun map-commands-to-command-names (commands)
   (apply #'append (mapcar #'names commands)))

(defun find-command (name commands)
  "Find a command matching a name"
  (find-if #'(lambda (names) (find  name names :test #'string-equal))
           commands :key #'names))

(defun parse-wordlist (wl player)
  (let ((cmd (find-command (first wl) (commands-available-for-player player))))
    (if (not (null cmd))
        (applyCmd cmd player wl))))

;;
;;  The search engine  ;)
;;

(defparameter *stopwords* '("the" "at" "an"))

(defgeneric matches (List t)
  (:documentation "Describe something for a user")
  (:method ((query List) (object t))
           (member object query))
  
  (:method ((query List) (object List))
           (intersection query object :test #'string-equal))
  
  (:method ((query List) (object Describable))
           (matches query
                    (split-string-to-words (description object)))))


(defun remove-all-x (l r &key (test #'eql))
  (loop for i in l
        unless (find i r :test test)
        collect i))

(defun identify (query objects)
  "Identify the objects described by the query in the list of objects,
   return null if no objecs could be identified"

  ;; Remove all stopwords from query
  (setf query  (remove-all-x query *stopwords* :test #'string-equal))

  ;; Then try to match all the objects in the list with the
  ;; the query

  (loop for ob in objects
        when (matches query ob)
        collect ob))

;;
;; World building
;;

(defparameter *north* '("north" "n"))
(defparameter *south* '("south" "s"))
(defparameter *east*  '("east"  "e"))
(defparameter *west*  '("west"  "w"))
(defparameter *up*    '("up"    "u"))
(defparameter *down*  '("down"  "d"))

(defparameter *condensed-reverse-direction-map* 
  `((,*north* ,*south*)
    (,*east*  ,*west*)
    (,*up*    ,*down*)))

(defparameter *reverse-direction-map* 
  (append *condensed-reverse-direction-map* (mapcar #'reverse *condensed-reverse-direction-map*)))


(defun reverse-direction (direction)
  (second (find direction 
		   *reverse-direction-map*
		   :key #'first)))


(defun find-matching-navigations (directionnames location)
  (loop for nav in (navigation location)
        when (intersection directionnames (name location) :test #'string-equal)
        collect nav))

(defun set-navigation (origin destination names)
  "Will replace the names with a new navigation instance"

  (setf (navigation origin)
        (set-difference (navigation origin) (find-matching-navigations names origin)))
  (setf (navigation origin)
        (union (navigation origin)
               (list (make-instance 'navigation :names names :destination  destination)))))



;; XXX This function is very brittle, it should have more parameter checking,
;;     and it should be iterative, not tail recursive (although that isn't so
;;     awful in itself).
(defun navigation-path (&rest path)
  (when (>= (length path) 3)
	 (let*  ((source       (first  path))
		 (direction    (second path))
		 (destination  (third  path))
		 (reverse-dir  (reverse-direction direction)))

	   (if (null source)
	       (error "Source can't be null"))
	   (if (null destination)
	       (error "Destination can't be null, direction was ~S" direction))
	   (if (null direction)
	       (error "Direction can't be null"))
	   (if (null reverse-dir)
	       (error "Reverse direction can't be null"))

	   (adv:set-navigation source destination  direction)
	   (adv:set-navigation destination source  reverse-dir)
	   (navigation-path (cddr path)))))



;; XXX This is a work in progress.   When done, it will make the 
;;      world
(defmacro defworld (gameworld-description &body defworld-body)
  (let ((world-var (gensym))
	(tmp (gensym)))
    `(let ((,world-var (make-instance 'adv::GameWorld :description ,gameworld-description)))
       (labels ((current-world () 
			       ,world-var)
		(internalize-item (item)
				  (adv:add-to-inventory item ,world-var)
				  item)
		(create-internalized-item (class description params)
			  (internalize-item
			   (apply #'make-instance 
				  (cons class (cons :description (cons description  params)))))))
	 ;; XXX  This flet could be made much simpler!
	 (flet ((new-location (description &rest location-body)
			(create-internalized-item 'adv::Location description location-body))
		(new-item (description &rest description-body)
			(create-internalized-item 'adv::Item description description-body))
		(new-player (description &rest player-body)
			(create-internalized-item 'adv::Player description player-body))
		(new-monster (description &rest monster-body)
			(create-internalized-item 'adv::Monster description monster-body))
		(new-readable (description &rest readable-body)
			(create-internalized-item 'adv::Readable description readable-body))
		(new-weapon (description &rest weapon-body)
			(create-internalized-item 'adv::Weapon description weapon-body))
		(stash (recipient &rest items)
		       (dolist (item items)
			 (adv:move-object item nil  recipient))
		       recipient))
		  ,@defworld-body)
	 (current-world)))))
