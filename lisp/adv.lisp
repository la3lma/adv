;; -*-LISP-*-

;;;
;;; THE GAME MODEL
;;;

(defclass Describable ()
  ((description :accessor description :initarg :description :initform ""))
  (:documentation "Something that is describable for a user"))

(defclass Inventory ()
  ((inventory :accessor inventory :initarg :inventory :initform '()))
  (:documentation "Something that can contain other objects"))

(defclass Location (Describable Inventory)
  ((navigation  :accessor navigation   :initarg :navigation :initform '())))


(defclass Navigation ()
  ((name  :accessor name   :initarg :name)
   (destination :accessor destination :initarg :destination)))

(defgeneric describe-for-user (Describable)
  (:documentation "Describe something for a user")
  (:method ((l Location))
           (format *standard-output* "~% Location: ~A" (description l))
           (format *standard-output* "~% With inventory:~{~%  ~a~}." (mapcar #'description (inventory l))))

  (:method ((i Inventory)) (format *standard-output* "Inventory for ~s: ~{~%  ~a~}." (description i) (mapcar #'description (inventory i)))))

(defclass Player (Describable Inventory)
  ((location    :accessor location    :initarg :location)))

(defclass Located ()
  ((location    :accessor location    :initarg :location)))

(defclass Item (Describable Located)
  ())

(defgeneric move (Player List)
  (:documentation "Move the player somewhere based on some description of a direction")
  (:method ((p Player) (l list))
           (let* ((direction  (first l))
                  (location   (location p))
                  (navigation (navigation location)))

             (dolist (nav  navigation)
               (when (string-equal (name nav) direction)
                 (move-object p location (destination nav))
                 (return-from move)))
             (if (not (null direction))
                 (format *standard-output* "Don't know how to go ~{~s~^ ~}" l)))))
;;
;; The actual game objects. For testing, not playing (obviously)
;;

(defvar *initial-location* (make-instance 'Location :description "The start"))
(defvar *goal-location*    (make-instance 'Location :description "The goal"))
(defvar *initial-item*     (make-instance 'Item     :description "An item"))
(defvar *current-player*   (make-instance 'Player   :description "The player" :location *initial-location*))


(setf (navigation *initial-location*)
      (list (make-instance 'navigation :name "north" :destination *goal-location*)))

(setf (inventory *initial-location*)
      (list *initial-item*))

(setf (navigation *goal-location*)
      (list (make-instance 'navigation :name "south" :destination  *initial-location*)))


;;;
;;; COMMAND LINE PARSER
;;;

(defun game-repl ()
  "The main loop"
  (catch 'escape-from-game 
    (loop
     (format *standard-output* "~% Game>")
     (parse-wordlist (split-string-to-words (read-command-from-user))))))



(defun read-command-from-user ()
  "Read a simple line from the command line"
  (read-line))

(defun split-string-to-words (line)
  (REGEXP:REGEXP-SPLIT " "  line))



;;
;;  A command interpreter
;;

(defclass Command ()
  ((names :accessor names :initarg :names)))

(defclass InventoryCmd (Command) ())
(defclass LookCmd      (Command) ())
(defclass GoCmd        (Command) ())
(defclass HelpCmd      (Command) ())
(defclass QuitCmd      (Command) ())
(defclass TakeCmd      (Command) ())
(defclass DropCmd      (Command) ())


(defun add-to-inventory (ob destination)
  (setf (inventory destination)
        (union (list ob) (inventory destination))))

(defun remove-from-inventory (ob source)
  (setf (inventory source)
        (remove ob (inventory source))))


(defgeneric move-object (ob source destination)
  (:method-combination progn))

(defmethod move-object progn ((ob t) (source Located) (destination t))
  (setf (location source) destination) )
  
(defmethod move-object  progn ((ob t) (source Inventory) (destination Inventory))
  (remove-from-inventory ob source)
  (add-to-inventory ob destination))


(defun find-and-move (query source destination)
  (let ((objects  (identify query (inventory source))))
    (cond ((= 1 (length objects))
           (move-object (first objects) source destination)
           (format *standard-output* "~% Got it"))
          
          ((null objects)
           (format *standard-output* "~% Couldn't find anything like that"))
          
          (t
           (format *standard-output* "~% Hmmm. More than one thing can be described that way. Please be more specific.")))))


(defgeneric applyCmd (Command Player List)
  (:documentation "Apply a command to a player with some input parameters")

  (:method ((c TakeCmd) (p Player) (query List))
           (find-and-move query (location p) p))

  (:method ((c DropCmd) (p Player) (query List))
           (find-and-move query p (location p)))
  
  (:method ((c InventoryCmd) (p Player) (l List))
           (describe-for-user p))
  
  (:method ((c LookCmd) (p Player) (l List))
           (describe-for-user  (location p)))

  (:method ((c GoCmd) (p Player) (l List))
           (cond ((null l)
                  (format *standard-output* "~% You must say where you want to go"))
                 (t 
                  (move p (rest l)))))

    (:method ((c HelpCmd) (p Player) (l List))
             (format *standard-output* "~% Available commands are: ~{~s~^ ~}." (available-commands)))
    
    (:method ((c QuitCmd) (p Player) (l List))
             (throw 'escape-from-game 'user-quit)))

(defparameter *commands*
  (list
   (make-instance 'InventoryCmd :names '("inventory" "inv" "list"))
   (make-instance 'LookCmd      :names '("look" "peek" "see" "glance"))
   (make-instance 'GoCmd        :names '("go" "move" "run" "jump" "crawl"))
   (make-instance 'TakeCmd      :names '("take" "grab"))
   (make-instance 'DropCmd      :names '("drop" "leave" "stash"))
   (make-instance 'HelpCmd      :names '("?" "help" "what"))
   (make-instance 'QuitCmd      :names '("quit" "bye"))))

 (defun available-commands (&optional (commands *commands*))
   (apply #'append (mapcar #'names *commands*)))

(defun find-command (name &optional (commands *commands*))
  "Find a command matching a name"
  (find-if #'(lambda (names) (find  name names :test #'string-equal))
           commands :key #'names))

(defun parse-wordlist (wl)
  (let ((cmd (find-command (first wl))))
    (if (not (null cmd))
        (applyCmd cmd *current-player* wl))))

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




  
