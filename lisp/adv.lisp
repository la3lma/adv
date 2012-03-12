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


(defclass Located ()
  ((location    :accessor location    :initarg :location)))

(defclass Player (Describable Inventory Located)
  ((out-stream :accessor out-stream :initarg :out-stream :initform *standard-output*)
   (in-stream :accessor in-stream :initarg :in-stream :initform *standard-output*)))

(defclass Item (Describable Located)
  ())


(defclass Navigation ()
  ((name  :accessor name   :initarg :name)
   (destination :accessor destination :initarg :destination)))

(defgeneric describe-for-user (stream describable)
  (:documentation "Describe something for a user")
  (:method ((stream t) (l Location))
           (format stream "~% Location: ~A" (description l))
           (format stream "~% With inventory:~{~%  ~a~}." (mapcar #'description (inventory l))))

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
                   (find direction (name nav) :test #'string-equal)
                 (move-object p location (destination nav))
                 (return-from move)))
             (if (not (null direction))
                 (format (out-stream p) "Don't know how to go ~{~s~^ ~}" l)))))
;;
;; The actual game objects. For testing, not playing (obviously)
;;

(defvar *initial-location* (make-instance 'Location :description "The start"))
(defvar *goal-location*    (make-instance 'Location :description "The goal"))
(defvar *initial-item*     (make-instance 'Item     :description "An item"))
(defvar *current-player*   (make-instance 'Player   :description "The player" :location *initial-location*))


(defparameter *north* '("north" "n"))
(defparameter *south* '("south" "s"))

(setf (navigation *initial-location*)
      (list (make-instance 'navigation :name *north* :destination *goal-location*)))

(setf (inventory *initial-location*)
      (list *initial-item*))

(setf (navigation *goal-location*)
      (list (make-instance 'navigation :name *south* :destination  *initial-location*)))

;;;
;;; COMMAND LINE PARSER
;;;

(defun game-repl (&key (input *standard-input* ) (output *standard-output*) (player *current-player*))
  "The main loop"

  (setf (out-stream player) output)
  (setf (in-stream  player) input)
    (loop
     (format (out-stream player) "~% Adv>")
     (parse-wordlist (split-string-to-words (read-command-from-user :input input)))))

(defun read-command-from-user (&key (input *standard-input*))
  "Read a simple line from the command line"
  (read-line input))

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

(defmethod move-object progn ((ob Located) (source t) (destination t))
  (setf (location ob) destination) )
  
(defmethod move-object  progn ((ob t) (source Inventory) (destination Inventory))
  (remove-from-inventory ob source)
  (add-to-inventory ob destination))


(defun find-and-move (stream query source destination ack)
  (let ((objects  (identify query (inventory source))))
    (cond ((= 1 (length objects))
           (move-object (first objects) source destination)
           (format stream  "~% ~a" ack))
          
          ((null objects)
           (format stream "~% Couldn't find anything like that"))
          
          (t
           (format stream  "~% Hmmm. More than one thing can be described that way. Please be more specific.")))))


(defgeneric applyCmd (Command Player List)
  (:documentation "Apply a command to a player with some input parameters")

  (:method ((c TakeCmd) (p Player) (query List))
           (find-and-move (out-stream p) query (location p) p "Got it"))

  (:method ((c DropCmd) (p Player) (query List))
           (find-and-move (out-stream p) query p (location p) "Dropped"))
  
  (:method ((c InventoryCmd) (p Player) (l List))
           (describe-for-user (out-stream p) p))
  
  (:method ((c LookCmd) (p Player) (l List))
           (describe-for-user  (out-stream p) (location p)))

  (:method ((c GoCmd) (p Player) (l List))
           (cond ((null l)
                  (format *standard-output* "~% You must say where you want to go"))
                 (t 
                  (move p (rest l)))))

    (:method ((c HelpCmd) (p Player) (l List))
             (format (out-stream p) "~% Available commands are: ~{~s~^ ~}." (available-commands)))
    
    (:method ((c QuitCmd) (p Player) (l List))
             (format (out-stream p) "~% Ttfn~2%")
             (throw 'escape-from-game 'user-quit)))

(defparameter *commands*
  (list
   (make-instance 'InventoryCmd :names '("inventory" "inv" "list"))
   (make-instance 'LookCmd      :names '("look" "peek" "see" "glance"))
   (make-instance 'GoCmd        :names '("go" "move" "run" "jump" "crawl"))
   (make-instance 'TakeCmd      :names '("take" "grab"))
   (make-instance 'DropCmd      :names '("drop" "leave" "stash"))
   (make-instance 'HelpCmd      :names '("?" "help" "what"))
   (make-instance 'QuitCmd      :names '("quit" "bye" "q"))))

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

;;
;;  Setting up a server
;;

;; The server must allow some kind of login, and then that
;; login session has to be hooked up to users of various kinds,
;; then I guess we can just let it rip.



;; This is what I want
;; (defun run-as-server () 
;;   (let ((server (port:open-socket-server 4141)))
;;     (loop
     
;;      ;; Listen for incoming connections
;;      (let ((socket (socket-accept server)))
       
;;        ;; Spawn a process to handle the connection
;;        (make-process "Connection handler"
;;                      #'handle-connection
;;                      socket))
     
;;      ;; The main process is now free to accept a new connection
;;      )))

;; This is what I need to do in clisp
(defun run-repl-as-server ()
  (LET ((server (SOCKET:SOCKET-SERVER)))
       (FORMAT t "~&Waiting for a connection on ~S:~D~%"
               (SOCKET:SOCKET-SERVER-HOST server) (SOCKET:SOCKET-SERVER-PORT server))
       (catch 'escape-from-game
         (UNWIND-PROTECT
          ;; infinite loop, terminate with Control+C
          (LOOP (WITH-OPEN-STREAM (socket (SOCKET:SOCKET-ACCEPT server))
                                  (MULTIPLE-VALUE-BIND (local-host local-port) (SOCKET:SOCKET-STREAM-LOCAL socket)
                                                       (MULTIPLE-VALUE-BIND (remote-host remote-port) (SOCKET:SOCKET-STREAM-PEER socket)
                                                                            (FORMAT T "~&Connection: ~S:~D -- ~S:~D~%"
                                                                                    remote-host remote-port local-host local-port)))
                                  ;; loop is terminated when the remote host closes the connection or on EXT:EXIT
                                  (LOOP (WHEN (EQ :eof (SOCKET:SOCKET-STATUS (cons socket :input))) (RETURN))
                                        ;                                      (PRINT (EVAL (READ socket)) socket)
                                        (game-repl :input socket :output socket)
                                        ;; flush everything left in socket
                                        (LOOP :for c = (READ-CHAR-NO-HANG socket nil nil) :while c)
                                        (TERPRI socket))))
          ;; make sure server is closed
          (SOCKET:SOCKET-SERVER-CLOSE server)))))
