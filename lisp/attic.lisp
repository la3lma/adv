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


;; Misc. stuff that isn't used anywhere but might be useful one day




;;
;;  SETTING UP A NETWORK SERVER
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
#+clisp
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

