;;  This is -*-LISP-*- syntax

(in-package #:cl-user)

(defpackage #:adv-system (:use #:cl #:asdf))

(in-package #:adv-system)

(asdf:defsystem adv
  :name "adv"
  :author "Bjørn Remseth; la3lma@gmail.com"
  :version "0.1"
  :licence "Apache 2.0"
  :description "A tiiiny little text oriented adventure game"
  :depends-on (
               ;; :uuid  ;; Unique identifiers
               ;; :documentation-template
               :lisp-unit  ;; Unit tests
               ;; :parenscript ;; common lisp to javascript translator
               ;; :split-sequence ;; Splitting stuff into sequences. (This will actually be useful)
               ;; :usocket sockets
               )
  :serial t
  :components
  ((:module "src"
            :serial t
            :components ((:file "adv")
                         ))
   (:static-file "README.md")
   (:static-file "COPYING")))


(asdf:defsystem adv-regression
                :name "adv test"
                :author "Bjørn Remseth; la3lma@gmail.com"
                :version "0.1"
                :licence "Apache 2.0"
                :description "testing adv"
                :depends-on (:adv)
                :serial t
                :components
                ((:module "src" ;; XXX Separate directories would rule here
                          :serial t
                          :components (
                                       (:file "adv-regression")
                                       ))))

(asdf:defsystem adv-regression-core-functionality
                :name "adv test scenario 1"
                :author "Bjørn Remseth; la3lma@gmail.com"
                :version "0.1"
                :licence "Apache 2.0"
                :description "testing adv"
                :depends-on (:adv :adv-regression)
                :serial t
                :components
                ((:module "src" ;; XXX Separate directories would rule here
                          :serial t
                          :components (
                                       (:file "adv-regression-core-functionality")
                                       ))))

(asdf:defsystem adv-regression-scenario1
                :name "adv test scenario 1"
                :author "Bjørn Remseth; la3lma@gmail.com"
                :version "0.1"
                :licence "Apache 2.0"
                :description "testing adv"
                :depends-on (:adv :adv-regression)
                :serial t
                :components
                ((:module "src" ;; XXX Separate directories would rule here
                          :serial t
                          :components (
                                       (:file "adv-regression-scenario1")
                                       ))))


