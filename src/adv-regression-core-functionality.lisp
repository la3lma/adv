;; -*-LISP-*-

;; This is a hack, the ordinary require etc. should be used instead, but how?
(asdf:operate 'asdf:load-op :lisp-unit)

(defpackage :adv-regression-core-functionality
  (:use :common-lisp :lisp-unit :adv-regression :adv))

(in-package :adv-regression-core-functionality)

;;
;;  Test the core functionality of the adv code.  No scenario
;;  testing, just the basic methods and such.
;;


(define-test test-reverse-direction
  (dolist (sample adv::*reverse-direction-map*)
    (assert-true (eq (reverse-direction (first sample)) (second sample)))))