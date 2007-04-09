(defpackage :clpython.module.time
  (:use :clpython :common-lisp)
  (:shadow #:|sleep| #:|time|)
  (:export #:|clock| #:|sleep| #:|time|))

(in-package :clpython.module.time)

(defun |clock| ()
  "Current processor time, in seconds, floating point"
  (coerce (/ (get-internal-run-time)
	     internal-time-units-per-second)
	  'float))

(defun |sleep| (n)
  (common-lisp:sleep (clpython::py-val->number n)))

(defun |time| ()  
  (excl.osi:universal-to-unix-time (get-universal-time)))
