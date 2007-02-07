(defpackage :clpython.module.time
  (:use :clpython :common-lisp))

(in-package :clpython.module.time)

(clpython::in-python-module "time" :builtin t)

(defun |clock| ()
  "Current processor time, in seconds, floating point"
  (coerce (/ (get-internal-run-time)
	     internal-time-units-per-second)
	  'float))

(defun |sleep| (n)
  (sleep (clpython::py-val->number n)))

(defun |time| ()  
  (excl.osi:universal-to-unix-time (get-universal-time)))
