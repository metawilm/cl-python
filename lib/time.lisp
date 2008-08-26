;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.TIME -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module.time)

(defun |clock| ()
  "Current processor time, in seconds, floating point"
  (coerce (/ (get-internal-run-time)
	     internal-time-units-per-second)
	  'float))

(defun |sleep| (n)
  (common-lisp:sleep (clpython::py-val->number n)))

(defun |time| () 
  #+allegro (excl.osi:universal-to-unix-time (get-universal-time))
  #-allegro (error "todo"))
