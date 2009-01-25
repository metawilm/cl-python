;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.GC -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;; Garbage collector interface

(in-package :clpython.module.gc)

(defun |disable| ()
  (warn "Disabling GC not implemented: gc.disable() has no effect.")
  *the-none*)

(defun |collect| (&optional generation)
  (declare (ignorable generation))
  #+allegro (excl:gc t)
  #-allegro (warn "gc.collect() not implemented in this Lisp implementation"))