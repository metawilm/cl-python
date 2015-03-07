;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE._STRUCT -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module._struct)

(defun |_clearcache| ()
  (cl:error "TODO"))

(defvar |__doc__| "FIXME: _struct doc")

(defun |pack| (fmt &rest args)
  (declare (ignore fmt args))
  (cl:error "TODO"))

(defun |pack_into| (fmt buffer offset &rest args)
  (declare (ignore fmt buffer offset args))
  (cl:error "TODO"))

(defun |unpack| (fmt string)
  (declare (ignore fmt string))
  (cl:error "TODO"))

(defun |unpack_from| (fmt buffer &optional (offset 0))
  (declare (ignore fmt buffer offset))
  (cl:error "TODO"))

(defun |calcsize| (fmt)
  (declare (ignore fmt))
  (cl:error "TODO"))
