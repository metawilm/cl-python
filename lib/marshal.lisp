;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.MARSHAL -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module.marshal)

(defun |dump| (value file &optional version)
  (declare (ignore value file version))
  (error "TODO: marshal.dump"))

(defun |load| (file)
  (declare (ignore file))
  (error "TODO: marshal.load"))

(defun |dumps| (value &optional version)
  (declare (ignore value version))
  (error "TODO: marshal.dumps"))

(defun |loads| (string)
  (declare (ignore string))
  (error "TODO: marshal.loads"))

(defvar |version| -1)