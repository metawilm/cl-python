;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.IMP -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module.imp)

(defun |get_magic| ()
  ;; Does not make sense for this implementation
  "")

(defun |find_module| (name path)
  (declare (ignore name path))
  (break "Todo: imp.find_module()"))

(defun |load_module| (&rest args)
  (declare (ignore args))
  (break "Todo: imp.load_module()"))

;; imp.PY_SOURCE
;; imp.PY_COMPILED
;; imp.C_EXTENSION
;; imp.PKG_DIRECTORY
