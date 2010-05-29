;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON; Readtable: PY-USER-READTABLE -*-
;; 
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Support syntax error reporting when runtime not loaded
(in-package :clpython)
(in-syntax *user-readtable*)

(defvar *exceptions-loaded* nil)
(defvar *raise-syntax-error-hook* 'raise-syntax-error-default)

(defun raise-syntax-error (&rest args)
  "If the CLPython runtime is loaded, a SyntaxError is thrown, otherwise a regular ERROR.
Same for EOF error."
  (apply *raise-syntax-error-hook* args))

(defun raise-syntax-error-default (formatstring &rest args)
  "Raise SyntaxError, or (if that condition is unavailable) a regular error."
  (if *exceptions-loaded*
      (apply 'py-raise '{SyntaxError} formatstring args)
    (apply #'error (concatenate 'string "SyntaxError: " formatstring) args))
  (break "never"))
