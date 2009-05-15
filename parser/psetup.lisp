;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.PARSER; Readtable: PY-AST-USER-READTABLE -*-
;; 
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Parser utilities

(in-package :clpython.parser)
(in-syntax *ast-user-readtable*)

;;; Parser syntax error handling
;;
;; The parser may encounter syntax errors. If the core of CLPython is
;; loaded, a SyntaxError is thrown, otherwise a regular ERROR.
;; Same for EOF error.

(defvar clpython:*exceptions-loaded* nil)

(defparameter clpython:*raise-syntax-error-hook* 'raise-syntax-error-default)

(defun clpython:raise-syntax-error (&rest args)
  (apply *raise-syntax-error-hook* args))

(defun raise-syntax-error-default (formatstring &rest args)
  "Raise SyntaxError, or (if that condition is unavailable) a regular error."
  (declare (special clpython:*exceptions-loaded* clpython::*exceptions-are-python-objects*))
  (if clpython:*exceptions-loaded*
      (apply 'clpython:py-raise '{SyntaxError} formatstring args)
    (apply #'error (concatenate 'string "SyntaxError: " formatstring) args))
  (break "never"))

(defun raise-unexpected-eof (&optional line-no)
  (declare (special clpython:*exceptions-loaded*))
  (let ((msg (format nil "Unexpected end of file~@[ (line ~A)~]." line-no)))
    (if clpython:*exceptions-loaded*
	(funcall 'clpython:py-raise '{UnexpectedEofError} msg)
      (raise-syntax-error msg)))
  (break "unreachable"))

;;; Depenency on Allegro's YACC

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :yacc))


(defparameter *source-level-debugging* nil
  ;; #+#1=(and allegro-version>= (version>= 8 2)) t
  ;; #-#1# nil
  )

(register-feature :clpython-source-level-debugging *source-level-debugging*)
