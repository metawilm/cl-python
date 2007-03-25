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

(defun raise-syntax-error (formatstring &rest args)
  "Raise SyntaxError, or (if that condition is unavailable) a regular error."
  (declare (special clpython:*exceptions-loaded*))
  (if clpython:*exceptions-loaded*
      (funcall 'clpython:py-raise '{SyntaxError} formatstring args)
    (apply #'error (concatenate 'string "SyntaxError: " formatstring) args)))

(defun raise-unexpected-eof (&optional line-no)
  (declare (special clpython:*exceptions-loaded*))
  (let ((msg (format nil "Unexpected end of file~@[ (line ~A)~]." line-no)))
    (if clpython:*exceptions-loaded*
	(funcall 'clpython:py-raise '{UnexpectedEofError} msg)
      (raise-syntax-error msg))))

;;; Depenency on Allegro's YACC

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :yacc))
