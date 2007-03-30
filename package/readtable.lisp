;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Readtables that easy working with CLPython's AST and USER packages

(in-package :clpython.package)

;;; Readtable in which to write CL code

;; The readtable setup is as suggested by Kent Pitman in
;;  http://groups.google.nl/group/comp.lang.lisp/msg/d97a08bd49db2b82?dmode=source
;; 
;; When parsing Python code, instead of using package prefixes it is must easier to
;; use a syntactical shorthand: "[NAME]" will refer to the symbol clpython.ast::NAME,
;; so we have [+], [def], [newline] etc.
;;
;; In the same way, {NAME} refers to clpython.user::NAME. It is required that
;; [NAME] already exists, while {NAME} is interned if new.
;;
;; The escape character is backslash: token "]" is thus [\]]. The character "|" will
;; be superfluously escaped, so [\|] and [\|=] instead of [|] and [|=], in order
;; not to confuse emacs.

(defmacro in-syntax (readtable-expression)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *readtable* ,readtable-expression)))

(defun read-package-symbol-func (package start-char end-char &key intern)
  "Create a reader function that reads from start-char until endchar,
and returns a symbol in the given package. When no such symbol exists,
it will be interned if INTERN, otherwise an error is raised."
  (check-type start-char character)
  (check-type end-char character)
  
  (lambda (stream ch)
    (assert (char= ch start-char))
    (loop with finished = nil
	with name = (make-array 5 :element-type 'character
				:adjustable t :fill-pointer 0)
	until finished
	for prev-char = ch
	for ch = (read-char stream)
	do (cond ((and prev-char (char= prev-char #\\))
		  (setf (aref name (1- (length name))) ch))
		 ((char= ch end-char)
		  (setf finished t))
		 (t
		  (vector-push-extend ch name)))
	finally 
	  (return (or (find-symbol name package)
		      (when intern
			(intern name package))
		      (error 'reader-error
			     :stream stream
			     :format-control "No symbol named ~S in package ~S"
			     :format-arguments (list name (find-package package))))))))

(defun setup-ast-readmacro (&optional (readtable *readtable*))
  (let ((read-[-func (read-package-symbol-func :clpython.ast #\[ #\] :intern nil)))
    (set-macro-character #\[ read-[-func t readtable))
  readtable)

(defun setup-user-readmacro (&optional (readtable *readtable*))
  (let ((read-{-func (read-package-symbol-func :clpython.user #\{ #\} :intern t)))
    (set-macro-character #\{ read-{-func t readtable))
  readtable)

(defvar *ast-readtable* (setup-ast-readmacro (copy-readtable nil))
  "Readtable where [NAME] refers to clpython.ast::NAME")

(defvar *user-readtable* (setup-user-readmacro (copy-readtable nil))
  "Readtable where {NAME} refers to clpython.user::NAME")

(defvar *ast-user-readtable* (setup-user-readmacro (setup-ast-readmacro (copy-readtable nil)))
  "Readtable where [NAME] refers to clpython.ast::NAME and {NAME} to clpython.user::NAME")


;;; Readtable that takes in everything available

(defun setup-omnivore-readmacro (function &optional (readtable *readtable*))
  "Let readtable dispatch all characters to FUNCTION"
  (check-type function function)
  (let ((read-func (lambda (stream char)
		     (unread-char char stream)
		     (funcall function stream))))
    (dotimes (i 256) ;; use file encoding or char-code-limit?
      (set-macro-character (code-char i) read-func t readtable)))
  readtable)
