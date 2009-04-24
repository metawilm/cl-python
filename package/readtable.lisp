;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.PACKAGE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; These readtables ease working with Abstract Syntax Trees
;;;; and Python variable names.

(in-package :clpython.package)

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

(defconstant +reader-error-has-format+
    #+allegro t
    #+cmu t
    #+lispworks nil
    #+sbcl nil
    #-(or allegro cmu lispworks sbcl) nil)

(defun read-package-symbol-func (package start-char end-char &key intern)
  "Create a reader function that reads from start-char until endchar,
and returns a symbol in the given package. When no such symbol exists,
it will be interned if INTERN, otherwise an error is raised."
  (check-type start-char character)
  (check-type end-char character)
  
  (lambda (stream ch)
    (assert (char= ch start-char))
    (loop with name = (make-array 5 :element-type 'character
                                  :adjustable t :fill-pointer 0)
	for prev-char = ch
	for ch = (read-char stream)
	do (cond ((and prev-char (char= prev-char #\\))
		  (setf (aref name (1- (length name))) ch))
		 ((char/= ch end-char)
                  (vector-push-extend ch name))
                 (t 
                  (return (or (find-symbol name package)
                              (when intern
                                (intern name package))
                              (apply #'error 'reader-error :stream stream
                                     (when +reader-error-has-format+
                                       (list :format-control
                                             "No symbol named ~S in package ~S"
                                             :format-arguments
                                             (list name (find-package package))))))))))))
    
(defun setup-ast-readmacro (&optional (readtable *readtable*))
  (let ((read-[-func (read-package-symbol-func (find-package :clpython.ast) #\[ #\] :intern nil)))
    (set-macro-character #\[ read-[-func t readtable))
  readtable)

(defun setup-user-readmacro (&optional (readtable *readtable*))
  (let ((read-{-func (read-package-symbol-func (find-package :clpython.user) #\{ #\} :intern t)))
    (set-macro-character #\{ read-{-func t readtable))
  readtable)

(defvar *ast-readtable* 
    (setup-ast-readmacro (copy-readtable nil))
  "Readtable where [NAME] refers to clpython.ast::NAME")

(defvar *user-readtable*
    (setup-user-readmacro (copy-readtable nil))
  "Readtable where {NAME} refers to clpython.user::NAME")

(defvar *ast-user-readtable*
    (setup-user-readmacro (setup-ast-readmacro (copy-readtable nil)))
  "Readtable where [NAME] refers to clpython.ast::NAME and {NAME} to clpython.user::NAME")

;; To be able to mention these readtables in mode lines, they have to
;; be named.
#+allegro
(progn (setf (excl:named-readtable :py-ast-readtable) *ast-readtable*)
       (setf (excl:named-readtable :py-user-readtable) *user-readtable*)
       (setf (excl:named-readtable :py-ast-user-readtable) *ast-user-readtable*))


;;; Readtable that takes in everything available

(defun setup-omnivore-readmacro (&key function initial-forms (readtable *readtable*))
  "Create a readtable that dispatches all characters to FUNCTION.
The reader will initially return the INITAL-FORMS one by one, and then the result(s) of
calling FUNCTION on the stream.

INITIAL-FORMS typically contains an IN-PACKAGE form to ensure that compilation and loading of
the fasl file satisfies CLHS 3.2.4.4 \"Additional Constraints on Externalizable Objects\".
As a side effect, the function call of FUNCTION is executed with *PACKAGE* bound to PACKAGE."
  (check-type function function)
  (check-type readtable readtable)
  (flet ((omnivore-read-func (stream char)
           (unread-char char stream)
           (if initial-forms
               (pop initial-forms)
             (funcall function stream))))
    (dotimes (i 256) ;; use file encoding or char-code-limit?
      (set-macro-character (code-char i) #'omnivore-read-func t readtable)))
  readtable)

(defmacro with-ast-user-readtable (&body body)
  `(let ((*readtable* *ast-user-readtable*))
     ,@body))
