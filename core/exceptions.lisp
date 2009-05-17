;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON; Readtable: PY-AST-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Python Exceptions

(in-package :clpython)
(in-syntax *ast-user-readtable*)

;; Depending on the Lisp implementation, the exceptions are either Python objects with
;; the appropriate metaclass, or fairly normal Lisp conditions.

#+clpython-exceptions-are-python-objects
(progn

  (defun py-raise (exc-type string &rest format-args)
    "Raise a Python exception with given format string"
    (error exc-type :args (cons string format-args)))

  (defclass {Exception} (object error)
    ((args :initarg :args :initform nil :documentation "Arguments as Lisp list"
           :accessor exception-args))
    (:metaclass py-type))
  
  (def-py-method {Exception.__new__} :static (cls &rest args)
    (make-instance cls))
  
  (def-py-method {Exception.__init__} (x &rest args)
    ;; raise AttributeError("a %s b", 24)  =>  AttributeError: "a 24 b"
    (when (and (>= (length args) 2)
               (stringp (car args)))
      (setf args (py-string.__mod__ (car args) (cdr args))))
    (setf (exception-args x) args))
    
  (def-py-method {Exception.__repr__} (x)
    (with-output-to-string (s)
      (print-object x s)))
  
  (defun define-exception-subclass (exc-name &rest supers)
    (dolist (s supers) (check-type s symbol))
    (ensure-class exc-name
                  :direct-superclasses supers
                  :metaclass 'py-type)))

#-clpython-exceptions-are-python-objects
(progn 

  (defun py-raise (exc-type string &rest format-args)
    "Raise a Python exception with given format string"
    (error exc-type :args (cons string format-args)))

  (define-condition {Exception} (error)
    ((args :initarg :args :initform nil :documentation "Arguments as Lisp list"
           :accessor exception-args)))

  (defun define-exception-subclass (exc-name &rest supers)
    (eval `(define-condition ,exc-name ,supers nil))))


;; Works in #+/#- either case

(defmethod print-object ((x {Exception}) stream)
  (format stream "~A" (class-name (class-of x)))
  (whereas ((args (exception-args x)))
    (destructuring-bind (string . format-args)
        args
      (format stream ": ~@<~@;~A~:>" (if format-args (apply #'format nil string format-args) string)))))


(defparameter *exception-tree* ;; XXX CPython has explanation string for every exception
    `({SystemExit}
      {StopIteration}
      {GeneratorExit}
      ({StandardError} {KeyboardInterrupt} 
		       {ImportError}
		       ({EnvironmentError} {IOError} 
					   ({OSError} {WindowsError} 
						      {VMSError} ))
		       {EOFError}
		       ({RuntimeError} {NotImplementedError} )
		       ({NameError}    {UnboundLocalError} )
		       {AttributeError}
		       ({SyntaxError} ({IndentationError} {TabError})
				      {UnexpectedEofError}) ;; UnexpectedEof: not std Python
		       {TypeError}
		       {AssertionError}
		       ({LookupError} {IndexError}
				      {KeyError})
		       ({ArithmeticError} {OverflowError}
					  {ZeroDivisionError}
					  {FloatingPointError} )
		       ({ValueError} ({UnicodeError} {UnicodeEncodeError}
						     {UnicodeDecodeError} 
						     {UnicodeTranslateError} ))
		       {ReferenceError}
		       {SystemError}
		       {MemoryError} )
      
      ({Warning} {DeprecationWarning}
		 {FutureWarning}
                 {ImportWarning}
		 {OverflowWarning}
		 {PendingDeprecationWarning}
		 {SyntaxWarning}
		 {RuntimeWarning}
		 {UserWarning} )))

(defvar *exception-classes* ())

(defun def-python-exceptions-1 (parent child-tree)
  (declare (optimize (debug 3))
	   (notinline def-python-exceptions))
  (flet ((def-sub-exc (super exc-name)
	     (push (define-exception-subclass exc-name super) *exception-classes*)))
    (if (symbolp child-tree)
	(def-sub-exc parent child-tree)
      (progn
	(def-sub-exc parent (car child-tree))
	(loop for subchild in (cdr child-tree)
	    do (def-python-exceptions-1 (car child-tree) subchild))))))

(defun def-python-exceptions ()
  (setf *exception-classes* (list (find-class '{Exception})))
  (loop for branch in *exception-tree*
      do (def-python-exceptions-1 '{Exception} branch)))

(def-python-exceptions)

(defparameter *cached-StopIteration*
    #+clpython-exceptions-are-python-objects
  (make-instance '{StopIteration} :args (list "Iterator has finished"))
  #-clpython-exceptions-are-python-objects
  (make-condition '{StopIteration})
  "Shared instance of this commonly used exception")

(defun raise-StopIteration ()
  (error *cached-StopIteration*))

(setf *exceptions-loaded* t)
