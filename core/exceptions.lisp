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

(defvar *exceptions-loaded* nil)

(defun py-raise (exc-type string &rest format-args)
  "Raise a Python exception with given format string"
  (declare (ignorable string format-args))
  #+clpython-exceptions-are-python-objects
  (error exc-type :args (cons string format-args))
  #-clpython-exceptions-are-python-objects
  (error exc-type))

#+clpython-exceptions-are-python-objects
(defclass {Exception} (py-object error)
  ((args :initarg :args :initform nil :documentation "Arguments as Lisp list"
	 :accessor exception-args))
  (:metaclass py-type))

#-clpython-exceptions-are-python-objects
(define-condition {Exception} (error)
  ((args :initarg :args :initform nil :documentation "Arguments as Lisp list"
	 :accessor exception-args)))

#+clpython-exceptions-are-python-objects
(def-py-method {Exception.__new__} :static (cls &rest args)
  #+(or)(assert (subtypep cls (load-time-value (find-class '{Exception} ))))
  (make-instance cls))

#+clpython-exceptions-are-python-objects
(def-py-method {Exception.__init__} (x &rest args)
  ;; raise AttributeError("a %s b", 24)  =>  AttributeError: "a 24 b"
  (when (and (>= (length args) 2)
	     (stringp (car args)))
    (setf args (py-string.__mod__ (car args) (cdr args))))
  (setf (slot-value x 'args) args))
    
(defparameter *exception-tree* ;; XXX CPython has explanation string for every exception
    `({SystemExit}
      {StopIteration}
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
      
      ({Warning} {UserWarning}
		 {DeprecationWarning} 
		 {PendingDeprecationWarning} 
		 {SyntaxWarning} 
		 {OverflowWarning} 
		 {RuntimeWarning}   
		 {FutureWarning} )))

(defvar *exception-classes* ())

(defun def-python-exceptions-1 (parent child-tree)
  (declare (optimize (debug 3))
	   (notinline def-python-exceptions))
  (flet ((def-sub-exc (super exc-name)
	     (let ((c #+clpython-exceptions-are-python-objects
                      (ensure-class exc-name
                                    :direct-superclasses (list super)
                                    :metaclass 'py-type)
                      #-clpython-exceptions-are-python-objects
                      (eval `(define-condition ,exc-name ,(list super) nil))))
	       (push c *exception-classes*))))
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

#+clpython-exceptions-are-python-objects
(defmethod print-object ((x {Exception}) stream)
  (format stream "~A" (class-name (class-of x)))
  (when (and (slot-boundp x 'args)
	     (slot-value x 'args))
    (destructuring-bind (string . format-args)
        (slot-value x 'args)
      (format stream ": ~@<~@;~A~:>" (if format-args (apply #'format nil string format-args) string)))))

#+clpython-exceptions-are-python-objects
(def-py-method {Exception.__repr__} (x)
  (with-output-to-string (s)
    (print-object x s)))


(defparameter *cached-StopIteration*
    #+clpython-exceptions-are-python-objects
  (make-instance '{StopIteration} :args (list "Iterator has finished"))
  #-clpython-exceptions-are-python-objects
  (make-condition '{StopIteration})
  "Shared instance of this commonly used exception")

(defun raise-StopIteration ()
  (error *cached-StopIteration*))

(setf *exceptions-loaded* t)
