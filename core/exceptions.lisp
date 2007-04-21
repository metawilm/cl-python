;; -*- package: clpython; readtable: py-ast-user-readtable -*-
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  
(defclass {Exception} (py-object error)
  ((args :initarg :args :initform nil :documentation "Arguments as Lisp list"
	 :accessor exception-args))
  (:metaclass py-type))

(def-py-method {Exception.__new__} :static (cls &rest args)
	       (declare (ignore args))
	       #+(or)(assert (subtypep cls (load-time-value (find-class '{Exception} ))))
	       (make-instance cls))

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
	     (let ((c (mop:ensure-class exc-name
					:direct-superclasses (list super)
					:metaclass 'py-type)))
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

) ;; eval-when

(defun py-raise-runtime-error ()
  ;; RuntimeError object is allocated at load-time, to prevent causing
  ;; a new error.
  (error (load-time-value (make-instance '{RuntimeError}))))

(defmethod print-object ((x {Exception}) stream)
  (format stream "~A" (class-name (class-of x)))
  (when (and (slot-boundp x 'args)
	     (slot-value x 'args))
    (format stream "~@[: ~@{~A~^, ~}~]"
	    (destructuring-bind (string . format-args)
		(slot-value x 'args)
	      (apply #'format nil string format-args)))))

(def-py-method {Exception.__repr__} (x)
  (with-output-to-string (s)
    (print-object x s)))


(defparameter *cached-StopIteration*
    (make-instance '{StopIteration} :args (list "Iterator has finished"))
  "Shared instance of this commonly used exception")

(defun raise-StopIteration ()
  (assert *cached-StopIteration*)
  (error *cached-StopIteration*))

(setf *exceptions-loaded* t)
