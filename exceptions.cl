(in-package :python)

;;; Exceptions

;; Because DEFINE-CONDITION has no :metaclass option, we use DEFCLASS.

;; The exceptions are generated at compile time, so they can be set
;; in a DEFCONSTANT form in file BUILTIN-TYPES.CL.

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass Exception (py-object condition)
  ((args :initarg :args :initform nil :documentation "Arguments as Lisp list"))
  (:metaclass py-type))

(defvar *exception-tree* ;; XXX CPython has explanation string for every exception
    `(SystemExit
      StopIteration
      (StandardError KeyboardInterrupt 
		     ImportError
		     (EnvironmentError IOError 
				       (OSError WindowsError 
						VMSError))
		     EOFError
		     (RuntimeError NotImplementedError)
		     (NameError    UnboundLocalError)
		     AttributeError
		     (SyntaxError (IndentationError (TabError)))
		     TypeError
		     AssertionError
		     (LookupError IndexError
				  KeyError)
		     (ArithmeticError OverflowError 
				      ZeroDivisionError
				      FloatingPointError)
		     (ValueError (UnicodeError UnicodeEncodeError
					       UnicodeDecodeError 
					       UnicodeTranslateError))
		     ReferenceError
		     SystemError 
		     MemoryError)
      
      (Warning UserWarning
	       DeprecationWarning 
	       PendingDeprecationWarning 
	       SyntaxWarning 
	       OverflowWarning 
	       RuntimeWarning   
	       FutureWarning)))

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
  (setf *exception-classes* (list (find-class 'Exception)))
  (loop for branch in *exception-tree*
      do (def-python-exceptions-1 'Exception branch)))

(def-python-exceptions)

) ;; eval-when


(defun py-raise (exc-type string &rest format-args)
  "Raise a Python exception with given format string"
  (assert (find-class exc-type nil) ()
    (error "Python exception class ~A not found" exc-type))
  (error exc-type :args (apply #'format nil string format-args)))

(defmethod print-object ((x Exception) stream)
  (format stream "~A~@[: ~@{~A~^, ~}~]"
	  (class-name (class-of x))
	  (slot-value x 'args)))

(def-py-method Exception.__repr__ (x)
  (with-output-to-string (s)
    (print-object x s)))