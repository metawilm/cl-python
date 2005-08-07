(in-package :python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exceptions
;;  
;; Because DEFINE-CONDITION has no :metaclass option, we use DEFCLASS.

(defclass Exception (py-object condition)
  ((args :initarg :args :initform nil :documentation "Arguments as Lisp list"))
  (:metaclass py-type)) 

;; The `Exception' class is a Python class, a regular Python object,
;; and a Lisp condition. CLOS is pretty impressive.

(defmethod print-object ((x Exception) stream)
  (format stream "~A~@[: ~{~A~^, ~}]"
	  (class-name (class-of x))
	  (slot-value x 'args)))

(def-py-method Exception.__repr__ (x)
  (with-output-to-string (s)
    (print-object x s)))
    
(defvar *exceptions-tree* ;; XXX CPython has explanation string for every exception
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

(defparameter *python-exceptions* (list (cons 'Exception (find-class 'Exception))))

(defun def-sub-exc (super exc-name)
  (let ((c (mop:ensure-class exc-name
			     :direct-superclasses (list super)
			     :metaclass 'py-type)))
    (push (cons exc-name c) *python-exceptions*)))

(defun def-python-exceptions (parent child-tree)
  (declare (optimize (debug 3))
	   (notinline def-python-exceptions))
  
  (if (symbolp child-tree)
      (def-sub-exc parent child-tree)
    (progn
      (def-sub-exc parent (car child-tree))
      (loop for subchild in (cdr child-tree)
	  do (def-python-exceptions (car child-tree) subchild)))))

(loop for branch in *exceptions-tree*
    do (def-python-exceptions 'Exception branch))
