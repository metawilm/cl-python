(in-package :python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exceptions
;;  
;; Because DEFINE-CONDITION has no :metaclass option, we use DEFCLASS.

(defclass Exception (python-object condition)
  ((args :initarg :args :documentation "Exception arguments (as tuple)"))
  (:documentation "The Exception type.")
  (:metaclass python-type)) 

;; This `Exception' class is a Python class, a regular Python object,
;; and a Lisp condition type: CLOS is impressive.

(defmethod print-object ((x Exception) stream)
  (format stream "~A" (class-name (class-of x)))
  (when (slot-boundp x 'args)
    (format stream ": ~A" (slot-value x 'args))))


(defvar *exceptions-tree* ;; XXX CPython has explanation string for every exception
    (quote
     (SystemExit 
      StopIteration
      (StandardError KeyboardInterrupt 
		     ImportError
		     (EnvironmentError (IOError 
					(OSError (WindowsError 
						  VMSError))))
		     EOFError
		     (RuntimeError (NotImplementedError))
		     (NameError    (UnboundLocalError))
		     AttributeError
		     (SyntaxError (IndentationError (TabError)))
		     TypeError
		     AssertionError
		     (LookupError (IndexError
				   KeyError))
		     (ArithmeticError (OverflowError 
				       ZeroDivisionError
				       FloatingPointError))
		     (ValueError (UnicodeError (UnicodeEncodeError
						UnicodeDecodeError 
						UnicodeTranslateError)))
		     ReferenceError
		     SystemError 
		     MemoryError)
      (Warning UserWarning 
	       DeprecationWarning 
	       PendingDeprecationWarning 
	       SyntaxWarning 
	       OverflowWarning 
	       RuntimeWarning   
	       FutureWarning))))

(defparameter *python-exceptions* ())

(push (cons 'Exception (find-class 'Exception)) *python-exceptions*)

(defun def-python-exceptions (root tree)
  (flet ((def-sub-exc (exc-name super)
	     ;; (format t "defining exception   ~A  (~A)~%" exc-name super)
	     (let ((c (mop:ensure-class exc-name
					:direct-superclasses (list super)
					:metaclass 'python-type)))
	       (push (cons exc-name c) *python-exceptions*))))
    (loop for sub in tree
	do (if (symbolp sub)
	       (def-sub-exc sub root)
	     (progn
	       (def-sub-exc (car sub) root)
	       (def-python-exceptions (car sub) (cdr sub)))))))

(def-python-exceptions 'Exception *exceptions-tree*)
