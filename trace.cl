(in-package :python)

;;; Tracing callables
;;  
;;  X is a callable Python object. Examples: a function, a class
;;  (meaning, instantiating), or a class instance whose class has a
;;  __call__ method.


(defgeneric py-trace   (x) (:documentation "Trace callable Python object"))
(defgeneric py-untrace (x) (:documentation "Untrace traced Python object"))
  
#-allegro
(defmethod py-trace (x &optional pos-args kwd-args)
  (declare (ignore x pos-args kwd-args))
  (error "todo: py-trace #-allegro"))

#-allegro
(defmethod py-untrace (x)
  (declare (ignore x))
  (error "todo: py-untrace #-allegro"))


;; From here, it's all Allegro-specific

(defvar *traced-objects* (make-hash-table :test 'eq))
(defvar *trace-print-level* 0)



;; (excl:def-fwrapper py-trace-wrapper (x &optional pos-args kwd-args) ;; todo: know when pos/key, and when only pos

#+allegro
(excl:def-fwrapper py-trace-wrapper (&rest args)
  (dotimes (i *trace-print-level*)
    (format t "  "))
  (format t "[~A]  ~A~%" *trace-print-level* args)

  (let ((exception-causing-unwinding nil)
	(res nil))
    (unwind-protect
	
	(handler-bind
	    ((Exception (lambda (exc)
			  (setf exception-causing-unwinding exc))))
	  (setf res (let ((*trace-print-level* (1+ *trace-print-level*)))
		      (excl:call-next-fwrapper)))
	  res) ;; return value

      (dotimes (i *trace-print-level*)
	(format t "  "))
      (format t "[~A]  " *trace-print-level*)
      (if exception-causing-unwinding
	  (format t "...exception raised -> unwinding (~A)~%" (py-str exception-causing-unwinding))
	(format t "returned ~A~%" (py-str res))))))

#+allegro
(defmethod py-trace ((x function))
  (excl:fwrap x 'py-trace-wrapper 'py-trace-wrapper))

#+allegro
(defmethod py-untrace ((x function))
  (excl:funwrap x 'py-trace-wrapper))

#+allegro
(defmethod py-trace ((x user-defined-function))
  (declare (ignorable x))
  (error "todo: py-trace udf"))

