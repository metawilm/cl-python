(in-package :python)

;;; These modules are always available (after importing) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `__builtin__'
;; 
;; Built-in functions, types and some special variables.

(defun make-bi-module ()
  (multiple-value-bind (mod ns)
      (make-std-module __builtin__)

    (setf *__builtin__-module* mod
	  *__builtin__-module-namespace* ns)
    
    (do-external-symbols (s 'python-builtin-functions)
      (namespace-bind ns (symbol-name s) (symbol-function s)))
  
    (do-external-symbols (s 'python-builtin-types)
      (if (boundp s) ;; check needed, as some symbols are TODO
	  (namespace-bind ns (symbol-name s) (symbol-value s))))

    (loop for (key val) in
	  `((None ,*None*)
	    (Ellipsis ,*Ellipsis*)
	    (NotImpemented ,*NotImplemented*)
	    (True ,*True*)
	    (False ,*False*)
	    (__debug__ ,*__debug__*))
	do (namespace-bind ns key val))
  
    (loop for (name . exc) in *python-exceptions*
	do (namespace-bind ns name exc))))

(make-bi-module)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `sys' module

;; The Python `sys' module: keeps status of the Python interpreter
;; XXX check that this is evaluated at the right time 

(defun make-sys-module ()
  (make-std-module sys
		   ((path    *sys.path*)
		    (modules *sys.modules*))))
(make-sys-module)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `time' module

(defun make-time-module ()
  (let* ((ns (loop with ns = (make-namespace :builtins t)
		 for (k v) in 
		   `((__name__ "time")
		     (clock ,(lambda ()
			       #+allegro
			       (mp:process-cpu-msec-used sys:*current-process*)))
		     (sleep ,(lambda (n)
			       (ensure-py-type n number
					       "time.sleep() expects number arg, got: ~A")
			       (mp:process-sleep n))))
		 do (namespace-bind ns k v)
		 finally (return ns)))
	 (mod (make-py-module :namespace ns)))
    (py-dict-sethash *sys.modules* "time" mod)))

(make-time-module)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `clpy' module: various useful tricks

#+(or) ;; :around doesn't work: no call-next-method
(defmethod clpy.trace (f)
  (let ((meth (make-instance 'standard-method
		:specializers (list (mop:intern-eql-specializer f))
		:lambda-list '(x &optional pos-args kwd-args)
		:qualifiers '(:around)
		:function (lambda (x &optional pos-args kwd-args)
			    (let ((res (call-next-method x pos-args kwd-args)))
			      (format nil ";;  ~A (~A ~A) -> ~A"
				      (py-str x)
				      (mapcar #'py-str pos-args)
				      (loop for (k . v) in kwd-args 
					  collect `(,k . ,(py-str v)))
				      res)
			      res)))))
    (add-method (ensure-generic-function 'py-call) meth)
    (format t ";; ~A is now traced~%" (py-str f))))


(defun make-clpy-module ()
  (make-std-module clpy
		   ((brk  (lambda () (break)))
		    #+(or)(trace #'clpy.trace))))

(make-clpy-module)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keep a copy of the initial modules; to let REPL restart fresh.

(defparameter *initial-sys.modules* (dict-copy *sys.modules*))
