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

(defmethod py-time (x)
  (time (py-call x))
  (terpri))

(defmethod py-profile-count (x)
  (prof:with-profiling (:count t)
    (py-call x))
  (terpri)
  (prof:show-call-counts))

(defmethod py-profile-space-graph (x)
  (prof:with-profiling (:type :space)
    (py-call x))
  (terpri)
  (prof:show-call-graph))
  
(defmethod py-profile-time (x)
  (prof:with-profiling (:type :time)
    (py-call x))
  (terpri)
  (prof:show-flat-profile))

(defmethod py-profile-time-graph (x)
  (prof:with-profiling (:type :time)
    (py-call x))
  (terpri)
  (prof:show-call-graph))

(defmethod py-profile-space (x)
  (prof:with-profiling (:type :space)
    (py-call x))
  (terpri)
  (prof:show-flat-profile))

(defmethod py-profile-space-count (x)
  (prof:with-profiling (:type :space :count t)
    (py-call x))
  (terpri)
  (prof:show-flat-profile))

(defmethod py-profile-time-count (x)
  (prof:with-profiling (:type :time :count t)
    (py-call x))
  (terpri)
  (prof:show-flat-profile))

(defun make-clpy-module ()
  (make-std-module clpy
		   ((brk  (lambda () (break))) ;; `break' is a reserved word
		    (trace #'py-trace)
		    (untrace #'py-untrace)
		    (time #'py-time)
		    (prof_c #'py-profile-count)
		    (prof_t #'py-profile-time)
		    (prof_s #'py-profile-space)
		    (prof_sc #'py-profile-space-count)
		    (prof_tc #'py-profile-space-count)
		    (prof_sg #'py-profile-space-graph)
		    (prof_tg #'py-profile-time-graph))))

(make-clpy-module)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keep a copy of the initial modules; to let REPL restart fresh.

(defparameter *initial-sys.modules* (dict-copy *sys.modules*))
