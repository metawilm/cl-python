(in-package :python)

;;; These modules are always available (after importing) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `__builtin__'
;; 
;; Built-in functions, types and some special variables.

(defun def-bi-module ()
  (multiple-value-bind (mod ns)
      (def-std-module '__builtin__)

    (setf *__builtin__-module* mod
	  *__builtin__-module-namespace* ns)
    
    (do-external-symbols (s 'python-builtin-functions)
      (namespace-bind ns (intern s #.*package*) (symbol-function s))) ;; sym-name
  
    (do-external-symbols (s 'python-builtin-types)
      (if (boundp s) ;; check needed, as some symbols are TODO
	  (namespace-bind ns (intern s #.*package*) (symbol-value s))))

    (loop for (key val) in
	  `((None           ,*None*)
	    (Ellipsis       ,*Ellipsis*)
	    (NotImpemented  ,*NotImplemented*)
	    (True           ,*True*)
	    (False          ,*False*)
	    (__debug__      ,*__debug__*))
	do (namespace-bind ns key val))
  
    (loop for (name . exc) in *python-exceptions*
	do (namespace-bind ns name exc))))

(def-bi-module)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `sys' module

;; The Python `sys' module: keeps status of the Python interpreter
;; XXX check that this is evaluated at the right time 

(def-std-module 'sys
    `((path    ,*sys.path*)
      (modules  ,*sys.modules*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `time' module

(def-std-module 'time
    `((__name__ "time")
      (clock ,(lambda ()
		#+allegro
		(mp:process-cpu-msec-used sys:*current-process*)))
      (sleep ,(lambda (n)
		(ensure-py-type n number
				"time.sleep() expects number arg, got: ~A")
		(mp:process-sleep n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `clpy' module: various useful tricks

(defmethod py-time (x &optional (times 1))
  (time (dotimes (i times)
	  (py-call x)))
  (terpri))

(defmethod py-timeit (x)
  #+allegro
  (let* ((start (mp:process-cpu-msec-used sys:*current-process*))
	 (ignored (py-call x))
	 (stop (mp:process-cpu-msec-used sys:*current-process*)))
    (declare (ignore ignored))
    (terpri)
    (format t "~A" (coerce (/ (- stop start) 1000) 'float))))

(defmethod py-profile (x what)
  #+allegro
  (ecase what
    (:count
     (prof:with-profiling (:count t) (py-call x)) (terpri) (prof:show-call-counts))
    
    (:time
     (prof:with-profiling (:type :time) (py-call x)) (terpri) (prof:show-flat-profile))
    (:time-count
     (prof:with-profiling (:type :time :count t) (py-call x)) (terpri) (prof:show-flat-profile))
    (:time-graph
     (prof:with-profiling (:type :time) (py-call x)) (terpri) (prof:show-call-graph))
    
    (:space
     (prof:with-profiling (:type :space) (py-call x)) (terpri) (prof:show-flat-profile))
    (:space-count
     (prof:with-profiling (:type :space :count t) (py-call x)) (terpri) (prof:show-flat-profile))
    (:space-graph
     (prof:with-profiling (:type :space) (py-call x)) (terpri) (prof:show-call-graph))))


(def-std-module 'clpy
    `((brk     ,(lambda () (break))) ;; `break' is a reserved word, can't be name
      (trace   ,#'py-trace) ;; tracing
      (untrace ,#'py-untrace)
      (time    ,#'py-time)  ;; profiling
      (timeit  ,#'py-timeit)
		     
      (prof_c  ,(lambda (x) (py-profile x :count)))
      (prof_t  ,(lambda (x) (py-profile x :time)))
      (prof_tc ,(lambda (x) (py-profile x :time-count)))
      (prof_tg ,(lambda (x) (py-profile x :time-graph)))
      (prof_s  ,(lambda (x) (py-profile x :space)))
      (prof_sc ,(lambda (x) (py-profile x :space-count)))
      (prof_sg ,(lambda (x) (py-profile x :space-graph)))
		     
      (craise  ,(lambda (exc)  ;; `continuable raise'
		  (with-simple-restart (continue "Resume execution")
		    (error (if (typep exc 'class) (make-instance exc) exc)))))
		    
      #+py-exception-stack
      (active_excepts ,(lambda ()  ;; list of tuples of active `except' clauses
			 (make-py-list-from-list
			  (mapcar #'make-tuple-from-list *active-excepts*))))
		    
      #+py-exception-stack
      (catcher ,(lambda (exc)
		  (block catcher
		    (loop for x in *active-excepts*
			do (loop for e in x
			       when (subtypep exc e)
			       do (return-from catcher e))
			finally (return *False*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-std-module 'operator
    `((add ,#'py-+)
      (div ,#'py-/)
      (mul ,#'py-*)
      (sub ,#'py--)
      
      #+(or) ;; CPython operator modules also contains:
      (__abs__ __add__ __and__ __concate__ __contains__ __delitem__
	       __delslice__ __div__ __doc__ __eq__ __file__ __floordiv__
	       __ge__ __getitem__ __getslice__ __gte__ __neg__ __not__
	       __or__ __pos__ __pow__ __repreat__ __rshift__ __setitem__
	       __setslice__ __sub__ __truediv__ __xoe__
	       abs and_ attrgetter concat and-more----)))

		     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keep a copy of the initial modules; to let REPL restart fresh.

(defparameter *initial-sys.modules* (dict-copy *sys.modules*))





