(in-package :python)

;; (declaim (optimize (debug 3)))

;;; Calling objects

(defgeneric py-call (x &optional pos-args kwd-args)
  (:documentation "Call X with given arguments. Call arg rewriting must take ~@
                   place _before_ calling PY-CALL")) ;; or in py-call :AROUND ?


(defmethod py-call ((x function) &optional pos-args kwd-args)
  (when kwd-args
    (warn "py-call on regular Lisp Function ~A got keyword args?! ~@
           (pos-args: ~A; kwd-args: ~A - ignored)"
	  x pos-args kwd-args))
  (apply x pos-args))


(defmethod py-call ((x lisp-function-accepting-kw-args) &optional pos-arg kwd-arg)
  (funcall (slot-value x 'func) pos-arg kwd-arg))


;;; user-defined functions/methods:   

(defmethod py-call ((x user-defined-function) &optional pos-args key-args)
  "Evaluate function body in function definition namespace."
  
  ;; If the AST contains `return', it exists from this FUNCTION-BLOCK
  ;; (or a FB inside this one). `return' can only occur inside a
  ;; function body.
  ;; When the function doesn't do `return', None is returned implicitly.
  ;; 
  ;; TODO: rewrite function body to contain block as `return' target

  (with-slots (call-rewriter namespace ast) x
    (let ((actual-args (funcall call-rewriter pos-args key-args))
	  (namespace-copy (namespace-copy namespace)))
      
      ;; Copying the function namespace for every call is expensive,
      ;; but otherwise recursive programs don't work right as
      ;; different calls mess with each others local variables.
      
      ;; Evaluate function AST in the function definition namespace
      ;; extended with the current argument values.
      
      (loop for (arg . val) in actual-args
	  do (namespace-bind namespace-copy arg val))
      
      (let ((*scope* namespace-copy))
	(declare (special *scope*))
	(catch 'function-block
	  (py-eval-1 ast))))))


(defmethod py-call ((x python-function) &optional pos-args key-args)

  ;; used in user-defined functions
  (with-slots (call-rewriter namespace ast) x
    (let ((actual-args (funcall call-rewriter pos-args key-args)))
      
      ;; Evaluate function AST in the function definition namespace
      ;; extended with the current argument values.

      (loop for (arg . val) in actual-args
	  do (namespace-bind namespace arg val))
      
      (let ((*scope* namespace))
	(declare (special *scope*))
	(py-eval-1 ast)))))


(defmethod py-call ((x python-function-returning-generator) &optional pos-args key-args)
  (with-slots (call-rewriter generator-creator) x
    (declare (special *scope*))
    (let ((ns (make-namespace :name "generator namespace"
			      :inside *scope*))
	  (value-producing-f (funcall generator-creator)))
      
      (loop for (arg . val) in (funcall call-rewriter pos-args key-args)
	  do (namespace-bind ns arg val))
      
      (make-iterator-from-function (lambda () 
				     (let ((*scope* ns))
				       (declare (special *scope*))
				       (funcall value-producing-f)))))))

(defmethod py-call ((x bound-method) &optional pos-args kwd-args)
  "The instance enclosed in the bound method is prefixed to pos-args"
  (py-call (slot-value x 'func)
	   (cons (slot-value x 'object)
		 pos-args)
	   kwd-args))


(defmethod py-call ((x unbound-method) &optional pos-args kwd-args)
  "X must be of right class, then call class method with given args."

  (with-slots ((um-class class) (um-func func)) x
    (let ((inst (car pos-args)))
      (cond ((null pos-args) (py-raise 'ValueError
				       "Unbound method takes instance as first arg (got: no pos args)"))
	    ((or (typep inst um-class)  ;; common case
		 (subtypep (__class__ inst) um-class)) ;; first arg is a python value designator
	     (py-call um-func pos-args kwd-args))
	    
	    (t (py-raise 'TypeError
			 "Unbound method ~A must be called with instance of ~
                          class ~%~A ~%as first argument (got as first pos arg: ~S)"
			 x um-class inst))))))

(defmethod py-call ((x static-method) &optional pos-args kwd-args)
  (declare (ignore pos-args kwd-args))
  (break "py-call static-method"))
;;  (with-slots (func) x
;;    (py-call func pos-args kwd-args)))


;;;; Calling a class that is a subclass of `python-object' creates an instance.
;;; Calling a class that is a subclass of `python-type' creates a class.

(defmethod py-call ((cls class) &optional pos-args kwd-args)
  
  (when (subtypep cls 'Exception)
    (return-from py-call (py-call-exception cls pos-args kwd-args)))
  
  ;; type(x) is a function returning type of x
  (when (and (eq cls (load-time-value (find-class 'python-type)))
	     (= (length pos-args) 1))
    (warn "type(x) -> the type of x")
    (return-from py-call (py-type (car pos-args))))

  ;; <type>(..args..) creates an instance
  (py-call-class cls pos-args kwd-args))


(defmethod py-call-class ((cls class) pos-args kwd-args)
  (let ((cls-name (class-name cls)))
    (multiple-value-bind (__new__-meth found)
	(internal-get-attribute cls '__new__)
      (unless found
	(py-raise 'TypeError "Class ~A was called, but it has no __new__ method"
		  cls-name))
      (let ((inst (py-call __new__-meth (cons cls pos-args) kwd-args)))
	(multiple-value-bind (res found)
	    (call-attribute-via-class inst '__init__ pos-args kwd-args)
	  (declare (ignore res))
	  (unless found
	    (warn "Class ~S has no __init__ method" cls-name)))
	(return-from py-call-class inst)))))

(defmethod py-call ((cls user-defined-class) &optional pos-args kwd-args)
  (py-call-class cls pos-args kwd-args))
		    

(defmethod py-call ((x user-defined-object) &optional pos-args kwd-args)
  (if (typep x 'class)
      (error "py-call: need method for ~A" x)
    (py-call (getattr-of-class x '__call__) 
	     (let ((args (cons x pos-args))) 
	       (declare (dynamic-extent args))
	       args)
	     kwd-args)))


(defmethod py-call-exception ((cls class) &optional pos-args kwd-args)
  (assert (subtypep cls 'Exception))
  (if (typep cls 'user-defined-class)
      (error "TODO: calling UDC exceptions")
    (progn (when (or kwd-args (cdr pos-args))
	     (py-raise 'TypeError "Built-in exceptions must be called with at ~@
                                   most one positional arg (got: ~A with ~A ~A)"
		       cls pos-args kwd-args))
	   (make-condition cls :args (or (car pos-args) *None*)))))

#+(or)
(let ((cr (make-call-rewriter '(a b) '((c . 3)(d . 4)) 'args 'kwd))
      (posargs '(1 7 8 9 1 2 3))
      (kwargs '((e . 5)))
      )
  (funcall cr posargs kwargs))

(defun make-call-rewriter (fname fpos fkey f* f**)
  "For a given function with formal arguments FPOS, FKEY, F*, F**
   where FPOS list of atoms, FKEY list of (atom . value), F* atom or nil,
   F** atom or nil,  return a function that parses the arguments to it
   and return it as a list of (key . val) pairs."
  ;; Example:
  ;; (setq r (make-call-rewriter
  ;;          '()
  ;;          '((fget . *None*) (fset . *None*) (fdel . *None*) (doc . *None*))
  ;;          nil nil))
  ;; (funcall r '("asdf") '((doc . "doc!")))
  ;; -> ((fget . "asdf") (fset . *None*) (fdel . *None*) (doc . "doc!"))
  (let* ((fpos-len (length fpos))
	 (farg-len (+ fpos-len (length fkey)))
	 (argnames-list (concatenate 'list fpos (mapcar #'car fkey)))
	 (argnames-v (make-array farg-len
				 :element-type 'atom
				 :initial-contents argnames-list))
	 (pos-arg-marker 'pos-arg)
	 (opt-arg-marker 'opt-arg)
	 (defvalues-list
	     (append 
	      (mapcar (lambda (x) (declare (ignore x)) pos-arg-marker) fpos)
	      (mapcar (lambda (kv) (cons opt-arg-marker (cdr kv))) fkey)))
	 (argvalues-v (make-array farg-len :initial-contents defvalues-list)))
    
    (unless (equalp argnames-list (remove-duplicates argnames-list))
      (error "Duplicated formal argument names found: should be different (~A ~A)"
	     fpos fkey))
    (when (and f* (member f* argnames-list :test 'eq))
      (error "Formal argument name for f* (~A) is already used (~A ~A)"
	     f* fpos fkey))
    (when (and f** (member f** (cons f* argnames-list) :test 'eq))
      (error "Formal argument name for f** (~A) already used (~A ~A ~A)"
	     f** fpos fkey f*))
    
    (lambda (pos-args key-args)
      #+(or)(format t "call rewriter for {pos: ~A, key: ~A, f*: ~A, f**: ~A}"
		    fpos fkey f* f**)
      #+(or)(format t "got ~A ~A~%" pos-args key-args)
      
      (let ((result ()))
	(declare (dynamic-extent result))
	;; Because the result is only used for extending an
	;; environment with the proper values, the RESULT list can be
	;; allocated just once.
	;; 
	;; Alternatively, there is no need for a RESULT list if the
	;; environment to be extended is passed as an argument to the
	;; LAMBDA.
	
	;; reset argvalues-v (as its overwritten every time this lambda is called)
	;; 
	;; XXX maybe this should happen right after creating the
	;; RESULT list, to enable GC?
	(map-into argvalues-v #'identity defvalues-list)
	
	;; f* -> |apos| not limited
	;; f** -> |akey| not limited
	;;
	;; always:  |apos| + |akey| >= |fpos| (all positionals must be covered)
	;;          
	;; not f** ->  |akey| <= |fpos| + |fkey|
	;;    (all args can be covered by kwargs, not more)
	;; not f*  ->  |apos| <= |fpos| + |fkey| 
	;;    (all args can be covered by nonkwargs, not more)
	
	;; Assign positional args
	(let ((final-pos-arg 'foo)
	      (pos-arg-2 pos-args))
	   
	  (loop for arg-index from 0 below farg-len
	      until (or (not pos-arg-2)
			(= arg-index farg-len))
	      do (setf (aref argvalues-v arg-index) (car pos-arg-2)
		       pos-arg-2 (cdr pos-arg-2))
	      finally (setf final-pos-arg pos-arg-2))
	   
	  (assert (not (eq final-pos-arg 'foo)))
	   
	  (if f*
	      (push (cons f* (make-tuple-from-list pos-arg-2)) result)
	    (if pos-arg-2
		(py-raise 'TypeError
			  "~A: Too many positional args supplied (pos-args: ~A; declared: ~A)"
			  fname pos-args fpos))))
	 
	;; Assign kw args
	(let ((for-f** ()))
	  (dolist (kw key-args)
	    (let* ((key (car kw))
		   (val (cdr kw))
		   (found (loop for k from 0 below farg-len
			      when (eq (aref argnames-v k) key)
			      do (let ((oldval (aref argvalues-v k)))
				   (unless
				       (or (eq oldval pos-arg-marker)
					   (and (consp oldval)
						(eq (car oldval) opt-arg-marker)))
				     (error "~A: Got multiple values for parameter ~A (~A, ~A)"
					    fname key oldval val)))
				 (setf (aref argvalues-v k) val)
				 (return t)
			      finally (return nil))))
	       
	      (unless found
		(if f**
		    (push kw for-f**)
		  (error "~A: Got unexpected keyword argument: ~A" fname key)))))
	 
	  (when f**
	    (push (cons f** (make-dict for-f**)) result)))
		     
	;; Collect all formal arguments except f*, f**
	(loop for i from 0 below farg-len
	    do (let ((val (aref argvalues-v i)))
		 (cond
		  ((eq val pos-arg-marker)
		   (error "~A: Got no value for argument: ~A" fname (aref argnames-v i)))
		  ((and (consp val) (eq (car val) opt-arg-marker))
		   (setf val (cdr val))))
		 (push (cons (aref argnames-v i) val) result)))
	
	;; NREVERSE is only to easy debugging
	;; (format t "call rewriter: result: ~A~%" (reverse result))
	(nreverse result)))))
