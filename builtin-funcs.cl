;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Built-in functions
;;; 
;;; The built-in functions below are in the same order as listed at
;;; http://www.python.org/doc/current/lib/built-in-funcs.html#built-in-funcs
;;; 
;;; As these functions may return a Lisp value (like T, NIL), the
;;; result has to be given to py-eval before returning it to a user
;;; program.

(in-package :python)

(defun pyb:__import__ (name &optional globals locals fromlist)
  "This function is invoked by the import statement."
  (declare (ignore name globals locals fromlist))
  (error "__import__: todo (import functionality hardcoded in py-eval for now)"))

(defun pyb:abs (x)
  "Return the absolute value of object X. ~@
   Raises AttributeError when there is no `__abs__' method."
  (multiple-value-bind (val meth-found)
      (call-attribute-via-class x '__abs__)
    (if meth-found
	val
      (py-raise 'TypeError "Bad operand type for abs(): ~S" x))))

(defun pyb:apply (function &optional pos-args kw-dict)
  "Apply FUNCTION (a callable object) to given args. ~@
   POS-ARGS is any iterable object; KW-DICT must be of type PY-DICT." 
  
  (warn "Function 'apply' is deprecated; use extended call ~@
         syntax instead:  f(*args, **kwargs)")
  
  (eval-call function `( ,@(when pos-args `((* ,pos-args)))
			   ,@(when kw-dict) `((** ,kw-dict)))))


(defun pyb:callable (x)
  "Returns whether x can be called (function, class, or callable class instance)
   as True or False."
  (if (pyb::callable-1 x) *True* *False*))

(defgeneric pyb::callable-1 (x)  
  (:documentation "Returns callable-ness as T or NIL"))

(defmethod pyb::callable-1 ((x function)) t)

(defmethod pyb::callable-1 ((x python-type))
  ;; Classes are considered callable, even though some
  ;; (NoneType) disallow creating instances and raise an
  ;; exception when called.
  t)

(defmethod pyb::callable-1 ((x python-function)) t)
(defmethod pyb::callable-1 ((x py-method)) t)

(defmethod pyb::callable-1 ((x udc-instance))
  (or (internal-get-attribute x '__call__)
      ;; might inherit from callable built-in class XXX ?!
      (progn (break "call next method? pyb::callable-1" x)
	     (when (next-method-p)
	       (call-next-method)))))

(defmethod pyb::callable-1 ((x builtin-instance))
  (break "actually used? (pyb::callable-1 builtin-instance)")
  (assert (not (some (lambda (meth) 
		       (typep x (car (mop:method-specializers meth))))
		     (mop:generic-function-methods #'py-call)))
      () "PYB::CALLABLE-1 should specialize on ~A, defining it as callable!" x)
  nil)

(defmethod pyb::callable-1 (x)
  (declare (ignore x))
  nil)



(defun pyb:chr (i)
  "Return a string of one character whose ASCII code is the integer i. ~@
   This is the inverse of pyb:ord."
  (multiple-value-bind (int-des-p lisp-int)
      (py-int-designator-p i)
    (if (and int-des-p
	     (<= 0 lisp-int 255))
	(make-py-string (code-char lisp-int))
      (py-raise 'TypeError
		 "Function chr() should be given an integer in range 0..255 ~
                 (got: ~A)" lisp-int))))

(defun pyb:cmp (x y)
  "Compare two objects. ~@
   Returns one of (-1, 0, 1): -1 iff x < y; 0 iff x == y; 1 iff x > y"
  
  ;; This function is used in comparisons like <, <=, == (see
  ;; *math-cmp-mapping* in mathops.cl)
   
  ;; The CPython logic is a bit complicated; hopefully the following
  ;; is a correct translation.
  
  #+(or)(when (eq x y)
	  (return-from pyb:cmp 0))
  
  (macrolet ((normalize (x)
	       `(cond ((< ,x 0) -1) ;; object.c - adjust_tp_compare(c)
		      ((= ,x 0) 0)
		      ((> ,x 0) 1)
		      (t (py-raise 'TypeError
				   "Result of comparison should be int (got: ~A)"
				   ,x)))))

    ;; CPython: object.c - do_cmp(v,w)

    (let ((x-class (__class__ x))
	  (y-class (__class__ y))) ;; XXX or just class-of ?
      

      ;; If X, Y are instances of the same class, and that class defines
      ;; __cmp__, use that.
      
      (when (eq x-class y-class)

	(handler-case (__cmp__ x y)
	  ((or %magic-method-missing% %not-implemented-result%) ()) ;; continue
	  (:no-error (res) (return-from pyb:cmp (normalize res)))))

      ;; The "rich comparison" operations __lt__, __eq__, __gt__ are
      ;; called before __cmp__ is called.
      ;; 
      ;; Normally, we take these methods of X.  However, if class(Y)
      ;; is a subclass of class(X), the first look at Y's magic
      ;; methods.  This allows the subclass to override its parent's
      ;; comparison operations.
      ;; 
      ;; It is assumed that the subclass overrides all of
      ;; __{eq,lt,gt}__. For example, if sub.__eq__ is not defined,
      ;; first super.__eq__ is called, and after that __sub__.lt (or
      ;; super.__lt__)
      ;; 
      ;; object.c - try_rich_compare_bool(v,w,op) / try_rich_compare(v,w,op)
      (let ((y-sub-of-x (and (not (eq (__class__ x) (__class__ y)))
			     (subtypep (__class__ y) (__class__ x)))))
	
	;; Try each `meth'; if the outcome it True, return `res-value'.
	(loop for (meth . res-value) in `((,#'__eq__ .  0)
					  (,#'__lt__ . -1)
					  (,#'__gt__ .  1))
	    do (handler-case (if y-sub-of-x
				 (funcall meth y x)
			       (funcall meth x y))
		 ((or %magic-method-missing% %not-implemented-result%) ()) ;; cont.
		 (:no-error (meth-result)
		   (when (py-val->lisp-bool meth-result)
		     (return-from pyb:cmp
		       (if y-sub-of-x (- res-value) res-value)))))))
      
      ;; So the rich comparison operations didn't lead to a result.
      ;; 
      ;; object.c - try_3way_compare(v,w)
      ;; 
      ;; Now, first try X.__cmp__ (even it y-class is a subclass of
      ;; x-class) and Y.__cmp__ after that.
      
      (handler-case (__cmp__ x y)
	((or %magic-method-missing% %not-implemented-result%) ()) ;; cont.
	(:no-error (res)
	  (return-from pyb:cmp (normalize res))))
      
      (handler-case (__cmp__ y x)
	((or %magic-method-missing% %not-implemented-result%) ()) ;; cont.
	(:no-error (res)
	  (return-from pyb:cmp (normalize res))))
      
      ;; CPython now does some number coercion attempts that we don't
      ;; have to do, I suppose. (Numbers are instances in this
      ;; implementation, while they are not in CPython (?)).
      
      ;; object.c - default_3way_compare(v,w)
      ;; 
      ;; Two instances of same class without any comparison operator,
      ;; are compared by pointer value.
      
      ;; XXX (pointer< x y)
      
      ;; None is smaller than everything (excluding itself, but that
      ;; is catched above already, when testing for same class).
      
      (cond ((eq x *None*) (return-from pyb:cmp -1))
	    ((eq y *None*) (return-from pyb:cmp 1)))
      
      ;; Instances of different class are compared by class name, but
      ;; numbers are always smaller.
      ;; 
      ;; XXX In Python, there can be two different classes with the
      ;; same name, while in CL there can't. See comment in
      ;; MAKE-PYTHON-CLASS in classes.cl.

      (warn "CMP can't find any proper comparison function and has to compare ~
             class names now (CMP ~A ~A)" x y)
      
      (when (eq x-class y-class)
	(return-from pyb:cmp -1)) ;; XXX or 1, or pointer comparison, whatever
      
      (let ((x-class-name (if (typep x 'py-number-designator) "" (class-name x-class)))
	    (y-class-name (if (typep y 'py-number-designator) "" (class-name y-class))))

	(when (and (string= x-class-name y-class-name)
		   (not (string= x-class-name "")))
	  (error "There are multiple classes with name ~S: ~
                  sorry, no support for that yet (PY-CMP)"
		 x-class-name))
	
	;;(warn "x-class-name: ~S, y-class-name: ~S~%" x-class-name y-class-name)
      	(return-from pyb:cmp 
	  (if (string< x-class-name y-class-name) -1 1)))
      
      ;; Finally, we have either two instances of different non-number
      ;; classes, or two instances that are of incomparable numeric
      ;; types. Last decision criterium is based on the pointer values.
      
      ;; XXX  (if (< (pointer-value X) (pointer-value Y)) -1 1)
      (warn "hard case for cmp() - would be resolved by pointer values in CPython")
      -1)))

(defun pyb:coerce (x y)
  (declare (ignore x y))
  (error "Function 'coerce' is deprecated, and not implemented"))

;; XXX todo: compile functions! :-)
(defun pyb:compile (string filename kind &optional flags dont-inherit)
  "Compile string into code object."
  (declare (ignore string filename kind flags dont-inherit))
  (error "todo: py-compile"))

(defun pyb:delattr (x name)
  (check-type x python-object)
  (check-type name attribute-name-designator)
  (error "todo: delattr"))

(defun pyb:dir (&optional x)
  "Without args, returns names in current scope. ~@
   With arg X, return list of valid attributes of X. ~@
   Result is sorted alphabetically, and may be incomplete."
  (declare (ignore x))
  (error "todo: dir"))

(defun pyb:divmod (x y)
  "Return (x/y, x%y) as tuple"
  ;; CPython doesn't try `__div__' and `__mod__' as fallback, so
  ;; neither do we.
  (__divmod__ x y))

(defun pyb:eval (s &optional globals locals)
  (declare (ignore s globals locals))
  ;; ( [user-]py-eval ...)
  (error "todo: eval-string"))

(defun pyb:execfile (filename &optional globals locals)
  "Executes Python file FILENAME in a scope with LOCALS (defaulting ~@
   to GLOBALS) and GLOBALS (defaulting to scope in which `execfile' ~@
   is called) as local and global variables. Returns None."
  (declare (ignore filename globals locals))
  (error "todo: execfile"))

(defun pyb:filter (func list)
  "Construct a list from those elements of LIST for which FUNC is true.
   LIST: a sequence, iterable object, iterator
         If list is a string or a tuple, the result also has that type,
         otherwise it is always a list.
   FUNC: if None, identity function is assumed"
  (when (eq func *None*)
    (setf func #'identity))
  (make-py-list-from-list (loop for x in list
			   when (py-val->lisp-bool (py-call func x))
			   collect x)))

(defun pyb:getattr (x attr &optional (default nil default-p))
  "Return the value of attribute NAME of X. ~@
   If attribute doesn't exist, returns supplied DEFAULT or raises AttributeError."
  (check-type x python-object-designator)
  (check-type attr attribute-name-designator)
  (when default-p
    (check-type default python-object-designator))
  
  ;; Lookup attribute, or raise AttributeError (other exceptions
  ;; raised while looking up are not catched)
  (multiple-value-bind (val found)
      (internal-get-attribute x attr)
    (cond (found val)
	  (default-p default)
	  (t (py-raise 'AttributeError
		       "Object ~A has no attribute ~A" x attr)))))

(defun pyb:globals ()
  "Return a dictionary (namespace) representing the current global symbol table. ~@
   This is the namespace of the current module."
  (error "todo: globals"))

(defun pyb:hasattr (x name)
  "Returns True is X has attribute NAME, False if not. ~@
   (Uses `getattr'; catches _all_ exceptions.)"
  (check-type x python-object-designator)
  (ensure-py-type name string "hasattr: attribute name must be string (got: ~A)")
  
  (handler-case
      (progn (pyb:getattr x name)
	     *True*)
    
    ;; XXX maybe need to catch more than Exception here (like
    ;; %magic-method-missing%) ?
    (Exception () *False*)
    (condition (c)
      (warn "pyb:hasattr catched condition ~A, not sure it should catch it?" c)
      *False*)))

(defun pyb:hash (x)
  ;; XX todo: once calculated, store hash in object
  (__hash__ x))

(defun pyb:hex (x)
  (__hex__ x))


;; As objects may be moved in memory, while the `id' value must remain
;; the same, a hash-table is used to keep track of pseudo object
;; id's. As this hash-table must have weak keys, this is
;; non-portable.

#+allegro 
(let ((ht (make-hash-table :test #'eq :weak-keys t))
      (counter 0))
  (defun pyb:id (x)
    (or (gethash x ht)
	(setf (gethash x ht) (incf counter)))))

#-allegro
(defun pyb:id (x)
  (error "ID not implemented"))


(defun pyb:input (&rest args)
  (declare (ignore args))
  (error "todo: py-input"))

(defun pyb:intern (x)
  (declare (ignore x))
  (error "Function 'intern' is deprecated, and not implemented"))


(defun pyb::isinstance-1 (x cls)
  ;; CLS is either a class or a _tuple_ of classes (only tuple is
  ;; allowed, not other iterables).
  (if (typep cls 'py-tuple)
      (py-iterate (c cls)
		  (when (typep x c)
		    (return-from pyb::isinstance-1 t)))
    (typep x cls)))

(defun pyb:isinstance (x cls)
  (lisp-val->py-bool (pyb::isinstance-1 x cls)))


(defun pyb::issubclass-1 (x cls)
  (if (typep cls 'py-tuple)
      
      (py-iterate (c cls)
		  (when (subtypep x c)
		    (return-from pyb::issubclass-1 t)))
    
    (subtypep x cls)))

(defun pyb:issubclass (x cls)
  ;; SUPER is either a class, or a tuple of classes -- denoting
  ;; Lisp-type (OR c1 c2 ..).
  (lisp-val->py-bool (pyb::issubclass-1 x cls)))


(defun pyb:iter (x &optional y)
  ;; Return iterator for sequence x
  ;; 
  ;; When Y supplied: make generator that calls and returns X() until
  ;; it's equal to Y.
  
  (if (not y)
      ;; return iterator for X
      (handler-case (__iter__ x)
	(AttributeError () #1=(py-raise 'TypeError 
					"Iteration over non-sequence (got: ~A)" x))
	(:no-error (iterator)
	  iterator))
    
    (if (eq (pyb:callable x) *True*)
	(make-iterator-from-function (lambda () (py-call x))
				     y)
      #1#)))
	    
(defun pyb:len (x)
  (__len__ x))

(defun pyb:locals ()
  ;; return local variables
  (error "todo: locals()"))

(defun pyb:map (func &rest sequences)
  (apply #'mapcar func sequences))

(defun pyb:ord (s)
  (multiple-value-bind (string-des-p lisp-str)
      (py-string-designator-p s)
    (if (and string-des-p
	     (= (length lisp-str) 1))
	(char-code (char lisp-str 0))	
      (py-raise 'TypeError
		"Function ord() should be given a string with ~
                 length 1 as argument (got: ~A)" lisp-str))))

(defun pyb:pow (x y &optional (z nil z-p))
  ;; If third argument Z is supplied, __rpow__ will not be tried.
  (macrolet ((err (&rest args)
	       `(py-raise 'TypeError
			  "Unsupported operands for pow(): ~A"
			  ',args)))
    (if z-p
	
	(handler-case (__pow__ x y z)
	  ((or %not-implemented-result% %magic-method-missing%) ()
	    (err x y))
	  (:no-error (res)
	    (return-from pyb:pow res)))
      
      (progn (handler-case (__pow__ x y)
	       ((or %not-implemented-result% %magic-method-missing%) ()) ;; cont.
	       (:no-error (res)
		 (return-from pyb:pow res)))
	     
	     (handler-case (__rpow__ y x)
	       ((or %not-implemented-result% %magic-method-missing%) ()
		 (err x y))
	       (:no-error (res)
		 (return-from pyb:pow res)))))))

(defun pyb:range (x &optional y z)
  "range( [start,] stop [,step] ) -> (start, start+1, .., stop-1)"
  ;; In fact, X is optional, while Y is required...
  (flet ((range-2 (start stop step)
	   (ensure-py-type (start stop step) integer
			   "arguments to range() must be int (got: ~A)")
	   (make-py-list-from-list
	    (cond
	     
	     ((and (< start stop) (< 0 step))
	      (loop for i from start below stop by step
		  collect i))
	     
	     ((and (> start stop) (> 0 step))
	      (loop for i from start above stop by (- step)
		  collect i))
	     
	     (t ())))))
    
    (cond (z (range-2 x y z))
	  (y (range-2 x y 1))
	  (t (range-2 0 x 1)))))


(defun pyb:raw_input (&optional prompt)
  "Pops up a GUI entry window to type text; returns entered string"
  (declare (ignore prompt))
  (error "todo: raw_input")) ;; XXX hmm no "prompt" CL function?

(defun pyb:reduce (func seq &optional initial)
  (let (res)
    (if initial
	(progn
	  (setf res initial)
	  (py-iterate (x seq)
		      (setf res (py-call func res x)))
	  res)
      (let ((first t))
	(py-iterate (x seq)
		    (if first
			(setf res x
			      first nil)
		      (setf res (py-call func res x))))))))

(defun pyb:reload (module)
  (declare (ignore module))
  (error "todo: reload"))

(defun pyb:repr (x)
  (__repr__ x))

(defun pyb:round (x &optional (ndigits 0))
  "Round number X to a precision with NDIGITS decimal digits (default: 0).
   Returns float. Precision may be negative"
  
  (multiple-value-bind (x-des x2)
      (py-number-designator-p x)
    
    (multiple-value-bind (nd-des ndigits2)
	(py-number-designator-p ndigits)
      
      (if (and x-des nd-des)
	  (setf x x2
		ndigits ndigits2)
	(py-raise 'TypeError
		  "Function round() must be given one or two numbers as ~
                   arguments (got: ~A ~A)" x ndigits))))
  
  ;; implementation taken from: bltinmodule.c - builtin_round()
  ;; idea: round(12.3456, 2) ->
  ;;       12.3456 * 10**2 = 1234.56  ->  1235  ->  1235 / 10**2 = 12.35
  
  (let ((f (expt 10 (abs ndigits))))
    (setf x (if (< ndigits 0)
		(/ x f)
	      (* x f )))
    (setf x (if (>= x 0)
		(floor (+ x 0.5))
	      (ceiling (- x 0.5))))
    (setf x (if (< ndigits 0)
		(* x f)
	      (/ x f)))
    
    ;; By only coercing here at the end, the result could be more
    ;; exact than what CPython gives.
    (coerce x 'double-float)))


(defun pyb:setattr (x attr val)
  (internal-set-attribute x attr val))

(defun pyb:sorted (x)
  ;;; over sequences, or over all iterable things?
  (declare (ignore x))
  (error "todo: sorted")
  )

(defun pyb:sum (seq &optional (start 0))
  (ensure-py-type start number
		  "Sum() requires number value as START argument (got: ~A)")
  (let ((res start))
    (py-iterate (x seq)
		(ensure-py-type x number
				"Sum() only takes numbers (got: ~A)")
		(incf res x))
    res))

;; `type' is also the name of the builtin class `python-type'
(defun pyb:type (x &optional bases dict)
  (if (or bases dict)
      (error "type(...) to create a new type: not implemented yet (got: ~A ~A ~A)" x bases dict)
    (__class__ x)))

(defun pyb:unichr (i)
  ;; -> unicode char i
  (declare (ignore i))
  (error "todo: unichr"))

(defun pyb:vars (&optional x)
  "If X supplied, return it's dict, otherwise return local variables."
  (if x
      (multiple-value-bind (val found)
	  (internal-get-attribute x '__dict__)
	(if found
	    val
	  (py-raise 'AttributeError
		    "Instances of class ~A have no attribute '__dict__' (got: ~A)"
		    (class-of x) x)))
    (pyb:locals)))

(defun pyb:zip (&rest sequences)
  "Return a list with tuples, where tuple i contains the i-th argument of ~
   each of the sequences. The returned list has length equal to the shortest ~
   sequence argument."
  ;;XXX CPython looks up __len__, __iter__, __getitem__ attributes here
  ;; need to make an iterator for each sequence first, then call the iterators
  (declare (ignore sequences))
  (error "todo: zip")
  #+(or)(make-py-list (apply #'mapcar
			  (lambda (&rest args) (apply #'make-tuple args))
			  sequences)))
    