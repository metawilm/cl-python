(in-package :python)

;;; Built-in functions

;; The built-in functions below are in the same order as listed at
;; http://www.python.org/doc/current/lib/built-in-funcs.html#built-in-funcs
;; 
;; These function will always return a Python value.


(defun pybf:__import__ (name &optional globals locals fromlist)
  "This function is invoked by the import statement."
  (declare (ignore name globals locals fromlist))
  (error "__import__: todo"))

(defun pybf:abs (x)
  "Return the absolute value of object X.
Raises AttributeError when there is no `__abs__' method."
  (py-abs x))

(defun pybf:apply (function &optional pos-args kw-dict)
  "Apply FUNCTION (a callable object) to given args.
POS-ARGS is any iterable object; KW-DICT must be of type PY-DICT." 
  
  (warn "Function 'apply' is deprecated; use extended call ~@
         syntax instead:  f(*args, **kwargs)")
  
  (let* ((pos-args (py-iterate->lisp-list (deproxy pos-args)))
	 (kw-dict  (deproxy kw-dict))
	 (kw-list  (progn (unless (typep kw-dict 'hash-table)
			    (py-raise 'TypeError
				      "APPLY: third arg must be dict (got: ~A)" kw-dict))
			  (loop for key being the hash-key in kw-dict
			      using (hash-value val)
			      for key-sym = (etypecase key
					      (string (intern key :keyword))
					      (symbol key))
			      collect key-sym
			      collect val))))
    (apply #'py-call function (nconc pos-args kw-list))))


(defun pybf:callable (x)
  "Returns whether x can be called (function, class, or callable class instance)
   as True or False."
  (py-bool (recursive-class-dict-lookup (py-class-of x) '__call__)))

#+(or)
(defgeneric pybf::callable-1 (x)  
  (:documentation "Returns callable-ness as T or NIL")
  (:method ((x function))  t)
  (:method ((x py-type))   
	   ;; Classes are considered callable, even though some
	   ;; (NoneType) disallow creating instances and raise an
	   ;; exception when called.
	   t)
  (:method ((x py-meta-type)) t)
  (:method ((x py-function))  t)
  (:method ((x py-method))    t)
  (:method ((x py-user-object)) (recursive-class-dict-lookup (py-class-of x) '__call__))
  (:method ((x t))
	   (warn "pybf:callable-1: got ~A, assuming nil" x)
	   nil))


(defun pybf:chr (x)
  "Return a string of one character whose ASCII code is the integer i. ~@
   This is the inverse of pybf:ord."
  (let ((i (deproxy x)))
    (unless (typep i '(integer 0 255))
      (py-raise 'TypeError
		"Built-in function chr() should be given an integer in ~
                 range 0..255 (got: ~A)" x))
    (string (code-char i))))


;; compare numbers 
(defgeneric pybf:cmp (x y)
  (:documentation
   "Compare two objects, of which at least one is a user-defined-object.
Returns one of (-1, 0, 1): -1 iff x < y; 0 iff x == y; 1 iff x > y")
  (:method ((x t) (y t))
   
	   ;; This function is used in comparisons like <, <=, ==.
	   ;; 
	   ;; The CPython logic is a bit complicated; hopefully the following
	   ;; is a correct translation.
	   ;; 
	   ;; Note: (eq X Y) does not guarantee "X == Y".

	   (flet ((normalize (x)  ;; object.c - adjust_tp_compare(c)
		    (let ((i (deproxy x)))
		      (cond ((< i 0) -1)
			    ((= i 0) 0)
			    ((> i 0) 1)))))

	     ;; CPython: object.c - do_cmp(v,w)
	     (let ((x.class (py-class-of x))
		   (y.class (py-class-of y)))
      
	       (when (member (find-class 'py-type) (list x.class y.class))
		 (return-from pybf::cmp
		   (if (eq x y)
		       0
		     -1)))

	       ;; If X, Y are instances of the same class, it must be a
	       ;; user-defined class, otherwise we wouldn't be in this
	       ;; method.
      
	       ;; If the class is equal and it defines __cmp__, use that.
      
	       (when (eq x.class y.class)
		 (let* ((__cmp__ (recursive-class-dict-lookup x.class '__cmp__))
			(cmp-res (when __cmp__ (py-call __cmp__ x y))))
		   (when (and cmp-res
			      (not (eq cmp-res *the-notimplemented*)))
		     (return-from pybf::cmp (normalize cmp-res)))))

	       ;; The "rich comparison" operations __lt__, __eq__, __gt__ are
	       ;; now called before __cmp__ is called.
	       ;; 
	       ;; Normally, we take these methods of X.  However, if class(Y)
	       ;; is a subclass of class(X), the first look at Y's magic
	       ;; methods.  This allows the subclass to override its parent's
	       ;; comparison operations.
	       ;; 
	       ;; It is assumed that the subclass overrides all of
	       ;; __{eq,lt,gt}__. For example, if sub.__eq__ is not defined,
	       ;; first super.__eq__ is called, and after that __sub__.__lt__
	       ;; (or super.__lt__).
	       ;; 
	       ;; object.c - try_rich_compare_bool(v,w,op) / try_rich_compare(v,w,op)
      
	       (let ((y-sub-of-x (and (not (eq x.class y.class))
				      (subtypep y.class x.class))))
	
		 ;; Try each `meth'; if the outcome it True, return `res-value'.
		 (loop for (meth-name res-value) in `((__eq__   0)
						      (__lt__  -1)
						      (__gt__   1))
		     do (let* ((meth (recursive-class-dict-lookup 
				      (if y-sub-of-x y.class x.class)
				      meth-name))
			       (res (when meth
				      (py-call meth
					       (if y-sub-of-x y x)
					       (if y-sub-of-x x y)))))
		 
			  (when (and res (not (eq res *the-notimplemented*)))
			    (let ((true? (py-val->lisp-bool res)))
			      (when true?
				(return-from pybf::cmp
				  (if y-sub-of-x (- res-value) res-value))))))))
      
	       ;; So the rich comparison operations didn't lead to a result.
	       ;; 
	       ;; object.c - try_3way_compare(v,w)
	       ;; 
	       ;; Now, first try X.__cmp__ (even it y.class is a subclass of
	       ;; x.class) and Y.__cmp__ after that.

	       (let* ((meth (recursive-class-dict-lookup x.class '__cmp__))
		      (res (when meth
			     (py-call meth x y))))
		 (when (and res (not (eq res *the-notimplemented*)))
		   (let ((norm-res (normalize res)))
		     (return-from pybf::cmp norm-res))))

	       (let* ((meth (recursive-class-dict-lookup y.class '__cmp__))
		      (res (when meth
			     (py-call meth y x))))
		 (when (and res (not (eq res *the-notimplemented*)))
		   (let ((norm-res (- (normalize res))))
		     (return-from pybf::cmp norm-res))))
      
	       ;; CPython now does some number coercion attempts that we don't
	       ;; have to do because we have first-class numbers. (I think.)
      
	       ;; object.c - default_3way_compare(v,w)
	       ;; 
	       ;; Two instances of same class without any comparison operator,
	       ;; are compared by pointer value. Our function `pybf:id' fakes
	       ;; that.
      
	       (when (eq x.class y.class)
		 (let ((x.id (pybf:id x))
		       (y.id (pybf:id y)))
		   (return-from pybf::cmp 
		     (cond ((< x.id y.id) -1)
			   ((= x.id y.id) 0)
			   (t             1)))))
      
	       ;; None is smaller than everything (excluding itself, but that
	       ;; is catched above already, when testing for same class;
	       ;; NoneType is not subclassable).
      
	       (cond ((eq x *the-none*) (return-from pybf::cmp -1))
		     ((eq y *the-none*) (return-from pybf::cmp  1)))
      
	       ;; Instances of different class are compared by class name, but
	       ;; numbers are always smaller.
      
	       ;; Probably, when we arrive here, there is a bug in the logic
	       ;; above. Therefore print a warning.
      
	       (warn "[debug] CMP can't properly compare ~A and ~A." x y)
      
	       (return-from pybf::cmp
		 (if (string< (class-name x.class) (class-name y.class))
		     -1
		   1))
      
	       ;; Finally, we have either two instances of different non-number
	       ;; classes, or two instances that are of incomparable numeric
	       ;; types.
	       (return-from pybf::cmp
		 (cond ((eq x y)                   0)
		       ((< (pybf:id x) (pybf:id y)) -1)
		       (t                          1)))))))


(defun pybf:coerce (x y)
  (declare (ignore x y))
  (error "Function 'coerce' is deprecated, and not implemented"))

(defun pybf:compile (string filename kind &optional flags dont-inherit)
  "Compile string into code object."
  (declare (ignore string filename kind flags dont-inherit))
  (error "todo: py-compile"))

(defun pybf:delattr (x name)
  (check-type x python-object)
  (check-type name attribute-name-designator)
  (error "todo: delattr"))

(defun pybf:dir (&optional x)
  "Without args, returns names in current scope. ~@
   With arg X, return list of valid attributes of X. ~@
   Result is sorted alphabetically, and may be incomplete."
  (declare (ignore x))
  (error "todo: dir"))

(defun pybf:divmod (x y)
  "Return (x/y, x%y) as tuple"
  (py-divmod x y))

(defun pybf:eval (s &optional globals locals)
  (declare (ignore s globals locals))
  ;; ( [user-]py-eval ...)
  (error "todo: eval-string"))

(defun pybf:execfile (filename &optional globals locals)
  "Executes Python file FILENAME in a scope with LOCALS (defaulting ~@
   to GLOBALS) and GLOBALS (defaulting to scope in which `execfile' ~@
   is called) as local and global variables. Returns None."
  (declare (ignore filename globals locals))
  (error "todo: execfile"))

(defun pybf:filter (func iterable)
  "Construct a list from those elements of LIST for which FUNC is true.
   LIST: a sequence, iterable object, iterator
         If list is a string or a tuple, the result also has that type,
         otherwise it is always a list.
   FUNC: if None, identity function is assumed"
  (when (eq func *the-none*)
    (setf func #'identity))
  (make-py-list-from-list (loop for x in (py-iterate->lisp-list iterable)
			   when (py-val->lisp-bool (py-call func (list x)))
			   collect x)))

(defun pybf:getattr (x attr &optional default)
  "Return the value of attribute NAME of X. ~@
   If attribute doesn't exist, returns supplied DEFAULT or raises AttributeError."
  (declare (ignore x attr default))
  (error "todo"))

(defun pybf:globals ()
  "Return a dictionary (namespace) representing the current global symbol table. ~@
   This is the namespace of the current module."
  (error "todo: globals"))

(defun pybf:hasattr (x name)
  "Returns True is X has attribute NAME, False if not. ~@
   (Uses `getattr'; catches _all_ exceptions.)"
  (declare (ignore x name))
  (error "todo"))

(defun pybf:hash (x)
  ;; XX todo: once calculated, store hash in object
  (py-hash x))

(defun pybf:hex (x)
  (py-hex x))

(defun pybf:id (x)
  
  ;; In contrast to CPython, in Allegro the `id' (memory location) of
  ;; Python objects can change during their lifetime.
  #+allegro
  (excl:lispval-to-address x)
  
  #-allegro
  (error "TODO: id() not implemented for this Lisp implementation"))


(defun pybf:input (&rest args)
  (declare (ignore args))
  (error "todo: py-input"))

(defun pybf:intern (x)
  (declare (ignore x))
  (error "Function 'intern' is deprecated; it is not implemented."))

(defun pybf:isinstance (x cls)
  (declare (ignore x cls))
  (py-bool (error "todo")))

(defmethod pybf::isinstance-1 (x cls)
  ;; CLS is either a class or a _tuple_ of classes (only tuple is
  ;; allowed, not other iterables).
  (if (typep cls 'py-tuple)
      (dolist (c (py-iterate->lisp-list cls)
		(when (subtypep (py-class-of x) c)
		  (return-from pybf::isinstance-1 t))))
    (subtypep (py-class-of x) cls)))


(defun pybf:issubclass (x cls)
  ;; SUPER is either a class, or a tuple of classes -- denoting
  ;; Lisp-type (OR c1 c2 ..).
  (py-bool (pybf::issubclass-1 x cls)))

(defun pybf::issubclass-1 (x cls)
  (if (typep cls 'py-tuple)
      (dolist (c (py-iterate->lisp-list cls))
	(when (subtypep x c)
	  (return-from pybf::issubclass-1 t)))
    (subtypep x cls)))


(defun pybf:iter (x &optional y)
  ;; Return iterator for iterable X
  ;; 
  ;; When Y supplied: make generator that calls and returns iterator
  ;; of X until the value returned is equal to Y.
  
   (if y
       
       (make-iterator-from-function
	(let ((iterf (get-py-iterate-fun x)))
	  (lambda () 
	    (let ((val (funcall iterf)))
	      (if (py-== val y)
		  nil
		val)))))
     
     (make-iterator-from-function
      (get-py-iterate-fun x))))

(defun pybf:len (x)
  (py-len x))

(defun pybf:locals ()
  ;; return local variables
  (error "todo: locals()"))

(defun pybf:map (func &rest sequences)
  
  ;; Apply FUNC to every item of sequence, returning real list of
  ;; values. With multiple sequences, traversal is in parallel and
  ;; FUNC must take multiple args. Shorter sequences are extended with
  ;; None. If function is None, use identity function (multiple
  ;; sequences -> list of tuples).
  
  (cond ((and (eq func *the-none*) (null (cdr sequences)))  ;; identity of one sequence
	 (make-py-list-from-list (py-iterate->lisp-list (car sequences))))
	
	((null (cdr sequences)) ;; func takes 1 arg
	 
	 ;; Apply func to each val yielded before yielding next val
	 ;; might be more space-efficient for large sequences when
	 ;; function "reduces" data.

	 (make-py-list-from-list
	  (mapcar (lambda (val)
		    (py-call func (list val)))
		  (py-iterate->lisp-list (car sequences)))))
	
	(t
	 (let* ((vectors (mapcar (lambda (seq)
				   (apply #'vector (py-iterate->lisp-list seq)))
				 sequences)))
	   
	   (let ((num-active (loop for v in vectors
				 when (> (length v) 0)
				 count 1)))
	     
	     (make-py-list-from-list 
	      (loop while (> num-active 0)
		  for i from 0
		  collect (let ((curr-items 
				 (mapcar (lambda (vec)
					   (let ((vec-length (1- (length vec))))
					     (cond ((> vec-length i)
						    (aref vec i))
					      
						   ((< vec-length i)
						    *the-none*)
					      
						   ((= vec-length i) ;; last of this vec
						    (decf num-active)
						    (aref vec i)))))
					 vectors)))
			    (if (eq func *the-none*)
				(make-tuple-from-list curr-items)
			      (py-call func curr-items))))))))))

(defun pybf:max (item &rest items)
  (let ((res nil))
    (if (null items)
	(map-over-py-object (lambda (k) (when (or (null res) (py-> k res))
					  (setf res k)))
			    item)
      (progn (setf res item)
	     (dolist (k items)
	       (when (or (py-> k res))
		 (setf res k)))))
    res))

(defun pybf:min (item &rest items)
  (let ((res nil))
    (if (null items)
	(map-over-py-object (lambda (k) (when (or (null res) (py-< k res))
					  (setf res k)))
			    item)
      (progn (setf res item)
	     (dolist (k items)
	       (when (or (py-< k res))
		 (setf res k)))))
    res))

(defun pybf:oct (n)
  (py-oct n))

(defun pybf:ord (s)
  (let ((s2 (deproxy s)))
    (if (and (stringp s2)
	     (= (length s2) 1))
	(char-code (char s2 0))
      (error "wrong arg ORD: ~A" s))))

(defun pybf:pow (x y &optional z)
  (py-** x y z))

(defun pybf:range (x &optional y z)
  (declare (ignore x y z))
  (error "todo: range"))

(defun pybf:raw_input (&optional prompt)
  "Pops up a GUI entry window to type text; returns entered string"
  (declare (ignore prompt))
  (error "todo: raw_input")) ;; XXX hmm no "prompt" CL function?

(defun pybf:reduce (func seq &optional initial)
  (let ((res nil))
    (if initial
	
	(progn (setf res initial)
	       (map-over-py-object (lambda (x) (setf res (py-call func (list res x))))
				   seq))
      
      (map-over-py-object (lambda (x) (cond ((null res) (setf res x))
					    (t (setf res (py-call func (list res x))))))
			  seq))
    (or res
	(py-raise 'TypeError "reduce() of empty sequence with no initial value"))))

(defun pybf:reload (m)
  ;; m py-module
  #+(or)(with-slots (module namespace) m
	  (let ((mod-ast (parse-python-string (read-file module)))
		(__name__ (namespace-lookup namespace '__name__))
		(__file__(namespace-lookup namespace '__file__))
		(new-ns (make-namespace :builtins t)))
	    (namespace-bind new-ns '__name__ __name__)
	    (namespace-bind new-ns '__file__ __file__)
	    (let ((*scope* new-ns))
	      (declare (special *scope*))
	      (py-eval mod-ast))
	    (setf namespace new-ns)))
  m)


(defmethod pybf:repr (x)
  (py-repr x))

(defmethod pybf:round (x &optional (ndigits 0))
  "Round number X to a precision with NDIGITS decimal digits (default: 0).
   Returns float. Precision may be negative"
  (declare (ignore x ndigits))
  
  #+(or)
  (progn
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
      (coerce x 'double-float))))


(defun pybf:setattr (x attr val)
  ;; XXX attr symbol/string
  (setf (py-attr x attr) val))

(defun pybf:sorted (x)
  ;;; over sequences, or over all iterable things?
  (declare (ignore x))
  (error "todo: sorted"))

(defun pybf:sum (seq &optional (start 0))
  (declare (ignore seq start))
  (error "todo")
  #+(or) 
  (progn (ensure-py-type start number
			 "Sum() requires number value as START argument (got: ~A)")
	 (let ((res start))
	   (ensure-py-type res number
			   "Sum() only takes numbers (got as start: ~A)")
	   (map-over-py-object
	    (lambda (x) (ensure-py-type x number "Sum() only takes numbers (got: ~A)")  
		    (incf res x))
	    seq)
	   res)))

(defun pybf:unichr (i)
  ;; -> unicode char i
  (declare (ignore i))
  (error "todo: unichr"))

(defun pybf:vars (&optional x)
  "If X supplied, return it's dict, otherwise return local variables."
  (declare (ignore x))
  (error "todo")
  #+(or)(if x
      (multiple-value-bind (val found)
	  (internal-get-attribute x '__dict__)
	(if found
	    val
	  (py-raise 'AttributeError
		    "Instances of class ~A have no attribute '__dict__' (got: ~A)"
		    (class-of x) x)))
    (pybf:locals)))

(defun pybf:zip (&rest sequences)
  "Return a list with tuples, where tuple i contains the i-th argument of ~
   each of the sequences. The returned list has length equal to the shortest ~
   sequence argument."
  
  ;;XXX CPython looks up __len__, __iter__, __getitem__ attributes here
  ;; need to make an iterator for each sequence first, then call the iterators
  (declare (ignore sequences))
  (error "todo")
  #+(or)
  (loop with iter-vec = (make-array (length sequences)
				    :initial-contents (mapcar #'get-py-iterate-fun sequences))
      with res = (make-array 20 :adjustable t :fill-pointer 0)
      with current-tuple-values = (make-array (length sequences))
      for tuple-no from 0
      do (loop for iter-i from 0
	     for iter-func across iter-vec
	     do (let ((val (funcall iter-func)))
		  (if val
		      (setf (aref current-tuple-values iter-i) val)
		    (return-from pybf:zip (make-py-list res)))))
	 (vector-push-extend (make-tuple (copy-seq current-tuple-values)) res)))
