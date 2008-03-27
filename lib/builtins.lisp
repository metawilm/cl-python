;; -*- package: clpython; readtable: py-user-readtable -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)
(in-syntax *user-readtable*)

;;;; Built-in types, functions and values.

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun def-builtin-types ()
  (loop for (sym cls-name) in
	'(({basestring}   py-string       )
	  ({bool}         py-bool         )
	  ({classmethod}  py-class-method )
	  ({complex}      py-complex      )
	  ({dict}         py-dict         )
	  ({enumerate}    py-enumerate    )
	  ({file}         py-file         )
	  ({float}        py-float        )
	  ({int}          py-int          )
	  ({list}         py-list         )
	  ({long}         py-int          )
	  ({number}       py-number       )
	  ({object}       py-object       )
	  ({property}     py-property     )
	  ({slice}        py-slice        )
	  ({staticmethod} py-static-method)
	  ({str}          py-string       )
	  ({super}        py-super        )
	  ({tuple}        py-tuple        )
	  ({type}         py-type         )
	  ({unicode}      py-string       )
	  ({xrange}       py-xrange       ))
      do (setf (symbol-value sym) (find-class cls-name))))

(def-builtin-types)

(defun def-builtin-exceptions ()
  `(loop for sym in *exception-classes*
       do (setf (symbol-value sym) (find-class sym))))

(def-builtin-exceptions)

) ;; eval-when


;; The functions below are in the same order as
;; http://www.python.org/doc/current/lib/built-in-funcs.html#built-in-funcs
;; 
;; These function will always return a Python value (never a
;; Lisp generalized boolean etc).

(defun {__import__} (name &optional globals locals fromlist)
  "This function is invoked by the import statement."
  (declare (ignore name globals locals fromlist))
  (error "__import__: todo"))

(defun {abs} (x)
  "Return the absolute value of object X.
Raises AttributeError when there is no `__abs__' method."
  (py-abs x))

(defun {apply} (function &optional pos-args kw-dict)
  "Apply FUNCTION (a callable object) to given args.
POS-ARGS is any iterable object; KW-DICT must be of type PY-DICT." 
  
  (warn "Function 'apply' is deprecated; use extended call ~@
         syntax instead:  f(*args, **kwargs)")
  
  (let* ((pos-args (py-iterate->lisp-list (deproxy pos-args)))
	 (kw-dict  (deproxy kw-dict))
	 (kw-list  (progn (unless (typep kw-dict 'hash-table)
			    (py-raise '{TypeError}
				      "APPLY: third arg must be dict (got: ~A)" kw-dict))
			  (loop for key being the hash-key in kw-dict
			      using (hash-value val)
			      for key-sym = (etypecase key
					      (string (intern key :keyword))
					      (symbol key))
			      collect key-sym
			      collect val))))
    (apply #'py-call function (nconc pos-args kw-list))))


(defun {callable} (x)
  "Returns whether x can be called (function, class, or callable class instance)
   as True or False."
  (py-bool (recursive-class-dict-lookup (py-class-of x) '{__call__})))

(defun {chr} (x)
  "Return a string of one character whose ASCII code is the integer i. ~@
   This is the inverse of {ord}."
  (let ((i (deproxy x)))
    (unless (typep i '(integer 0 255))
      (py-raise '{TypeError}
		"Built-in function chr() should be given an integer in ~
                 range 0..255 (got: ~A)" x))
    (string (code-char i))))

(defun {cmp} (x y)
  (py-cmp x y))

(defun {coerce} (x y)
  (declare (ignore x y))
  (error "Function 'coerce' is deprecated, and not implemented"))

(defun {compile} (string filename kind &optional flags dont-inherit)
  "Compile string into code object."
  (declare (ignore string filename kind flags dont-inherit))
  (error "todo: py-compile"))

(defun {delattr} (x attr)
  (unless (stringp attr)
    (py-raise '{TypeError} "Attribute (for delattr) must be string (got: ~S)" attr))
  (let ((attr.sym (ensure-user-symbol attr)))
    (setf (py-attr x attr.sym) nil)))

(defun {dir} (&optional x)
  "Without args, returns names in current scope. ~@
   With arg X, return list of valid attributes of X. ~@
   Result is sorted alphabetically, and may be incomplete."
  (let (res)
    (flet ((add-dict (d)
	     (when d
	       (dolist (k (py-iterate->lisp-list (py-dict.keys d)))
		 (pushnew k res)))))
      ;; instance dict
      (let ((d (dict x)))
	(when d
	  (add-dict (dict x))
	  (pushnew "__dict__" res :test #'string=)))
      
      (let ((x.class (py-class-of x)))
        (loop for c in (class-precedence-list x.class)
	    until (or (eq c (ltv-find-class 'standard-class))
		      (eq c (ltv-find-class 'py-dict-mixin))
                      (eq c (ltv-find-class 'standard-generic-function)))
	    do (add-dict (dict c))))
      
      (when (typep x 'py-module)
	(loop for k in (mapcar #'car (py-module-get-items x))
	    do (pushnew (if (stringp k) k (py-val->string k)) res)))

      (when (packagep x)
	(do-external-symbols (s x)
	  (pushnew (symbol-name s) res))))
    
    (assert (every #'stringp res))
    (setf res (sort res #'string<))
    res))

(defun {divmod} (x y)
  "Return (x/y, x%y) as tuple"
  (py-divmod x y))

(defun {eval} (s &optional globals locals)
  "Returns value of expression."
  (declare (ignore s globals locals))
  (error-indirect-special-call '{eval}))

(defun {execfile} (filename &optional globals locals)
  "Executes Python file FILENAME in a scope with LOCALS (defaulting ~@
   to GLOBALS) and GLOBALS (defaulting to scope in which `execfile' ~@
   is called) as local and global variables. Returns value of None."
  (declare (ignore filename globals locals))
  
  (error "todo: execfile"))

(defun {filter} (func iterable)
  "Construct a list from those elements of LIST for which FUNC is true.
   LIST: a sequence, iterable object, iterator
         If list is a string or a tuple, the result also has that type,
         otherwise it is always a list.
   FUNC: if None, identity function is assumed"
  
  (when (eq func *the-none*)
    (setf func #'identity))
  
  (make-py-list-from-list (loop for x in (py-iterate->lisp-list iterable)
			   when (py-val->lisp-bool (py-call func x))
			   collect x)))

(defun {getattr} (x attr &optional default)
  ;; Exceptions raised during py-attr are not caught.
  ;; 
  ;; This interns the ATTR in the :clpython.user package - probably a
  ;; small price to pay for using symbols in attribute lookup everywhere
  ;; else.
  (unless (stringp attr)
    (py-raise '{TypeError} "Attribute (for getattr) must be string (got: ~S)" attr))
  (let* ((attr.sym (ensure-user-symbol attr))
	 (val (catch :getattr-block
		(handler-case
		    (py-attr x attr.sym :via-getattr t)
		  ({AttributeError} () :py-attr-not-found)))))
    
    (if (eq val :py-attr-not-found)
	(or default
	    (py-raise '{AttributeError} "Object `~A' has no attribute `~A'." x attr))
      val)))

(defun getattr-nobind (x attr &optional default)
  ;; Exceptions raised during py-attr are not catched.
  ;; Returns :class-attr <meth> <inst>
  ;;      or <value>
  (let ((attr.sym (ensure-user-symbol attr)))
    (multiple-value-bind (a b c)
	(catch :getattr-block
	  (handler-case
	      (py-attr x attr.sym :via-getattr t :bind-class-attr nil)
	    ({AttributeError} () nil)))
      (case a
	(:class-attr (values a b c))
	((nil :py-attr-not-found) (or default
				      (py-raise '{AttributeError}
						"Object `~A' has no attribute `~A'." x attr)))
	(t a)))))

(defun {globals} ()
  "Return a dictionary (namespace) representing the current global symbol table. ~@
   This is the namespace of the current module."
  (error-indirect-special-call '{globals}))

(defun {hasattr} (x name)
  "Returns True is X has attribute NAME, False if not. ~@
   (Uses `getattr'; catches _all_ exceptions.)"
  (check-type name string)
  (py-bool (ignore-errors (py-attr x (ensure-user-symbol name)))))

(defun {hash} (x)
  (py-hash x))

(defun {hex} (x)
  (py-hex x))

(defun {id} (x)
  ;; In contrast to CPython, in Allegro the `id' (memory location) of
  ;; Python objects can change during their lifetime.
  (py-id x))  

(defun {input} (&rest args)
  (declare (ignore args))
  (error "todo: py-input"))

(defun {intern} (x)
  (declare (ignore x))
  (error "Function 'intern' is deprecated; it is not implemented."))

(defun {isinstance} (x cls)
  (let ((cls (deproxy cls)))
    (if (listp cls)
	(some (lambda (c) ({isinstance} x c)) cls)
      (py-bool (or (eq cls (load-time-value (find-class 'py-object)))
		   (typep x cls)
		   (subtypep (py-class-of x) cls))))))

(defmethod isinstance-1 (x cls)
  ;; CLS is either a class or a _tuple_ of classes (only tuple is
  ;; allowed, not other iterables).
  (if (typep cls 'py-tuple)
      (dolist (c (py-iterate->lisp-list cls)
		(when (subtypep (py-class-of x) c)
		  (return-from isinstance-1 t))))
    (subtypep (py-class-of x) cls)))


(defun {issubclass} (x cls)
  ;; SUPER is either a class, or a tuple of classes -- denoting
  ;; Lisp-type (OR c1 c2 ..).
  (py-bool (issubclass-1 x cls)))

(defun issubclass-1 (x cls)
  (if (typep cls 'py-tuple)
      
      (dolist (c (py-iterate->lisp-list cls))
	(when (issubclass-1 x c)
	  (return-from issubclass-1 t)))
    
    (or (eq cls (load-time-value (find-class 'py-object)))
	(subtypep x cls))))

(defun {iter} (x &optional y)
  ;; Return iterator for iterable X
  ;; 
  ;; When Y supplied: make generator that calls and returns iterator
  ;; of X until the value returned is equal to Y.

  (make-iterator-from-function
   :func (if y
	     (let ((iterf (get-py-iterate-fun x)))
	       (lambda () 
		 (let ((val (funcall iterf)))
		   (if (py-== val y)
		       nil
		     val))))
	   (get-py-iterate-fun x))))

(defun {len} (x)
  (py-len x))

(defun {locals} ()
  (error-indirect-special-call '{locals}))

(defun error-indirect-special-call
    (which)
  (py-raise '{ValueError}
	    "Attempt to call the special built-in function `~A' indirectly. ~_~
             Either (1) call `~A' directly, e.g. `~A()' instead of `x = ~A; x()';~_
                 or (2) set ~A to ~A and recompile/re-evaluate the Python code."
	    which which which which '*allow-indirect-special-call* t))

(defun {map} (func &rest sequences)
  "Apply FUNC to every item of sequence, returning real list of values.
With multiple sequences, traversal is in parallel and FUNC must take
multiple args. Shorter sequences are extended with None. If function is
None, use identity function (multiple sequences -> list of tuples)."
  
  (cond ((and (eq func *the-none*) (null (cdr sequences)))  ;; identity of one sequence
	 (make-py-list-from-list (py-iterate->lisp-list (car sequences))))
	
	((null (cdr sequences)) ;; func takes 1 arg
	 
	 ;; Apply func to each val yielded before yielding next val
	 ;; might be more space-efficient for large sequences when
	 ;; function "reduces" data.

	 (make-py-list-from-list
	  (mapcar (lambda (val) (py-call func val))
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

(defun {max} (item &rest items)
  (maxmin #'py-> item items))

(defun {min} (item &rest items)
  (maxmin #'py-< item items))

(defun maxmin (cmpfunc item items)
  (let ((res nil))
    (map-over-py-object (lambda (k)
			  (when (or (null res) (py-val->lisp-bool (funcall cmpfunc k res)))
			    (setf res k)))
			
			(if (null items) 
			    item
			  (cons item items)))
    res))
    
(defun {oct} (n)
  (py-oct n))

(defun {ord} (s)
  (let ((s2 (deproxy s)))
    (if (and (stringp s2)
	     (= (length s2) 1))
	(char-code (char s2 0))
      (error "wrong arg ORD: ~A" s))))

(defun {pow} (x y &optional z)
  (py-** x y z))

(defun {range} (x &optional y z)
  (let ((lst (cond (z (error "todo: range with 3 args"))
		   (y (loop for i from x below (py-val->integer y) collect i))
		   (x (loop for i from 0 below x collect i)))))
    (make-py-list-from-list lst)))

(defun {raw_input} (&optional prompt)
  "Pops up a GUI entry window to type text; returns entered string"
  (declare (ignore prompt))
  (error "todo: raw_input")) ;; XXX hmm no "prompt" CL function?

(defun {reduce} (func seq &optional initial)
  (let ((res nil))
    (if initial
	
	(progn (setf res initial)
	       (map-over-py-object (lambda (x) (setf res (py-call func res x)))
				   seq))
      
      (map-over-py-object (lambda (x) (cond ((null res) (setf res x))
					    (t (setf res (py-call func res x)))))
			  seq))
    (or res
	(py-raise '{TypeError} "reduce() of empty sequence with no initial value"))))

(defun {reload} (m &optional (verbose 1))
  ;; VERBOSE is not a CPython argument
  ;; remove from habitat loaded-modules ?
  (let* ((*import-force-recompile* t)
         (*import-force-reload* t)
         (v (py-val->lisp-bool verbose))
         (*import-compile-verbose* v)
         (*import-load-verbose*    v)
         (mod-name-as-symbol-list (list (py-string-val->symbol (slot-value m 'name)))))
    (funcall 'py-import mod-name-as-symbol-list))
  m)

(defun {repr} (x)
  (py-repr x))

(defun {round} (x &optional (ndigits 0))
  "Round number X to a precision with NDIGITS decimal digits (default: 0).
   Returns float. Precision may be negative"
  (setf ndigits (py-val->integer ndigits))
  
  ;; implementation taken from: bltinmodule.c - builtin_round()
  ;; idea: round(12.3456, 2) ->
  ;;       12.3456 * 10**2 = 1234.56  ->  1235  ->  1235 / 10**2 = 12.35
  
  (let ((f (expt 10 (abs ndigits))))
    (let* ((x-int (if (< ndigits 0) 
		      (/ x f)
		    (* x f )))
	   (x-int-rounded (round x-int))
	   (x-rounded (if (< ndigits 0)
			  (* x-int-rounded f)
			(/ x-int-rounded f))))
      
      ;; By only coercing here at the end, the result could be more
      ;; exact than what CPython gives.
      (coerce x-rounded 'double-float))))

(defun {setattr} (x attr val)
  (unless (stringp attr)
    (py-raise '{TypeError} "Attribute (for setattr) must be string (got: ~S)" attr))
  (let ((attr.sym (ensure-user-symbol attr)))
    (setf (py-attr x attr.sym) val)))

(defun {sorted} (x)
  ;;; over sequences, or over all iterable things?
  (declare (ignore x))
  (error "todo: sorted"))

(defun {sum} (seq &optional (start 0))
  (let ((total (py-val->number start)))
    (map-over-py-object (lambda (x) (incf total (py-val->number x)))
			seq)
    total))

(defun {unichr} (i)
  ;; -> unicode char i
  (declare (ignore i))
  (error "todo: unichr"))

(defun {vars} (&optional x)
  "If X supplied, return it's dict, otherwise return local variables."
  (declare (ignore x))
  (error "todo")
  #+(or)(if x
      (multiple-value-bind (val found)
	  (internal-get-attribute x '{__dict__})
	(if found
	    val
	  (py-raise '{AttributeError}
		    "Instances of class ~A have no attribute '__dict__' (got: ~A)"
		    (class-of x) x)))
    ({locals})))

(defun {zip} (&rest sequences)
  "Return a list with tuples, where tuple i contains the i-th argument of ~
   each of the sequences. The returned list has length equal to the shortest ~
   sequence argument."

  ;; CPython looks up __len__, __iter__, __getitem__ attributes here
  ;; need to make an iterator for each sequence first, then call the iterators
  
  (unless sequences
    (py-raise '{TypeError} "zip(): must have at least one sequence"))
  
  (loop with iters = (mapcar #'get-py-iterate-fun sequences)
      for tup = (loop for iter in iters
		      if (funcall iter)
		      collect it into cur-tuple-vals
		      else return nil
		      finally (return cur-tuple-vals))
      if tup
      collect it into tuples
      else return (make-array (length tuples)
			      :adjustable t
			      :fill-pointer (length tuples)
			      :initial-contents tuples)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 3) Built-in values

(defvar {None}           *the-none*           )
(defvar {Ellipsis}       *the-ellipsis*       )
(defvar {True}           *the-true*           )
(defvar {False}          *the-false*          )
(defvar {NotImplemented} *the-notimplemented* )
