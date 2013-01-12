;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON; Readtable: PY-USER-READTABLE -*-
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
	`(({basestring}   py-string       )
	  ({bool}         py-bool         )
          ({buffer}       buffer          )
	  ({classmethod}  py-class-method )
	  ({complex}      py-complex      )
	  ({dict}         dict            )
	  ({enumerate}    py-enumerate    )
	  ({file}         py-file         )
	  ({float}        py-float        )
          ({frozenset}    |frozenset|     )
	  ({int}          py-int          )
	  ({list}         py-list         )
	  ({long}         py-int          )
	  ({number}       py-number       )
	  ({object}       object          )
	  ({property}     py-property     )
	  ({slice}        py-slice        )
          ({set}          |py-set|        )
	  ({staticmethod} py-static-method)
	  ({str}          py-string       )
	  ({super}        py-super        )
	  ({tuple}        py-tuple        )
	  ({type}         py-type         )
	  ({unicode}      py-string       )
	  ({xrange}       py-xrange       ))
      do (setf (symbol-value sym) (if (eq cls-name *the-notimplemented*)
                                      *the-notimplemented*
                                    (find-class cls-name)))))
(def-builtin-types)

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

(defun {all} (x)
  (let ((iter-fun (get-py-iterate-fun x)))
    (loop (let ((item (funcall iter-fun)))
            (cond ((null item) 
                   (return-from {all} +the-true+))
                  ((not (py-val->lisp-bool item))
                   (return-from {all} +the-false+)))))))

(defun {any} (x)
  (let ((iter-fun (get-py-iterate-fun x)))
    (loop (let ((item (funcall iter-fun)))
            (cond ((null item) 
                   (return-from {any} +the-false+))
                  ((py-val->lisp-bool item)
                   (return-from {any} +the-true+)))))))

(defun {apply} (function &optional pos-args kw-dict)
  "Apply FUNCTION (a callable object) to given args.
POS-ARGS is any iterable object; KW-DICT must be of type DICT." 
  
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
  (py-bool (class.attr-no-magic (py-class-of x) '{__call__})))

(defun {chr} (x)
  "Return a string of one character whose ASCII code is the integer i. ~@
   This is the inverse of {ord}."
  (let ((i (deproxy x)))
    (unless (typep i '(integer 0 255))
      (py-raise '{TypeError}
		"Built-in function chr() should be given an integer in ~
                 range 0..255 (got: ~A)" x))
    (svref #.(coerce (loop for i from 0 to 255 collect (string (code-char i))) 'simple-vector) i)))

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
  (let ((attr.sym (ensure-user-symbol (py-val->string attr))))
    (setf (attr x attr.sym) nil)))

(defun {dir} (&optional x)
  "Without args, returns names in current scope. ~@
   With arg X, return list of valid attributes of X. ~@
   Result is sorted alphabetically, and may be incomplete."
  (let (res)
    (when x (loop for (k . nil) in (dir-items x :use-all nil)
                do (push k res)))
    (make-py-list-from-list (sort res #'string<))))
  
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
  (let* ((attr.sym (ensure-user-symbol (py-val->string attr)))
	 (val (handler-case (attr x attr.sym)
                ({AttributeError} () nil))))
    (if (null val)
	(or default
	    (py-raise '{AttributeError} "Object `~A' has no attribute `~A'." x attr))
      val)))

#+(or)
(defun getattr-nobind (x attr &optional default)
  ;; Exceptions raised during py-attr are not catched.
  ;; Returns :class-attr <meth> <inst>
  ;;      or <value>
  (let ((attr.sym (ensure-user-symbol attr)))
    (multiple-value-bind (a b c)
	(catch :getattr-block
	  (handler-case
	      (attr x attr.sym :via-getattr t :bind-class-attr nil)
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
  (py-bool (ignore-errors (attr x (ensure-user-symbol name)))))

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

(defvar *intern-warned* nil)
(defvar *intern-hashtable* nil)

(defun {intern} (x)
  (unless *intern-warned*
    (warn "Function 'intern' is deprecated.")
    (setf *intern-warned* t))
  (unless *intern-hashtable*
    (setf *intern-hashtable* (make-hash-table :test 'equal)))
  (or (gethash x *intern-hashtable*)
      (setf (gethash x *intern-hashtable*) x)))

(defun class-tuple-tester (x cls test)
  "Is (test x c) for some class C?
CLS is a class or tuple (list) of classes."
  (let ((cls (deproxy cls)))
    (py-bool (if (listp cls)
                 (some (lambda (c) (funcall test x c)) cls)
               (funcall test x cls)))))

(defun {isinstance} (x cs)
  (flet ((isinstance-test (x c)
           (or (eq c (load-time-value (find-class 'object)))
               (typep x c)
               (subtypep (py-class-of x) c))))
    (class-tuple-tester x cs #'isinstance-test)))

(defun {issubclass} (x cls)
  (flet ((issubclass-test (x c)
           (or (eq c (load-time-value (find-class 'object)))
               (subtypep x c))))
    (class-tuple-tester x cls #'issubclass-test)))

(defun {iter} (x &optional y)
  "Return iterator for iterable X
When Y supplied: make generator that calls and returns iterator of X
until the value returned is equal to Y."
  ;; A generator is its own iterator
  (if (and (not y) (typep x 'generator))
      x
    (make-iterator-from-function
     :func (if y
               (let ((iterf (get-py-iterate-fun x)))
                 (lambda () 
                   (let ((val (funcall iterf)))
                     (if (py-== val y)
                         nil
                       val))))
             (get-py-iterate-fun x)))))

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
	 (let* ((lists (coerce (mapcar #'py-iterate->lisp-list sequences) 'vector))
                (iters (loop for x across lists minimize (length x))))
           (make-py-list-from-list
            (loop for i from 0 below iters
                collect (loop for i from 0 below (length lists)
                            collect (or (pop (aref lists i)) *the-none*) into args
                            finally (return (if (eq func *the-none*)
                                                (make-tuple-from-list args)
                                              (apply #'py-call func args))))))))))

(defun {max} (item &rest items)
  (maxmin #'py-> item items))

(defun {min} (item &rest items)
  (maxmin #'py-< item items))

(defun maxmin (cmpfunc item items)
  (let ((res nil))
    (map-over-object (lambda (k)
			  (when (or (null res) (py-val->lisp-bool (funcall cmpfunc k res)))
			    (setf res k)))
			
			(if (null items) 
			    item
			  (cons item items)))
    res))
    
(defun {oct} (n)
  (py-oct n))

(defun {open} (name &optional mode buffering)
  (declare (ignore buffering)) ;; todo
  (funcall (find-symbol (symbol-name '#:|open|) 'clpython.module.posix) name nil mode))

(defun {ord} (s)
  (let ((s2 (deproxy s)))
    (if (and (stringp s2)
	     (= (length s2) 1))
	(char-code (char s2 0))
      (error "wrong arg ORD: ~A" s))))

(defun {pow} (x y &optional z)
  (py-** x y z))

(defun {range} (x &optional y z)
  (make-py-list-from-list
   (cond ((not (or y z))
          (loop for i from 0 below (py-val->integer x) collect i))
         ((not z)
          (loop for i from (py-val->integer x) below (py-val->integer y) collect i))
         ((plusp z)
          (loop for i from (py-val->integer x) below (py-val->integer y) by z collect i))
         ((minusp z)
          (loop for i downfrom (py-val->integer x) above (py-val->integer y) by (- z) collect i))
         ((zerop z)
          (py-raise '{ValueError} "Range step size can't be zero.")))))

(defun {raw_input} (&optional prompt)
  "Pops up a GUI entry window to type text; returns entered string"
  (declare (ignore prompt))
  (error "todo: raw_input")) ;; XXX hmm no "prompt" CL function?

(defun {reduce} (func seq &optional initial)
  (let ((res nil))
    (if initial
	
	(progn (setf res initial)
	       (map-over-object (lambda (x) (setf res (py-call func res x)))
				   seq))
      
      (map-over-object (lambda (x) (cond ((null res) (setf res x))
					    (t (setf res (py-call func res x)))))
			  seq))
    (or res
	(py-raise '{TypeError} "reduce() of empty sequence with no initial value"))))

(defun {reload} (m)
  (etypecase m
    (package m) ;; Can't really reload; return as-is.
    (module (let* ((*import-force-reload* t)
                   (mod-name-as-symbol-list (list (py-string-val->symbol (slot-value m 'name))))) ;; XXX only works for toplevel modules
              ;; XXX Check semantics. Should recompile certain pathname, not modulename?
              (let ((new-mod (funcall 'py-import mod-name-as-symbol-list)))
                (copy-module-contents :from new-mod :to m)
                m)))))

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
  (let ((attr.sym (ensure-user-symbol (py-val->string attr))))
    (setf (attr x attr.sym) val)))

(defun {sorted} (x)
  ;;; over sequences, or over all iterable things?
  (declare (ignore x))
  (error "todo: sorted"))

(defun {sum} (seq &optional (start 0))
  (let ((total (py-val->number start)))
    (map-over-object (lambda (x) (incf total (py-val->number x)))
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
			      :fill-pointer t
			      :initial-contents tuples)))

;;; 3) Built-in values

(defvar {None}           *the-none*           )
(defvar {Ellipsis}       *the-ellipsis*       )
(defvar {True}           +the-true+           )
(defvar {False}          +the-false+          )
(defvar {NotImplemented} *the-notimplemented* )

