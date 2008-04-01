;; -*- package: clpython; readtable: py-ast-user-readtable -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Python classes and metaclasses; the built-in classes including
;;;; their methods.

;;; To have Emacs properly indent the DEF-PY-METHOD form, add to .emacs:
;;;  (put 'def-py-method 'fi:common-lisp-indent-hook (get 'defmethod 'fi:common-lisp-indent-hook))

;;; Class frameword:  py-meta-type, py-type, py-dictless-object

(in-package :clpython)
(in-syntax *ast-user-readtable*)

(defmacro ltv-find-class (clsname)
  `(load-time-value (find-class ,clsname)))

(defclass py-dict-mixin ()
  ((dict :initarg :dict :initform (make-dict) :accessor dict)))

(defmethod dict ((x t))
  nil)

(defmacro sub/dict-get (d key)
  "Returns value or NIL; does not use dict's __getitem__ etc"
  `(dikt-get (py-dict-dikt ,d) ,key))

(defmacro sub/dict-set (d key val)
  "Does not use dict's __getitem__ etc"
  `(dikt-set (py-dict-dikt ,d) ,key ,val))

(defun sub/dict-del (d key)
  "Returns if succesfull (i.e. key was present)"
  (check-type d py-dict)
  (if (eq (class-of d) (ltv-find-class 'py-dict))
      (dikt-del (py-dict-dikt d) key)
    (py-classlookup-bind-call d '{__delitem__} key)))


;; Python metatype. Class `type' and subclasses thereof are instances
;; of py-meta-type.

(defun finalize-inheritance (c)
  #+allegro (mop:finalize-inheritance c)
  #+lispworks (clos:finalize-inheritance c)
  #-(or allegro lispworks) (error "No FINALIZE-INHERITANCE defined in this implementation."))

(defclass py-meta-type (py-dict-mixin standard-class)
  ())

#+lispworks ;; Apparently needed; should look into
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod cl::validate-superclass ((class py-meta-type) superclass)
    t)

  (defmethod cl::validate-superclass ((class standard-class) (superclass py-meta-type))
    t))

(defmethod initialize-instance :after ((cls py-meta-type) &rest initargs)
  (declare (ignore initargs))
  (finalize-inheritance cls))


;; A class for Python classes. This is an instance of py-meta-type

(defclass py-type (py-dict-mixin standard-class)
  ()
  (:metaclass py-meta-type))

#+lispworks ;; Apparently needed; should look into
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod cl::validate-superclass ((class py-type) superclass)
    t)

  (defmethod cl::validate-superclass ((class standard-class) (superclass py-type))
    t))

(defmethod initialize-instance :after ((cls py-type) &rest initargs)
  (declare (ignore initargs))
  (finalize-inheritance cls))


;; Python classes are instances of (a subclass of) python-type

(defclass py-dictless-object (standard-object)
  ()
  (:metaclass py-type))


(finalize-inheritance (find-class 'py-dictless-object))

(defclass py-object (py-dict-mixin py-dictless-object)
  ()
  (:metaclass py-type))

(finalize-inheritance (find-class 'py-object))


;; Core type/object

(defclass py-core-object (py-dictless-object) ())
(defclass py-core-type   (py-type)   ())

;; User type/object
(defclass py-user-type   (py-type) ())
(defclass py-user-object (py-object)
  ((dict :initform (make-dict)))  ;; XXX __slots__ ignored
  (:metaclass py-user-type))

;; Lisp type/object
(defclass py-lisp-type (py-type)
  ()
  (:documentation "Metaclass for proxy classes"))

(defclass py-lisp-object (py-dictless-object)
  ((lisp-object :initarg :lisp-object :accessor proxy-lisp-val))
  (:metaclass py-lisp-type)
  (:documentation "Base class for proxy classes"))


(defun ensure-class (&rest args)
  #+allegro (apply #'mop:ensure-class args)
  #+lispworks (apply #'clos:ensure-class args)
  #-(or allegro lispworks) (error "No ENSURE-CLASS defined in this implementation."))

(defun ensure-class-using-class (&rest args)
  #+allegro (apply #'mop:ensure-class-using-class args)
  #+lispworks (apply #'clos:ensure-class-using-class args)
  #-(or allegro lispworks) (error "No ENSURE-CLASS-USING-CLASS defined in this implementation."))

(defun class-direct-superclasses (&rest args)
  #+allegro (apply #'mop:class-direct-superclasses args)
  #+lispworks (apply #'clos:class-direct-superclasses args)
  #-(or allegro lispworks) (error "No CLASS-DIRECT-SUPERCLASSES defined in this implementation."))

;; Fix superclass and metaclass of PY-DICT.

(ensure-class 'py-dict
              :direct-superclasses (list 'py-core-object)
              :metaclass 'py-core-type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This is a bit ugly, but method classes must be evaluated prior to their
;;; use. Therefore here are method classes, and below their methods.
;;;
;;; Note that PY-METHOD is not a Lisp FUNCTION.

(defclass py-method (py-core-object)
  ((func :initarg :func :accessor py-method-func))
  (:metaclass py-core-type))

(defclass py-class-method (py-method)
  ((class :initarg :class))
  (:metaclass py-core-type))

(defclass py-attribute-method (py-method)
  ()
  (:metaclass py-core-type))

(defclass py-class-attribute-method (py-attribute-method)
  ()
  (:metaclass py-core-type))

(defclass py-writable-attribute-method (py-attribute-method)
  (write-func)
  (:metaclass py-core-type))

(defclass py-static-method (py-method)
  ()
  (:metaclass py-core-type))

(defparameter *writable-attribute-methods* (make-hash-table :test #'eq))

(eval-when (compile load eval)

(defun ensure-pkg-symbol (str pkg)
  (check-type str string)
  (or (find-symbol str pkg)
      (intern str pkg)))

(defun ensure-user-symbol (str)
  (ensure-pkg-symbol str #.(find-package :clpython.user)))

) ;; eval-when

(defmacro def-py-method (cls.meth &rest args)
  ;; As `cls.meth' is a symbol, but methods of Python classes are always in
  ;; lowercase, the `meth' part is converted to lowercase iff the symbol name
  ;; is all in upper case. This means you cannot create a full-upper-case method
  ;; name with DEF-PY-METHOD.
  (labels ()
    
    (let* ((cm (symbol-name cls.meth))
	   (dot-pos (or (position #\. cm)
			(error "Need dot in name: (def-py-method classname.methodname ..); got: ~A"
			       cls.meth)))
	   ;; The class names must be a symbol defined in the same pkg
	   ;; as the `cls.meth' argument to this macro.
	   (cls  (intern (subseq cm 0 dot-pos) (symbol-package cls.meth)))
	   (meth (let ((methname (subseq cm (1+ dot-pos))))
		   (when (not (some #'lower-case-p methname))
		     ;; Can't use (every #'upper-case-p) because of dashes
		     (setf methname (nstring-downcase methname)))
		   (ensure-user-symbol methname)))
	   (modifiers (loop while (keywordp (car args)) collect (pop args)))
	   (func-name (if (eq (car modifiers) :attribute-write)
			  (ensure-user-symbol (format nil "~A-writer" cls.meth))
			cls.meth)))
      
      (assert (<= (length modifiers) 1) ()
	"Multiple modifiers for a py-method: ~A. Todo?" modifiers)

      `(progn
	 ,(destructuring-bind (func-args &body func-body) args
	    (loop with real-args
		with body = `(locally ,@func-body) ;; allows DECLARE forms at head
			    
		for sym in func-args
		for sym-name = (when (symbolp sym) (symbol-name sym))

		if (not (symbolp sym))
		do (push sym real-args)
		   
		else if (char= #\^ (aref sym-name (1- (length sym-name))))
		do (let ((real-name (intern (subseq sym-name 0 (1- (length sym-name)))
					    #.*package*)))
		     (push real-name real-args)
		     (setf body `(let ((,real-name (deproxy ,real-name)))
				   ,body)))
		   
		else do (push sym real-args)
		     
		finally (return `(defun ,func-name ,(nreverse real-args)
				   (block ,cls.meth
				     ,body)))))
	 
	 (let* ((cls (or (find-class ',cls) (error "No such class: ~A" ',cls)))
                (dict (or (dict cls)
                          (error "Class ~A has no dict (to store method ~A)"
                                 cls ',cls.meth))))
	   
	   (let ((obj
		  ,(ecase (car modifiers)
		     ((nil)             `(let ((f (function ,cls.meth)))
					   f))
		     
		     (:static           `(make-instance 'py-static-method
					   :func (function ,cls.meth)))
		     
		     (:attribute        `(make-instance 'py-attribute-method
					   :func (function ,cls.meth)))
		     
		     (:class-attribute  `(make-instance 'py-class-attribute-method
					   :func (function ,cls.meth)))
		     
		     (:attribute-read   `(let ((x (make-instance 'py-writable-attribute-method
						    :func (function ,cls.meth))))
					   (setf (gethash ',cls.meth *writable-attribute-methods*) x)
					   x))
		     
		     (:attribute-write  `(let ((f (function ,func-name))
					       (read-f (or (gethash ',cls.meth
								    *writable-attribute-methods*)
							   (error "Attribute read function ~A not defined yet"
								  ',cls.meth))))
					   (setf (slot-value read-f 'write-func) f)
					   nil ;; read function is already stored in dict
					   )))))
	     (when obj
	       (sub/dict-set dict ',meth obj))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; dynamic class creation


(defun make-py-class (&key name context-name namespace supers cls-metaclass mod-metaclass)
  (declare (ignore context-name)) ;; XXX for now

  #+(or)(assert (symbolp name))
  #+(or)(assert (listp supers))
  #+(or)(assert (typep namespace 'py-dict))
  
  ;; XXX is this a true restriction?  Custom metaclasses may allow
  ;; more kinds of `bases' in their __new__(...) ?
  
  ;; either:
  ;;  1) all supers are subtype of 'py-type   (to create a new metaclass)
  ;;  2) all supers are subtype of 'py-dictless-object (to create new "regular user-level" class)
  
  (flet ((of-type-class (s) (typep s 'class))
	 (subclass-of-py-dl-object-p (s) (subtypep s (ltv-find-class 'py-dictless-object)))
	 (subclass-of-py-type-p (s)      (subtypep s (ltv-find-class 'py-type))))
    
    (unless (every #'of-type-class supers)
      (py-raise '{TypeError} "Not all superclasses are classes (got: ~A)." supers))

    (loop for s in supers
	unless (or (subclass-of-py-type-p s)
		   (subclass-of-py-dl-object-p s))
	do (error "BUG? Superclass ~A is neither sub of 'type nor sub of 'object!" s))
    
    #+(or)
    (let ((core-supers (remove-if-not (lambda (s) (typep s 'py-core-type)) supers)))
      (when core-supers
	(py-raise '{TypeError} "Cannot subclass from these classes: ~A" core-supers)))
    
    (when (and (some #'subclass-of-py-type-p supers)
	       (some #'subclass-of-py-dl-object-p supers))
      (py-raise 'TypeError "Superclasses are at different levels (some metaclass, ~
                            some regular class) (got: ~A)." supers))
    
    (let ((metaclass (or cls-metaclass
			 (when supers (class-of (car supers)))
			 mod-metaclass
			 (ltv-find-class 'py-type))))
      
      #+(or)(warn "metaclass: ~A" metaclass)
      
      (unless (typep metaclass 'class)
	(py-raise '{TypeError} "Metaclass must be a class (got: ~A)" metaclass))
      
      (unless (or (eq metaclass (ltv-find-class 'py-meta-type))
		  (subtypep metaclass (ltv-find-class 'py-type)))
	(py-raise '{TypeError} 
		  "Metaclass must be subclass of `type' (got class: ~A)" metaclass))
      
      ;; When inheriting from py-lisp-type (like `int'), use
      ;; py-user-type as metaclass.
      ;; XXX subclassing `function' ?
      (when (or (eq metaclass (ltv-find-class 'py-lisp-type))
		(eq metaclass (ltv-find-class 'py-core-type)))
	(setf metaclass (ltv-find-class 'py-user-type)))
      
      ;; Subclass of `type' has metaclass 'py-meta-type
      (when (eq metaclass (ltv-find-class 'py-meta-type))
	(let ((cls (ensure-class
		    (make-symbol (symbol-name name))
		    :direct-superclasses supers
		    :metaclass (ltv-find-class 'py-meta-type)
		    :dict namespace)))
	  (return-from make-py-class cls)))
            
      ;; Not a subclass of `type', so at the `object' level
      (let ((__new__ (recursive-class-dict-lookup metaclass '{__new__})))
	(assert __new__ ()
	  "recur: no __new__ found for class ~A, yet it is a subclass of PY-TYPE ?!" metaclass)

	#+(or)(warn "binding __new__: ~A ~A" __new__ metaclass)
	(let ((cls (if (and (eq (class-of __new__) (ltv-find-class 'py-static-method))
			    (eq (py-method-func __new__) (symbol-function 'py-type.__new__)))
		       
		       ;; Optimize common case:  py-type.__new__
		       (progn 
			 #+(or)(warn "Inlining make-py-class")
			 (py-type.__new__ metaclass
					  (string name)
					  supers ;; MAKE-TUPLE-FROM-LIST not needed
					  namespace))
		     
		     (let ((bound-_new_ (bind-val __new__ metaclass (py-class-of metaclass))))
		       ;; If __new__ is a static method, then bound-_new_ will
		       ;; be the underlying function.
		       
		       (or (py-call bound-_new_ metaclass
				    (string name) 
				    (make-tuple-from-list supers) ;; ensure not NIL
				    namespace)
			   (break "Class' bound __new__ returned NIL: ~A" bound-_new_))))))

	  ;; Call __init__ when the "thing" returned by
	  ;; <metaclass>.__new__ is of type <metaclass>.
	  (if (typep cls metaclass)
		
	      (let ((__init__ (recursive-class-dict-lookup metaclass '{__init__})))
		#+(or)(warn "  __init__ method ~A is: ~A" metaclass __init__)
		(when __init__
		  (py-call __init__ cls)))
	    
	    #+(or)(warn "Not calling __init__ method, as class ~A is not instance of metaclass ~A"
			cls metaclass))
	  
	  cls)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *the-true* 1)
(defconstant *the-false* 0)

(defun py-bool (lisp-val)
  (if lisp-val *the-true* *the-false*))

(define-compiler-macro py-bool (lisp-val)
  `(if ,lisp-val *the-true* *the-false*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Built-in Python object types:
;;; 
;;; -- type ----------- repr ----- subclassable -- examples -------------------
;;;  py-lisp-object:  lisp value    yes       number, string, tuples, vectors, dict
;;;  py-core-object:  py cls inst   no        function, method
;;;  py-user-object:  py cls inst   yes       file, module, property
;;;
				       

;; (defun f (&rest args)
;;   (with-parsed-py-arglist ("f" (a b) args)
;;     (+ a b)))
;;
;; (f 1 2) (f 1 :b 2) (f :a 1 :b 2)

(defmacro with-parsed-py-arglist ( (func-name formal-args actual-args) &body body)
  (let ((alist '#:alist))
    `(let* ((,alist (parse-poskey-arglist ,func-name ',formal-args ,actual-args))
	    ,@(loop for f in formal-args
		  collect `(,f (cdr (assoc ',f ,alist :test #'eq)))))
       ,@body)))
    
(defun parse-poskey-arglist (func-name formal-pos-args actual-args)
  (let ((pos-args (loop until (symbolp (car actual-args))
		      collect (pop actual-args)))
	(kw-args (loop while actual-args
		     for key = (pop actual-args)
		     for val = (pop actual-args)
		     unless (and (symbolp key) val)
		     do (error "Invalid arglist: ~S" actual-args)
		     collect (cons key val))))
    
    (let ((res ())
	  (formal-pos-args (copy-list formal-pos-args)))
      
      (loop while (and formal-pos-args pos-args)
	  do (push (cons (pop formal-pos-args) (pop pos-args)) res))
      
      (when pos-args
	(py-raise '{TypeError} "Too many arguments for function ~A (got: ~A)" 
		  func-name))
      
      (loop while kw-args
	  for kwa = (pop kw-args)
	  for key = (car kwa)
	  for val = (cdr kwa)
	  do (let ((fkw (find key formal-pos-args :test #'string=))) ;; (string= |:a| '|a|)
	       (if fkw
		   (progn (setf formal-pos-args (delete fkw formal-pos-args :test #'eq))
			  (push (cons fkw val) res))
		 (py-raise '{ValueError}
			   "Invalid argument list: unknown keyword arg (or duplicated arg): ~A"
			   key))))
      
      (loop for f in formal-pos-args
	  do (push (cons f nil) res))
      
      res)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Core objects (function, method, None...; not subclassable by the user)
;;;

(def-py-method py-dictless-object.__class__ :attribute (x)
  ;; needed for:  type(3) -> int
  (py-class-of x))

(def-py-method py-type.__class__ :attribute (x)
  (py-class-of x))


;; None

(defclass py-none (py-core-object) () (:metaclass py-core-type))
(defvar *the-none* (make-instance 'py-none))


;; py-class-method

(def-py-method py-class-method.__new__ :static (cls func)
  (declare (ignore func))
  (make-instance cls))

(defun classp (x)
  #+allegro (excl::classp x)
  #+lispworks (clos::classp x)
  #-(or allegro lispworks) (error "No CLASSP defined in this implementation."))
  
(def-py-method py-class-method.__get__ (x inst class)
  (declare (ignore class))
  (let ((arg (if (classp inst) inst (py-class-of inst))))
    (make-instance 'py-bound-method
      :func (slot-value x 'func)
      :instance arg)))

(def-py-method py-class-method.__init__ (x^ func)
  (setf (slot-value x 'func) func))

(def-py-method py-class-method.__call__ (x^ obj &rest args)
  (let ((arg (if (classp obj) obj (py-class-of obj))))
    (apply #'py-call (py-method-func x) arg args)))


;; py-attribute-method

(def-py-method py-attribute-method.__get__ (x inst class)
  #+(or)(break "pam.get ~A ~A ~A" x inst class)
  (declare (ignore class))
  (if (and inst (not (eq inst *the-none*)))
      (py-call (slot-value x 'func) inst)
    nil))

(def-py-method py-attribute-method.__set__ (x obj val)
  (declare (ignore val))
  (py-raise '{TypeError}
	    "Attribute ~A of object ~A is read-only (value: ~A)"
	    x obj (py-call (slot-value x 'func) obj)))

(def-py-method py-attribute-method.__repr__ (x)
  (with-output-to-string (s)
    (print-unreadable-object (x s :type t :identity t))))

;; py-class-attribute-method

(def-py-method py-class-attribute-method.__get__ (x inst class)
  (unless (classp inst)
    (break "py-class-attribute-method.__get__ wants class instance as first arg! ~A ~A ~A"
           x inst class))
  (py-call (slot-value x 'func) inst))

;; py-writable-attribute-method

(def-py-method py-writable-attribute-method.__set__ (x obj val)
  (if (slot-boundp x 'write-func)
      (py-call (slot-value x 'write-func) obj val)
    (error "No writer defined for writable attribute: ~A" x)))

(def-py-method py-writable-attribute-method.__get__ (x inst class)
  (declare (ignore class))
  (if (and inst (not (eq inst *the-none*)))
      (py-call (slot-value x 'func) inst)
    nil))

(defclass py-bound-method (py-method)
  ((instance :initarg :instance :accessor py-method-instance))
  (:metaclass py-core-type))

(defmethod print-object ((x py-bound-method) stream)
  (print-unreadable-object (x stream :identity t :type nil)
    (with-slots (instance func) x
      (format stream "bound-method ~A on the ~A ~A"
	      (py-function-name func)
	      (class-name (py-class-of instance))
	      instance))))
	
(def-py-method py-bound-method.__repr__ (x)
  (with-output-to-string (s)
    (print-object x s)))

(def-py-method py-bound-method.__call__ (x &rest args)
  (when (and (null (cdr args))
	     (eq (car args) '{__dict__}))
    #+(or)(break "pym.c args: ~A ~A" x args))
  (with-slots (func instance) x
    (if (functionp func)
	(apply (the function func) instance args)
      (apply #'py-call func instance args))))

(def-py-method py-bound-method.__get__ (x &rest args)
  
  ;; Somewhat surprisingly, when a bound method is __get__ again, the
  ;; underlying function is bound again:
  ;; 
  ;; >>> class C:
  ;; ...   def m(self): pass
  ;; ... 
  ;; >>> x,y = C(), C()
  ;; >>> m = x.m
  ;; >>> m
  ;; <bound method C.m of <__main__.C instance at 0x4021e8ec>>
  ;; >>> m2 = m.__get__(y,C)
  ;; >>> m2
  ;; <bound method C.m of <__main__.C instance at 0x4021e92c>>
  ;; >>> x
  ;; <__main__.C instance at 0x4021e8ec>
  ;; >>> y
  ;; <__main__.C instance at 0x4021e92c>
  
  (apply #'py-call (recursive-class-lookup-and-bind (py-method-func x) '{__get__}) args))

(def-py-method py-bound-method.__name__ :attribute (x)
  (py-bound-method.__repr__ x))	       
    
    
(defclass py-unbound-method (py-method)
  ((class :initarg :class :accessor py-method-class))
  (:metaclass py-core-type))

(def-py-method py-unbound-method.__repr__ (x)
  (with-output-to-string (s)
    (print-object x s)))

(defmethod print-object ((x py-unbound-method) stream)
  (print-unreadable-object (x stream :identity t :type nil)
    (with-slots (class func) x
      (format stream "unbound-method ~A on class ~A"
	      (py-function-name func)
	      (class-name class)))))

(def-py-method py-unbound-method.__call__ (x &rest args)
  (with-slots (func) x
    (apply #'py-call func args)))

(def-py-method py-unbound-method.__get__ (x &rest args)
  ;; same as py-bound-method.__get__ ?!
  (apply #'py-call (recursive-class-lookup-and-bind (py-method-func x) '{__get__}) args))


;; py-static-method

(def-py-method py-static-method.__new__ :static (cls func)
  (assert (eq cls (ltv-find-class 'py-static-method)))
  (make-instance 'py-static-method :func func))

(def-py-method py-static-method.__get__ (x inst class)
  (declare (ignore inst class))
  (slot-value x 'func))

(def-py-method py-static-method.__repr__ (x)
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t)
      (format s "the static method ~A" (slot-value x 'func)))))

(def-py-method py-static-method.__call__ (x &rest args)
  (apply #'py-call (slot-value x 'func) args))

(defmethod print-object ((x py-static-method) stream)
  (print-unreadable-object (x stream :identity t :type t)
    (format stream ":func ~A" (slot-value x 'func))))
    

;; Function (Core object)

(defclass py-lisp-function (py-core-object)
  ()
  (:metaclass py-core-type))

(finalize-inheritance (find-class 'py-lisp-function))

(def-py-method py-lisp-function.__repr__ (func)
  (with-output-to-string (s)
    (print-object func s)))

(def-py-method py-lisp-function.__get__ (func inst cls)
  (assert inst)
  (let ((to-make (cond ((eq inst *the-none*)
			(if (and (typep cls 'class)
				 (eq (class-name cls) 'py-none))
			    :bound-method
			  :unbound-method))
		       ((eq cls *the-none*)
			(py-raise '{ValueError}
				  "function.__get__(None, None) : invalid args"))
		       (t
			:bound-method))))
    (ecase to-make
      (:bound-method   (make-instance 'py-bound-method :instance inst :func func))
      (:unbound-method (make-instance 'py-unbound-method :class cls :func func)))))

(def-py-method py-lisp-function.__name__ :attribute (func)
  #+allegro (string (excl::func_name func))
  #-allegro "[nameless function]")

(defclass funcallable-python-class (#+allegro mop:funcallable-standard-class 
                                    #+lispworks clos::funcallable-standard-class

                                    py-core-type)
  ;; When subclassable python classes also get such a metatype,
  ;; s/py-core-type/py-type/
  ())

(defclass py-function (standard-generic-function py-core-object py-dict-mixin)
  ;; mop:funcallable-standard-class defines :name initarg, but I don't know how to access it...
  ((name         :initarg :fname        :accessor py-function-name)
   (context-name :initarg :context-name :accessor py-function-context-name)
   (lambda       :initarg :lambda       :accessor py-function-lambda))
  (:metaclass funcallable-python-class))

(defun set-funcallable-instance-function (inst func)
  (#+allegro mop:set-funcallable-instance-function
   #+lispworks clos::set-funcallable-instance-function
   inst func))

(defun make-py-function (&key name context-name lambda)
  (let ((x (make-instance 'py-function :fname (string name) :lambda lambda :context-name context-name)))
    (set-funcallable-instance-function x lambda)
    ;; fill dict?
    x))

(def-py-method py-function.__get__ (func inst cls)
  (py-lisp-function.__get__ func inst cls))

(def-py-method py-function.__repr__ (func)
  (with-output-to-string (s)
    (print-unreadable-object (func s :identity t)
      (if (typep func 'py-function)
	  
	  (progn
	    (format s "python-function ~A~@[ (~A)~]"
                    (py-function-name func)
                    (unless (compiled-function-p (py-function-lambda func)) "interpreted"))
	    (when (string/= (py-function-name func)
			    (py-function-context-name func))
	      (format s " (~A)" (py-function-context-name func))))
	
	(format s "function ~A" (py-function-name func))))))

(def-py-method py-function.__name__ :attribute (func)
  (py-function-name func))

(def-py-method py-function._fif :attribute (x)
  "The funcallable instance function of X."
  ;; CLPython-specific.
  (when (typep x 'py-function)
    (setf x (py-function-lambda x)))
  (assert (functionp x))
  x)

(def-py-method py-function._dis :attribute (x)
  ;; CLPython-specific attribute, to ease debugging.
  ;;
  ;; Calling DISASSEMBLE directly on X will print (at least) two functions:
  ;;  - the instructions that load and call the funcallable instance function
  ;;  - the instructions of the funcallable instance function
  ;; The first is not very interesting imho.
  ;; 
  ;; DISASSEMBLE accepts fbound symbols or lambda expressions, according to the spec.
  ;; It might thus be Allegro-specific that its DISASSEMBLE accepts a function object.
  (when (typep x 'py-function)
    (setf x (py-function-lambda x)))
  (eval `(disassemble ,x)))

(defmethod py-function-name ((x function))
  #+allegro (format nil "~A" (excl::func_name x))
  #-allegro (call-next-method))

(defmethod py-function-name ((x t))
  ;; fall-back
  (format nil "~A" x))

(def-py-method py-function.__call__ (func &rest args)
  (apply func args))

;; Enumerate (core object)

(defclass py-enumerate (py-core-object)
  ((gener :initarg :gener))
  (:metaclass py-core-type))

(finalize-inheritance (find-class 'py-enumerate))

(def-py-method py-enumerate.__new__ :static (cls iterable)
  #+(or)(assert (subtypep cls 'py-enumerate))
  (let ((gener (make-iterator-from-function
		:name :enumerater
		:func (let ((iter (get-py-iterate-fun iterable))
			    (i 0))
			(lambda ()
			  (let ((val (funcall iter)))
			    (when val
			      (prog1 
				  (make-tuple-from-list (list i val))
				(incf i)))))))))
    (make-instance cls :gener gener)))

(def-py-method py-enumerate.__repr__ (x)
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t :type t))))

(def-py-method py-enumerate.__iter__ (x)
  (slot-value x 'gener))


(defclass py-slice (py-core-object)
  ((start :initarg :start)
   (stop  :initarg :stop)
   (step  :initarg :step))
  (:metaclass py-core-type))

(defun make-slice (start stop step)
  (make-instance 'py-slice
    :start (or start *the-none*)
    :stop  (or stop  *the-none*)
    :step  (or step  *the-none*)))

(def-py-method py-slice.indices (x^ length^)
  "Return tuple of three integers: START, STOP, STEP.
In case of empty range, returns (0,0,1)."
  
  (setf length (py-val->integer length :min 0))
  
  (multiple-value-bind (start stop step)
      (destructuring-bind (kind &rest args)
	  (slice-indices x length)
	(ecase kind
	  
	  ((:empty-slice-bogus :empty-slice-before :empty-slice-after :empty-slice-between)
	   (values 0 0 1))
	  
	  (:nonempty-slice
	   (destructuring-bind (start stop num) args
	     (declare (ignore num))
	     (values start (1+ stop) 1)))
	  
	  (:nonempty-stepped-slice
	   (destructuring-bind (start stop step num) args
	     (declare (ignore num))
	     (values start stop step)))))
    
    (make-tuple-from-list (list start stop step))))


;; XXX THis comment ignores fourth value for extended stepped slices
;; 
;; Function SLICE-INDICES returns multiple values, best explained by example:
;; Assume x = [0,1,2] so LENGTH = 3
;; 
;;  -slice- -extr- -assignment-                -values- 
;;  x[:0]    []     x[:0] = [42] => [42,0,1,2] :empty-slice-before
;;  x[3:]    []     x[3:] = [42] => [0,1,2,42] :empty-slice-after
;;  x[1:1]   []     x[1:1] = [42]=> [0,42,1,2] :empty-slice-between 0 1  = BEFORE-I, AFTER-I
;;  x[1:2]   [1]    x[1:2] = [42]=> [0,42,2]   :nonempty-slice 1 1       = FIRST-I, LAST-I
;;  x[1:]    [1,2]  x[1:] = [42] => [0,42]     :nonempty-slice 1 2
;;  x[:1]    [0]    x[:1] = [42] => [42,1 2]   :nonempty-slice 0 0
;;  x[:] [0,1,2](copy) x[:] = [42] => [42](modifies) :nonempty-slice 0 2
;;  x[2:1]   []     (error)                    :empty-slice-bogus
;;  x[100:200] []                              :empty-slice-bogus
;; 
;; These have implicit step=1. Other steps result in a `stepped-slice'
;; Now with other step values, using x = range(10) = [0,1,2,3,4,5,6,7,8,9] so LENGTH = 10
;;
;;  -slice-   -extr-       -assignment-                -values- 
;;  x[::0]    - - - - - step=0 is error - - - - - - - - - - - - - - - -
;;  x[::2]    [0,2,4,6,8]  :extended-slice 0 9 2       = FIRST LAST STEP
;;  x[::-2]   [9,7,5,4,1]  x[..] = [1,2,3,4,5] =>      :nonempty-stepped-slice 9 1 -2
;;                           [0, 5, 2, 4, 4, 3, 6, 2, 8, 1]
;;  x[0:2:-1] []           (error)                     (error)
;;  x[100:200:2] []        (error)                     :empty-slice-bogus
;;  x[0:0:4]  []           x[..] = [] => ok            :empty-slice-bogus
;;  x[0:1:4]  [0]          x[..] = [42] => [42, 2, ..] :nonempty-stepped-slice 0 0 4
;; 
;; Assigning to a stepped-slice can only when the thing assigned
;; containes the same number of items as teh stepped slice. For an
;; empty-slice-bogus, this means only the empty list (or another
;; iterable containing 0 items) can be assigned to it.

(defmethod slice-indices ((x py-slice) (length integer))
  "Return three integers: START, STOP, STEP.
START and END are _inclusive_, absolute indices >= 0. STEP is != 0."
  (let* ((start      (or (slot-value x 'start) *the-none*)) ;; not with-slots: assigned to
	 (stop       (or (slot-value x 'stop)  *the-none*))
	 (step       (or (slot-value x 'step)  *the-none*))
	 reversed-p)
    
    (setf step  (if (eq step *the-none*) 1 (py-val->integer step))
	  reversed-p (< step 0)
	  start (if (eq start *the-none*) 
		    (if reversed-p (1- length) 0)
		  (py-val->integer start))
	  stop  (if (eq stop *the-none*)
		    (if reversed-p -1 length)
		  (py-val->integer stop)))
    
    (assert (every #'integerp (list start stop step)))
    #+(or)(warn "a: start,stop = ~A,~A" start stop)
    (when (< start 0) (incf start length))
    (when (< stop  0) (unless (and reversed-p (= stop -1)) ;; XXX right?
			(incf stop length)))
    #+(or)(warn "b: start,stop = ~A,~A" start stop)
    (cond ((= step 0)
	   (py-raise '{ValueError} "Slice step cannot be zero (got: ~S)" x))
	  
	  ((or (and (> step 0) (or (> start length)
				   (< stop 0)
				   (> start stop)))
	       (and (< step 0) (or (< start 0)
				   (>= stop length)
				   (< start stop))))
	   (values :empty-slice-bogus))
	    
	  ((= step 1)
	   (setf start (max 0 start)
		 stop (min length stop))
	   (assert (<= 0 start stop length))
	   (cond ((= start stop)
		  (cond ((= start 0)      (values :empty-slice-before))
			((= start length) (values :empty-slice-after))
			(t                (values :empty-slice-between (1- start) start))))
		 
		 ((< start stop)
		  (values :nonempty-slice start (1- stop) (- stop start)))))
	  
	  ((/= step 1)
	   (cond ((= start stop) (values :empty-slice-bogus))
		   
		 ((and (> step 0) (< start stop))
		  (let* ((start (max start 0))
			 (stop  (min (1- stop) (1- length)))
			 (num-increments (floor (- stop start) step))
			 (real-stop (+ start (* step num-increments))))
		    
		    (assert (<= 0 start real-stop stop (1- length)) ()
		      "not (<= 0 start real-stop stop (1- length)): ~A"
		      `(<= 0 ,start ,real-stop ,stop (1- ,length)))
		    
		    (values :nonempty-stepped-slice
			    start real-stop step (1+ num-increments))))
		 
		 ((and (< step 0) (> start stop))
		  (let* ((start (min start (1- length)))
			 (stop  (max (incf stop) 0))
			 (num-increments (floor (- stop start) step))
			 (real-stop (- start (* (- step) num-increments))))
		    
		    (assert (<= 0 stop real-stop start (1- length)) ()
		      "not (<= 0 stop real-stop start (1- length)): ~A"
		      `(<= 0 ,stop ,real-stop ,start (1- ,length)))
		    
		    (values :nonempty-stepped-slice
			    start real-stop step (1+ num-increments))))
		 
		 (t (break :unexpected))))
	  
	  (t (break :unexpected)))))

;; super( <B class>, <C instance> ) where C derives from B:
;;   :object = <C instance>
;;   :current-class = <B class>
;; 
;; A typical use for calling a cooperative superclass method is:
;; 
;;  class C(B):
;;    def meth(self, arg):
;;      super(C, self).meth(arg)

(defclass py-super (py-core-object) ;; subclassable?
  ((object        :initarg :object)
   (current-class :initarg :current-class))
  (:metaclass py-core-type))

(def-py-method py-super.__new__ :static (cls class-arg &optional second-arg)
  (cond ((not (typep class-arg 'class))
	 (py-raise '{TypeError}
		   "First arg to super.__new__() must be class (got: ~A)"
		   class-arg))
	
	((null second-arg)
	 (warn "super() with one arg is TODO (faking for now)")
	 (lambda (sec-arg) (py-super.__new__ cls class-arg sec-arg)))
	
	((typep second-arg class-arg)
	 ;; like:  super( <B class>, <C instance> )
	 ;; in the CPL of class C, find the class preceding class B
	 ;; 
	 ;; CPython returns a `super' instance and not directly the
	 ;; class to allow subclassing class `super' and overriding
	 ;; the __getattribute__ method.
	 (make-instance cls :object second-arg :current-class class-arg))
	
	((typep second-arg 'class)
	 (unless (or (eq second-arg class-arg) ;; <- efficiency optimization
		     (subtypep second-arg class-arg))
	   (py-raise '{TypeError}
		     "When calling `super' with two classes: second must be ~
                                   subclass of first (got: ~A, ~A)" class-arg second-arg))
	 (make-instance cls :object second-arg :current-class class-arg))
	
	(t (error "TODO super clause?"))))


(def-py-method py-super.__getattribute__ (x attr)
  (assert (stringp attr))
  (flet ((find-remaining-mro (mro cls)
	   #+(or)(warn "find-preceding-class-in-mro ~A ~A..." mro cls)
	   (let ((res (loop until (eq (pop mro) cls)
			  finally (return mro))))
	     #+(or)(warn "-> ~A" res)
	     res)))
    
    ;; XXX check when attribute is static method and super is bound to class, etc
    (with-slots (object current-class) x
      
      (let* ((remaining-mro (find-remaining-mro (py-type.__mro__ (if (typep object 'class)
								     object
								   (py-class-of object)))
						current-class))
	     (class (car remaining-mro))
	     (attr.sym (ensure-user-symbol attr))
	     (val (recursive-class-dict-lookup class attr.sym remaining-mro)))
	
	(if val
	    (bind-val val
		      (if (eq object class) *the-none* object)
		      class)
	  (py-raise '{AttributeError}
		    "No such attribute found for `super' object: ~S.~S; ~
                     looked up attr ~S in class ~S" x attr attr class))))))

(def-py-method py-super.__repr__ (x^)
  (with-output-to-string (s)
    (print-unreadable-object (x s :type t :identity t)
      (with-slots (object current-class) x
	(format s ":object ~S  :current-class ~A" object current-class)))))
  
(defmethod print-object ((x py-super) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (with-slots (object current-class) x
      (format stream ":object ~S  :current-class ~A" object current-class))))

(defclass py-xrange (py-core-object)
  (start stop step)
  (:metaclass py-core-type))

(finalize-inheritance (find-class 'py-xrange))

(def-py-method py-xrange.__init__ (x &rest args)
  (with-slots (start stop step) x
    (setf start nil
	  stop nil
	  step nil)
    (ecase (length args)
      (0 (py-raise '{TypeError} "xrange: >= 1 arg needed"))
      (1 (unless (>= (car args) 0)
	   (break "xrange: invalid 1-arg: ~A" (car args)))
	 (setf start 0 
	       stop (car args)
	       step 1))
      (2 (break "xrange 2 todo"))
      (3 (break "xrange 3 todo")))))

(def-py-method py-xrange.__iter__ (x^)
  (with-slots (start stop step) x
    (let ((i start))
      (make-iterator-from-function :name :xrange-iterator
				   :func (lambda ()
					   (unless (or (and (<= start stop) (>= i stop))
						       (and (<= stop start) (<= i stop)))
					     (prog1 i
					       (incf i step))))))))

(def-py-method py-xrange.__str__ (x^)
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t :type t)
      (with-slots (start stop step) x
	(format s ":start ~A :stop ~A :step ~A" start stop step)))))

;; None

(defun none-p (x) (eq x *the-none*))

(def-py-method py-none.__repr__ (x)
  (declare (ignore x))
  "None")

(def-py-method py-none.__nonzero__ (x)
  (declare (ignore x))
  *the-false*)
   
;; Ellipsis

(defclass py-ellipsis (py-core-type) () (:metaclass py-core-type))
(defvar *the-ellipsis* (make-instance 'py-ellipsis))

;; NotImlemented

(defclass py-notimplemented (py-core-type) () (:metaclass py-core-type))
(defvar *the-notimplemented* (make-instance 'py-notimplemented))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User objects (Object, Module, File, Property)

;; Object (User object)

(defun bind-val (val x x.class)
  #+(or)(break "bind-val ~A ~A" val x)
  (let ((get-meth (recursive-class-dict-lookup
		   (py-class-of val) '{__get__})))
    (if get-meth
	(py-call get-meth val (or x *the-none*) (or x.class *the-none*))
      val)))

;; ACL 7.0 has an issue with compiler-macro + notinline.
#+(and allegro-version>= (version>= 8 0))
(define-compiler-macro bind-val (val x x.class)
  `(locally (declare (notinline bind-val))
     (let ((.val ,val)
	   (.x ,x))
       (if (and (functionp .val)
		(not (eq .x *the-none*)))
	   (progn 
	     #+(or)(warn "bind-val ~S ~S -> bound method" val x)
	     (make-instance 'py-bound-method :instance .x :func .val))
	 (bind-val .val .x ,x.class)))))

(defun class-precedence-list (c)
  #+allegro (mop:class-precedence-list c)
  #+lispworks (clos:class-precedence-list c))
  
(defun py-get-dict-attr (x x.class)
  (or (dict x)
      (loop for c in (class-precedence-list x.class)
	  when (dict c) return it)
      (error "XXX No dict found in object ~S or its class ~A?!" x x.class)))


(defvar *py-object.__getattribute__-active* ())

(def-py-method py-object.__getattribute__ (x attr)
  (assert (stringp attr))
  ;; ATTR may be a symbol, a string, or instance of user-defined subclass of string
  (when (string= (py-val->string attr) "__dict__")
    (return-from py-object.__getattribute__ (dict x)))
  
  (let* ((attr.sym (ensure-user-symbol attr))
	 (*py-object.__getattribute__-active*
	  (cons (cons x attr.sym) *py-object.__getattribute__-active*)))
    (py-attr x attr.sym)))


(defvar *py-object.__setattr__-active* ())

(def-py-method py-object.__setattr__ (x attr^ val)
  (check-type attr (or string symbol))
  (let ((attr.sym (if (stringp attr) (ensure-user-symbol attr) attr))
       (*py-object.__setattr__-active* (cons (cons x attr) *py-object.__setattr__-active*)))
    (setf (py-attr x attr.sym) val)))


(defvar *py-object.__delattr__-active* ())

(def-py-method py-object.__delattr__ (x attr)
  (check-type attr (or string symbol))
  (let ((attr.sym (if (stringp attr) (ensure-user-symbol attr) attr))
	(*py-object.__delattr__-active* (cons (cons x attr) *py-object.__delattr__-active*)))
    (setf (py-attr x attr.sym) nil)))

(def-py-method py-object.__new__ :static (cls &rest attr)
  (declare (ignore attr))
  (make-instance cls))

(def-py-method py-object.__init__ (&rest attr)
  (declare (ignore attr)))

(def-py-method py-object.__class__ :attribute-read (x)
  (py-class-of x))

(def-py-method py-object.__class__ :attribute-write (x new-class)
  (assert (classp new-class))
  (change-class x new-class))

(def-py-method py-object.__repr__ (x)
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t :type nil)
      (format s "~A" (type-of x)))))

(def-py-method py-object.__nonzero__ (x)
  ;; By default, objects are nonzero. Override in subclass.
  (declare (ignore x))
  (py-bool t))

(defun recursive-class-dict-lookup (cls attr &optional cls-list)
  ;; Look for ATTR in class CLS and all its superclasses.
  ;; and finally (which is in this implementation not a superclass of a class).
  (assert (symbolp attr))
  #+(or)(format t "recursive-class-dict-lookup: ~A ~A~%" attr cls)
  #+(or)(assert (typep cls 'class))
  (loop for c in (or cls-list (class-precedence-list cls))
      until (or (eq c (ltv-find-class 'standard-class))
  		(eq c (ltv-find-class 'py-dict-mixin))
                (eq c (ltv-find-class 'standard-generic-function))) ;; condition copied from py-attr 
	    
      do (let* ((d (dict c))
		(val (when d
		       (sub/dict-get d attr))))
           (when val
	     (return-from recursive-class-dict-lookup val)))

      finally ;; this seems not needed, in the finally clause
	(let ((obj-attr (sub/dict-get (load-time-value (dict (find-class 'py-object))) attr)))
	  (when obj-attr
	    #+(or)(warn "rec van py-object: ~A (cpl: ~A)" attr (class-precedence-list cls))
	    (return obj-attr)))))
	

(defun recursive-class-lookup-and-bind (x attr)
  #+(or)(warn "recursive-class-lookup-and-bind ~A ~A" x attr)
  (let* ((x.cls (py-class-of x))
	 (val   (recursive-class-dict-lookup x.cls attr)))
    (when val
      (bind-val val x x.cls))))
    
(def-py-method py-object.__get__ (value instance class)
  (declare (ignore instance class))
  value)

;; Type (User object)

(def-py-method py-type.__new__ :static (metacls name supers^ dict)
  
  ;; deproxy supers, as it may be *the-empty-tuple* instance
  (unless (symbolp name)
    (let ((str (deproxy name)))
      (if (stringp str)
	  (setf name (ensure-user-symbol str))
	(py-raise '{TypeError} "Invalid class name: ~A" name))))
  
  (let* ((cls-type (if (and (some (lambda (s) (let ((pt (ltv-find-class 'py-type)))
						(or (eq s pt)
						    (subtypep s 'py-type))))
				  supers)
			    (eq metacls 'py-type)) ;; XXX (subtypep meta pytype)?
		       :metaclass
		     :class))
	 
	 (c (ensure-class
	     (make-symbol (symbol-name name)) 
	     
	     :direct-superclasses (if supers
				      (let ((res (subst (ltv-find-class 'py-user-object)
							(ltv-find-class 'py-object)
							supers)))
					(unless (member (ltv-find-class 'py-user-object) res :test #'eq)
					  (nconc res (load-time-value (list (find-class 'py-user-object)))))
					res)
				    (load-time-value (list (find-class 'py-user-object))))
	     
	     :metaclass (ecase cls-type
			  (:metaclass (ltv-find-class 'py-meta-type))
			  (:class     metacls))
	     :dict dict)))
    
    (finalize-inheritance c)
    
    c))

(def-py-method py-type.__init__ (cls &rest args)
  (declare (ignore cls args))
  nil)

(def-py-method py-type.__name__ :attribute (cls)
  ;; XXX remove prefix `py-' etc
  (symbol-name (class-name cls)))

(def-py-method py-type.__dict__ :attribute-read (cls)
  (dict cls))

(def-py-method py-type.__dict__ :attribute-write (cls new-dict)
  ;; XXX check NEW-DICT is of (sub)type PY-DICT.
  (setf (slot-value cls 'dict) new-dict)
  (signal-class-dict-replacement cls (dict cls) new-dict))

(def-py-method py-type.__bases__ :attribute-read (cls)
  (assert (classp cls))
  (let* ((supers (class-direct-superclasses cls)))
    (assert supers () "Class ~A has no direct superclasses?!" cls)
    (make-tuple-from-list (remove (ltv-find-class 'py-user-object) supers))))

(def-py-method py-type.__bases__ :attribute-write (cls bases^)
  (assert (classp cls))
  (let ((bases (py-iterate->lisp-list bases)))
    (ensure-class-using-class cls (class-name cls) 
                              :direct-superclasses bases)
    cls))

(defvar *mro-filter-implementation-classes* t
  "Whether to filter reasonable intermediate classes from the returned MRO.
For example, `py-int' derives in this implementation from `py-real' and `py-number',
but the latter two classes are not in CPython.")

(def-py-method py-type.__mro__ :class-attribute (x)
  ;; We could filter out the Lisp implementation classes.
  (make-tuple-from-list
   (loop for cls in (class-precedence-list x)
       until (or (eq cls (ltv-find-class 'standard-class))
		 (eq cls (ltv-find-class 'standard-object)))
       unless (member (class-name cls) '(py-user-object
                                         py-lisp-object
                                         py-dictless-object
                                         py-dict-mixin
                                         t))
       unless (and *mro-filter-implementation-classes*
                   (member (class-name cls) '(py-real py-number)))
       collect cls into res
       finally (let ((base-cls (if (subtypep x 'py-meta-type)
				   'py-type
				 'py-object)))
		 (unless (member base-cls res :key #'class-name)
		   (nconc (last res) (list (find-class base-cls))))
		 (return res)))))

(defun class-direct-subclasses (c)
  #+allegro (mop:class-direct-subclasses c)
  #+lispworks (clos:class-direct-subclasses c))

(def-py-method py-type.__subclasses__ (x)
  (make-py-list-from-list (class-direct-subclasses x)))

(defvar *class-display-without-py-prefix* t
  "Whether to show class names without `py-' prefix: `int' instead of `py-int' etc.")

(def-py-method py-type.__repr__ (x) ;; XXX deproxy not needed
  (let ((name (if *class-display-without-py-prefix*
                  (let ((n (symbol-name (class-name x))))
                    (if (and (> (length n) 3)
                             (string-equal (subseq n 0 3) "py-"))
                        (subseq n 3)
                      n))
                x)))
    (with-output-to-string (s)
      (print-unreadable-object (x s :identity t)
        (format s "~@[meta~*~]class ~A"
                (typep x (ltv-find-class 'py-meta-type))
                name)))))

(def-py-method py-type.__str__ (x) ;; XXX deproxy not needed
  (py-type.__repr__ x))

(def-py-method py-type.__call__ (cls &rest args)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  
  (cond ((and (eq cls (ltv-find-class 'py-type))
	      args
	      (not (cdr args)))
	 (return-from py-type.__call__
	   (py-class-of (car args))))
	
	((eq cls (ltv-find-class 'py-object))
	 ;; object() -> an instance without __dict__
	 (return-from py-type.__call__
	   (make-instance 'py-dictless-object)))
	
	(t (if (eq cls (ltv-find-class 'py-type))
	       
	       ;; Inline common case: creating new classes with TYPE as requested metaclass
	       (apply #'py-type.__new__ cls args)
	     
	     (let* ((__new__ (recursive-class-dict-lookup cls '{__new__}))
                    (bound-new (if (eq (class-of __new__) (ltv-find-class 'py-static-method))
                                   __new__ ;; inline common case
                                 (bind-val __new__ cls (py-class-of cls))))
		    (inst    (apply #'py-call bound-new cls args))) ;; including CLS as arg!
	       
	       (when (or (eq (class-of inst) cls) ;; <- Efficiency optimization
			 (subtypep (py-class-of inst) cls)) ;; <- real test
		 ;; Don't run __init__ when inst is not of type cls
                 (let ((__init__-unbound (recursive-class-dict-lookup cls '{__init__})))
                   (if (functionp __init__-unbound)
                       (apply __init__-unbound inst args) ;; inline common case
                     (let ((__init__ (recursive-class-lookup-and-bind inst '{__init__})))
                       (apply #'py-call __init__ args)))))
	       
	       inst)))))

;; Module (User object)

(defclass py-module (py-user-object) ;; no dict-mixin!?
  ((globals-names  :initarg :globals-names  :type vector :initform #())
   (globals-values :initarg :globals-values :type vector :initform #())
   (dyn-globals    :initarg :dyn-globals    :type hash-table
		   :initform (make-hash-table :test #'eq))
   (name           :initarg :name           
		   :type string
		   :initform "__main__" 
		   :accessor module-name
		   :documentation "The (dotted) module name")
   (filepath       :initarg :path           :initform nil :accessor module-filepath :type pathname)
   (builtinp       :initarg :builtin        :initform nil)
   (packagep       :initarg :package        :initform :maybe :accessor module-package-p))
  (:metaclass py-user-type))

;; XXX module has dict attribute, but it is not updated correctly

(defun make-module (&rest options)
  (apply #'make-instance 'py-module options))

(def-py-method py-module.__new__ :static (cls name)
	       (let* ((x (make-instance cls)))
		 (sub/dict-set (dict x) '{__name__} name)
		 x))

(def-py-method py-module.__init__ (&rest args)
	       (declare (ignore args))
	       nil)

(def-py-method py-module.__repr__ (x^)
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t)
      (with-slots (name filepath builtinp) x
	(format s "~@[~A ~]module `~A'~@[ from file ~S~]"
		(when builtinp "builtin") 
		name
		(unless builtinp filepath))))))

(defun slot-definition-name (s)
  #+allegro (mop:slot-definition-name s)
  #+lispworks (clos:slot-definition-name s))

(defun class-slots (c)
  #+allegro (mop:class-slots c)
  #+lispworks (clos:class-slots c))

(defun copy-module-contents (&key from to)
  (check-type from py-module)
  (check-type to py-module)
  (dolist (s (mapcar #'slot-definition-name
		     (class-slots (find-class 'py-module))))
    (setf (slot-value to s) (slot-value from s)))
  t)

(defmethod print-object ((x py-module) stream)
  (write-string (py-module.__repr__ x) stream))

(defun py-module-get-items (x &key import-*)
  (check-type x py-module)
  (flet ((return-name-p (name)
	   (when (symbolp name)
	     (setf name (symbol-name name)))
	   (check-type name string)
	   (or (not import-*)
	       (char/= (aref name 0) #\_))))
    (with-slots (globals-names globals-values dyn-globals) x
      (let ((full-list (nconc (loop for k across globals-names and v across globals-values
				  when (and v (return-name-p k))
				  collect (cons k v))
			      (loop for k being the hash-key in dyn-globals
				  using (hash-value v)
				  when (return-name-p k)
				  collect (cons k v)))))
      (when import-*
	;; Only those names listed in the module's __all__ attribute
	;; XXX Error when name in __all__ not bound?
	(let* ((__all__ (cdr (assoc '{__all__} full-list))))
	  (when __all__
	    (let ((all-names-list (py-iterate->lisp-list __all__)))
	      (setf full-list 
		(delete-if-not (lambda (kv) (member (car kv) all-names-list :test #'string=))
			       full-list))))))

      full-list))))

(defun py-module-set-kv (x k v)
  ;; V may be null, to remove item
  (check-type x py-module)
  (check-type k symbol)
  (with-slots (globals-names globals-values dyn-globals) x
    (loop for kn across globals-names and i from 0
	when (eq k kn) 
	do (setf (aref globals-values i) v)
	   (return)
	finally
	  (if (null v)
	      (remhash k dyn-globals)
	    (setf (gethash k dyn-globals) v)))))

(def-py-method py-module.__getattribute__ (x^ attr)
  (flet ((raise-attr-error (attr)
	   (py-raise '{AttributeError} "Module ~A has no attribute ~A" x attr)))
    
    (let ((attr.sym (py-string-val->symbol attr :intern nil)))
      (with-slots (globals-names globals-values dyn-globals) x
	
	(when attr.sym 
	  ;; If symbol not interned, then it cannot be in the globals-names vector
	  (let ((i (position attr.sym globals-names :test #'eq)))
	    (if i
		(let ((val (svref globals-values i)))
		  (if (null val)
		      (raise-attr-error attr)
		    (return-from py-module.__getattribute__ val))))))
	
	(let ((val (gethash (or attr.sym attr) dyn-globals)))
	  (when val
	    (return-from py-module.__getattribute__ val))))
      
      (raise-attr-error (or attr.sym attr)))))

(def-py-method py-module.__setattr__ (x^ attr val)
  (when (slot-value x 'builtinp)
    (warn "Setting attribute '~A' on built-in module ~A." attr x))
  (set-module-attr x attr val))
  
(defun set-module-attr (x attr val)
  "Returns new value"
  (check-type x py-module)
  (let ((attr.sym (py-string-val->symbol attr :intern nil)))
    (with-slots (name globals-names globals-values dyn-globals) x

      ;; XXX Does x.attr.__set__ play a role? (Think not)
      (when attr.sym
	;; If symbol not interned, then it cannot be in the globals-names vector
	(let ((i (position attr.sym globals-names :test #'eq)))
	  (when i
	    (unless (or val (svref globals-names i))
	      (py-raise 'Attribute "Module ~A has no attribute ~A to delete" x attr))
	    (setf (svref globals-values i) val)
	    (return-from set-module-attr val))))
      
      (progn
	(unless (or val (gethash (or attr.sym attr) dyn-globals))
	  (py-raise '{AttributeError} "Module ~A has no attribute ~A to delete" x attr))
	(if val
	    (setf (gethash (or attr.sym attr) dyn-globals) val)
	  (remhash (or attr.sym attr) dyn-globals))
	val))))
  
(def-py-method py-module.__delattr__ (x^ attr)
  (when (slot-value x 'builtinp)
    (warn "Deleting attribute '~A' on built-in module ~A." attr x))
  (set-module-attr x attr nil)
  *the-none*)

(defun create-python-module (code)
  (check-type code string)
  (let* ((ast (parse code))
         (mod nil)
         (*module-hook* (lambda (m) (setf mod m))))
    (declare (special *module-hook*))
    (funcall (compile nil `(lambda () ,ast)))
    mod))

;; Lisp Packages can be used like Python modules
;;
;; The rule regarding their attributes exposed to the Python world:
;;  a Lisp package has an attribute `x' <-> the package has external symbol `x'.
;; 
;; We supply a few modules that resemble the ones in CPython. But because
;; our implementation may still be incomplete, we have the following rule
;; regarding unfinished functions and variables:
;;
;;  - If the value of an attribute is `:todo', it is not implemented at all.
;;    An error of type {NotImplementedError} is signalled.
;;
;;  - if the value is a list of two items, with `:todo' as first value,
;;    as string as second value, and something non-nil as third value, this
;;    communicates that the attribute is available, but not implemented 100%
;;    yet. A warning is signalled.
;;
;; If we encounter a value :todo upon retrieving an attribute, 

(defconstant +impl-status-prop+         'clpython::.impl-status.)
(defconstant +impl-status-comment-prop+ 'clpython::.impl-status-comment.)
(defconstant +impl-warned-prop+         'clpython::.impl-warned.)

(defconstant +impl-statuses+ '((t           . "complete")
                               (:incomplete . "incomplete")
                               (:todo       . "todo")
			       (:n/a        . "not applicable")
                               (nil         . "unknown status")))

(defun impl-status (symbol &optional want-comment)
  "Returns impl status of symbol, and optionally the corresponding comment."
  (check-type symbol symbol)
  (let ((status (get symbol +impl-status-prop+)))
    (unless (member status +impl-statuses+ :key #'car)
      (warn "Retrieved implementation status ~S, which is an ~
invalid status; must be one of ~S"
      status (mapcar #'car +impl-statuses+)))
    (if want-comment
	(values status (get symbol +impl-status-comment-prop+))
      status)))

(defun set-impl-status (symbol new-status &optional comment)
  "Sets implementation status of attribute, and optionally records comment."
  (assert (or (symbolp symbol) (listp symbol)))
  (assert (member new-status +impl-statuses+ :key #'car)
      (new-status) "~S is an invalid implementation status; must be one of ~S"
      new-status (mapcar #'car +impl-statuses+))
  (flet ((set-one-sym (sym)
	   (multiple-value-bind (sym kind)
	       (find-symbol (symbol-name sym) (symbol-package sym))
	     (unless (eq kind :external)
	       (error "Cannot set impl status: symbol `~S' is not external in ~
 package ~S (status: ~A)." sym (symbol-package sym) kind)))
	   (setf (get sym +impl-status-prop+) new-status)
	   (when comment
	     (setf (get sym +impl-status-comment-prop+) comment))))
    (if (listp symbol)
	(dolist (x symbol)
	  (set-one-sym x))
      (set-one-sym symbol)))
  new-status)

(defun package-impl-status (pkg)
  "Returns ALIST, COMPLETENESS, DETAILS
ALIST: e.g. ((:incomplete . 2) (:todo . 20) (:n/a . 3) (nil . 1) (t . 10))
COMPLETENESS: ratio between 0 and 1, or NIL if unknown
DETAILS: alist ((:incomplete . (foo bar)) (:todo . (baz)))"
  (let ((ht (make-hash-table :test 'eq)))
    (do-external-symbols (s pkg)
      (let ((st (clpython::impl-status s)))
        (push s (gethash st ht))))
    
    (multiple-value-bind (summary details)
        (loop for k in (mapcar #'first +impl-statuses+)
            collect (cons k (length (gethash k ht))) into summary
            collect (cons k (gethash k ht)) into details
            finally (return (values summary details)))
      
      (let ((completeness (let ((complete (+ (length (gethash t ht))
                                             (length (gethash :n/a ht))))
                                (half-complete (length (gethash :incomplete ht)))
                                (missing (length (gethash :todo ht))))
                            (if (> (+ complete half-complete missing) 0)
                                (/ (+ complete (* 1/2 half-complete))
                                   (+ complete half-complete missing))
                              nil))))
        
        (values summary completeness details)))))

(defclass lisp-package (py-core-object)
  ()
  (:metaclass py-core-type))

(defun relative-package-name (pkg &optional relative-to)
  "Returns part of package name after last dot; or full name if no dot in name.
But if RELATIVE-TO package name is given, result may contains dots."
  (let* ((name (package-name pkg)))
    (if relative-to
        (let* ((rel-name (package-name relative-to))
               (rel-name-len (length rel-name)))
          (if (and (> (length name) (1+ rel-name-len))
                   (string-equal (subseq name 0 rel-name-len) rel-name)
                   (char= (aref name rel-name-len) #\.))
              (subseq name (1+ rel-name-len))
            name))
      (let ((dot-ix (position #\. name :from-end t)))
        (if dot-ix
            (subseq name (1+ dot-ix))
          name)))))

#||
(assert (string-equal (clpython::relative-package-name :clpython.module.math)
                      "math"))
(assert (string-equal (clpython::relative-package-name :clpython.module.math
                                                       :clpython.module)
                      "math"))
(assert (string-equal (clpython::relative-package-name :clpython.module.math
                                                       :clpython)
                      "module.math"))
||#

(def-py-method lisp-package.__repr__ (pkg)
  (with-output-to-string (s)
    (print-unreadable-object (pkg s :identity t :type nil)
      (format s "module ~A (package ~S with ~A external symbols)"
              (relative-package-name pkg)
              (package-name pkg)
              (loop for s being the external-symbol in pkg count s)))))

(def-py-method lisp-package.__getattribute__ (pkg name)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (assert (stringp name))
  (flet ((todo-error ()
	       (py-raise '{NotImplementedError} 
			  "Attribute `~A' of module `~A' is not implemented yet."
			  name (relative-package-name pkg)))
	     
	     (n/a-error ()
	       (py-raise '{NotImplementedError}
			  "Attribute `~A' of module `~A' is not applicable for this ~
 implementation, therefore it is not implemented."
			  name (relative-package-name pkg)))
	     
	     (unbound-error ()
	       (py-raise '{AttributeError}
			  "Attribute `~A' of module `~A' is unbound."
			  name (relative-package-name pkg)))
	     
	     (no-attr-error ()
	       (py-raise '{AttributeError}
			  "Module `~A' has no attribute named `~A'."
			  (relative-package-name pkg) name))

	     (incomplete-warning ()
	       (warn "Attribute `~A' of module `~A' was accessed. ~
 The implementation of this attribute is still incomplete, therefore ~
 unexpected results may occur."
		     name (relative-package-name pkg)))

             (bound-in-some-way (sym)
               (cond ((boundp sym)
                      (symbol-value sym))
                     ((fboundp sym)
                      (symbol-function sym))
                     (t nil))))
    ;(declare (dynamic-extent todo-error n/a-error unbound-error no-attr-error))
    (multiple-value-bind (sym kind)
	(find-symbol name pkg)
      (unless sym
	(no-attr-error))
      (when (member kind '(:inherited :internal))
        (cerror (let ((*package* (find-package :common-lisp)))
                  (format nil "Use the value of the internal symbol ~S: ~A."
                          sym (or (bound-in-some-way sym) "<unbound>")))
                "Package `~A' has no attribute `~A': symbol is not external."
                (relative-package-name pkg) name))

      (ecase (impl-status sym)
	((nil t) )
	((:todo)       (todo-error))
	((:n/a)        (n/a-error))
	((:incomplete) (unless (get sym +impl-warned-prop+)
			 (setf (get sym +impl-warned-prop+) t)
			 (incomplete-warning))))
      
      (let ((val (or (bound-in-some-way sym) (unbound-error))))
	val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property (User object)

(defclass py-property (py-user-object)
  (fget fset fdel doc)
  (:metaclass py-user-type))

;; standard __new__ behaviour

(def-py-method py-property.__init__ (x &rest args)
  (with-parsed-py-arglist ("property.__init__" (fget fset fdel doc) args)
    (setf (slot-value x 'fget) (or fget *the-none*)
	  (slot-value x 'fset) (or fset *the-none*)
	  (slot-value x 'fdel) (or fdel *the-none*)
	  (slot-value x 'doc)  (or doc  ""))))

(def-py-method py-property.__get__ (x obj class)
  (declare (ignore class))
  (with-slots (fget) x
    (if (eq fget *the-none*)
	(py-raise '{AttributeError} "Cannot get attribute")
      (py-call fget obj))))

(def-py-method py-property.__set__ (x obj val)
  (with-slots (fset) x
    (if (eq fset *the-none*)
	(py-raise '{AttributeError} "Cannot set attribute")
      (py-call (slot-value x 'fset) obj val)))) ;; bind?

(def-py-method py-property.__del__ (x obj)
  (with-slots (fdel) x
    (if (eq fdel *the-none*)
	(py-raise '{AttributeError} "Cannot delete attribute")
      (py-call (slot-value x 'fdel) obj))))

(def-py-method py-property.fget :attribute (x)
  (slot-value x 'fget))

(def-py-method py-property.fset :attribute (x)
  (slot-value x 'fset))	       

(def-py-method py-property.fdel :attribute (x)
  (slot-value x 'fdel))

(def-py-method py-property.__doc__ :attribute (x)
  (slot-value x 'doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Lisp objects (proxies around Lisp values: number, string, list, tuple, dict)

(defvar *proxy-classes* ())

(defun deproxy (x)
  (typecase x
    ((or number string list function vector hash-table)
     x)
    (py-lisp-object
     (proxy-lisp-val x))
    (t
     x)))

#+(or) ;; original version; a bit slower
(defgeneric deproxy (x)
  (:method ((x py-lisp-object))  (proxy-lisp-val x))
  (:method ((x t))               x))


(defmacro def-proxy-class (py-name &optional supers slots)
  `(progn (defclass ,py-name ,(or supers '(py-lisp-object))
	    ,slots
	    (:metaclass py-lisp-type))
	  (finalize-inheritance (find-class ',py-name))))

;; Number (Lisp object: number)

(def-proxy-class py-number)

;; Seems that no py-number.__new__ method is needed: Python does not
;; have a corresponding 'number' class, so user is always
;; instantiating a subclass (int, real, ...).

(def-py-method py-number.__abs__ (x^) (abs x))

(def-py-method py-number.__add__ (x^ y^) 
  (if (and (numberp x) (numberp y))
      (+ x y)
    (load-time-value *the-notimplemented*)))

(def-py-method py-number.__cmp__ (x^ y^)
  (cond ((not (and (numberp x) (numberp y)))
	 (load-time-value *the-notimplemented*))
	((= x y) 0) ;; also for complex
	((and (realp x) (realp y))
	 (cond ((< x y) -1)
	       (t       +1)))
	((or (complexp x) (complexp y))
	 (py-raise '{TypeError} "Cannot compare complexes"))))

(def-py-method py-number.__div__ (x^ y^)
  (if (and (numberp x) (numberp y))
      (/ x y) ;; overruled for integers
    (load-time-value *the-notimplemented*)))

(def-py-method py-number.__divmod__ (x y)
  (cond ((not (and (numberp x) (numberp y)))
	 (load-time-value *the-notimplemented*))
	(t
	 (make-tuple-from-list (list (py-/ x y) (py-% x y))))))

(def-py-method py-number.__hash__ (x^)
  (cond ((integerp x)   (sxhash x))
	
	((floatp x)
	 (multiple-value-bind (int-part float-part)
	     (truncate x)
	   (if (= float-part 0)
	       (sxhash int-part)
	     (sxhash x))))
	       
	((complexp x) (if (= (imagpart x) 0)
			  (sxhash (realpart x))
			(sxhash x)))
	
	(t (break :unexpected))))

(def-py-method py-number.__eq__  (x^ y^) (py-bool (and (numberp x) (numberp y) (= x y))))
(def-py-method py-number.__mul__ (x^ y^) 
  (if (and (numberp x) (numberp y))
      (* x y)
    (load-time-value *the-notimplemented*)))

(def-py-method py-number.__neg__ (x^) (- x))
(def-py-method py-number.__nonzero__ (x^) (py-bool (/= x 0)))
(def-py-method py-number.__pos__ (x^) x)
(def-py-method py-number.__pow__ (x^ y^ &optional z^) 
  (if z
      (progn (setf z (py-val->integer z))
	     (mod (expt x y) z))
    (if (numberp y)
        (expt x y)
      *the-notimplemented*)))

(def-py-method py-number.__repr__ (x^) (format nil "~A" (deproxy x)))
(def-py-method py-number.__str__ (x^)  (py-number.__repr__ x))
(def-py-method py-number.__sub__ (x^ y^) (- x y))

(def-py-method py-number.__truediv__ (x^ y^) (/ x y)) ;; overruled for integers

;; In CPython, only complex numbers have 'real','imag' and 'conjugate'
;; attributes; in our world all numbers have them.

(def-py-method py-number.real :attribute (x^) (realpart x))
(def-py-method py-number.imag :attribute (x^) (imagpart x))
(def-py-method py-number.conjugate       (x^) (conjugate x))

;; Complex

(def-proxy-class py-complex (py-number))

(def-py-method py-complex.__new__ :static (cls &optional (real 0) (complex 0))
	       (let ((c (complex real complex)))
		 (if (eq cls (ltv-find-class 'py-complex))
		     c
		   (make-instance 'cls :lisp-object c))))

(def-py-method py-complex.__repr__ (x^)
  (let ((r (realpart x))
	(i (imagpart x)))
    (cond ((= i 0)
	   (error :unexpected)) ;; due to CL simplification rules
	  ((= r 0)
	   (format nil "~Aj" (py-repr-string i)))
	  ((> i 0)
	   (format nil "(~A+~Aj)" (py-repr-string r) (py-repr-string i)))
	  (t
	   (format nil "(~A-~Aj)" (py-repr-string r) (py-repr-string (- i)))))))
  
;; Real

(def-proxy-class py-real (py-number))
(def-py-method py-real.__mod__ (x^ y^) (mod x y))

;; Integer

(def-proxy-class py-int (py-real))

(def-py-method py-int.__new__ :static (cls &optional (arg 0) (base 0))
  ;; If base = 0, then derive base from literal ARG, or use base = 10.
  
  (flet ((invalid-arg-error (a)
	   (py-raise '{TypeError} "Invalid arg for int.__new__: ~S" a)))
    
    (let ((val (typecase arg
		 (integer arg)
		 (float  (truncate arg))
		 (t      (setf arg (py-val->string arg)
			       base (py-val->integer base :min 0))
			 
			 (flet ((read-arg (arg &optional (base 10))
				  ;; Can't use (parse-integer arg :radix base)
				  ;; because that does not accept "1.2".
				  ;; Can't use (parse-integer arg :radix base :junk-allowed t)
				  ;; because that accepts "1.2asdjfkalsjdf".
				  (let ((v (with-standard-io-syntax
					     (let ((*read-base* base))
					       (read-from-string arg)))))
				    (if (numberp v)
					(truncate v)
				      (invalid-arg-error arg)))))
			   
			   (cond ((and (>= (length arg) 2)
				       (char= (aref arg 0) #\0)
				       (member (aref arg 1) '(#\x #\X) :test #'char=))
				  (read-arg (subseq arg 2) 16))
			  
				 ((and (= (length arg) 1))
				  (or (digit-char-p (aref arg 0))
				      (invalid-arg-error arg)))
			  
				 ((and (>= (length arg) 1)
				       (char= (aref arg 0) #\0))
				  (read-arg (subseq arg 1) 8))
				 
				 ((= base 0)
				  (read-arg arg))
				 
				 ((/= base 0)
				  (check-type base (integer 2 36))
				  (read-arg arg base))))))))
		  
      (if (eq cls (ltv-find-class 'py-int))
	  val
	(make-instance cls :lisp-object val)))))

(def-py-method py-int.__init__ (&rest args) nil)

(def-py-method py-int.__floordiv__ (x^ y^)
  (values (floor x y)))

(def-py-method py-int.__div__ (x^ y^)
  (cond ((and (integerp x) (integerp y))
	 (floor x y))
	((and (numberp x) (numberp y))
	 (/ x y))
	(t (load-time-value *the-notimplemented*))))
       
(def-py-method py-int.__lshift__ (x^ y^)  (ash x y))
(def-py-method py-int.__rshift__ (x^ y^)  (ash x (- y)))

(def-py-method py-int.__xor__ (x^ y^) (logxor x y))
(def-py-method py-int.__and__ (x^ y^) (logand x y))
(def-py-method py-int.__or__  (x^ y^) (logior x y))

(def-py-method py-int.__hex__ (x^) (format nil "0x~x" x))
(def-py-method py-int.__oct__ (x^) (format nil "0~o" x))

(def-py-method py-int.__invert__ (x^)
  "2's complement inverse"
  (1- (- x)))

(def-py-method py-int.__mul__ (x^ y^)
  (typecase y
    (string (cond ((<= x 0) "")
                  (t (apply #'concatenate 'string (loop repeat x collect y)))))
    (t (py-number.__mul__ x y))))
  
;; Bool

(def-proxy-class py-bool (py-int))

(def-py-method py-bool.__new__ :static (cls &optional (val 0))
  (let ((bool-val (if (py-val->lisp-bool val) *the-true* *the-false*)))
    (if (eq cls (ltv-find-class 'py-bool))
	bool-val
      (make-instance 'cls :lisp-object bool-val))))

;; Float

(def-proxy-class py-float (py-real))

(def-py-method py-float.__new__ :static (cls &optional (val 0))
  (setf val (deproxy val))
  (when  (stringp val)
    (setf val (read-from-string val)))
  (let* ((num (py-val->number val))
	 (f   (coerce num 'double-float)))
    (if (eq cls (ltv-find-class 'py-float))
	f
      (make-instance 'cls :lisp-object f))))

(def-py-method py-float.__str__ (x)
  ;; To ensure the `d0' suffix is removed.
  (py-float.__repr__ x))

(def-py-method py-float.__repr__ (x^)
  (let* ((s (format nil "~F" x))
	 (s.len (length s)))

    ;;  '1.2d0'  -> '1.2'
    ;;  '1.2d+9' -> '1.2e+9'
    ;;  '1.2d-9' -> '1.2e-9'
    
    (loop for ix from (1- s.len) downto 0
	do (when (char= (aref s ix) #\d)
	     (ecase (aref s (1+ ix))
	       
	       (#\0 ;; 1.2d0
		(setf s (delete #\d s :start ix :end (1+ ix))
		      s (delete #\0 s :start ix :end (1+ ix))))
	       
	       ((#\+ #\-) ;; 1.2d+9  1.2d+9
		(setf (aref s ix) #\e)))))
    
    s))
  
;;; non-numeric classes

;; Dict

;; Class py-dict is already defined above, as it's needed for
;; bootstrapping Python class hierarchy.

(defun convert-if-symbol (x)
  (if (symbolp x)
      (symbol-name x)
    x))

(defmacro make-dict-unevaled-list (items)
  ;; XXX move to compiler
  (let ((dict '#:dict))
    `(let* ((,dict (make-dict)))
       ,@(loop for (k . v) in items
	     collect `(sub/dict-set ,dict ,k ,v))
       ,dict)))

(defun make-dict-from-symbol-alist (alist)
  (loop with d = (make-dict)
      for (k . v) in alist 
      do (sub/dict-set d (string k) v)
      finally (return d)))

(def-py-method py-dict.__new__ :static (cls &rest kwargs)
  (assert (not (eq (class-name cls) 'py-dict-classdict))) ;; loading order issue, so cmp name
  (if (eq cls (ltv-find-class 'py-dict))
      (make-dict)
    (make-instance cls :lisp-object (make-dict))))

(def-py-method py-dict.__init__ (x &rest kwargs)
  (case (length kwargs)
    
    (0 )
    
    (1 (let* ((iterable (car kwargs))
	      (items-meth (recursive-class-lookup-and-bind iterable '{items}))
	      (items (if items-meth
			 (py-call items-meth)
		       (py-iterate->lisp-list iterable)))
	      (setitem-meth (or (recursive-class-lookup-and-bind x '{__setitem__})
				(py-raise '{TypeError}
					  "py-dict.__init__ called on object without __setitem__ (~A)"
					  x))))
	 (map-over-py-object (lambda (obj)
			       (let ((lst (py-iterate->lisp-list obj)))
				 (if (= (length lst) 2)
				     (py-call setitem-meth (pop lst) (pop lst))
				   (py-raise '{ValueError}
					     "Got more than 2 values for element in .items() of ~A"
					     iterable))))
			     items)))
    
    (t (loop with setitem-meth = (or (recursive-class-lookup-and-bind x '{__setitem__})
				     (py-raise '{TypeError}
					       "py-dict.__init__ called on object without __setitem__ (~A)"
					       x))
	   while kwargs
	   do (let ((key (pop kwargs))
		    (val (pop kwargs)))
		(unless (symbolp key)
		  (py-raise '{TypeError} "dict.__new__: invalid key (not a symbol): ~S" key))
		(py-call setitem-meth (symbol-name key) val))))))

(def-py-method py-dict.__delitem__ (dict k)
  (or (dikt-del (py-dict-dikt dict) k)
      (py-raise '{KeyError} "Dict ~A has no such key: ~A" dict k)))

(def-py-method py-dict.__eq__ (dict1 dict2)
  (py-bool (cond ((eq dict1 dict2) t)
                 ((and (typep dict1 'py-dict)
                       (typep dict2 'py-dict))
                  (dikt-equal (py-dict-dikt dict1) (py-dict-dikt dict2)))
                 (t nil))))

(def-py-method py-dict.__getitem__ (dict k)
  "KEY may be symbol (converted to string)"
  (or (dikt-get (py-dict-dikt dict) k)
      (py-raise '{KeyError} "Dict ~A has no such key: ~A" dict k)))

(def-py-method py-dict.__iter__ (dict)
  (py-dict.iterkeys dict))

(def-py-method py-dict.__len__ (dict)
  (dikt-count (py-dict-dikt dict)))

(def-py-method py-dict.__nonzero__ (dict)
  (py-bool (> (py-dict.__len__ dict) 0)))

(def-py-method py-dict.__repr__ (x)
  (let ((kv-iter-func (dikt-iter-keys-values-func (py-dict-dikt x)))
        (first-iter   t))
    (with-output-to-string (s)
      (format s "{")
      (loop
        (multiple-value-bind (k v)
            (funcall kv-iter-func)
          (unless k
            (return))
          (if first-iter
              (setf first-iter nil)
            (format s ", "))
          (if (symbolp k)
              (write (symbol-name k) :stream s)
            (repr-fmt s k))
          (format s ": ")
          (repr-fmt s v)))
      (format s "}"))))
  
(def-py-method py-dict.__setitem__ (x key val)
  (sub/dict-set x key val))

(def-py-method py-dict.clear (d)
  (progn (setf (py-dict-dikt d) (make-dikt))
         *the-none*))

(def-py-method py-dict.copy (d1)
  (let* ((d2 (make-dict))
         (d2.dikt (py-dict-dikt d2))
         (f (dikt-iter-keys-values-func (py-dict-dikt d1))))
    (loop (multiple-value-bind (k v)
              (funcall f)
            (unless k
              (return))
            (dikt-set d2.dikt k v)))
    d2))

(def-py-method py-dict.fromkeys :static (seq &optional val)
  (unless val
    (setf val *the-none*))
  (let* ((d (make-dict)))
    (map-over-py-object (lambda (key) (sub/dict-set d key val))
			seq)
    d))

(def-py-method py-dict.get (x k &optional (default *the-none*))
  (or (sub/dict-get x k)
      default))
      
(def-py-method py-dict.has_key (x k)
  (py-bool (sub/dict-get x k)))
  
(def-py-method py-dict.items (x)
  (let ((kv-iter-func (dikt-iter-keys-values-func (py-dict-dikt x)))
        res)
    (loop (multiple-value-bind (k v)
          (funcall kv-iter-func)
          (unless k (return))
          (push (make-tuple-from-list (list (convert-if-symbol k) v)) res)))
    (make-py-list-from-list res)))

(def-py-method py-dict.iteritems (x)
  (make-iterator-from-function
   :name :dict-iter-items
   :func (let ((f (dikt-iter-keys-values-func (py-dict-dikt x))))
           (lambda ()
             (multiple-value-bind (k v) (funcall f)
               (and k (make-tuple-from-list (list k v))))))))

(def-py-method py-dict.iterkeys (x)
  (make-iterator-from-function
   :name :dict-iter-keys
   :func (dikt-iter-keys-func (py-dict-dikt x))))

(def-py-method py-dict.itervalues (x)
  (make-iterator-from-function
   :name :dict-iter-values
   :func (dikt-iter-values-func (py-dict-dikt x))))

(def-py-method py-dict.keys (x)
  (make-py-list-from-list (loop with f = (dikt-iter-keys-func (py-dict-dikt x))
                              for key = (funcall f)
                              while key collect (convert-if-symbol key))))

(def-py-method py-dict.pop (x key &optional default)
  (let* ((dikt (py-dict-dikt x))
         (val (dikt-get dikt key)))
    (if val
        (prog1 val (dikt-del dikt key))
      (or default (py-raise '{KeyError} "Dict has no key `~A' to pop()." key)))))

(def-py-method py-dict.popitem (x)
  (let ((dikt (py-dict-dikt x)))
    (when (= (dikt-count dikt) 0)
      (py-raise '{KeyError} "Dict is empty, can not popitem()."))
    ;; XXX Only interested in first key/val pair, not whole mapping.
    (dikt-map dikt
              (lambda (k v)
                (dikt-del dikt k)
                (return-from py-dict.popitem (make-tuple-from-list (list k v)))))))

(def-py-method py-dict.setdefault (x key &optional default)
  (let ((dikt (py-dict-dikt x)))
    (or (dikt-get dikt key)
        (prog1 default (dikt-set dikt key default)))))

(def-py-method py-dict.update (x y)
  (let ((x.d (py-dict-dikt x))
        (y.d (py-dict-dikt y)))
    (dikt-map y.d (lambda (k v) (dikt-set x.d k v))))
  *the-none*)
   
(def-py-method py-dict.values (x)
  (make-py-list-from-list (loop with f = (dikt-iter-values-func (py-dict-dikt x))
                              for val = (funcall f)
                              while val collect val)))

(defun clear-dict (x)
  (check-type x py-dict)
  (dikt-clear (py-dict-dikt x)))

;; Dictionaries that act as proxy for module can be accessed using
;; "globals()"; apply changes to module too.
(defclass py-dict-moduledictproxy (py-dict)
  ((module :initarg :module :accessor mdp-module)
   (updater :initarg :updater :accessor mdp-updater))
  (:metaclass py-core-type))

(defmethod (setf py-subs) :after (val (x py-dict-moduledictproxy) key)
  ;; Modifying this dict modifies the module globals.
  (py-module-set-kv (mdp-module x) (py-string->symbol key) val))

;; The dict returned by globals should reflect the current global module
;; variables. This is enforced by triggering an update of globals dicts
;; every time they are accessed. The assumption is that all accesses
;; call at some point a py-dict.xxx function.

(defvar *module-dict-update-level* 0)

(defmethod py-repr ((x py-dict-moduledictproxy))
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t)
      (format s "dict-proxy for the globals of module `~A'" (module-name (mdp-module x))))))

(defun update-dict-and-call-func (original-method args)
  (let* ((check-upd (= *module-dict-update-level* 0))
         (*module-dict-update-level* (1+ *module-dict-update-level*)))
    (when check-upd
      (dolist (a args)
        (when (typep a 'py-dict-moduledictproxy)
          (funcall (mdp-updater a)))))
    (apply #'py-call original-method args)))

(defun set-py-moduledictproxy-methods ()
  (let ((py-dict.d (dict (find-class 'py-dict)))
        (py-m-dict.d (dict (find-class 'py-dict-moduledictproxy))))
    (dict-map py-dict.d 
              (lambda (meth-name meth-val)
                (assert (and meth-name meth-val))
                (when (typep meth-val 'function)
                  (sub/dict-set py-m-dict.d meth-name
                                (let ((meth-val meth-val))
                                  (lambda (&rest args)
                                    (update-dict-and-call-func meth-val args)))))))))

(eval-when (:load-toplevel :execute)
  (set-py-moduledictproxy-methods))


;; List (Lisp object: adjustable array)

(def-proxy-class py-list)

(defun make-py-list (&optional (size 0))
  (make-array size :adjustable t :fill-pointer 0))

(def-py-method py-list.__new__ :static (cls &optional iterable)
	       (declare (ignore iterable))
	       (let ((vec (make-py-list)))
		 (if (eq cls (ltv-find-class 'py-list))
		     vec
		   (make-instance cls :lisp-object vec))))
		    
(def-py-method py-list.__init__ (x^ &optional iterable)
  (when iterable
    (let* ((items (py-iterate->lisp-list iterable))
	   (len (length items)))
      (adjust-array x len)
      (loop for i from 0 below len
	  do (setf (aref x i) (pop items)))
      (setf (fill-pointer x) len)
      x)))

(def-py-method py-list.__add__ (x^ y^)
  (unless (and (vectorp x) (vectorp y))
    (py-raise '{TypeError} "list.__add__: only lists as second arg (got: ~A)" y))
  (let ((vec (make-py-list (+ (length x) (length y)))))
    (let ((i 0))
      (loop for xi across x
	  do (setf (aref vec i) xi)
	     (incf i))
      (loop for yi across y
	  do (setf (aref vec i) yi)
	     (incf i))
      (setf (fill-pointer vec) i))
    vec))

(def-py-method py-list.__add2__ (x^ y^)
  (unless (and (vectorp x) (vectorp y))
    (py-raise '{TypeError} "list.__add__: only lists as second arg (got: ~A)" y))
  (let* ((sz (+ (length x) (length y)))
	 (vec (make-py-list sz)))
    (setf (fill-pointer vec) sz)
    (replace vec x)
    (replace vec y :start1 (length x))
    vec))

(def-py-method py-list.__cmp__ (x^ y^)
  (unless (and (vectorp x) (vectorp y))
    (return-from py-list.__cmp__ *the-notimplemented*))
  (let ((x.len (length x))
	(y.len (length y)))
    
    (cond ((< x.len y.len) -1)
	  ((> x.len y.len) 1)
	  (t (loop for xi across x and yi across y
		 do (ecase (py-cmp xi yi)
		      (0 ) ;; cont
		      (-1 (return -1))
		      (1  (return  1)))
		 finally (return 0))))))

(def-py-method py-list.__delitem__ (x^ item)
  (typecase item
    (integer (when (< item 0)
	       (incf item (length x)))
	     (unless (<= 0 item (1- (length x)))
	       (py-raise '{ValueError}
			 "del <list>[i] : i outside range (got ~A, length list = ~A)"
			 item (length x)))
	     (replace x x :start1 item :start2 (1+ item))
	     (decf (fill-pointer x)))
    (py-slice (with-slots (start stop step) item
		(cond ((and (eq start *the-none*) (eq stop *the-none*) (eq step *the-none*)) ;; del x[:]
		       (fill x nil)
		       (setf (fill-pointer x) 0))
		      (t (break "Todo: del x[-2:] etc")))))))

(def-py-method py-list.__eq__ (x^ y^)
  (py-bool (and (vectorp x)
                (vectorp y)
                (= (length x) (length y))
		(loop for xi across x and yi across y
		    unless (py-==->lisp-val xi yi)
		    do (return nil)
		    finally (return t)))))

(def-py-method py-list.__getitem__ (x^ item^)
  (vector-getitem x item (lambda (x single-p)
			   (if single-p x (make-py-list-from-list x)))))

(defun vector-getitem (x item make-seq-func)
  (typecase item
    
    (integer (when (< item 0)
	       (incf item (length x)))
	     (unless (<= 0 item (1- (length x)))
	       (py-raise '{ValueError}
			 "String subscript outside range (got ~A, length string = ~A)"
			 item (length x)))
	     (funcall make-seq-func (aref x item) t))
    
    (py-slice (destructuring-bind (kind &rest args)
		  (multiple-value-list (slice-indices item (length x)))
		
		(ecase kind
	  
		  ((:empty-slice-bogus
		    :empty-slice-before :empty-slice-after :empty-slice-between)
		   (funcall make-seq-func () nil))
		  
		  ((:nonempty-slice :nonempty-stepped-slice)
		   (multiple-value-bind (items exp-num)
		       (ecase kind
			 
			 (:nonempty-slice
			  (destructuring-bind (start stop num) args
			    (values (loop for i from start upto stop
					collect (aref x i))
				    num)))
			 
			 (:nonempty-stepped-slice
			  (destructuring-bind (start stop step num) args
			    (values (loop for i from start upto stop by step
					collect (char x i))
				    num))))
		     (assert (= exp-num (length items)) ()
		       "Expected ~A items, got ~A" exp-num (length items))
		     
		     (funcall make-seq-func items nil))))))
    
    (t (py-raise '{TypeError} "Expected integer or slice as subscript; got: ~S."
		 item))))

(def-py-method py-list.__iter__ (x^)
  (let ((i -1)
	(max-i (1- (length x))))
    (make-iterator-from-function
     :name :list-iterator
     :func (lambda ()
	     (when (<= (incf i) max-i)
	       (aref x i))))))

(def-py-method py-list.__len__ (x^)
  (length x))

(def-py-method py-list.__mul__ (x^ y)
  (let* ((n (py-val->integer y :min 0))
	 (x.len (length x))
	 (res.len (* n x.len))
	 (res (make-array res.len :adjustable t :fill-pointer res.len
			  :initial-element (when (= x.len 1) 
					      (aref x 0)))))
    (unless (= x.len 1)
      (dotimes (i n)
	(replace res x :start1 (* i x.len))))
    res))

(def-py-method py-list.__repr__ (x^)
  (with-output-to-string (s)
    (format s "[")
    (loop for item across x and i from 0
 	do (unless (= i 0)
 	     (format s ", "))
 	   (repr-fmt s item))
    (format s "]")))

(def-py-method py-list.__setitem__ (x^ item val)
  (check-type item integer)
  (when (< item 0)
    (incf item (length x)))
  (unless (<= 0 item (1- (length x)))
    (py-raise '{ValueError}
	      "<list>[i] = x : i outside range (got ~A, length list = ~A)"
	      item (length x)))
  (setf (aref x item) val))

(defvar *py-print-safe* nil)

(def-py-method py-list.__str__ (x^)
  (if *py-print-safe*
      (with-output-to-string (s)
	(print-unreadable-object (x s :type nil :identity t)
	  (format s "list with ~A items" (length x))))
    (py-list.__repr__ x)))


(def-py-method py-list.append (x^ y)
  (vector-push-extend y x)
  *the-none*)


(def-py-method py-list.pop (x^ &optional index)
  "Remove and return item at index (default: last item)"
  (let* ((x.len (length x)))
    (setf index (if index
		    (deproxy index)
		  (1- x.len)))
					   
    (let ((ix (if (< index 0) (+ index x.len) index)))
      (if (<= 0 ix (1- x.len))
	  (prog1 (aref x ix)
	    (replace x x :start1 ix :start2 (1+ ix))
	    (decf (fill-pointer x)))
	(py-raise '{ValueError}
		  "list.pop(x, i): ix wrong (got: ~A; x.len: ~A)"
		  ix x.len)))))

(def-py-method py-list.sort (x^ &optional fn)
  (let* ((sort-fun (if fn
		       (lambda (x y) (< (signum (deproxy (py-call fn x y))) 0))
		     (lambda (x y) (< (signum (py-cmp x y)) 0))))
	 (res (sort x sort-fun)))

    ;; It's not guaranteed by ANSI that (eq res x).
    (unless (eq res x)
      (replace x res))
    
    *the-none*))


(defmacro make-py-list-unevaled-list (items)
  (let ((vec '#:vec))
    `(let ((,vec (make-array ,(length items)
			     :adjustable t
			     :fill-pointer ,(length items)
			     :initial-contents (list ,@items))))
       ,vec)))

(defun make-py-list-from-list (list)
  (let* ((len (length list))
	 (vec (make-array len :adjustable t :fill-pointer t)))
    (replace vec list)
    vec))

(defun make-py-list-from-vec (vec)
  vec)

;; String (Lisp object: string)

(def-proxy-class py-string)

(def-py-method py-string.__new__ :static (cls &optional (val "") unicode-encoding)
	       (unless (stringp val)
		 (setf val (py-str-string val))) ;; convert to string using __str__
	       
	       (let ((the-val
		      (if unicode-encoding
			  (py-decode-unicode val (py-val->string unicode-encoding))
			val)))
		 
		 (if (eq cls (find-class 'py-string))
		     the-val
		   (make-instance cls :lisp-object the-val))))

(defvar *py-char-strings* (make-array 256
				      :element-type 'string
				      :initial-element nil))
(defun py-string-from-char (ch)
  (let ((code (char-code ch)))
    (if (<= 0 code 255)
	(or (aref *py-char-strings* (char-code ch))
	    (setf (aref *py-char-strings* (char-code ch)) (string ch)))
      (string ch))))

(defun py-unicode-external-format->lisp-external-format (name)
  ;; Returns NAME, MAX-CHAR-CODE, MAX-ENCODED-OCTET-CODE
  
  ;; Based on:
  ;;  http://meta.kabel.utwente.nl/specs/Python-Docs-2.3.3/lib/node127.html
  ;;  http://www.franz.com/support/documentation/7.0/doc/iacl.htm#external-formats-1
  ;; For now only ASCII and UTF-8 are supported.
  
  (when (null name)
    (setf name "ascii"))
  
  (setf name (string-downcase name))
  
  (cond 
   ((member name '("ascii" "646" "us") :test 'string=) 
    (values :latin1 127 127))
   
   ((member name '("latin" "latin1" "latin-1") :test 'string=) 
    (values :latin1 255 255))
   
   ((member name '("utf8" "utf_8" "utf-8" "utf" "u8") :test 'string=)
    (values :utf8 #16x0010FFFF 255))
   
   (t (py-raise '{UnicodeError} "Unrecognized Unicode external format: ~S" name))))

(defun py-encode-unicode (string &optional (external-format "ascii") errors)
  ;; The result is a string (containing characters), not a vector of
  ;; octets, because Python doesn't have vectors. Python could use
  ;; regular lists, but strings are immutable so more efficient.
  ;; 
  ;; A future version of Python will have a `byte vector' data type.
  
  (when errors
    (error "TODO: errors parameter for unicode.encode"))

  (multiple-value-bind (ex-format max-code max-octet-code)
      (py-unicode-external-format->lisp-external-format external-format)
    (declare (ignore max-octet-code))

    (let ((max-found-code (reduce #'max string :key #'char-code)))
      (when (> max-found-code max-code)
	(py-raise '{UnicodeEncodeError}
		  "During encoding of string, encountered a character whose ~
                   code is out of the allowed range (got character code: ~A; ~
                   external format: ~A; max code allowed for external format: ~A)"
		   max-found-code external-format max-code)))

    (multiple-value-bind (octets num-bytes-copied)
        #+allegro
	(excl:string-to-octets string
			       :external-format ex-format
			       :null-terminate nil)
        #-allegro
        (error "No STRING-TO-OCTECTS defined in this implementation.")
      
      (when (< num-bytes-copied (length string))
	(py-raise '{UnicodeEncodeError}
		  "During encoding (string -> bytes): Not all bytes valid"))

      (let ((string (make-array (length octets) :element-type 'character)))
	(map-into string #'code-char octets)
	string))))

(defun py-decode-unicode (string &optional (external-format "ascii") errors)
  
  (when errors
    (error "TODO: `errors' parameter for unicode encode"))

  ;; Python has no separate data type for the returned vector of
  ;; octets: that's also a string.

  (multiple-value-bind (ex-format max-code max-octet-code)
      (py-unicode-external-format->lisp-external-format external-format)
    (declare (ignore max-code))
    
    (let ((vec (make-array (length string) :element-type '(unsigned-byte 8))))
      (map-into vec #'char-code string)

      ;; EXCL:OCTETS-TO-STRING replaces characters out of range of
      ;; external format with question marks #\?, but we want to get
      ;; an error instead.

      (let ((max-found-code (reduce #'max vec)))
	(when (> max-found-code max-octet-code)
	  (py-raise '{UnicodeDecodeError}
		    "During decoding of string, encountered a character whose ~
                     code is out of allowed range (got character code: ~A; ~
                     external format: ~A; max octet code allowed for external format: ~A)"
		    max-found-code ex-format max-octet-code)))
      
      (multiple-value-bind (string chars-copied octets-used)
          #+allegro
	  (excl:octets-to-string vec :external-format ex-format)
          #-allegro
          (error "No OCTETS-TO-STRING defined in this implementation.")
	(declare (ignore chars-copied))
	
	(when (< octets-used (length vec))
	  (py-raise '{UnicodeDecodeError}
		    "In unicode.decode (bytes -> string): Not all octets valid"))
	string))))


(def-py-method py-string.__add__ (x^ y^) (concatenate 'string
					   (the string x) (the string y)))
(def-py-method py-string.__cmp__ (x^ y^)
  (cond ((not (stringp y)) -1) ;; whatever
	((string< x y) -1)
	((string= x y)  0)
	(t              1)))

(def-py-method py-string.__iter__ (x^)
  (make-iterator-from-function :name :string-iterator
			       :func (let ((i 0)) (lambda ()
						    (when (< i (length x))
						      (prog1
							  (py-string-from-char (char x i))
							(incf i)))))))

(def-py-method py-string.__getitem__ (x^ item^)
  (vector-getitem x item (lambda (char/charlist single-p)
			   (if single-p
			       (py-string-from-char char/charlist)
			     (coerce char/charlist 'string)))))

(def-py-method py-string.__hash__ (x^)  (sxhash x))
(def-py-method py-string.__len__  (x^)  (length x))

(def-py-method py-string.__mod__ (x^ args)
  (let ((fs-struct (funcall 'ensure-parsed-format-string x)))
    (funcall 'make-formatted-string fs-struct args)))

(def-py-method py-string.__mul__ (x^ n^)
  (unless (typep n '(integer 0 *))
    (py-raise '{TypeError} "str.__mul__: arg must be nonzero integer (got: ~S)" n))
  (let ((x.len (length x)))
    (if (or (= n 0)
	    (= x.len 0))
      ""
    (let ((res (make-array (* n x.len) :element-type 'character)))
      (dotimes (i n)
	(replace res x :start1 (* i x.len)))
      res))))

(def-py-method py-string.__nonzero__ (x^) (py-bool (> (length x) 0)))
(def-py-method py-string.__repr__ (x^) (py-string.strip (clpython.parser:py-pprint x nil)))

(def-py-method py-string.__str__  (x^)  x)

(def-py-method py-string.capitalize (x^)
  (let ((y (copy-seq x)))
    (when (> (length x) 0)
      (setf (aref y 0) (char-upcase (aref y 0))))
    y))

(def-py-method py-string.center (x^ width &optional (fillchar " "))
  (declare (ignorable x width))
  (assert (= (length fillchar) 1))
  (error :todo))
   
(def-py-method py-string.decode (x^ &optional encoding^ errors)
  (py-decode-unicode x encoding errors))

(def-py-method py-string.encode (x^ &optional encoding^ errors)
  (py-encode-unicode x encoding errors))

(def-py-method py-string.endswith (x^ suffix &optional start end)
  (when (or start end) (error :todo))
  (py-bool (string= (subseq x (- (length x) (length suffix))) suffix)))
  
(def-py-method py-string.find (x^ item &rest args)
  (declare (ignore x item args))
  (warn "todo :string.find")
  -1)

(def-py-method py-string.isalpha (x^) (py-bool (every (lambda (c)
							(let ((cc (char-code c)))  ;; CL:ALPHA-CHAR-P is different
							  (or (<= #.(char-code #\a) cc #.(char-code #\z))
							      (<= #.(char-code #\A) cc #.(char-code #\Z)))))
						      x)))
(def-py-method py-string.isalnum (x^) (py-bool (every #'alphanumericp x)))
(def-py-method py-string.isdigit (x^) (py-bool (every #'digit-char-p x)))
(def-py-method py-string.islower (x^) (py-bool (every #'lower-case-p x)))

(def-py-method py-string.isspace (x^)
  (py-bool (and (> (length x) 0) ;; empty string is defiend as "not space"
		(every (lambda (ch) (member ch '(#\Space #\Tab #\Newline)))
		       x))))

(def-py-method py-string.join (x^ seq-of-strings)
  (let ((strings (mapcar #'py-val->string (py-iterate->lisp-list seq-of-strings))))
    (cond ((null strings)        "")
	  ((null (cdr strings))  (car strings))
	  (t                     (let* ((num-strings (length strings))
					(tot-num-chars (reduce #'+ strings
							       :key #'length
							       :initial-value (* (length x) (1- num-strings))))
					(res (make-array tot-num-chars :element-type 'character)))
				   (replace res (car strings))
				   (loop with ix = (length (car strings))
				       for s in (cdr strings)
				       do (replace res x :start1 ix)
					  (incf ix (length x))
					  (replace res s :start1 ix)
					  (incf ix (length s)))
				   res)))))

(def-py-method py-string.lower (x^)
  (string-downcase x))

(defun verify-string-strip-chars (char-list)
  ;; Returns list of chars
  (loop for c in (py-iterate->lisp-list char-list)
      if (characterp c) collect c
      else if (and (stringp c) (= (length c) 1)) collect (aref c 0)
      else if (and (setf c (py-val->string c))
		   (= (length c) 1)) collect (aref c 0)
      else do (py-raise '{ValueError}
			"string.strip wants list of single-char strings, got: ~S"
			char-list)))

(def-py-method py-string.lstrip (x^ &optional (chars '(#\Newline #\Space #\Tab)))
  (string-left-trim (verify-string-strip-chars chars) x))
  
(def-py-method py-string.rstrip (x^ &optional (chars '(#\Newline #\Space #\Tab)))
  (string-right-trim (verify-string-strip-chars chars) x))

(def-py-method py-string.strip (x &optional (chars '(#\Newline #\Space #\Tab)))
  (string-trim (verify-string-strip-chars chars) x))

(def-py-method py-string.replace (x^ old new &optional count^)
  (substitute (py-val->string new) (py-val->string old) x :count count))

(def-py-method py-string.startswith (x^ prefix &optional start end)
  (when (or start end) (error :todo))
  (py-bool (and (>= (length x) (length prefix))
                (string= (subseq x 0 (length prefix)) prefix))))

(def-py-method py-string.upper (x^)
  (string-upcase x))
   

;; Tuple (Lisp object: consed list)

(def-proxy-class py-tuple)

(def-py-method py-tuple.__new__ :static (cls &optional iterable)
	       
	       (let ((tup (make-tuple-from-list (when iterable 
						  (py-iterate->lisp-list iterable)))))
		 
		 (cond ((eq cls (ltv-find-class 'py-tuple)) tup)
		       
		       ((subtypep cls (ltv-find-class 'py-tuple))
			(let ((x (make-instance cls)))
			  (setf (proxy-lisp-val x) tup)
			  x))
		       
		       (t (error "invalid py-tuple.__new__ cls: ~A" cls)))))


(defvar *the-empty-tuple* (make-instance 'py-tuple :lisp-object nil))

(defun make-tuple-from-list (list)
  (or list *the-empty-tuple*))

(defmacro make-tuple-unevaled-list (items)
  `(make-tuple-from-list (list ,@items)))

;; Efficient tuple functions

(defun tuple-p-fast (x)
  (listp x))

(defun tuple-length-fast (x)
  (assert (listp x))
  (length x))

(def-py-method py-tuple.__cmp__ (x^ y^)
  (unless (and (listp x) (listp y))
    (return-from py-tuple.__cmp__ *the-notimplemented*))
  (let ((x.len (length x))
	(y.len (length y)))
    
    (cond ((= x.len y.len)
	   (loop for xi in x and yi in y
	       do (ecase (py-cmp xi yi)
		    (-1 (return -1))
		    (1  (return 1))
		    (0  ))
	       finally (return 0)))
	  
	  ((< x.len y.len) -1)
	  (t                1))))

(def-py-method py-tuple.__eq__ (x^ y^)
  (py-bool (and (listp x)
                (listp y)
                (loop 
                    for xi = (pop x)
                    for yi = (pop y)
                    do (cond ((null (or xi yi))        (return t))
                             ((or (null xi) (null yi)) (return nil))
                             ((py-==->lisp-val xi yi))
                             (t (return nil)))))))

(def-py-method py-tuple.__getitem__ (x^ item^)
  ;; XXX item may be a slice
  (etypecase item
    (integer (cond ((<= 0 item (1- (length x)))
		    (nth item x))
		   ((<= 0 (+ (length x) item) (1- (length x)))
		    (nth (+ (length x) item) x))
		   (t (py-raise '{IndexError}
				"Attempt to retrieve element ~A from tuple of size ~A: ~:A."
				item (length x) x))))
    (py-slice (let ((vec (coerce x 'vector))) ;; Use slice handling in vector-getitem
                (vector-getitem vec item (lambda (item/s single-p)
                                           (make-tuple-from-list (if single-p (list item/s) item/s))))))))

(def-py-method py-tuple.__hash__ (x^)
  (declare (optimize (speed 3)))
  (loop with res = 0
      for i in x
      for ix fixnum from 0
      for i.hash = (py-hash i)
      for i.hash.16bit = (the fixnum (logand (the integer i.hash) #xFFFF)) ;; use 16 bits
      for i.shift = (the fixnum (mod (* 7 ix) #.(- (1- (integer-length most-positive-fixnum)) 16)))
      for i.h.rotated = (ash i.hash.16bit i.shift)
      do #+(or)(format t "elm ~A = ~A -> h=~A=~A  shift=~A~%" j i i.hash i.hash.16bit i.shift)
         (unless (typep i.h.rotated 'fixnum)
           (error "tuple hash bug"))
         (setf res (logxor res (sxhash i.h.rotated)))
      finally (return res)))

(def-py-method py-tuple.__iter__ (x^)
  (make-iterator-from-function
   :name :tuple-iterator
   :func (let ((i -1))
	   (lambda ()
	     (nth (incf i) x)))))

(def-py-method py-tuple.__len__ (x^)
  (length x))

(def-py-method py-tuple.__nonzero__ (x^)
  (py-bool x))

(def-py-method py-tuple.__repr__ (x^)
  (cond ((null x)       "()")
	((null (cdr x)) (format nil "(~/clpython:repr-fmt/,)" (car x)))
	(t              (format nil "(~{~/clpython:repr-fmt/~^, ~})" x))))

(def-py-method py-tuple.__str__ (x^)
  (if *py-print-safe*
      (with-output-to-string (s)
	(print-unreadable-object (x s :type nil :identity t)
	  (format s "tuple with ~A items" (length x))))
    (py-tuple.__repr__ x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(defgeneric py-class-of (x)
  
  ;; Lisp objects lead to their proxy class
  (:method ((x integer)) (ltv-find-class 'py-int    ))
  (:method ((x real))    (ltv-find-class 'py-float  ))
  (:method ((x complex)) (ltv-find-class 'py-complex))
  (:method ((x string))  (ltv-find-class 'py-string ))
  (:method ((x vector))  (ltv-find-class 'py-list   ))
  (:method ((x list))    (ltv-find-class 'py-tuple  ))
  (:method ((x function))    (ltv-find-class 'py-function))
  (:method ((x py-function)) (ltv-find-class 'py-function))
  (:method ((x package)) (ltv-find-class 'lisp-package))
  
  (:method ((x py-core-type)) (ltv-find-class 'py-type))
  (:method ((x py-user-type)) (ltv-find-class 'py-type))
  
  (:method ((x py-meta-type)) ;; metatypes (including `type')
                              ;;  fake being of type `type'
                              (ltv-find-class 'py-type))
  
  (:method ((x py-type)) (class-of x))
  
  (:method ((x (eql (find-class 'py-meta-type))))
           ;; the metatypes is posing as `type'
           (ltv-find-class 'py-type))
    
  (:method ((x t))       (class-of x)))


;;; Attributes are a fundamental thing: getting, setting, deleting

(defun py-attr* (x &rest chained-attr)
  "Chained attribute lookup. Attributes may be strings or symbols.
Returns nil upon lookup failure."
  (dolist (attr chained-attr x)
    (let* ((attr.sym (ensure-user-symbol (string attr)))
           (res (catch :getattr-block (py-attr x attr.sym :via-getattr t))))
      (when (eq res :py-attr-not-found)
        (return-from py-attr* nil))
      (setf x res))))

(defun (setf py-attr*) (val x attr)
  (let ((attr.sym (ensure-user-symbol (string attr))))
    (setf (py-attr x attr.sym) val)))

(defun py-attr (x attr.as_sym &key (bind-class-attr t) via-getattr)
  ;; If BIND-CLASS-ATTR = NIL, then if the attribute value is a function
  ;; found in a class dict, these values are returned:
  ;;   :class-attr <class-attr-val> <x>
  ;; which saves allcation of bound method.
  ;;
  ;; If VIA-GETATTR is true, then when lookup fails a THROW to
  ;; :getattr-block takes place; this saves creation of condition.
  (assert (symbolp attr.as_sym))
  (let* ((x.class         (py-class-of x))
	 (inside-object-getattribute (loop for (obj . at) in *py-object.__getattribute__-active*
					 when (and (eq obj x)
						   (string= (py-val->string at) (symbol-name attr.as_sym)))
					 return t
					 finally (return nil))))
    
    (multiple-value-bind (result __getattr__ class-attr-val)
	(py-attr-class-dicts x x.class attr.as_sym inside-object-getattribute)
      #+(or)(format t "p-a-c-d ~A ~A => ~A ~A ~A" x x.class result __getattr__ class-attr-val)
      
      ;; Check if result determined by __getattribute__
      (when result (return-from py-attr result))

      ;; A class attribute that is a data descriptor (i.e. has a `__set__' attribute)
      ;; has higher priority than an instance attribute.
      (when (and class-attr-val (data-descriptor-p class-attr-val))
	(return-from py-attr
	  (if (and (not bind-class-attr)
		   (functionp class-attr-val))
	      (values :class-attr class-attr-val x)
	    (bind-val class-attr-val x x.class))))

      ;; Try instance dict
      (let ((val (py-attr-instance-dict x attr.as_sym)))
	(when val
	  (return-from py-attr val)))

      ;; Fall back to a class attribute that is not a `data descriptor'.
      (when class-attr-val
	(return-from py-attr
	  (if (and (not bind-class-attr)
		   (functionp class-attr-val))
	      (values :class-attr class-attr-val x)
	    (bind-val class-attr-val x x.class))))

      ;; Fall back to the __getattr__ hook (AttributeError is not caught)
      (when __getattr__
	(return-from py-attr
	  (py-call (bind-val __getattr__ x x.class) (symbol-name attr.as_sym))))

      (when (eq attr.as_sym '{__dict__})
	(return-from py-attr (py-get-dict-attr x x.class)))
      
      ;; Give up.
      (if via-getattr
	  (throw :getattr-block :py-attr-not-found)
	(py-raise '{AttributeError} "Object ~A has no attribute `~A'." x (symbol-name attr.as_sym))))))

(defun try-calling-getattribute (ga x x.class attr.as_sym)
  (assert (symbolp attr.as_sym))
  (handler-case
      (values (py-call (bind-val ga x x.class) (symbol-name attr.as_sym)))
    ({AttributeError} () nil) ;; __getattribute__ gave exception; also trying __getattr__ (if present)
    (:no-error (val) (return-from try-calling-getattribute val))))

(defun py-attr-class-dicts (x x.class attr.as_sym inside-object-getattribute)
  ;; Returns: RESULT (if determined by __getattribute__)
  ;;       or:  NIL, __GETATTR__, CLASS-ATTR
  ;;(declare (notinline py-call))
  (let (__getattribute__ __getattr__ class-attr)

    ;; XXX find stop classes
    (loop for c in (class-precedence-list x.class)
	until (or (eq c (ltv-find-class 'standard-class))
		  (eq c (ltv-find-class 'py-dict-mixin))
                  (eq c (ltv-find-class 'standard-generic-function)))
	      
	for c.dict = (cond
		      ((eq c (ltv-find-class 'py-user-object)) nil) ;; has no methods
		      ((eq c (ltv-find-class 'py-object))      (load-time-value (dict (find-class 'py-object))))
		      ((eq c (ltv-find-class 'py-function))    (load-time-value (dict (find-class 'py-function))))
		      (t                                       (dict c))) ;; may be NIL
		     
	when c.dict do 
	  (unless (or inside-object-getattribute __getattribute__)
	    ;; Try only the first __getattribute__ method found (but not py-object's).
	    (let ((getattribute-meth (sub/dict-get c.dict '{__getattribute__})))
	      (when (and getattribute-meth
			 (not (eq getattribute-meth #'py-object.__getattribute__)))
		(setf __getattribute__ getattribute-meth)
                (let ((res (try-calling-getattribute __getattribute__ x x.class attr.as_sym)))
                  (when res (return-from py-attr-class-dicts res))
                  ;; If __getattribute__ exists but gave exception, class-attr will not be looked up.
                  (setf class-attr nil)))))
	  
	  (unless (or class-attr __getattribute__)
	    ;; Try to find attribute in class dict.
	    (let ((val (sub/dict-get c.dict attr.as_sym)))
	      (when val
		(setf class-attr val))))
	  
	  (unless (or inside-object-getattribute __getattr__)
	    ;; Look for __getattr__ method (used if instance/class dicts fail).
	    (let ((getattr-meth (sub/dict-get c.dict '{__getattr__})))
	      (when getattr-meth
                (setf __getattr__ getattr-meth)))))

    (values nil __getattr__ class-attr)))

(defun py-attr-instance-dict (x attr.as_sym)
  (if (typep x 'class)

      (progn
	#+(or)(break "cls ~A: cpl ~A" x (class-precedence-list x))
	(loop for c in (class-precedence-list x)
	    until (or (eq c (ltv-find-class 'standard-class))
		      (eq c (ltv-find-class 'py-dict-mixin)))
		  
	    for c.dict = (dict c) ;; may be NIL
			 
	    for val = (when c.dict
                        (sub/dict-get c.dict attr.as_sym))
	    when val do
	      #+(or)(warn "binding attr ~A to cls ~A" val x)
	      (let ((bound-val (bind-val val *the-none* x)))
		(when bound-val 
		  ;; Binding can result in NIL: attribute-method bounded to
		  ;; class (e.g. "complex.real").
		  ;; Otherwise probably unbound method, or classmeth -> func.
		  (return-from py-attr-instance-dict bound-val)))))

    (let ((x.dict (dict x)))
      (when x.dict
	(let ((val (sub/dict-get x.dict attr.as_sym)))
	  (when val
	    ;; don't bind: coming from instance dict
	    (return-from py-attr-instance-dict val)))))))

#+(and allegro-version>= (version>= 8 0))
(define-compiler-macro py-attr (&whole whole x attr &key (bind-class-attr t) via-getattr &environment e)
  (declare (ignore bind-class-attr via-getattr))
  
  ;; Work around Allegro CL issues w.r.t. compiler macros and SETF forms:
  ;; don't run this PY-ATTR compilar macro on (setf (py-attr ..) ..) forms
  ;; if the expanded form is invalid -- e.g. if expansion becomes a 'let,
  ;; then 'setf form becomes (setf (let ..) ..) which is invalid.
  ;; 
  ;; The (setf py-attr) case is detected usign our :inside-setf-py-attr
  ;; declaration.
  
  (cond ((eq attr '{__class__})  
	 ;; As "x.__class__ = y" is intercepted, __class__ is always a valid class.
         ;; 
         ;; Note that this is also okay within :inside-setf-py-attr, as (setf py-class-of)
         ;; is defined.
	 `(py-class-of ,x))
	
	((and (fboundp 'get-pydecl)
	      (not (funcall 'get-pydecl :inside-setf-py-attr e))
	      (listp attr)
	      (eq (first attr) 'identifier-expr))
	 (case (second attr)
	   ({__name__}  `(let ((.x ,x))
			   (if (classp .x)
			       (symbol-name (class-name .x))
                             (locally
				 (declare (notinline py-attr))
			       (py-attr .x ,attr)))))
	
	   ({__dict__}  `(let ((x ,x))
			   (or (dict x)
			       (py-raise '{AttributeError} "No __dict__ attribute: ~A" x))))
	
	   (t            whole)))
	
	(t whole)))

(defun (setf py-class-of) (new-cls x)
  (change-class x new-cls))
	   
(defun (setf py-attr) (val x attr.sym)
  ;; When val is NIL, it means attribute is deleted ("del x.attr")
  (assert (symbolp attr.sym))
  (let* ((x.class (py-class-of x))
	 (x.attr  (recursive-class-dict-lookup x.class attr.sym)))
    
    (if (null val)

	;; del x.attr
	;; <cls>.__delattr__ takes precedence over <cls.attr>.__del__
	(let* ((delattr-active  (loop for (obj . at) in *py-object.__delattr__-active*
				    if (and (eq obj x) (string= at attr.sym))
				    return t
				    finally (return nil)))
	       
	       (x.__delattr__   (unless delattr-active
				  (let ((da (recursive-class-dict-lookup x.class '{__delattr__})))
				    (unless (eq da #'py-object.__delattr__)
				      da))))
	       
	       (x.attr.__del__  (unless x.__delattr__
				  (when x.attr
				    (recursive-class-lookup-and-bind x.attr '{__del__})))))
	  
	  (or (when x.__delattr__
                (py-call (bind-val x.__delattr__ x x.class) (symbol-name attr.sym)))
	      
	      (when x.attr.__del__
		(if (eq x.attr.__del__ *the-none*)
		    (py-raise '{AttributeError}
			      "Cannot delete attribute ~A of ~A (__del__ = None)"
			      x attr.sym)
		  (py-call x.attr.__del__ x)))
		
	      (let ((d (dict x)))
		(when d
		  #+(or)(warn "delattr ~A ~A: removing from dict ~A"
			      x attr.sym d)
		  (or (sub/dict-del d (symbol-name attr.sym))
                      (py-raise '{AttributeError}
				"Cannot delete attribute ~A of object ~A: does not exist"
				attr.sym x))))
	      
	      (py-raise '{AttributeError}
			"Cannot delete attribute ~A of ~A"
			attr.sym x)))
      
      ;; x.attr = val
      ;; <cls>.__setattr__ takes precedence over <cls.attr>.__set__
      (let* ((x.attr.__set__  (when x.attr
				(recursive-class-lookup-and-bind x.attr '{__set__})))
	     (setattr-active  (loop for (obj . at) in *py-object.__setattr__-active*
				  if (and (eq obj x) (string= at attr.sym))
				  return t
				  finally (return nil)))
	     
	     (x.__setattr__   (unless setattr-active
				(let ((sa (recursive-class-dict-lookup x.class '{__setattr__})))
				  (unless (eq sa #'py-object.__setattr__)
				    sa)))))
	
	#+(or)(warn "x: ~A attr: ~A x.attr: ~A x.attr.__set__: ~A x.__setattr__: ~A"
		    x attr x.attr x.attr.__set__ x.__setattr__)
	
	(or (when x.__setattr__
		(py-call (bind-val x.__setattr__ x x.class) (symbol-name attr.sym) val))
	      
	    (when x.attr.__set__
	      (if (eq x.attr.__set__ *the-none*)
		  (py-raise '{AttributeError}
			    "Cannot set attribute ~A of ~A (__set__ = None)"
			    x attr.sym)
		(py-call x.attr.__set__ x val)))
	    
	    (when (eq attr.sym '__dict__)
	      (setf (slot-value x 'dict) val)
	      t)
	    
	    (when (eq attr.sym '__class__)
	      (setf (py-class-of x) val)
	      t)
	    
	    (let ((d (dict x)))
	      (when d
		(sub/dict-set d attr.sym val)
                t))

	    (py-raise '{AttributeError}
		      "(setf py-attr): no __setattr__ method for ~S" x))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Descriptors

;; The Python Reference Manual says:
;; 
;;   3.3.2.2 Implementing Descriptors
;;   
;;   In general, a descriptor is an object attribute with ``binding
;;   behavior'', one whose attribute access has been overridden by
;;   methods in the descriptor protocol: __get__(), __set__(), and
;;   __delete__().  If any of those methods are defined for an object,
;;   it is said to be a descriptor.
;; 
;; A descriptor is called a "data descriptor" iff it implements
;; `__set__'. Whether this is the case influences the attribute lookup
;; order.

(defun descriptor-p (x)
  (let ((x.class (py-class-of x)))
    (or (recursive-class-dict-lookup x.class '{__get__})
	(recursive-class-dict-lookup x.class '{__set__})
	(recursive-class-dict-lookup x.class '{__delete__}))))

(defun data-descriptor-p (x)
  "Returns DES-P, __SET__"
  ;; check for None?
  (recursive-class-dict-lookup (py-class-of x) '{__set__}))


;; iterator from lambda

(defclass py-func-iterator (py-core-object)
  ((name        :initarg :name)
   (func        :initarg :func :type function)
   (stopped-yet :initform nil))
  (:metaclass py-core-type))

(defclass py-func-iterator-sendable (py-func-iterator)
  ()
  (:metaclass py-core-type))

(defun make-iterator-from-function (&rest args &key func &allow-other-keys)
  "Create an iterator that calls f again and again. F somehow has
to keep its own state. As soon as F returns NIL, it is considered
finished; F will then not be called again."
  (check-type func function)
  (apply #'make-instance 'py-func-iterator args))

(def-py-method py-func-iterator.next (fi)
  (py-func-iterator.next-or-send fi))

(defun py-func-iterator.next-or-send (fi &rest args)
  (declare (dynamic-extent args))
  (with-slots (stopped-yet func) fi
    (tagbody
      (when stopped-yet (go stop))
      (let ((val (apply func args)))
        (when val
          (return-from py-func-iterator.next-or-send val)))
      (setf stopped-yet t)
     stop
      (funcall 'raise-StopIteration))))

(def-py-method py-func-iterator-sendable.send (fi val)
  (py-func-iterator.next-or-send fi val))

(def-py-method py-func-iterator.__repr__ (fi)
  (with-output-to-string (s)
    (print-unreadable-object (fi s :identity t)
      (with-slots (name) fi
	(format s "generator ~@[~A~]" name)))))

(def-py-method py-func-iterator.__iter__ (f)
  f)

(defmethod print-object ((x py-func-iterator) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (with-slots (name func stopped-yet) x
      (format stream ":name ~A  :func ~A  :stopped ~A"
	      name func stopped-yet))))

(defclass generator-process (py-func-iterator)
  ((process :initarg :process))
  (:metaclass py-core-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Calling objects (functions, classes, instances)

(defgeneric py-call (f &rest args)

  (:method ((f null) &rest args)
	   (error "PY-CALL of NIL"))
  
  (:method ((f t) &rest args)
	   (let ((__call__ (recursive-class-lookup-and-bind f '{__call__})))
	     (if __call__
		 (apply #'py-call __call__ args)
	       (error "Don't know how to call: ~S (args: ~A)" f args))))

  ;; XXX For bound/unbound/static method: need to check if object is
  ;; not instance of (user-defined) subclass?
  
  (:method ((f py-bound-method) &rest args)
	   (apply #'py-bound-method.__call__ f args))
  
  (:method ((f py-unbound-method) &rest args)
	   (apply #'py-unbound-method.__call__ f args))
  
  (:method ((f py-static-method) &rest args)
	   (apply #'py-static-method.__call__ f args))
  
  ;; Avoid infinite recursion:
  (:method ((f function) &rest args) (apply f args)))


(defun py-classlookup-bind-call (x attr &rest args)
  "Returns RESULT, METH-FOND-P"
  (let ((m (recursive-class-lookup-and-bind x attr)))
    (if m
	(values (apply #'py-call m args) t)
      (values nil nil))))


;; Compiler macros are defined in optimize.cl; PY-CALL is exceptional
;; in that this function is used many times in this module already,
;; and we benefit from inlining it.

#+(and allegro-version>= (version>= 8 0))
(define-compiler-macro py-call (&whole whole prim &rest args)
  (declare (ignorable whole))
  `(locally (declare (notinline py-call))
     
     ,(cond ((and (listp prim)
		  (eq (first prim) 'py-attr)
		  (= (length prim) 3))
             ;; Optimize "obj.attr(..args..)" = (py-call (py-attr obj attr) ..args..)
             ;; so the allocation of a bound method object is skipped.
	     #+(or)(warn "inlining (py-call (py-attr ..) ..):  ~A.~A(..)"
			 (second prim) (second (third prim)))
	     (destructuring-bind (x attr) (cdr prim)
	       (assert (and (listp attr)
			    (eq (first attr) 'quote)
			    (symbolp (second attr))))
	       `(multiple-value-bind (.a .b .c)
		    (py-attr ,x ,attr :bind-class-attr nil)
		  (if (eq .a :class-attr)
		      (progn #+(or)(assert (functionp .b))
			     #+(or)(warn "saving bound method ~A ~A" .b .c)
			     (funcall .b .c ,@args))
		    (py-call .a ,@args)))))
	    
	    ((and (listp prim)
		  (eq (first prim) 'bind-val)
		  (= (length prim) 4))
             ;; Optimize  (py-call (bind-val val x x.class) ..args..)
             ;; where val is a function and x an instance, so it doesn't allocate
             ;; bound method.
	     #+(or)(warn "inlining (py-call (bind-val ..) ..) ~A" whole)
	     (destructuring-bind (val x x.class) (cdr prim)
	       `(let ((.val ,val)
		      (.x ,x)
		      (.x.class ,x.class))
		  (if (functionp .val) ;; XXX Maybe check for user-defined subclasses of function?
		      (progn #+(or)(warn "saving binding ~A" ',whole)
			     (funcall .val .x ,@args))
		    (py-call (bind-val .val .x .x.class) ,@args)))))
	    (t 
             ;; Optimize case where PRIM is a function.
             (let ((a (gensym "args"))
                   (p (gensym "prim")))
               (let ((res `(locally (declare (optimize (speed 3)))
                             (let ((,p ,prim))
                               (with-stack-list (,a ,@args)
                                 (if (functionp ,p)
                                     (progn #+(or)(warn "inlined py-call <function> ~A" ,p)
                                            (apply 'funcall (the function ,p) ,a))
                                   (apply 'py-call ,p ,a)))))))
                 #+(or)(format t ";; py-call => ~A" res)
                 res))))))


;;; Subscription of items (sequences, mappings)

(defgeneric py-subs (x item)
  (:method ((x t) (item t))
	   (let ((gi (recursive-class-dict-lookup (py-class-of x) '{__getitem__})))
	     (if gi
		 (py-call gi x item)
	       (py-raise '{TypeError} "Object ~S does not support item extraction" x)))))

(defgeneric (setf py-subs) (new-val x item)
  (:method (new-val x item)
	   #+(or)(warn "(setf py-subs) T T: ~S ~S" x item)
	   
	   (if (null new-val)
	       
	       ;; delete item
	       (let* ((x.cls (py-class-of x))
			   (__delitem__ (recursive-class-dict-lookup x.cls '{__delitem__})))
		      (if __delitem__
			    (py-call __delitem__ x item)
			(py-raise '{TypeError}
				  "Object ~A (a ~A) has no `__delitem__' method"
				  x (class-name (py-class-of x)))))
	     
	     (let ((si (recursive-class-dict-lookup (py-class-of x) '{__setitem__})))
	       (if si
		   (py-call si x item new-val)
		 (py-raise '{TypeError} "Object ~S does not support item assignment" x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Math ops:  + - << ~  (etc)  and inplace variants:  += -= <<= ~=  (etc)

;; a + b   ->  (py-+ a b)   ->  a.__add__(b) or b.__radd__(a)
;; 
;; x += y  ->  (py-+= x y)  ->  x.__iadd__(y)
;;                              fallback: x = x.__add__(y)
;; 
;; The fall-back is not part of the py-+= function, because it
;; operates on /places/, while the function only tries the in-place
;; operation on a /value/.
;; 
;; The return value of py-+= indicates whether the in-place method
;; __iadd__ was found.

(defun raise-invalid-operands (operation left &optional (right nil right-p))
  (py-raise '{TypeError} "Operation `~A' not supported for operand~@[s~*~] `~A'~:[~; and `~A'~]."
            operation right-p left right-p right))

(defvar *binary-op-funcs-ht* (make-hash-table :test #'eq))
(defvar *binary-iop-funcs-ht* (make-hash-table :test #'eq))
(defvar *binary-iop->op-ht* (make-hash-table :test #'eq))
  
(defun get-binary-op-func-name (op)
  (or (gethash op *binary-op-funcs-ht*)
      (error "missing binary op func: ~A" op)))

(defun get-binary-iop-func-name (iop)
  (or (gethash iop *binary-iop-funcs-ht*)
      (error "missing binary iop func: ~A" iop)))

(defun get-binary-op-func-name-from-iop (iop)
  (let ((op (or (gethash iop *binary-iop->op-ht*)
		(error "IOP ~S has no OP counterpart" iop))))
    (get-binary-op-func-name op)))
  
(defmacro def-math-func (op-syntax op-func l-meth r-meth iop-syntax iop-func i-meth)
  `(progn
     
     (defgeneric ,op-func (x y)
       (:method ((x t) (y t))
		(let* ((x.class (py-class-of x))
		       (y.class (py-class-of y))
		       (y-sub-of-x (and (not (eq x.class y.class))
					(subtypep y.class x.class))))
		  
		  (loop
		      with try-right = y-sub-of-x
		      with finish = nil
		      do (let* ((op-meth (recursive-class-dict-lookup
					  (if try-right y.class x.class)
					  (if try-right ',r-meth ',l-meth)))
				(res (and op-meth 
					  (py-call op-meth
						   (if try-right y x)
						   (if try-right x y)))))
			   (when res
			     (unless (eq res (load-time-value *the-notimplemented*))
			       (return res))))
			 
			 (if finish
			     (raise-invalid-operands ',op-syntax x y)
			   (setf try-right (not try-right)
				 finish t))))))
     
     ,(when op-syntax
	`(setf (gethash ',op-syntax *binary-op-funcs-ht*) ',op-func))
     
     ,(when iop-func
	`(defgeneric ,iop-func (x val)
	   (:method ((x t) (val t))
		    (let* ((iop-meth (recursive-class-dict-lookup (py-class-of x) ',i-meth))
			   (res (and iop-meth 
				     (py-call iop-meth x val))))
		      (and iop-meth
			   (not (eq res (load-time-value *the-notimplemented*))))))))
     
     ,(when iop-syntax
	`(setf (gethash ',iop-syntax *binary-iop-funcs-ht*) ',iop-func))
     
     ,(when (and iop-syntax op-syntax)
	`(setf (gethash ',iop-syntax *binary-iop->op-ht*) ',op-syntax))))


;; /t/ is not Python syntax, but a hack to support __future__ feature
;; `true division'
(def-math-func [+]   py-+    {__add__}      {__radd__}       [+=]   py-+=   {__iadd__}      )
(def-math-func [-]   py--    {__sub__}      {__rsub__}       [-=]   py--=   {__isub__}      )
(def-math-func [*]   py-*    {__mul__}      {__rmul__}       [*=]   py-*=   {__imul__}      )
(def-math-func [/t/] py-/t/  {__truediv__}  {__rtruediv__}   [/t/]  py-/t/= {__itruediv__}  )
(def-math-func [//]  py-//   {__floordiv__} {__rfloordiv__}  [//=]  py-//=  {__ifloordiv__} ) 
(def-math-func [/]   py-/    {__div__}      {__rdiv__}       [/=]   py-/=   {__idiv__}      )
(def-math-func [%]   py-%    {__mod__}      {__rmod__}       [%=]   py-%=   {__imod__}      )
(def-math-func [<<]  py-<<   {__lshift__}   {__rlshift__}    [<<=]  py-<<=  {__ilshift__}   )
(def-math-func [>>]  py->>   {__rshift__}   {__rrshift__}    [>>=]  py->>=  {__irshift__}   )
(def-math-func [&]   py-&    {__and__}      {__rand__}       [&=]   py-&=   {__iand__}      )
(def-math-func [\|]  py-\|   {__or__}       {__ror__}        [\|=]  py-\|=  {__ior__}       )
(def-math-func [^]   py-^    {__xor__}      {__rxor__}       [^=]   py-^=   {__ixor__}      )
(def-math-func [<divmod>] py-divmod {__divmod__} {__rdivmod__}   nil    nil     nil        )

;; a**b (to-the-power) is a special case:
;;   
;; - method __pow__ takes an optional third argument: the c in
;;   (a**b)%c that argument can be supplied using the built-in
;;   function POW, but not syntactically using **
;;  
;; - there are no __rpow__ methods
;;   
;; - so, this function py-** always gets 2 arguments when called for
;;   the a**b syntax, but may have a third argument when called for
;;   the built-in function POW.

(defun py-** (x y &optional z)
  (let* ((op-meth (recursive-class-lookup-and-bind x '{__pow__}))
	 (res (and op-meth (if z
			       (py-call op-meth y z)
			     (py-call op-meth y)))))
    
    (if (and res (not (eq res (load-time-value *the-notimplemented*))))
	res
      (raise-invalid-operands '** x y))))

(setf (gethash '[**] *binary-op-funcs-ht*) 'py-**)

;; **= has similar ugliness

(defun py-**= (x y &optional z)
  (let* ((x.class (py-class-of x))
	 (iop-meth (recursive-class-dict-lookup x.class '{__ipow__}))
	 (res (and iop-meth (if z
				(py-call iop-meth x y z)
			      (py-call iop-meth x y)))))
    
    (if (and iop-meth (not (eq res (load-time-value *the-notimplemented*))))
	res
      nil)))

(setf (gethash '**= *binary-iop-funcs-ht*) 'py-**=)


(defvar *unary-op-funcs-ht* (make-hash-table :test #'eq))
(defun get-unary-op-func-name (op) (or (gethash op *unary-op-funcs-ht*)
				  (error "missing unary op func: ~A" op)))

(defmacro def-unary-op-func (syntax fname meth)
  `(progn (defgeneric ,fname (x)
	    (:method ((x t))
		     (let* ((x.class (py-class-of x))
			    (op-meth (recursive-class-dict-lookup x.class ',meth))
			    (res (and op-meth (py-call op-meth x))))
		       (if (or (null op-meth)
			       (eq res (load-time-value *the-notimplemented*)))
			   (raise-invalid-operands ',syntax x)
			 res))))
	  (setf (gethash ',syntax *unary-op-funcs-ht*) ',fname)))

(def-unary-op-func [~]  py-unary-~  {__invert__} )
(def-unary-op-func [+]  py-unary-+  {__pos__}    )
(def-unary-op-func [-]  py-unary--  {__neg__}    )

(defgeneric py-not (x)
  (:method ((x t))
	   (py-bool (not (py-val->lisp-bool x)))))

(setf (gethash '[not] *unary-op-funcs-ht*) 'py-not)

;; Equality and membership testing:  a in b, a not in b, a is b, a is not b

(defvar *binary-test-funcs-ht* (make-hash-table :test #'eq))
(defun get-binary-test-func (op) (or (gethash op *binary-test-funcs-ht*)
				     (error "missing binary test func: ~A" op)))

(defgeneric py-in (x seq)
  (:method ((x t) (seq t))
	   #+(or)(warn "py-in T T: ~S ~S" x seq)
	   ;; use __contains__, fall back on iterator
	   (let ((contains-meth (recursive-class-dict-lookup
				 (py-class-of seq) '{__contains__})))
	     (if contains-meth
		 (let ((res (py-call contains-meth seq x)))
		   (cond ((eq res t)   (load-time-value *the-true*))
			 ((eq res nil) (load-time-value *the-false*))
			 (t            res)))
	       (loop with f = (get-py-iterate-fun seq)
		   for seq-item = (funcall f)
		   while seq-item
		   when (py-==->lisp-val x seq-item) return (load-time-value *the-true*)
		   finally (return (load-time-value *the-false*)))))))
	     
(defgeneric py-not-in (x seq)
  (:method ((x t) (seq t))
	   (py-not (py-in x seq))))

(setf (gethash '[in] *binary-op-funcs-ht*) 'py-in)
(setf (gethash '[not in] *binary-op-funcs-ht*) 'py-not-in)

(defgeneric py-is (x y)
  (:method ((x t) (y t))
	   (if (eq x y) *the-true* *the-false*)))

(defgeneric py-is-not (x y)
  (:method ((x t) (y t))
	   (if (eq x y) *the-false* *the-true*)))

(setf (gethash '[is] *binary-op-funcs-ht*) 'py-is)
(setf (gethash '[is not] *binary-op-funcs-ht*) 'py-is-not)



;; Binary comparison operations
;; 
;; It appears that all comparisons -- < > <= >= == != <> -- are
;; defined in terms of the outcome of built-in function `cmp'
;; (#'py-cmp).
;; 
;; #'py-< implements the logic for "a < b"
;; 
;; Note this important point: "a < b" is NOT directly translated into
;; a.__lt__(b), although this __lt__ method *might* be called by
;; __cmp__, as might b.__ge__ in this case.
;; 
;;  '<  -->  #'py-<
;; 
;; This mapping is used by the interpreter, in EVAL-COMPARISON
;; (pyeval.cl).

(defvar *binary-comparison-funcs-ht* (make-hash-table :test #'eq))
(defun get-binary-comparison-func-name (op)
  (or (gethash op *binary-comparison-funcs-ht*)
      (error "missing comparison func: ~A" op)))

(defmacro def-comparison (syntax func test-x-y)
  `(progn (defgeneric ,func (x y)
	    (:method ((x t) (y t))
		     (if ,test-x-y
			 (load-time-value *the-true*)
		       (load-time-value *the-false*))))
	  (setf (gethash ',syntax *binary-comparison-funcs-ht*) ',func)))
	    
(defun py-id (x)
  #+allegro (excl:lispval-to-address x)
  #+lispworks (system:object-address x)
  #-(or allegro lispworks) (error "TODO: id() not implemented for this Lisp implementation"))

(defgeneric py-cmp (x y)
  (:documentation
   "Compare two objects, of which at least one is a user-defined-object.
Returns one of (-1, 0, 1): -1 iff x < y; 0 iff x == y; 1 iff x > y")
  
  (:method ((x t) (y t))
	   #+(or)(warn "cmp ~S ~S" x y)
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
      
	       (let ((pt (ltv-find-class 'py-type)))
		 (when (or (eq pt x.class) (eq pt y.class))
		   (return-from py-cmp
		     (if (eq x y)
			 0
		       -1))))

	       ;; If the class is equal and it defines __cmp__, use that.
      
	       (when (eq x.class y.class)
		 (let* ((__cmp__ (recursive-class-dict-lookup x.class '{__cmp__})) ;; XXX bind
			(cmp-res (when __cmp__ (py-call __cmp__ x y))))
		   (when (and cmp-res
			      (not (eq cmp-res *the-notimplemented*)))
		     (return-from py-cmp (normalize cmp-res)))))

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
		 (loop for (meth-name res-value) in `(({__eq__}   0)
						      ({__lt__}  -1)
						      ({__gt__}   1))
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
				(return-from py-cmp
				  (if y-sub-of-x (- res-value) res-value))))))))

	       ;; So the rich comparison operations didn't lead to a result.
	       ;; 
	       ;; object.c - try_3way_compare(v,w)
	       ;; 
	       ;; Now, first try X.__cmp__ (even it y.class is a subclass of
	       ;; x.class) and Y.__cmp__ after that.

	       (let* ((meth (recursive-class-dict-lookup x.class '{__cmp__}))
		      (res (when meth
			     (py-call meth x y))))
		 (when (and res (not (eq res *the-notimplemented*)))
		   (let ((norm-res (normalize res)))
		     (return-from py-cmp norm-res))))

	       (let* ((meth (recursive-class-dict-lookup y.class '{__cmp__}))
		      (res (when meth
			     (py-call meth y x))))
		 (when (and res (not (eq res *the-notimplemented*)))
		   (let ((norm-res (- (normalize res))))
		     (return-from py-cmp norm-res))))
      
	       ;; CPython now does some number coercion attempts that we don't
	       ;; have to do because we have first-class numbers. (I think.)
      
	       ;; object.c - default_3way_compare(v,w)
	       ;; 
	       ;; Two instances of same class without any comparison operator,
	       ;; are compared by pointer value. Our function `py-id' fakes
	       ;; that.
      
	       (when (eq x.class y.class)
		 (let ((x.id (py-id x))
		       (y.id (py-id y)))
		   (return-from py-cmp
		     (cond ((< x.id y.id) -1)
			   ((= x.id y.id) 0)
			   (t             1)))))
      
	       ;; None is smaller than everything (excluding itself, but that
	       ;; is catched above already, when testing for same class;
	       ;; NoneType is not subclassable).
      
	       (cond ((eq x *the-none*) (return-from py-cmp -1))
		     ((eq y *the-none*) (return-from py-cmp  1)))
      
	       ;; Instances of different class are compared by class name, but
	       ;; numbers are always smaller.
      
	       ;; Probably, when we arrive here, there is a bug in the logic
	       ;; above. Therefore print a warning.
      
	       #+(or)(warn "[debug] CMP can't properly compare ~A and ~A." x y)
      
	       (return-from py-cmp
		 (if (string< (class-name x.class) (class-name y.class))
		     -1
		   1))
      
	       ;; Finally, we have either two instances of different non-number
	       ;; classes, or two instances that are of incomparable numeric
	       ;; types.
	       (return-from py-cmp
		 (cond ((eq x y)                   0)
		       ((< (py-id x) (py-id y))   -1)
		       (t                          1)))))))


(def-comparison  [<]  py-<   (=  (the (integer -1 1) (py-cmp x y)) -1))
(def-comparison  [>]  py->   (=  (the (integer -1 1) (py-cmp x y))  1))
(without-redefinition-warnings
 (fmakunbound 'py-==)
 (def-comparison [==] py-==  (=  (the (integer -1 1) (py-cmp x y))  0)))
(def-comparison  [!=] py-!=  (/= (the (integer -1 1) (py-cmp x y))  0)) ;; lexer: <> --> !=
(def-comparison  [<=] py-<=  (<= (the (integer -1 1) (py-cmp x y))  0))
(def-comparison  [>=] py->=  (>= (the (integer -1 1) (py-cmp x y))  0))

;; Necessary for bootstrapping (py-dict + def-py-method)
(defmethod py-== ((x symbol) (y symbol))
  (unless (eq (symbol-package x) (symbol-package y))
    (break "py-== called for two symbols in different packages: ~S and ~S" x y))
  (if (eq x y) 1 0))

(defmethod py-== ((x symbol) (y string))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (or (eq (symbol-name x) y)
          (and (char= (aref (symbol-name x) 0) (aref y 0))
               (string= (symbol-name x) y)))
      1
    0))

(defmethod py-== ((x string) (y symbol))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (rotatef x y)
  (if (or (eq (symbol-name x) y)
          (and (char= (aref (symbol-name x) 0) (aref y 0))
               (string= (symbol-name x) y)))
      1
    0))

(defmethod py-== ((x symbol) y)
  (py-== (symbol-name x) y))

(defmethod py-== (x (y symbol))
  (py-== x (symbol-name y)))


(defgeneric py-val->lisp-bool (x)
  (:method ((x number)) (/= x 0))
  (:method ((x string)) (> (length x) 0))
  (:method ((x vector)) (> (length x) 0))
  (:method (x)          (py-val->lisp-bool (py-nonzero x))))


;; Shortcut functions

(defmacro def-py-shortcut-func (funcname method &key error)
  `(defgeneric ,funcname (x)
     (:method ((x t))
	      #+(or)(warn "~A T: ~S" ',funcname x)
	      (let* ((,method (recursive-class-lookup-and-bind x ',method)))
			(if ,method
			    (py-call ,method)
			  ,(or error
			       `(py-raise '{TypeError}
					  "Object ~A (a ~A) has no `~A' method"
					  x (class-name (py-class-of x))
					  ',method)))))))

(def-py-shortcut-func py-abs  {__abs__} )
(def-py-shortcut-func py-repr {__repr__})
(def-py-shortcut-func py-str  {__str__} :error (py-repr x))
(def-py-shortcut-func py-hex  {__hex__} )
(def-py-shortcut-func py-oct  {__oct__} )
(def-py-shortcut-func py-len  {__len__} )
(def-py-shortcut-func py-nonzero {__nonzero__} )
(def-py-shortcut-func py-float {__float__})

(without-redefinition-warnings
 (fmakunbound 'py-hash)
 (def-py-shortcut-func py-hash {__hash__}))

(defmethod py-hash ((x symbol))
  ;; Returning (py-hash (symbol-name x)) leads to infinite recursion.
  (or (get x 'py-hash)
      (let ((hash (py-string.__hash__ (symbol-name x))))
        (setf (get x 'py-hash) hash))))

#|| 
;; Defining the shortcut functions like this would be more efficient,
;; in that is skips creation of bound method.
(defmethod py-hash ((x t))
  (catch :getattr-block
    (multiple-value-bind (.a .b .c)
        (py-attr x '{__hash__} :bind-class-attr nil :via-getattr t) ;; but should also skip instance dict
      ;;(break "ph: ~A ~A ~A" .a .b .c)
      (if (eq .a :class-attr)
          (funcall .b .c)
        (return-from py-hash (py-call .a)))))
  (py-raise '{TypeError}
            "Object ~A (a ~A) has no `~A' method"
            x (class-name (py-class-of x))
            '{__hash__}))
||#

(defun py-val->string (x)
  (if (symbolp x) ;; Symbols don't represent Python values, but this is just handy for ourselves
      (symbol-name x)
  (let ((s (deproxy x))) ;; deproxy, as it may be py-string subclass instance
    (if (stringp s)
	s
      (py-raise '{TypeError} "Expected a string, but got: ~S (a ~A)" x (class-of x))))))

(defun py-val->integer (x &key min)
  (let ((i (deproxy x)))
    (if (and (integerp i) (if min (>= i min) t))
	i
      (py-raise '{TypeError} "Expected an integer ~@[>= ~A~]; got: ~S" min x))))

(defun py-val->number (x)
  (let ((n (deproxy x)))
    (if (numberp n)
	n
      (py-raise '{TypeError} "Expected a number; got: ~S" x))))


(defun py-repr-string (x &key circle) 
  "Convert `repr(x)' to Lisp string."
  (py-val->string (if circle (py-repr-circle x) (py-repr x))))

(defun py-str-string (x &key circle)
  "Convert `str(x)' to Lisp string."
  (py-val->string (if circle (py-str-circle x) (py-str x))))

;;; Printing with circle (recursion) detection

(defvar *circle-detection-mechanism*
    #+allegro :hash-table
    #+lispworks :level
    #-(or allegro lispworks) :level)

(defvar *circle-print-abbrev* "...")

(defvar *circle-print-ht* nil)
(defvar *circle-print-max-occur* 3)
(defvar *circle-print-max-num-objects* 100)

(defvar *circle-level* 0)
(defconstant +max-circle-level+ 5)

#+allegro
(excl:def-fwrapper print-circle-wrapper (x)
  (cond ((typep x '(or string number))
         (excl:call-next-fwrapper))
        ((and (typep x '(vector list))
              (> (length x) *circle-print-max-num-objects*))
         *circle-print-abbrev*)
        ((or (> (incf (gethash x *circle-print-ht* 0)) *circle-print-max-occur*)
             (>= (hash-table-count *circle-print-ht*) *circle-print-max-num-objects*))
         *circle-print-abbrev*)
        (t 
         (excl:call-next-fwrapper))))

(defmacro with-circle-detection (&body body)
  `(let ((f (lambda () ,@body)))
     (declare (dynamic-extent f))
     (call-with-circle-detection f)))

(defun call-with-circle-detection (f)
  (declare (dynamic-extent f))
  (ecase *circle-detection-mechanism*
    (:hash-table (if *circle-print-ht*
                     (funcall f)
                   (let ((*circle-print-ht* (make-hash-table :test 'eq)))
                     #+allegro
                     (unwind-protect
                         (progn (excl:fwrap #'py-repr :print-circle-wrapper 'print-circle-wrapper)
                                (funcall f))
                       (excl:funwrap #'py-repr :print-circle-wrapper))
                     #-allegro (error "No hash-table based circle detection defined in this implementation."))))
    (:level (let ((*circle-level* (1+ *circle-level*)))
              (if (>= *circle-level* +max-circle-level+)
                  *circle-print-abbrev*
                (funcall f))))))
  
(defun py-repr-circle (x)
  (with-circle-detection (py-repr x)))

(defun py-str-circle (x)
  (with-circle-detection (py-str x)))


(defun py-string->symbol  (x &optional (package #.(find-package :clpython.user)))
  ;; {symbol,string} -> symbol
  (if (symbolp x) 
      x
    (let ((str (deproxy x)))
      (if (stringp str)
	  (or (find-symbol str package)
	      (intern str package))
	(py-raise '{TypeError} "Object is not a string (or symbol): ~A" x)))))

(defun py-symbol->string (x)
  ;; {symbol,string} -> string
  (etypecase x
    (symbol (symbol-name x))
    (string x)))


(defun repr-fmt (stream argument &optional colon-p at-p &rest parameters)
  "Wrapper function for PY-REPR that is usable inside format string, using
the ~/.../ directive: ~/clpython:repr-fmt/"
  (when (or colon-p at-p parameters)
    (error "Format string function py-repr-fmt does not support colon, ~
            at or parameters"))
  
  (let ((s (py-repr-string argument :circle t)))
    (write-string s stream)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general useful iteration constructs
   
(defgeneric get-py-iterate-fun (x)
  (:documentation
   "Return a function that when called repeatedly returns VAL, T, where VAL is the
next value gotten by iterating over X. Returns NIL, NIL upon exhaustion.")
  (:method ((x t))
	   (let* ((x.cls       (py-class-of x))
		  (__iter__    (recursive-class-dict-lookup x.cls '{__iter__}))
		  (__getitem__-unb (unless __iter__
				     (recursive-class-dict-lookup x.cls '{__getitem__})))
		  (__getitem__ (when __getitem__-unb
				 (bind-val __getitem__-unb x x.cls))))

	     ;; TODO: binding __getitem__ using __get__ is not done at
	     ;; all yet.
	     
	     #+(or)(warn "GET-PY-ITERATE-FUN ~A (a ~A)~% -> __iter__ = ~A; __getitem = ~A"
			 x x.cls __iter__ __getitem__)
	     
	     (cond (__iter__ ;; Preferable, use __iter__ to retrieve x's iterator
		    (let* ((iterator     (py-call (bind-val __iter__ x x.cls)))
			   (iterator.cls (py-class-of iterator))
			   
			   (next-meth-unbound
			    (or (recursive-class-dict-lookup iterator.cls '{next})
				(py-raise
				 '{TypeError} "The value returned by ~A's `__iter__' method ~
                                 	       is ~A, which is not an iterator (no `next' method)"
				 x iterator)))
			   
			   (next-meth-bound 
			    ;; Efficiency optimization: skip creation of bound methods
			    (unless (functionp next-meth-unbound)
			      (bind-val next-meth-unbound iterator iterator.cls))))
		      
		      ;; Note that we just looked up the `next' method before the first
		      ;; value is demanded. This is semantically incorrect (in an
		      ;; ignorable way) as the method should be looked up on the
		      ;; request of every new value. For efficiency that's not done.
		      
		      ;; (excl:named-function (:py-iterate-fun using __iter__)
		      (lambda ()
			(handler-case (values (if next-meth-bound
						  (py-call next-meth-bound)
						(funcall next-meth-unbound iterator)))
			  ({StopIteration} () (values nil nil))
			  (:no-error (val)  (values val t)))))) ;; )
		   
		   
		   (__getitem__ ;; Fall-back: call __getitem__ with successive integers
		    (let ((index 0))
		      ;; (excl:named-function (:py-iterate-fun using __getitem__)
		      (lambda ()
			(handler-case (values (py-call __getitem__ index))
			  
			  ;; ok if this happens when index = 0: then it's an empty sequence
			  ({IndexError} () (values nil nil)) 
			  
			  (:no-error (val) (progn (incf index)
						  (values val t))))))) ;; )
		   
		   (t
		    (py-raise '{TypeError} "Iteration over non-sequence: ~A." x))))))

(defgeneric map-over-py-object (func object)
  (:documentation 
   "Iterate over OBJECT, calling the Lisp function FUNC on each value. Returns nothing.")
  
  (:method ((func function) (object t))
	   #+(or)(break "map over ~A" object)
	   (loop with it-fun = (get-py-iterate-fun object)
	       for val = (funcall it-fun)
	       while val do (funcall func val))))


(defgeneric py-iterate->lisp-list (object)
  (:documentation
   "Returns a Lisp list, that may not be modified destructively (as for e.g. tuples,
the lisp list will be returned).")
  (:method ((x t))
	   (loop with it-fun = (get-py-iterate-fun x)
	       for val = (funcall it-fun)
	       while val collect val)))



(defun get-py-iterator-for-object (x)
  (let* ((x.cls       (py-class-of x))
	 (__iter__    (recursive-class-dict-lookup x.cls '{__iter__})))
    
    (if __iter__
	(py-call __iter__ x)
      (let ((f (get-py-iterate-fun x)))
	(make-iterator-from-function :func f)))))


(defvar *stdout-softspace* 0 "should a space be printed in front of next arg?")

(defun py-print (dest items comma?)
  (let ((*print-pretty* nil))
    (let* ((write-func (if dest 
			   (py-attr dest '{write})
			 (lambda (s) (write-string s) (finish-output))))
	   
	   (softspace-val (if dest
			      (handler-case 
				  (py-attr dest '{softspace})
			      
				({Exception} (c) (progn 
						 (warn "PY-PRINT: exception while retrieving ~
                                                    'softspace: ~S" c)
						 nil))
				(error (c) (progn (warn "PY-PRINT: getting attr 'softspace of ~S ~
                                                     gave Lisp error: ~S" dest c)
						  nil)))
			    *stdout-softspace*))
	   (softspace-p (py-val->lisp-bool softspace-val))
	   (last-char-written nil))
      
      (loop
	  for x in items
	  for i from 0
	  do (when (or (> i 0)
		       (and (= i 0) softspace-p))
	       (py-call write-func " "))

	     (let ((s (if (stringp x) x (py-str-string x))))
	       (py-call write-func s)
	       (when (> (length s) 0)
		 (setf last-char-written (aref s (1- (length s)))))))

      (unless comma?
        (py-call write-func #.(string #\Newline)))
      
      (let* ((printed-newline-already (or (not comma?)
					  (char= last-char-written #\Newline)))
	     (must-print-newline-next-time (py-bool (not printed-newline-already))))
	
	(if dest
	    (setf (py-attr dest '{softspace}) must-print-newline-next-time)
	  (setf *stdout-softspace* must-print-newline-next-time))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class change signals; only called for user-defined classes that are not
;; at the metaclass level.

#+(or)
(without-redefinition-warnings
 ;; already defined above; not really needed
 (defmethod initialize-instance :after ((cls py-type) &rest initargs)
   (declare (ignore initargs))
   (finalize-inheritance cls)
   (signal-class-created cls)))

#+(or)
(defmethod reinitialize-instance :before ((cls py-type) &rest initargs)
  (declare (ignore initargs))
  (signal-class-changed-before cls))

#+(or)
(defmethod reinitialize-instance :after ((cls py-type) &rest initargs)
  (declare (ignore initargs))
  (signal-class-changed-after cls))

(defun signal-class-dict-replacement (cls old new)
  (declare (ignore cls old new))
  #+(or)(warn "** dict replacement cls ~A" (class-name cls))
  nil)

(defun signal-class-changed-before (cls) 
  (declare (ignore cls))
  nil)

(defun signal-class-changed-after (cls)
  (declare (ignore cls))
  #+(or)(warn "** Class ~A changed" (class-name cls)))

(defun signal-class-created (cls)
  (declare (ignore cls))
  #+(or)(warn "** Class ~A created" (class-name cls)))

(defun signal-class-dict-changed (dict key)
  (declare (ignore dict key))
  ;; not all clearing is required... XXX
  #+(or)(warn "** Class had attribute ~A modified" attr))
   
(defun signal-class-dict-set-key (dict key val)
  (declare (ignore val))
  (signal-class-dict-changed dict key)
  #+(or)(warn "** Class dict changed, key ~A" key))

(defun signal-class-dict-del-key (dict key)
  (signal-class-dict-changed dict key)
  #+(or)(warn "** Class dict deleted key ~A" key))

;; Utils

(defun py-string-val->symbol (x &key (intern t))
  (let (x.string)
    (typecase x
      (symbol (return-from py-string-val->symbol x))
      (string (setf x.string x))
      (t      (setf x.string (py-val->string x))))
    
    (or (find-symbol x.string #.(find-package :clpython.user))
	(when intern
	  (intern x.string #.(find-package :clpython.user))))))

;; At this point, we have redefined the functions PY-== and PY-HASH.
;; Therefore we need to rehash all dicts.

(defun rehash-py-dicts ()
  (flet ((get-py-hts ()
           #+allegro
           (loop for d across
                 (excl::get-objects #.(sys::typecode (make-py-hash-table)))
               when (and (hash-table-p d)
                         (eq (hash-table-test d) 'py-==->lisp-val))
               collect d)
           #+lispworks
           (let (res)
             (hcl:sweep-all-objects 
              (lambda (x)
                (when (and (hash-table-p x)
                           (eq (hash-table-test x) 'py-==->lisp-val))
                  (push x res))))
             res)
           #-(or allegro lispworks)
           (error "Required rehashing of dicts not possible in this implementation."))
                                  
         (fix-ht (ht)
           (check-type ht hash-table)
           (break "ht to rehash: ~A" ht)
           ;; Lets do this the KISS way; there are just about 12 dicts to fix.
           (let* ((contents (loop for k being the hash-key in ht
                                using (hash-value v)
                                collect (cons k v)))
                  (old-py-hash #'py-hash))
             (unwind-protect
                 ;; We now modify class dicts. That means we are temporarily unable
                 ;; to look up the __hash__ attribute of instances. Therefore,
                 ;; replace hash function with one specialized on strings/symbols.
                 (progn
                   (setf (fdefinition 'py-hash)
                     (lambda (x)
                       (etypecase x
                         (string (py-string.__hash__ x))
                         (symbol (py-string.__hash__ (symbol-name x))))))
                   (clrhash ht)
                   (loop for (k . v) in contents
                       do (assert (or (symbolp k) (stringp k)))
                          (setf (gethash k ht) v)))
               (setf (fdefinition 'py-hash) old-py-hash))
             ht)))
        
    ;; Simple test to see if we have the right hash function currently.
    (let ((hts (or (get-py-hts)
                   (break "There are no dicts to rehash ~@
                           (after redefining py-== and py-hash)?!"))))
      (mapc #'fix-ht hts)
      ;; Simple test to see if we have the original hash function back.
      (assert (integerp (py-hash 42)))
      (assert (= (py-hash "a") (py-hash 'a)))
      (length hts))))

#+(or) ;; Seems not needed anymore, as at this point all dicts only contain symbols?!
(rehash-py-dicts)
