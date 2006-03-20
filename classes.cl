(in-package :python)

;;;; Python classes and metaclasses; the built-in classes including
;;;; their methods.

(defvar *py-print-safe* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Class frameword:  py-meta-type, py-type, py-dictless-object

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun py-eq-dict-hash (x)
  ;; X is string or symbol
  (sxhash (etypecase x
	    (symbol (symbol-name x))
	    (string x))))

(defun py-eq-dict-test (x y)
  ;; X,Y can be strings or symbols
  (etypecase x
    (symbol (etypecase y
	      (symbol (eq x y))
	      (string (string= (symbol-name x) y))))
    (string (etypecase y
	      (symbol (string= x (symbol-name y)))
	      (string (string= x y))))))

(defun make-py-eq-dict ()
  (make-hash-table :hash-function 'py-eq-dict-hash :test 'py-eq-dict-test))

) ;; eval-when


(defclass py-dict-mixin ()
  ;; By default, instances have a PY-EQ dict. Overrule by supplying :DICT arg.
  ((dict :initarg :dict
	 :initform (make-py-eq-dict)
	 :accessor dict)))

(defmethod dict ((x t))
  nil)

(defun hash-table-is-regular-dict-p (x)
  (eq (hash-table-test x) 'py-==->lisp-val))

(defun dict-get (x key)
  (check-type key (or string symbol))
  (let ((d (dict x)))
    (sym-gethash key d)))
#||
    (sym-gethash 
    (assert d () "dict-get: object ~A has no dict (key ~A)" x key)
    (if (and (symbolp key) (hash-table-is-regular-dict-p d))
	(gethash (symbol-name key) d)
      
      (gethash key d))))
||#

(defun dict-del (x key)
  (check-type key (or string symbol))
  (let ((d (dict x)))
    (assert d () "dict-del: object ~A has no dict (key ~A)" x key)
    (if (and (symbolp key) (hash-table-is-regular-dict-p d))
	(remhash (symbol-name key) d)
      (remhash key d))))

(defun (setf dict-get) (new-val x key)
  ;; NEW-VAL == NIL -> delete key; return whether it existed
  (check-type key (or string symbol))
  (let ((d (dict x)))

    (unless d
      (error "(setf dict-get) on dictless object: ~A ~A ~A" x key new-val))
    
    (let ((key (if (and (symbolp key) (hash-table-is-regular-dict-p d))
		   (symbol-name key)
		 key)))
      
      (if (null new-val)
	  (remhash key d)
	(setf (gethash key d) new-val)))))

(defun sym-gethash (key ht &optional (is-reg-dict (hash-table-is-regular-dict-p ht)))
  ;; KEY is either a symbol or a regular string
  (if is-reg-dict
      
      (gethash (if (symbolp key) (symbol-name key) key) ht)
    
    (let ((key.sym (if (symbolp key) key (find-symbol key #.*package*))))
      ;; If symbol not found, it can not be present as key in the HT either
      (when key.sym
	(gethash key.sym ht)))))


(defclass py-class-mixin (py-dict-mixin)
  ((mro :initarg :mro :accessor class-mro)))


;; Python metatype. Class `type' and subclasses thereof are instances
;; of py-meta-type.

(defclass py-meta-type (py-class-mixin standard-class)
  ())

(defmethod initialize-instance :after ((cls py-meta-type) &rest initargs)
  (declare (ignore initargs))
  (mop:finalize-inheritance cls))


;; A class for Python classes. This is an instance of py-meta-type

(defclass py-type (py-class-mixin standard-class)
  ()
  (:metaclass py-meta-type))

(defmethod initialize-instance :after ((cls py-type) &rest initargs)
  (declare (ignore initargs))
  (mop:finalize-inheritance cls))


;; Python classes are instances of (a subclass of) python-type

(defclass py-dictless-object (standard-object)
  ()
  (:metaclass py-type))

(mop:finalize-inheritance (find-class 'py-dictless-object))

(defclass py-object (py-dict-mixin py-dictless-object)
  ()
  (:metaclass py-type))

(mop:finalize-inheritance (find-class 'py-object))


;; Core type/object

(defclass py-core-object (py-dictless-object) ())
(defclass py-core-type   (py-type)   ())

;; User type/object

(defclass py-user-type   (py-type) ())
(defclass py-user-object (py-object)
  ((dict :initform (make-dict)))  ;; not EQ-dict  XXX check if __slots__
  (:metaclass py-user-type))

;; Lisp type/object

(defclass py-lisp-type (py-type)
  ()
  (:documentation "Metaclass for proxy classes"))

(defclass py-lisp-object (py-dictless-object)
  ((lisp-object :initarg :lisp-object :accessor proxy-lisp-val))
  (:metaclass py-lisp-type)
  (:documentation "Base class for proxy classes"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This is a bit ugly, but method classes must be evaluated prior to their
;;; use; therefore here are method classes, and below their methods.
;;;

(defclass py-method (py-core-object)
  ((func :initarg :func :accessor py-method-func))
  (:metaclass py-core-type))

(defclass py-class-method (py-method)
  ((class :initarg :class))
  (:metaclass py-core-type))

(defclass py-attribute-method (py-method)
  ()
  (:metaclass py-core-type))

(defclass py-writable-attribute-method (py-attribute-method)
  (write-func)
  (:metaclass py-core-type))

(defclass py-static-method (py-method)
  ()
  (:metaclass py-core-type))

(defparameter *writable-attribute-methods* (make-hash-table :test #'eq))

(defmacro def-py-method (cls.meth &rest args)
  (let* ((cm (symbol-name cls.meth))
	 (dot-pos (or (position #\. cm)
		      (error "Need dot in name: (def-py-method classname.methodname ..); got: ~A"
			     cls.meth)))
	 (cls  (intern (subseq cm 0 dot-pos) #.*package*))
	 (meth (intern (subseq cm (1+ dot-pos)) #.*package*))
	 (modifiers (loop while (keywordp (car args)) collect (pop args)))
	 (func-name (if (eq (car modifiers) :attribute-write)
			(intern (format nil "~A-writer" cls.meth) #.*package*)
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
       
       (let* ((cls (or (find-class ',cls) (error "No such class: ~A" ',cls))))
	 (unless (dict cls)
	   (error "Class ~A has no dict" cls))
	 (setf (dict-get cls ',meth)
	   ,(ecase (car modifiers)
	      ((nil)                `(function ,cls.meth))
	      (:static              `(make-instance 'py-static-method
				       :func (function ,cls.meth)))
	      (:attribute           `(make-instance 'py-attribute-method
				       :func (function ,cls.meth)))
	      (:attribute-read      `(let ((x (make-instance 'py-writable-attribute-method
						:func (function ,cls.meth))))
				       (setf (gethash ',cls.meth *writable-attribute-methods*) x)
				       x))
	      (:attribute-write     `(let ((f (function ,func-name))
					   (read-f (or (gethash ',cls.meth *writable-attribute-methods*)
						       (error "Attribute read function ~A not found yet"
							      ',cls.meth))))
				       (setf (slot-value read-f 'write-func) f)
				       read-f ;; stored in dict under attr name
				       ))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; dynamic class creation

(defun make-py-class (&key name context-name namespace supers cls-metaclass mod-metaclass)
  (declare (ignore context-name)) ;; XXX for now

  (assert (symbolp name))
  (assert (listp supers))
  (assert (hash-table-p namespace))
  
  ;; XXX is this a true restriction?  Custom metaclasses may allow
  ;; more kinds of `bases' in their __new__(...) ?
  
  ;; either:
  ;;  1) all supers are subtype of 'py-type   (to create a new metaclass)
  ;;  2) all supers are subtype of 'py-dictless-object (to create new "regular user-level" class)
  
  (flet ((of-type-class (s) (typep s 'class))
	 (subclass-of-py-dl-object-p (s) (subtypep s 'py-dictless-object))
	 (subclass-of-py-type-p   (s) (subtypep s 'py-type)))
    
    (unless (every #'of-type-class supers)
      (py-raise 'TypeError "Not all superclasses are classes (got: ~A)." supers))

    (loop for s in supers
	unless (or (subclass-of-py-type-p s)
		   (subclass-of-py-dl-object-p s))
	do (error "BUG? Superclass ~A is neither sub of 'type nor sub of 'object!" s))
    
    (let ((core-supers (remove-if-not (lambda (s) (typep s 'py-core-type)) supers)))
      (when core-supers
	(py-raise 'TypeError "Cannot subclass from these classes: ~A" core-supers)))
    
    (when (and (some #'subclass-of-py-type-p supers)
	       (some #'subclass-of-py-dl-object-p supers))
      (py-raise 'TypeError "Superclasses are at different levels (some metaclass, ~
                            some regular class) (got: ~A)." supers))
    

    ;; Python class `object' corresponds to Lisp class 'py-dictless-object
    ;; but the new class should have a dict:
    ;; [XXX perhaps not needed anymore, as py-object has a dict?]
    (substitute (load-time-value (find-class 'py-object))
		(load-time-value (find-class 'py-dictless-object))
		supers)
    
    
    (let ((metaclass (or cls-metaclass
			 (when supers (class-of (car supers)))
			 mod-metaclass
			 (load-time-value (find-class 'py-type)))))
      
      #+(or)(warn "metaclass: ~A" metaclass)
      
      (unless (typep metaclass 'class)
	(py-raise 'TypeError "Metaclass must be a class (got: ~A)" metaclass))
      
      (unless (or (eq metaclass (load-time-value (find-class 'py-meta-type)))
		  (subtypep metaclass 'py-type))
	(py-raise 'TypeError 
		  "Metaclass must be subclass of `type' (got class: ~A)" metaclass))
      

      ;; When inheriting from py-lisp-type (like `int'), use
      ;; py-user-type as metaclass.
      ;; XXX subclassing `function' ?
      
      (when (member metaclass
		    (load-time-value (list (find-class 'py-lisp-type))
					   (find-class 'py-core-type)))
	(setf metaclass (load-time-value (find-class 'py-user-type))))
      
      
      ;; Subclass of `type' has metaclass 'py-meta-type
      
      (when (eq metaclass (load-time-value (find-class 'py-meta-type)))
	(let ((cls (mop:ensure-class
		    (make-symbol (symbol-name name))
		    :direct-superclasses supers
		    :metaclass (load-time-value (find-class 'py-meta-type))
		    :dict namespace)))
	  (return-from make-py-class cls)))
      
      
      ;; Not a subclass of `type', so at the `object' level
      
      (let ((__new__ (recursive-class-dict-lookup metaclass '__new__)))
	
	(assert __new__ ()
	  "recur: no __new__ found for class ~A, yet it is a subclass of PY-TYPE ?!"
	  metaclass)

	(let ((bound-_new_ (and __new__ 
				(bind-val __new__ metaclass (py-class-of metaclass)))))

	  (assert bound-_new_ () "bound __new__ failed")
	  #+(or)(warn "Calling this __new__ method: ~S" bound-_new_)
	  
	  ;; If __new__ is a static method, then bound-_new_ will
	  ;; be the underlying function.
	  
	  ;; Note that all params must be Python vals, therefore
	  ;; (string ..), make-tuple-...
	  (let ((cls (py-call bound-_new_
			      metaclass
			      (string name) 
			      (make-tuple-from-list supers)
			      namespace)))
	    
	    #+(or)(warn "The __new__ method returned class: ~S" cls)
	    (assert cls () "__new__ returned NIL: ~A" bound-_new_)
	    
	    ;; Call __init__ when the "thing" returned by
	    ;; <metaclass>.__new__ is of type <metaclass>.
	    
	    (if (typep cls metaclass)
		
		(let ((__init__ (recursive-class-dict-lookup metaclass '__init__)))
		  (if __init__
		      (progn #+(or)(warn "  __init__ method is: ~A" __init__)
			     (py-call __init__ cls))
		    #+(or)(warn "No __init__ found, for class ~A returned by metaclass ~A"
			  cls metaclass)))
	      
	      #+(or)(warn "Not calling __init__ method, as class ~A is not instance of metaclass ~A"
		    cls metaclass))
	    
	    cls))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *the-true* 1)
(defconstant *the-false* 0)

(defvar *the-none*)
(defvar *the-ellipsis*)
(defvar *the-notimplemented*)
    
(defun py-bool (lisp-val)
  (if lisp-val *the-true* *the-false*))

(define-compiler-macro py-bool (lisp-val)
  `(if ,lisp-val *the-true* *the-false*))


(def-py-method py-dictless-object.__class__ :attribute (x)
  (py-class-of x))


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
	(py-raise 'TypeError "Too many arguments for function ~A (got: ~A)" 
		  func-name))
      
      (loop while kw-args
	  for kwa = (pop kw-args)
	  for key = (car kwa)
	  for val = (cdr kwa)
	  do (let ((fkw (find key formal-pos-args :test #'string=))) ;; (string= |:a| '|a|)
	       (if fkw
		   (progn (setf formal-pos-args (delete fkw formal-pos-args :test #'eq))
			  (push (cons fkw val) res))
		 (py-raise 'ValueError
			   "Invalid argument list: unknown keyword arg (or duplicated arg): ~A"
			   key))))
      
      (loop for f in formal-pos-args
	  do (push (cons f nil) res))
      
      res)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Core objects (function, method, None...; not subclassable by the user)
;;;

;; Method (Core object)

;; py-method

;; py-class-method

(def-py-method py-class-method.__new__ :static (cls func)
  (declare (ignore func))
  (make-instance cls))

(def-py-method py-class-method.__get__ (x inst class)
  (declare (ignore class))
  (let ((arg (if (excl::classp inst) inst (py-class-of inst))))
    (make-instance 'py-bound-method
      :func (slot-value x 'func)
      :instance arg)))
    
(def-py-method py-class-method.__init__ (x^ func)
  (setf (slot-value x 'func) func))

(def-py-method py-class-method.__call__ (x^ obj &rest args)
  (let ((arg (if (excl::classp obj) obj (py-class-of obj))))
    (with-slots (func class) x
      (apply #'py-call (py-method-func x) arg args))))


;; py-attribute-method

(def-py-method py-attribute-method.__get__ (x inst class)
  (declare (ignore class))
  (if inst
      (py-call (slot-value x 'func) inst)
    nil))

(def-py-method py-attribute-method.__set__ (x obj val)
  (declare (ignore val))
  (py-raise 'TypeError
	    "Attribute ~A of object ~A is read-only (value: ~A)"
	    x obj (py-call (slot-value x 'func) obj)))

(def-py-method py-attribute-method.__repr__ (x)
  (with-output-to-string (s)
    (print-unreadable-object (x s :type t :identity t))))

;; py-writable-attribute-method

(def-py-method py-writable-attribute-method.__set__ (x obj val)
  (if (slot-boundp x 'write-func)
      (py-call (slot-value x 'write-func) obj val)
    (error "No writer defined for writable attribute: ~A" x)))

(def-py-method py-writable-attribute-method.__get__ (x inst class)
  (declare (ignore class))
  (if inst
      (py-call (slot-value x 'func) inst)
    nil))

(defclass py-bound-method (py-method)
  ((instance :initarg :instance :accessor py-method-instance))
  (:metaclass py-core-type))

(defmethod print-object ((x py-bound-method) stream)
  (print-unreadable-object (x stream :identity t :type t)
    (with-slots (instance func) x
      (format stream "~A.~A" instance func))))
	
(def-py-method py-bound-method.__repr__ (x)
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t :type t)
      (with-slots (instance func) x
	(format s "~A.~A" instance func)))))

(def-py-method py-bound-method.__call__ (x &rest args)
  (when (and (null (cdr args))
	     (eq (car args) '__dict__))
    (break "pym.c args: ~A ~A" x args))
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
  
  (apply #'py-call (recursive-class-lookup-and-bind (py-method-func x) '__get__) args))

(def-py-method py-bound-method.__name__ :attribute (x)
  (py-bound-method.__repr__ x))	       
    
    
(defclass py-unbound-method (py-method)
  ((class :initarg :class :accessor py-method-class))
  (:metaclass py-core-type))

(def-py-method py-unbound-method.__repr__ (x)
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t :type t)
      (with-slots (class func) x
	(format s "~A.~A" class func)))))

(def-py-method py-unbound-method.__call__ (x &rest args)
  (with-slots (class func) x
    (apply #'py-call func x args)))


;; py-static-method

(def-py-method py-static-method.__new__ :static (cls func)
  (assert (eq cls (load-time-value (find-class 'py-static-method))))
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

(mop:finalize-inheritance (find-class 'py-lisp-function))

(def-py-method py-lisp-function.__repr__ (func)
  (with-output-to-string (s)
    (print-object func s)))

(def-py-method py-lisp-function.__get__ (func inst cls)
  #+(or)(when (eq func #'py-lisp-function.__get__)
	  (break "py-lisp-function.__get__ self: ~S ~S" inst cls))
  (if (and inst (not (eq inst *the-none*)))
      (make-instance 'py-bound-method :instance inst :func func)
    (if (and cls (not (eq cls *the-none*)))
	(make-instance 'py-unbound-method :class cls :func func)
      (py-raise 'ValueError
		"Method function.__get__(self, inst, cls) must be called with at ~
                 least one of inst or cls arguments not-none (got: ~A ~A ~A)"
		func inst cls))))

(def-py-method py-lisp-function.__name__ :attribute (func)
 (string (excl::func_name func)))

(defclass funcallable-python-class (mop:funcallable-standard-class py-core-type)
  ;; When subclassable python classes also get such a metatype,
  ;; s/py-core-type/py-type/
  ())

(defclass py-function (standard-generic-function py-core-object py-dict-mixin)
  ;; mop:funcallable-standard-class defines :name initarg, but I don't know how to access it...
  ((name         :initarg :name         :accessor py-function-name)
   (context-name :initarg :context-name :accessor py-function-context-name))
  (:metaclass funcallable-python-class))

(defun make-py-function (&key name context-name lambda)
  (let ((x (make-instance 'py-function :name (string name)  :context-name context-name)))
    (mop:set-funcallable-instance-function x lambda)
    ;; fill dict?
    x))

(def-py-method py-function.__get__ (func obj class)
  (cond ((eq func #'py-function.__get__)
	 (break "eq py-f.__get__"))
	((or (null obj) (none-p obj))
	 (make-instance 'py-unbound-method :func func :class class))
	(t
	 (make-instance 'py-bound-method :func func :instance obj))))

(def-py-method py-function.__repr__ (func)
  (with-output-to-string (s)
    (print-unreadable-object (func s :identity t)
      (if (typep func 'py-function)
	  
	  (progn 
	    (format s "python-function ~A" (py-function-name func))
	    (when (string/= (py-function-name func)
			    (py-function-context-name func))
	      (format s " (~A)" (py-function-context-name func))))
	
	(format s "function ~A" (py-function-name func))))))

(def-py-method py-function.__name__ :attribute (func)
	       (py-function-name func))

(defmethod py-function-name ((x function))
  ;; OK?
  (string (excl::func_name x)))
   
(def-py-method py-function.__call__ (func)
  func)


;; Enumerate (core object)

(defclass py-enumerate (py-core-object)
  ((gener :initarg :gener))
  (:metaclass py-core-type))

(mop:finalize-inheritance (find-class 'py-enumerate))


(def-py-method py-enumerate.__new__ :static (cls iterable)
	       (assert (subtypep cls 'py-enumerate))
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
		 (make-instance cls) :gener gener))

(def-py-method py-enumerate.__repr__ (x)
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t :type t))))


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
	   (py-raise 'ValueError "Slice step cannot be zero (got: ~S)" x))
	  
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
		      (py-raise 'TypeError
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
		      (unless (subtypep second-arg class-arg)
			(py-raise 'TypeError
				  "When calling `super' with two classes: second must be ~
                                   subclass of first (got: ~A, ~A)" class-arg second-arg))
		      (make-instance cls :object second-arg :current-class class-arg))
		     
		     (t (error "TODO super clause?"))))


(def-py-method py-super.__getattribute__ (x attr)
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
	     (val (recursive-class-dict-lookup class attr remaining-mro)))
	
	(if val
	    (bind-val val
		      (if (eq object class) *the-none* object)
		      class)
	  (py-raise 'AttributeError
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

(mop:finalize-inheritance (find-class 'py-xrange))

(def-py-method py-xrange.__init__ (x &rest args)
  (with-slots (start stop step) x
    (setf start nil
	  stop nil
	  step nil)
    (ecase (length args)
      (0 (py-raise 'TypeError "xrange: >= 1 arg needed"))
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

(defclass py-none (py-core-object) () (:metaclass py-core-type))

(setf *the-none* (make-instance 'py-none))

(defmethod make-load-form ((x (eql *the-none*)) &optional e)
  (declare (ignore e))
  `(locally (declare (special *the-none*))
     *the-none*))


(defun none-p (x) (eq x *the-none*))

(def-py-method py-none.__repr__ (x)
  (declare (ignore x))
  "None")
   
;; Ellipsis

(defclass py-ellipsis (py-core-type) () (:metaclass py-core-type))
(setf *the-ellipsis* (make-instance 'py-ellipsis))

(defmethod make-load-form ((x (eql *the-ellipsis*)) &optional e)
  (declare (ignore e))
  `(locally (declare (special *the-ellipsis*))
     *the-ellipsis*))

;; NotImlemented
(defclass py-notimplemented (py-core-type) () (:metaclass py-core-type))
(setf *the-notimplemented* (make-instance 'py-notimplemented))

(defmethod make-load-form ((x (eql *the-notimplemented*)) &optional e)
  (declare (ignore e))
  `(locally (declare (special *the-notimplemented*))
     *the-notimplemented*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; User objects (Object, Module, File, Property)


;; Object (User object)

(defun bind-val (val x x.class)
  
  (let ((get-meth (recursive-class-dict-lookup
		   (py-class-of val) '__get__)))
    (if get-meth
	(py-call get-meth val (or x *the-none*) (or x.class *the-none*))
      val)))

(defun py-get-dict-attr (x x.class)
  (or (dict x)
      (loop for c in (mop:class-precedence-list x.class)
	  when (dict c) return it)
      (error "XXX No dict found in object ~S or its class ~A?!" x x.class)))


(defvar *py-object.__getattribute__-active* ())

(def-py-method py-object.__getattribute__ (x attr)
  ;; ATTR may be a symbol, a string, or instance of user-defined subclass of string
  (when (or (and (symbolp attr)
		 (eq attr '__dict__))
	    (string= (py-val->string attr) "__dict__"))
    (return-from py-object.__getattribute__ (dict x)))
  
  (let ((*py-object.__getattribute__-active*
	 (cons (cons x attr) *py-object.__getattribute__-active*)))
    (py-attr x attr)))


(defvar *py-object.__setattr__-active* ())

(def-py-method py-object.__setattr__ (x attr^ val)
  (check-type attr (or string symbol))
  (let ((*py-object.__setattr__-active* (cons (cons x attr) *py-object.__setattr__-active*)))
    (setf (py-attr x attr) val)))


(defvar *py-object.__delattr__-active* ())

(def-py-method py-object.__delattr__ (x attr)
  (check-type attr (or string symbol))
  (let ((*py-object.__delattr__-active* (cons (cons x attr) *py-object.__delattr__-active*)))
    (setf (py-attr x attr) nil)))


(def-py-method py-object.__new__ :static (cls &rest attr)
  (declare (ignore attr))
  (make-instance cls))

(def-py-method py-object.__init__ (&rest attr)
  (declare (ignore attr))
  ;; nothing
  )

(def-py-method py-object.__class__ :attribute-read (x)
  (py-class-of x))

(def-py-method py-object.__class__ :attribute-write (x new-class)
  (assert (excl::classp new-class))
  (change-class x new-class))

(def-py-method py-object.__repr__ (x)
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t :type t))))

(defun recursive-class-dict-lookup (cls attr &optional cls-list)
  ;; Look for ATTR in class CLS and all its superclasses.
  ;; and finally (which is in this implementation not a superclass of a class).
  (assert (or (stringp attr) (symbolp attr)))
  (when cls-list
    (assert (eq cls (car cls-list))))
  (loop for c in (or cls-list (mop:class-precedence-list cls))
      until (or (eq c (load-time-value (find-class 'standard-class)))
  		(eq c (load-time-value (find-class 'py-dict-mixin))))
	    ;; XXX standard-class is after py-dict-mixin etc
      when (and (dict c) (dict-get c attr)) return it
      finally ;; this seems not needed, in the finally clause
 	      (let ((obj-attr (dict-get (load-time-value (find-class 'py-object)) attr)))
 		(when obj-attr
 		  #+(or)(warn "rec van py-object: ~A (cpl: ~A)" attr (mop:class-precedence-list cls))
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
		       (setf name (intern str #.*package*))
		     (py-raise 'TypeError "Invalid class name: ~A" name))))
	       
	       (let* ((cls-type (if (and (some (lambda (s) (subtypep s 'py-type)) supers)
					 (eq metacls 'py-type)) ;; XXX (subtypep meta pytype)?
				    :metaclass
				  :class))
		      
		      (c (mop:ensure-class
			  (make-symbol (symbol-name name)) 
			  :direct-superclasses (or supers
						   (load-time-value
						    (list (find-class 'py-user-object))))
			  :metaclass (ecase cls-type
				       (:metaclass (load-time-value (find-class 'py-meta-type)))
				       (:class     metacls))
			  :dict dict)))
		 
		 (mop:finalize-inheritance c)
		 
		 c))

(def-py-method py-type.__init__ (cls &rest args)
  (declare (ignore cls args))
  nil)

(def-py-method py-type.__name__ :attribute (cls)
  ;; XXX remove prefix `py-' etc
  (string (class-name cls)))

(def-py-method py-type.__dict__ :attribute (cls)
  (dict cls))

(def-py-method py-type.__bases__ :attribute-read (cls)
  (make-tuple-from-list (mop:class-direct-superclasses cls)))

(def-py-method py-type.__bases__ :attribute-write (cls bases^)
  (assert (excl::classp cls))
  #+(or)(break "Old class: ~A" cls)
  (let ((bases (py-iterate->lisp-list bases)))
    #||(unless (member (find-class 'py-user-object) bases :test #'eq)
	 (setf bases (append bases (list (find-class 'py-user-object)))))
    ||#
    #+(or)(warn "bases: ~S" bases)
    (mop:ensure-class-using-class cls (mop::class-name cls) 
				  :direct-superclasses bases)
    #+(or)(break "New class: ~A" cls)
    cls))

(def-py-method py-type.__mro__ :attribute (x)
  ;; We could filter out the Lisp implementation classes.
  (mop:class-precedence-list x))

(def-py-method py-type.__subclasses__ (x)
  (make-py-list-from-list (mop:class-direct-subclasses x)))

(def-py-method py-type.__repr__ (x) ;; XXX deproxy not needed?
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t)
      (format s "~@[meta~*~]class ~A"
	      (typep x (load-time-value (find-class 'py-meta-type)))
	      (class-name x)))))

(def-py-method py-type.__call__ (cls &rest args)
  
  (cond ((and (eq cls (load-time-value (find-class 'py-type)))
	      args
	      (not (cdr args)))
	 (return-from py-type.__call__
	   (py-class-of (car args))))
	
	#+(or)
	((subtypep cls 'py-type)
	 ;; make a new class
	 (error "__call__ on subclass of 'type': ?  shouldn't it use type.__new__?"))
	
	((eq cls (load-time-value (find-class 'py-object)))
	 ;; object() -> an instance without __dict__
	 (return-from py-type.__call__
	   (make-instance 'py-dictless-object)))
	
	(t (let* ((__new__ (recursive-class-dict-lookup cls '__new__))
		  #+(or)(dummy (progn
			   (warn "~A" `(recursive-class-dict-lookup ,cls '__new__))
			   (warn "~S.__new__ => ~S" cls __new__)))
		  (inst (apply #'py-call __new__ cls args))) ;; including CLS as arg!
	     (declare (ignore dummy))
	     (when
		 (subtypep (py-class-of inst) cls)
		 #+(or)(typep inst cls)
	       ;; don't do this when inst is not of type cls
	       (let ((__init__ (recursive-class-lookup-and-bind inst '__init__)))
		 (apply #'py-call __init__ args)))
	     
	     (return-from py-type.__call__
	       inst)))))


;; Module (User object)

(defclass py-module (py-user-object) ;; no dict-mixin!?
  ((globals-names  :initarg :globals-names  :type vector :initform #())
   (globals-values :initarg :globals-values :type vector :initform #())
   (dyn-globals    :initarg :dyn-globals    :type hash-table
		   :initform (make-hash-table :test #'eq))
   (name           :initarg :name           :type symbol :initform "__main__"))
  (:metaclass py-user-type))

(defun make-module (&rest options)
  (apply #'make-instance 'py-module options))

(def-py-method py-module.__new__ :static (cls name)
	       (let ((x (make-instance cls)))
		 (setf (dict-get x '__name__) name)
		 x))
(def-py-method py-module.__init__ (&rest args)
	       (declare (ignore args))
	       nil)

(def-py-method py-module.__repr__ (x^)
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t)
      (format s "module ~A" (slot-value x 'name)))))




(defun py-string-val->symbol (attr)
  (if (symbolp attr)
      attr
    
    (let ((attr.string (if (stringp attr)
			   attr
			 (py-val->string attr))))
      
      (or (find-symbol attr.string #.*package*)
	  (intern attr.string #.*package*)))))
      
(def-py-method py-module.__getattribute__ (x^ attr)
  (let ((attr.sym (py-string-val->symbol attr)))
    
    (with-slots (name globals-names globals-values dyn-globals) x
      
      (loop for i from 0
	  for n across globals-names
	  when (eq n attr.sym) 
	  do (let ((val (svref globals-values i)))
	       (if (null val)
		   (py-raise 'AttributeError "Module ~A has no attribute ~A" x attr.sym)
		 (return-from py-module.__getattribute__ val))))
      
      (let ((val (gethash attr.sym dyn-globals)))
	(when val
	  (return-from py-module.__getattribute__ val)))
      
      (py-raise 'AttributeError "Module ~A has no attribute ~A" x attr.sym))))


(defun py-import (mod-name)
  
  (labels ((safe-file-mtime (fname)
	     (let ((stat (handler-case (excl.osi:stat fname)
			   (excl:syscall-error () nil))))
	       (when stat
		 (excl.osi:stat-mtime stat))))
	 
	   (recompile-py-and-write (mod-name py-fname cl-fname fasl-fname)
	     (let* ((*current-module-name* (string mod-name))
		    (file-ast (progn (warn "Parsing ~A ..." py-fname)
				     (parse-python-file py-fname)))
		    (mod-func (progn (warn "Compiling ~A ..." py-fname)
				     (let ((*compile-print* t))
				       (compile nil `(lambda () ,file-ast))))))
	       (declare (special *current-module-name*))
	     
	       ;; MOD-FUNC is a lambda that returns a module object.
	       ;; This function will be written as literal object in the
	       ;; FASL file. To that end, we need (?) to create a
	       ;; temporary Lisp file, which will be compiled to FASL.
	     
	       (warn "Writing ~A ..." cl-fname)
	     
	       (with-open-file (f cl-fname
				:direction :output
				:if-exists :supersede
				:if-does-not-exist :create)
	       
		 (format f "(in-package :python)~%")
		 (format f "(locally (declare (special %py-import-hook%)) ~%")
		 (format f "  (funcall %py-import-hook% ~%")
		 (format f "    (funcall #.(locally (declare (special %mod-func%)) ~%")
		 (format f "      %mod-func%))))"))
	     
	       (warn "Compiling ~A ..." cl-fname)
	       
	       (let ((%mod-func% mod-func))
		 (declare (special %mod-func%))
		 (compile-file cl-fname :output-file fasl-fname :verbose nil))
	     
	       (let ((fasl-mtime (safe-file-mtime fasl-fname)))
		 (assert (and fasl-mtime
			      (safe-file-mtime py-fname)
			      (>= fasl-mtime (safe-file-mtime py-fname)))))
	     
	       t)))
    
    ;; XXX need to look for file in all directories of sys.path
    
    (let* ((py-fname (format nil "~A.py" mod-name))
	   (py-mtime (safe-file-mtime py-fname))
	   
	   (cl-fname (format nil "~A.cl" mod-name))
	   
	   (fasl-fname (format nil "~A.fasl" mod-name))
	   (fasl-mtime (safe-file-mtime fasl-fname))
	   (fasl-ok-p (and fasl-mtime
			   (<= py-mtime fasl-mtime))))
      
      (unless py-mtime
	(py-raise 'ImportError
		  "Python source file '~A' not found (in current directory)."
		  py-fname))
      
      
      (if fasl-ok-p
	  
	  (warn "Cache file ~A is still up to date" fasl-fname)
	
	(progn 
	  (unless (recompile-py-and-write mod-name py-fname cl-fname fasl-fname)
	    (py-raise 'ImportError
		      "Compiling, or writing compiled code, failed."))
	  
	  (setf fasl-mtime (safe-file-mtime fasl-fname)
		fasl-ok-p (and fasl-mtime
			       (<= py-mtime fasl-mtime)))
	  
	  (assert fasl-ok-p ()
	    "Something strange with file timestamps going on: just modified file ~
             ~A mtime ~A, while existing file ~A has mtime ~A." 
	    fasl-fname fasl-mtime py-fname py-mtime)))
      
      
      (let* ((module-object nil)
	     (%py-import-hook% (lambda (mod) (setf module-object mod))))
	
	(declare (special %py-import-hook%))

	(warn "Loading ~A ..." fasl-fname)
	
	(load fasl-fname :verbose nil)
	
	(unless module-object
	  (break "imported module ~A did not call %py-import-hook%" mod-name))
	
	module-object))))

      
;; File (User object)

(defclass py-file (py-user-object py-dict-mixin)
  ((file-handle :initform nil   :accessor py-file-handle)
   (mode        :initform nil   :accessor py-file-mode)
   (binary-mode-p :initform nil :accessor py-file-binary-mode-p)
   (closed-p    :initform t     :accessor py-file-closed-p))
  (:metaclass py-user-type))

(defun ensure-open-file (f)
  (when (py-file-closed-p f)
    (py-raise 'ValueError "File is closed")))
    
(def-py-method py-file.__new__ :static (cls &rest args)
  (make-instance cls))

(def-py-method py-file.__init__ (f^ name^ &optional mode^ buffering^)
  ;; The mode can be 'r', 'w' or 'a' for reading (default), writing
  ;; or appending.  The file will be created if it doesn't exist when
  ;; opened for writing or appending; it will be truncated when opened
  ;; for writing.  Add a 'b' to the mode for binary files. Add a '+'
  ;; to the mode to allow simultaneous reading and writing.
  ;; 
  ;; Add a 'U' to mode to open the file for input with universal
  ;; newline support.  Any line ending in the input file will be seen
  ;; as a '\n' in Python.  Also, a file so opened gains the attribute
  ;; 'newlines'; the value for this attribute is one of None (no
  ;; newline read yet), '\\r', '\\n', '\\r\\n' or a tuple containing
  ;; all the newline types seen.  'U' cannot be combined with 'w' or
  ;; '+' mode.
  ;; 
  ;; If the buffering argument is given, 0 means unbuffered, 1 means
  ;; line buffered, and larger numbers specify the buffer size.
  ;; 
  ;; Note: open() is an alias for file().   [pydoc]
  
  (declare (ignore buffering))
  
  (unless (stringp name)
    (py-raise 'TypeError "Invalid file name: ~S" name))

  (setf mode (if mode
		 (string-downcase (py-val->string mode))
	       "r"))
  
  (let* ((binary-mode-p (find #\b mode))
	 (rest-mode (coerce (loop for ch across mode unless (char= ch #\b) collect ch) 'string))
	 (modes '(("r"  . :read)  ("w"  . :write) ("a"  . :append)
		  ("r+" . :read+) ("w+" . :write) ("a+" . :append)
		  ("u"  . :read)  ("ur" . :read)))
	 (fmode (or (cdr (assoc rest-mode modes :test #'string-equal))
		    (py-raise 'ValueError "Invalid file mode: ~S" mode)))
	 (stream (open name
		       ;;;; XXX Check is the options are set correctly
		       :direction (ecase fmode                  
				    ((:read :read+) :input)
				    ( :write        :output)
				    ( :append       :io))
		       :element-type 'character
		       :if-exists (ecase fmode
				    ((:read :read+) nil)
				    ( :write        :truncate)
				    ( :append       :append))
		       :if-does-not-exist (ecase fmode
					    ((:read read+) nil)
					    (:write        :create)
					    (:append       :create))))) ;; ?
    (unless stream
      (py-raise 'IOError "Opening file failed: ~S" name))
       
    (when (eq mode :read+)
      (unless (file-position stream :end)
	(py-raise 'IOError "Moving to end of file failed")))
    
    (setf (py-file-handle f)        stream
	  (py-file-binary-mode-p f) binary-mode-p
	  (py-file-closed-p f)      nil
	  (py-file-mode f)          mode)
    
    *the-none*))

(def-py-method py-file.__repr__ (f)
  (with-output-to-string (s)
    (print-unreadable-object (f s :identity t)
      (with-slots (file-handle mode closed-p) f
	(format s "file ~A :mode ~S" (py-file.name f) mode)
	(when closed-p
	  (write-string " :closed t" s))))))


(def-py-method py-file.close (f)
  ;; Calling close() more than once is allowed.
  (unless (py-file-closed-p f)
    (close (py-file-handle f))
    (setf (py-file-closed-p f) t))
  *the-none*)

(def-py-method py-file.flush (f)
  (ensure-open-file f)
  (finish-output (py-file-handle f))
  *the-none*)

(def-py-method py-file.fileno (f)
  (declare (ignore f))
  "File descriptor"
  ;; ?? (ensure-open-file f)
  (py-raise 'IOError "file.fileno(): todo"))

(def-py-method py-file.isatty (f)
  ;; Return True if the file is connected to a tty(-like) device, else
  ;; False.  If a file-like object is not associated with a real file,
  ;; this method should not be implemented. [pydoc]
  (py-bool (excl.osi:isatty (py-file-handle f))))

(def-py-method py-file.__iter__ (f)
  (ensure-open-file f)
  f)
  
(def-py-method py-file.next (f)
  ;; In order to make a for loop the most efficient way of looping
  ;; over the lines of a file (a very common operation), the next()
  ;; method uses a hidden read-ahead buffer. As a consequence of using
  ;; a read-ahead buffer, combining next() with other file methods
  ;; (like readline()) does not work right. However, using seek() to
  ;; reposition the file to an absolute position will flush the
  ;; read-ahead buffer. [pydoc]
  (ensure-open-file f)
  (multiple-value-bind (line eof-p)
      (py-file.readline f)
    (if eof-p
	(raise-StopIteration)
      line)))

(def-py-method py-file.read (f^ &optional size^)
  ;; Read at most size bytes from the file (less if the read hits EOF
  ;; before obtaining size bytes). If the size argument is negative or
  ;; omitted, read all data until EOF is reached. The bytes are
  ;; returned as a string object. An empty string is returned when EOF
  ;; is encountered immediately. (For certain files, like ttys, it
  ;; makes sense to continue reading after an EOF is hit.) Note that
  ;; this method may call the underlying C function fread() more than
  ;; once in an effort to acquire as close to size bytes as
  ;; possible. Also note that when in non-blocking mode, less data
  ;; than what was requested may be returned, even if no size
  ;; parameter was given.
  (ensure-open-file f)
  (let* ((size (if size
		   (py-val->integer size)
		 100))
	 (check-size-p (>= size 0))
	 (chars (loop
		    for i from 0
		    while (or (null check-size-p) (< i size))
		    for ch = (read-char (py-file-handle f) nil nil)
		    while ch 
		    collect ch)))
    (coerce chars 'string)))

(def-py-method py-file.readline (f^ &optional size^)
  ;; Read one entire line from the file. A trailing newline character
  ;; is kept in the string (but may be absent when a file ends with an
  ;; incomplete line). If the size argument is present and
  ;; non-negative, it is a maximum byte count (including the trailing
  ;; newline) and an incomplete line may be returned. An empty string
  ;; is returned only when EOF is encountered immediately. Note:
  ;; Unlike stdio's fgets(), the returned string contains null
  ;; characters ('\0') if they occurred in the input.
  
  ;; XXX Currently every kind of newline char (sequence) is replaced
  ;; by whatever #\Newline is for the platform.
  ;;
  ;; Second value returned indicates EOF-P.
  
  (ensure-open-file f)
  (when size
    (warn ":size arg to file.readline currently ignored XXX"))
  (multiple-value-bind (line eof-p)
      (read-line (py-file-handle f) nil nil)
    (cond ((null line) (values "" t))
	  (eof-p       line)
	  (t           (concatenate 'string line (string #\Newline))))))

(def-py-method py-file.readlines (f &optional sizehint^)
  ;; Read until EOF using readline() and return a list containing the
  ;; lines thus read. If the optional sizehint argument is present,
  ;; instead of reading up to EOF, whole lines totalling approximately
  ;; sizehint bytes (possibly after rounding up to an internal buffer
  ;; size) are read. Objects implementing a file-like interface may
  ;; choose to ignore sizehint if it cannot be implemented, or cannot
  ;; be implemented efficiently.
  (declare (ignore sizehint)) ;; XXX
  (ensure-open-file f)
  (make-py-list-from-list (loop for line = (multiple-value-bind (line eof-p) 
					       (py-file.readline f)
					     (and (null eof-p) line))
			      while line 
			      collect line)))

(def-py-method py-file.xreadlines (f)
  ;; This method returns the same thing as iter(f). New in version
  ;; 2.1. Deprecated since release 2.3. Use "for line in file"
  ;; instead.
  (ensure-open-file f)
  (make-iterator-from-function :func (lambda ()
				       (multiple-value-bind (line eof-p) 
					   (py-file.readline f)
					 (and (null eof-p) line)))
			       :name :file.xreadlines))


(def-py-method py-file.seek (f offset^ &optional whence)
  ;; Set the file's current position, like stdio's fseek(). The whence
  ;; argument is optional and defaults to 0 (absolute file
  ;; positioning) ; other values are 1 (seek relative to the current
  ;; position) and 2 (seek relative to the file's end). There is no
  ;; return value. Note that if the file is opened for appending (mode
  ;; 'a' or 'a+'), any seek() operations will be undone at the next
  ;; write. If the file is only opened for writing in append mode
  ;; (mode 'a'), this method is essentially a no-op, but it remains
  ;; useful for files opened in append mode with reading enabled (mode
  ;; 'a+'). If the file is opened in text mode (mode 't'), only
  ;; offsets returned by tell() are legal. Use of other offsets causes
  ;; undefined behavior.
  (declare (ignore offset whence))
  (ensure-open-file f)
  :todo)

(def-py-method py-file.tell (f)
  ;; Return the file's current position, like stdio's ftell().
  (ensure-open-file f)
  (file-position (py-file-handle f)))

(def-py-method py-file.truncate (f &optional size)
  ;; Truncate the file's size. If the optional size argument is
  ;; present, the file is truncated to (at most) that size. The size
  ;; defaults to the current position. The current file position is
  ;; not changed. Note that if a specified size exceeds the file's
  ;; current size, the result is platform-dependent: possibilities
  ;; include that file may remain unchanged, increase to the specified
  ;; size as if zero-filled, or increase to the specified size with
  ;; undefined new content. Availability: Windows, many Unix variants.
  (declare (ignore size))
  (ensure-open-file f)
  :todo)

(def-py-method py-file.write (f str)
  ;; Write a string to the file. There is no return value. Due to
  ;; buffering, the string may not actually show up in the file until
  ;; the flush() or close() method is called.
  (declare (ignore str))
  (ensure-open-file f)
  :todo)

(def-py-method py-file.writelines (f sequence)
  ;; Write a sequence of strings to the file. The sequence can be any
  ;; iterable object producing strings, typically a list of
  ;; strings. There is no return value. (The name is intended to match
  ;; readlines(); writelines() does not add line separators.)
  (declare (ignore sequence))
  (ensure-open-file f)
  :todo)

;; File attributes

(def-py-method py-file.closed :attribute (f)
  ;; bool indicating the current state of the file object. This is a
  ;; read-only attribute; the close() method changes the value. It may
  ;; not be available on all file-like objects.
  (py-bool (py-file-closed-p f)))

(def-py-method py-file.encoding :attribute (f)
  ;; The encoding that this file uses. When Unicode strings are
  ;; written to a file, they will be converted to byte strings using
  ;; this encoding. In addition, when the file is connected to a
  ;; terminal, the attribute gives the encoding that the terminal is
  ;; likely to use (that information might be incorrect if the user
  ;; has misconfigured the terminal). The attribute is read-only and
  ;; may not be present on all file-like objects. It may also be None,
  ;; in which case the file uses the system default encoding for
  ;; converting Unicode strings.
  (declare (ignore f))
  :todo)

(def-py-method py-file.mode :attribute (f)
  ;; The I/O mode for the file. If the file was created using the
  ;; open() built-in function, this will be the value of the mode
  ;; parameter. This is a read-only attribute and may not be present
  ;; on all file-like objects. 
  (py-file-mode f))

(def-py-method py-file.name :attribute (f)
  ;; If the file object was created using open(), the name of the
  ;; file. Otherwise, some string that indicates the source of the
  ;; file object, of the form "<...>". This is a read-only attribute
  ;; and may not be present on all file-like objects.
  (namestring (truename (py-file-handle f))))

(def-py-method py-file.newlines :attribute (f)
  ;; If Python was built with the --with-universal-newlines option to
  ;; configure (the default) this read-only attribute exists, and for
  ;; files opened in universal newline read mode it keeps track of the
  ;; types of newlines encountered while reading the file. The values
  ;; it can take are '\r', '\n', '\r\n', None (unknown, no newlines
  ;; read yet) or a tuple containing all the newline types seen, to
  ;; indicate that multiple newline conventions were encountered. For
  ;; files not opened in universal newline read mode the value of this
  ;; attribute will be None.
  (declare (ignore f))
  :todo)

(def-py-method py-file.softspace :attribute (f)
  ;; Boolean that indicates whether a space character needs to be
  ;; printed before another value when using the print
  ;; statement. Classes that are trying to simulate a file object
  ;; should also have a writable softspace attribute, which should be
  ;; initialized to zero. This will be automatic for most classes
  ;; implemented in Python (care may be needed for objects that
  ;; override attribute access); types implemented in C will have to
  ;; provide a writable softspace attribute. Note: This attribute is
  ;; not used to control the print statement, but to allow the
  ;; implementation of print to keep track of its internal state.
  (declare (ignore f))
  :todo)
    
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
	(py-raise 'AttributeError "Cannot get attribute")
      (py-call fget obj))))

(def-py-method py-property.__set__ (x obj val)
  (with-slots (fset) x
    (if (eq fset *the-none*)
	(py-raise 'AttributeError "Cannot set attribute")
      (py-call (slot-value x 'fset) obj val)))) ;; bind?

(def-py-method py-property.__del__ (x obj)
  (with-slots (fdel) x
    (if (eq fdel *the-none*)
	(py-raise 'AttributeError "Cannot delete attribute")
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

(defgeneric deproxy (x)
  (:method ((x py-lisp-object))  (proxy-lisp-val x))
  (:method ((x t))               x))

(defmacro def-proxy-class (py-name &optional supers)
  `(progn (defclass ,py-name ,(or supers '(py-lisp-object))
	    ()
	    (:metaclass py-lisp-type))
	  (mop:finalize-inheritance (find-class ',py-name))))

;; Number (Lisp object: number)

(def-proxy-class py-number)

;; Seems that no py-number.__new__ method is needed: Python does not
;; have a corresponding 'number' class, so user is always
;; instantiating a subclass (int, real, ...).

(def-py-method py-number.__abs__ (x^) (abs x))

(def-py-method py-number.__add__ (x^ y^) (+ x y))

(def-py-method py-number.__cmp__ (x^ y^) 
  (cond ((= x y) 0) ;; also for complex
	((and (realp x) (realp y))
	 (cond ((< x y) -1)
	       (t       +1)))
	((or (complexp x) (complexp y))
	 (py-raise 'TypeError "Cannot compare complexes"))))

(def-py-method py-number.__div__ (x^ y^) (/ x y))  ;; overruled for integers

(def-py-method py-number.__divmod__ (x y)
  (make-tuple-from-list (list (py-/ x y) (py-% x y))))

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

(def-py-method py-number.__eq__  (x^ y^) (py-bool (and (numberp y) (= x y))))

(def-py-method py-number.__mul__ (x^ y^) (* x y))

(def-py-method py-number.__neg__ (x^) (- x))

(def-py-method py-number.__pow__ (x^ y^ &optional z^) 
  (if z
      (progn (setf z (py-val->integer z))
	     (mod (expt x y) z))
    (expt x y)))

(def-py-method py-number.__repr__ (x^) (format nil "~A" x))
(def-py-method py-number.__sub__ (x^ y^) (- x y))

(def-py-method py-number.__truediv__ (x^ y^) (/ x y)) ;; overruled for integers


;; In CPython, only complex numbers have 'real','imag' and 'conjugate'
;; attributes, here all numbers have them:

(def-py-method py-number.real :attribute (x^)
	       (realpart x))

(def-py-method py-number.imag :attribute (x^)
	       (imagpart x))

(def-py-method py-number.conjugate (x^)
  (conjugate x))



;; Complex

(def-proxy-class py-complex (py-number))

(def-py-method py-complex.__new__ :static (cls &optional (real 0) (complex 0))
	       (let ((c (complex real complex)))
		 (if (eq cls (load-time-value (find-class 'py-complex)))
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
	       (if (eq cls (find-class (load-time-value 'py-int)))
		   
		   (typecase arg
		     (integer arg)
		     (float  (truncate arg))
		     (t      (setf arg (py-val->string arg)
				   base (py-val->integer base :min 0))
			     (let ((*read-base* (cond ((and (>= (length arg) 2)
							    (char= (aref arg 0) #\0)
							    (member (aref arg 1) '(#\x #\X) :test #'char=))
						       (setf arg (subseq arg 2))
						       16)
						      ((and (>= (length arg) 1)
							    (char= (aref arg 0) #\0))
						       (setf arg (subseq arg 1))
						       8)
						      ((= base 0) 
						       10)
						      (t (check-type base (integer 2 36))
							 base))))
			       (truncate (read-from-string arg)))))
		 
		 (make-instance cls :lisp-object arg)
		 
		 #+(or)
		 (let ((i (make-instance cls :lisp-object arg)))
		   (setf (proxy-lisp-val i) arg)
		   i)))

(def-py-method py-int.__init__ (&rest args) nil)

(def-py-method py-int.__floordiv__ (x^ y^)
  (values (floor x y)))

(def-py-method py-int.__div__ (x^ y^) 
  (if (and (integerp x) (integerp y))
      (floor x y)
    (/ x y)))
       
(def-py-method py-int.__lshift__ (x^ y^)  (ash x y))
(def-py-method py-int.__rshift__ (x^ y^)  (ash x (- y)))
(def-py-method py-int.__xor__ (x^ y^) (logxor x y))
(def-py-method py-int.__and__ (x^ y^) (logand x y))
(def-py-method py-int.__or__  (x^ y^) (logior x y))

(def-py-method py-int.__hex__ (x^) (format nil "0x~x" x))
(def-py-method py-int.__oct__ (x^) (format nil "0~o" x))

(def-proxy-class py-bool (py-int))

(def-py-method py-bool.__new__ :static (cls &optional (val 0))
	       (let ((bool-val (if (py-val->lisp-bool val) *the-true* *the-false*)))
		 (if (eq cls (load-time-value (find-class 'py-bool)))
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
		 (if (eq cls (load-time-value (find-class 'py-float)))
		     f
		   (make-instance 'cls :lisp-object f))))

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
  
;;; non-numberic classes

;; Dict

(def-proxy-class py-dict)

(defun py-==->lisp-val (x y)
  (/= (py-== x y) 0))
       
(defun make-dict ()
  ;; todo: (make-hash-table :test #'py-==  :hash #'py-hash)
  (make-hash-table :test 'py-==->lisp-val :hash-function 'py-hash))

(defmacro make-dict-unevaled-list (items)
  (let ((dict '#:dict))
    `(let ((,dict (make-dict)))
       ,@(loop for (k . v) in items
	     collect `(setf (gethash ,k ,dict) ,v))
       ,dict)))

(defun make-dict-from-symbol-alist (alist)
  (loop with ht = (make-dict)
      for (k . v) in alist 
      do (setf (gethash (py-symbol->string k) ht) v)
      finally (return ht)))

(def-py-method py-dict.__new__ :static (cls &rest kwargs)
  ;; XXX logic of filling dict must be moved to __init__
  (if (eq cls (load-time-value (find-class 'py-dict)))
      (make-dict)
    (make-instance cls :lisp-object (make-dict))))

(def-py-method py-dict.__init__ (x^ &rest kwargs)
  (case (length kwargs)
    (0 )
    (1 (let* ((iterable (car kwargs))
	      (items-meth (recursive-class-lookup-and-bind iterable 'items))
	      (items (if items-meth
			 (py-call items-meth)
		       (py-iterate->lisp-list iterable))))
	 
	 (map-over-py-object (lambda (obj)
			       (let ((lst (py-iterate->lisp-list obj)))
				 (if (= (length lst) 2)
				     (setf (gethash (pop lst) x) (pop lst))
				   (py-raise 'ValueError
					     "Got more than 2 values for element in .items() of ~A"
					     iterable))))
			     items)))
	 
    (t (loop while kwargs
	   do (let ((key (pop kwargs))
		    (val (pop kwargs)))
		(unless (symbolp key)
		  (py-raise 'TypeError "dict.__new__: invalid key (not a symbol): ~S" key))
		(setf (gethash (py-symbol->string key) x) val))))))

(def-py-method py-dict.__eq__ (dict1^ dict2^)
  (py-bool (and (typep dict2 (load-time-value (find-class 'hash-table)))
		(= (hash-table-count dict1) (hash-table-count dict2))
		(loop for d1.k being the hash-key in dict1 using (hash-value d1.v)
		    for d2.v = (gethash d1.k dict2)
		    when (py-val->lisp-bool (py-!= d1.v d2.v)) return nil
		    finally (return t)))))

(def-py-method py-dict.__getitem__ (dict^ key)
  (let ((key2 (if (symbolp key) (symbol-name key) key)))
    (or (gethash key2 dict)
	(py-raise 'KeyError "Dict has no such key: ~A" key2))))

(def-py-method py-dict.__iter__ (dict^)
  (with-hash-table-iterator (next-func dict)
    (make-iterator-from-function
     :name :py-dict-iterator
     :func (lambda () (multiple-value-bind (ret key val) 
			  (next-func)
			(declare (ignore val))
			(when ret key))))))

(def-py-method py-dict.__nonzero__ (dict^)
  (py-bool (> (hash-table-count dict) 0)))

(def-py-method py-dict.__repr__ (x^)
  (with-output-to-string (s)
    (write-char #\{ s)
    (loop with c = (hash-table-count x)
	for key being the hash-key in x
	using (hash-value val)
	for i from 1
	do (write-string (py-repr-string key) s)
	   (write-char #\: s)
	   (write-char #\Space s)
	   (write-string (py-repr-string val) s)
	   (unless (= i c)
	     (write-string ", " s)))
    (write-char #\} s)))

(def-py-method py-dict.__setitem__ (dict^ key val)
  (let ((key2 (if (symbolp key) (symbol-name key) key)))
    (setf (gethash key2 dict) val)))

(def-py-method py-dict.fromkeys :static (seq &optional val)
	       (unless val
		 (setf val *the-none*))
	       (let* ((d (make-dict)))
		 (map-over-py-object (lambda (key) (setf (gethash key d) val))
				     seq)
		 d))

(def-py-method py-dict.items (x^)
  (make-py-list-from-list
   (loop for k being the hash-key in x
       using (hash-value v)
       collect (make-tuple-from-list (list k v)))))

(def-py-method py-dict.keys (x^)
  (make-py-list-from-list
   (loop for k being the hash-key in x
       collect k)))

(def-py-method py-dict.values (x^)
  (make-py-list-from-list
   (loop for v being the hash-value in x
       collect v)))

;; List (Lisp object: adjustable array)

(def-proxy-class py-list)

(def-py-method py-list.__new__ :static (cls &optional iterable)
	       (declare (ignore iterable))
	       (let ((vec (make-array 0 :adjustable t :fill-pointer 0)))
		 (if (eq cls (load-time-value (find-class 'py-list)))
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

(def-py-method py-list.__cmp__ (x^ y^)
  (let ((x.len (length x))
	(y.len (length y)))
    
    (cond ((< x.len y.len) -1)
	  ((> x.len y.len) 1)
	  (t (loop for xi across x and yi across y
		 do (ecase (pybf:cmp xi yi)
		      (0 ) ;; cont
		      (-1 (return -1))
		      (1  (return  1)))
		 finally (return 0))))))

(def-py-method py-list.__delitem__ (x^ item)
  (typecase item
    (integer (when (< item 0)
	       (incf item (length x)))
	     (unless (<= 0 item (1- (length x)))
	       (py-raise 'ValueError
			 "del <list>[i] : i outside range (got ~A, length list = ~A)"
			 item (length x)))
	     (loop for i from item below (1- (length x))
		 do (setf (aref x i) (aref x (1+ i))))
	     (decf (fill-pointer x)))
    (py-slice (with-slots (start stop step) item
		(cond ((and (eq start *the-none*) (eq stop *the-none*) (eq step *the-none*))
		       (loop for i from 0 below (length x) do (setf (aref x i) nil))
		       (setf (fill-pointer x) 0))
		      (t (break "unexpected")))))))

(def-py-method py-list.__eq__ (x^ y^)
  (py-bool (and (= (length x) (length y))
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
	       (py-raise 'ValueError
			 "<string>[i] : i outside range (got ~A, length string = ~A)"
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
    
    (t (py-raise 'TypeError "Expected integer or slice as subscript; got: ~S."
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
			  :initial-elemnent (when (= x.len 1) 
					      (aref x 0)))))
    (unless (= x.len 1)
      (loop for ni from 0 below res.len by x.len
	  do (loop for i from 0 below x.len
		 do (setf (aref res (+ ni i)) (aref x i)))))
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
    (py-raise 'ValueError
	      "<list>[i] = x : i outside range (got ~A, length list = ~A)"
	      item (length x)))
  (setf (aref x item) val))

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
	    (loop for i from (1+ ix) below x.len
		do (setf (aref x (1- i)) (aref x i)))
	    (decf (fill-pointer x)))
	(py-raise 'ValueError
		  "list.pop(x, i): ix wrong (got: ~A; x.len: ~A)"
		  ix x.len)))))

(def-py-method py-list.sort (x^ &optional fn)
  
  ;; XXX Here it is assumed that SORT does not return a vector that is
  ;; different from the original one. This is explicitly not
  ;; guaranteed by ANSI.
  
  (let* ((sort-fun (if fn
		       (lambda (x y) (< (signum (deproxy (py-call fn x y))) 0))
		     (lambda (x y) (< (signum (pybf:cmp x y)) 0))))
	 (res (sort x sort-fun)))

    ;; It's not guaranteed by ANSI that (eq res x).
    (unless (eq res x)
      (loop for i from 0
	  for res.i across res
	  do (setf (aref x i) res.i)))
    
    *the-none*))


(defmacro make-py-list-unevaled-list (items)
  (let ((vec '#:vec))
    `(let ((,vec (make-array ,(length items) :adjustable t :fill-pointer ,(length items))))
       ,@(loop for x in items and i from 0
	     collect `(setf (aref ,vec ,i) ,x))
       ,vec)))

(defun make-py-list-from-list (list)
  (let* ((len (length list))
	 (vec (make-array len :adjustable t :fill-pointer t)))
    (loop for x in list and i from 0
	do (setf (aref vec i) x))
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
   
   (t (py-raise 'UnicodeError "Unrecognized Unicode external format: ~S" name))))

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
	(py-raise 'UnicodeEncodeError
		  "During encoding of string, encountered a character whose ~
                   code is out of the allowed range (got character code: ~A; ~
                   external format: ~A; max code allowed for external format: ~A)"
		   max-found-code external-format max-code)))

    (multiple-value-bind (octets num-bytes-copied)
	(excl:string-to-octets string
			       :external-format ex-format
			       :null-terminate nil)
      
      (when (< num-bytes-copied (length string))
	(py-raise 'UnicodeEncodeError
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
	  (py-raise 'UnicodeDecodeError
		    "During decoding of string, encountered a character whose ~
                     code is out of allowed range (got character code: ~A; ~
                     external format: ~A; max octet code allowed for external format: ~A)"
		    max-found-code ex-format max-octet-code)))
      
      (multiple-value-bind (string chars-copied octets-used)
	  (excl:octets-to-string vec :external-format ex-format)
	(declare (ignore chars-copied))
	
	(when (< octets-used (length vec))
	  (py-raise 'UnicodeDecodeError
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
							  (py-string-from-char (schar x i))
							(incf i)))))))

(def-py-method py-string.__getitem__ (x^ item^)
  (vector-getitem x item (lambda (char/charlist single-p)
			   (if single-p
			       (py-string-from-char char/charlist)
			     (coerce char/charlist 'string)))))

(def-py-method py-string.__hash__ (x^) (sxhash x))

(def-py-method py-string.__len__  (x^)  (length x))

(def-py-method py-string.__mod__ (x^ args)
  (let ((fs-struct (ensure-parsed-format-string x)))
    (make-formatted-string fs-struct args)))

(def-py-method py-string.__mul__ (x^ n^)
  (unless (typep n '(integer 0 *))
    (py-raise 'TypeError "str.__mul__: arg must be nonzero integer (got: ~S)" n))
  (if (or (= n 0)
	  (= (length x) 0))
      ""
    (let ((res (make-array (* n (length x)) :element-type 'character)))
      (loop for i from 0 below n
	  do (loop for xi from 0 below (length x)
		 do (setf (aref res (+ (* i (length x)) xi))
		      (aref x xi))))
      res)))

(def-py-method py-string.__repr__ (x^)
  (with-output-to-string (s) (py-pprint s x))) ;; XXX todo: optimize

(def-py-method py-string.__str__  (x^)  x)

(def-py-method py-string.decode (x^ &optional encoding^ errors)
  (py-decode-unicode x encoding errors))

(def-py-method py-string.encode (x^ &optional encoding^ errors)
  (py-encode-unicode x encoding errors))

(def-py-method py-string.find (x^ item &rest args) (declare (ignore x item args)) -1) ;; TODO

(def-py-method py-string.isalpha (x^) (py-bool (every #'alpha-char-p x)))
(def-py-method py-string.isalnum (x^) (py-bool (every #'alphanumericp x)))
(def-py-method py-string.isdigit (x^) (py-bool (every #'digit-char-p x)))
(def-py-method py-string.islower (x^) (py-bool (every #'lower-case-p x)))

(def-py-method py-string.isspace (x^)
  (py-bool (and (> (length x) 0) ;; empty string is defiend as "not space"
		(every (lambda (ch) (member ch '(#\Space #\Tab #\Newline)))
		       x))))


(def-py-method py-string.join (x^ seq-of-strings)
  (let* ((strings (mapcar #'py-val->string (py-iterate->lisp-list seq-of-strings))))
    
    (unless strings
      (return-from py-string.join ""))
    
    (let* ((num-strings (length strings))
	   (tot-num-chars (+ (* (1- num-strings) (length x))
			     (apply #'+ (mapcar #'length strings))))
	   (res (make-array tot-num-chars :element-type 'character)))
      (loop
	  with res-i = 0
	  for s in strings and i from 0
	  do (loop for ch across s do (setf (aref res res-i) ch)
				      (incf res-i))
	  unless (= i (1- num-strings))
	  do (loop for ch across x do (setf (aref res res-i) ch)
				      (incf res-i)))
      res)))

(def-py-method py-string.lower (x^)
  (string-downcase x))

(def-py-method py-string.upper (x^)
  (string-upcase x))
   
(def-py-method py-string.replace (x^ old new &optional count^)
  (let ((olds (py-val->string old))
	(news (py-val->string new)))
    (substitute news olds x :count count)))

;; Tuple (Lisp object: consed list)

(def-proxy-class py-tuple)

(def-py-method py-tuple.__new__ :static (cls &optional iterable)
	       
	       (let ((tup (make-tuple-from-list (when iterable 
						  (py-iterate->lisp-list iterable)))))
		 
		 (cond ((eq cls (load-time-value (find-class 'py-tuple))) tup)
		       
		       ((subtypep cls (load-time-value (find-class 'py-tuple)))
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
  (let ((x.len (length x))
	(y.len (length y)))
    
    (cond ((= x.len y.len)
	   (loop for xi in x and yi in y
	       do (ecase (pybf:cmp xi yi)
		    (-1 (return -1))
		    (1  (return 1))
		    (0  ))
	       finally (return 0)))
	  
	  ((< x.len y.len) -1)
	  (t                1))))

(def-py-method py-tuple.__eq__ (x^ y^)
  (py-bool (loop 
	       for xi = (pop x)
	       for yi = (pop y)
	       do (cond ((null (or xi yi))        (return t))
			((or (null xi) (null yi)) (return nil))
			((py-==->lisp-val xi yi))
			(t (return nil))))))

(def-py-method py-tuple.__getitem__ (x^ item^)
  ;; XXX item may be a slice
  (etypecase item
    (integer (cond ((<= 0 item (1- (length x)))
		    (nth item x))
		   ((<= 0 (+ (length x) item) (1- (length x)))
		    (nth (+ (length x) item) x))
		   (t (error "unexpected"))))
    (py-slice (error :todo-tuple-getitem-slice))))

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
	((null (cdr x)) (format nil "(~/python:repr-fmt/,)" (car x)))
	(t              (format nil "(~{~/python:repr-fmt/~^, ~})" x))))

(def-py-method py-tuple.__str__ (x^)
  (if *py-print-safe*
      (with-output-to-string (s)
	(print-unreadable-object (x s :type nil :identity t)
	  (format s "tuple with ~A items" (length x))))
    (py-tuple.__repr__ x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(defgeneric py-class-of (x)
    (:method ((x integer)) (load-time-value (find-class 'py-int    )))
    (:method ((x real))    (load-time-value (find-class 'py-float  )))
    (:method ((x complex)) (load-time-value (find-class 'py-complex)))
    (:method ((x string))  (load-time-value (find-class 'py-string )))
    (:method ((x vector))  (load-time-value (find-class 'py-list   )))
    (:method ((x list))    (load-time-value (find-class 'py-tuple  )))
    (:method ((x hash-table)) (load-time-value (find-class 'py-dict)))
    
    (:method ((x function))    (load-time-value (find-class 'py-function)))
    (:method ((x py-function)) (load-time-value (find-class 'py-function)))
    
    (:method ((x py-lisp-type)) (load-time-value (find-class 'py-type)))
    (:method ((x py-core-type)) (load-time-value (find-class 'py-type)))
    (:method ((x py-user-type)) (load-time-value (find-class 'py-type)))
    
    (:method ((x class))   (cond
			    ((eq x (load-time-value (find-class 'py-type)))
			     x)  ;; py-type is its own class
			    
			    ((eq x (load-time-value (find-class 'py-meta-type)))
			     ;; the metatypes is posing as `type'
			     (load-time-value (find-class 'py-type)))
			    
			    ((typep x (load-time-value (find-class 'py-meta-type)))
			     ;; metatypes fake being of type `type'
			     (load-time-value (find-class 'py-type)))
			    
			    ((typep x (load-time-value (find-class 'py-class-mixin)))
			     ;; maybe user-defined metaclass
			     (class-of x))
			    
			    ((or (typep x (load-time-value 'standard-class))
				 (typep x (load-time-value 'built-in-class)))
			     (load-time-value (find-class 'py-type)))
			    
			    (t (warn "py-class-of ~A -> ~A" x (class-of x))
			       (class-of x))))
    (:method ((x t))       (class-of x)))


;;; Attributes are a fundamental thing: getting, setting, deleting

(defun py-attr (x attr)
  ;; ATTR may be a symbol, a string, or instance of user-defined subclass of string
  (let* ((class-attr-val   nil)
	 (__getattr__      nil)
	 (__getattribute__ nil)
	 (x.class          (py-class-of x))
	 (attr.as_string   (if (symbolp attr)
			       (symbol-name attr)
			     (py-val->string attr)))
	 (attr.as_sym      (find-symbol attr.as_string #.*package*)) ;; may fail (= NIL)
	 
	 (inside-object-getattribute (loop for (obj . at) in *py-object.__getattribute__-active*
					 when (and (eq obj x)
						   (string= (py-val->string at) attr.as_string))
					 return t
					 finally (return nil))))
    
    (loop for c in (mop:class-precedence-list x.class)
	until (or (eq c (load-time-value (find-class 'standard-class)))
		  (eq c (load-time-value (find-class 'py-dict-mixin)))
		  (eq c (load-time-value (find-class 'py-class-mixin))))
	      
	for c.dict = (dict c) ;; may be NIL
	for c.dict-is-regular-dict = (and c.dict (hash-table-is-regular-dict-p c.dict))
				     
	when c.dict
	do 
	  (unless (or inside-object-getattribute
		      __getattribute__)
	    ;; Try only the first __getattribute__ method found (but not py-object's).
	    (let ((getattribute-meth (sym-gethash '__getattribute__ c.dict c.dict-is-regular-dict)))
	      (when (and getattribute-meth
			 (not (eq getattribute-meth #'py-object.__getattribute__)))
		(setf __getattribute__ getattribute-meth)
		(handler-case 
		    (values (py-call (bind-val getattribute-meth x x.class) attr.as_string))
		  (AttributeError () 
		    #+(or)(warn "__getattribute__ ~S ~S gave exception; also trying __getattr__ (if any)"
				x attr)
		    nil)
		  (:no-error (val)
		    (return-from py-attr val))))))
	  
	  (unless class-attr-val
	    ;; Try to find attribute in class dict.
	    (when (or c.dict-is-regular-dict attr.as_sym) ;; otherwise key can't exist
	      (let ((val (sym-gethash attr.as_sym c.dict c.dict-is-regular-dict)))
		(when val
		  (setf class-attr-val val)))))
	  
	  (unless (or inside-object-getattribute
		      __getattr__)
	    ;; Look for __getattr__ method (but don't call it yet:
	    ;; only if instance/class dicts fail).
	    (let ((getattr-meth (sym-gethash '__getattr__ c.dict c.dict-is-regular-dict)))
	      (when getattr-meth (setf __getattr__ getattr-meth)))))

    ;; Arriving here means: no __getattribute__ (or one that raised AttributeError),
    ;; but perhaps __getattr__ or class-attr-val.
    
    ;; When __getattribute__ exists, but raised AttributeError, then call __getattr__
    ;; irrespective (!) of presence of attribute in instance or class/superclasses:
    
    (when (and __getattribute__ __getattr__)
      #+(or)(warn "Both __getattribute__ and __getattr__: calling it  ~A ~A" x attr)
      (return-from py-attr
	(py-call (bind-val __getattr__ x x.class) attr.as_string)))

    ;; A class attribute that is a data descriptor (i.e. has a
    ;; `__set__' attribute) has higher priority than an instance
    ;; attribute.

    (when (and class-attr-val (data-descriptor-p class-attr-val))
      (return-from py-attr
	(bind-val class-attr-val x x.class)))
    
    ;; Try instance dict
    
    (if (typep x 'class)
	
	(loop for c in (mop:class-precedence-list x)
	    until (or (eq c (load-time-value (find-class 'standard-class)))
		      (eq c (load-time-value (find-class 'py-dict-mixin)))
		      (eq c (load-time-value (find-class 'py-class-mixin))))
		  
	    for c.dict = (dict c) ;; may be NIL
	    for c.dict-is-regular-dict = (and c.dict (hash-table-is-regular-dict-p c.dict))
					 
	    for val = (when (and c.dict
				 (or attr.as_sym c.dict-is-regular-dict))
			(sym-gethash attr c.dict c.dict-is-regular-dict))
	    when val do (return-from py-attr val))
      
      (when (dict x)
	(let ((val (dict-get x attr.as_string)))
	  (when val
	    (cond ((subtypep (py-class-of x) 'py-type)
		   
		   ;; XXX check the exact condition under which binding
		   ;; of instance dict item occurs

		   (let ((bound-val (bind-val val nil x)))
		     (when bound-val ;; attribute-method bounded to class -> NIL
		       (return-from py-attr bound-val))))

		  (t (return-from py-attr 
		       val)))))))
    
    ;; Fall back to a class attribute that is not a `data descriptor'.
    (when class-attr-val
      (return-from py-attr
	(bind-val class-attr-val x x.class)))

    ;; Right place??
    #||
    (when (eq attr.as_sym '__dict__)
      (return-from py-attr (py-get-dict-attr x x.class)))
    ||#
    
    ;; Fall back to the __getattr__ hook (not checked whether it raises AttributeError)
    (when __getattr__
      (return-from py-attr
	(py-call (bind-val __getattr__ x x.class) attr.as_string)))
     
    (when (eq attr.as_sym '__dict__)
      (return-from py-attr (py-get-dict-attr x x.class)))

    #||
    (unless __getattribute__
      (return-from py-attr
	(py-call (bind-val #'object.__getattribute__ x x.class) attr.as_string)))
    ||#
    
    ;; Give up.
    (py-raise 'AttributeError "No such attribute: ~A `~A' (class: ~A)" x attr (py-class-of x))))


(defun (setf py-attr) (val x attr)
  ;; When ATTR is NIL, it means attribute is deleted ("del x.attr")

  ;; XXX convert string to real string?
  ;; XXX special-case when ATTR==__dict__

  (let* ((attr.str        (py-val->string attr))
	 (x.class         (py-class-of x))
	 (x.attr          (recursive-class-dict-lookup x.class attr)))
    
    (if (null val)
	
	;; del x.attr
	;; <cls>.__delattr__ takes precedence over <cls.attr>.__del__
	(let* ((x.attr.__del__  (when x.attr
				  (recursive-class-lookup-and-bind x.attr '__del__)))
	       (delattr-active  (loop for (obj . at) in *py-object.__delattr__-active*
				    if (and (eq obj x) (string= at attr))
				    return t
				    finally (return nil)))
	       
	       (x.__delattr__   (unless delattr-active
				  (recursive-class-lookup-and-bind x '__delattr__))))
	  
	  (cond (x.__delattr__  (py-call x.__delattr__ attr.str))
		
		(x.attr.__del__ (if (eq x.attr.__del__ *the-none*)
				    (py-raise 'AttributeError
					      "Cannot delete attribute ~A of ~A (__del__ = None)"
					      x attr)
				  (py-call x.attr.__del__ x)))
		
		((dict x)       (unless (setf (dict-get x attr) nil)
				  (py-raise 'AttributeError
					    "Cannot delete attribute ~A of object ~A: does not exist"
					    attr x)))
		
		(t              (py-raise 'AttributeError
					  "Cannot delete attribute ~A of ~A: no __delattr__ method"
					  x attr))))
      
      ;; x.attr = val
      ;; <cls>.__setattr__ takes precedence over <cls.attr>.__set__
      (let* ((x.attr.__set__  (when x.attr
				(recursive-class-lookup-and-bind x.attr '__set__)))
	     (setattr-active  (loop for (obj . at) in *py-object.__setattr__-active*
				  if (and (eq obj x) (string= at attr))
				  return t
				  finally (return nil)))
	     
	     (x.__setattr__   (unless setattr-active
				(recursive-class-lookup-and-bind x '__setattr__))))
	
	#+(or)(warn "x: ~A attr: ~A x.attr: ~A x.attr.__set__: ~A x.__setattr__: ~A"
		    x attr x.attr x.attr.__set__ x.__setattr__)
	
	(cond (x.__setattr__  (py-call x.__setattr__ attr.str val))
	      
	      (x.attr.__set__ (if (eq x.attr.__set__ *the-none*)
				  (py-raise 'AttributeError
					    "Cannot set attribute ~A of ~A (__set__ = None)"
					    x attr)
				(py-call x.attr.__set__ x val)))
	      
	      ((dict x)       (setf (dict-get x attr) val))

	      
	      (t              (py-raise 'AttributeError
					"(setf py-attr): no __setattr__ method for ~S" x)))))))


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
    (or (recursive-class-dict-lookup x.class '__get__)
	(recursive-class-dict-lookup x.class '__set__)
	(recursive-class-dict-lookup x.class '__delete__))))

(defun data-descriptor-p (x)
  "Returns DES-P, __SET__"
  ;; check for None?
  (recursive-class-dict-lookup (py-class-of x) '__set__))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Calling objects (functions, classes, instances)

(defgeneric py-call (f &rest args)
  
  (:method ((f null) &rest args)
	   (error "PY-CALL of NIL"))
  
  (:method ((f t) &rest args)
	   (let ((__call__ (recursive-class-lookup-and-bind f '__call__)))
	     (if __call__
		 (apply #'py-call __call__ args)
	       (error "Don't know how to call: ~S (args: ~A)" f args))))
  
  ;; Avoid infinite recursion:
  
  (:method ((f function) &rest args) (apply f args))
  
  (:method ((f py-bound-method) &rest args)
	   (apply #'py-bound-method.__call__ f args))
  
  (:method ((f py-unbound-method) &rest args)
	   (apply #'py-unbound-method.__call__ f args))
  
  (:method ((f py-static-method) &rest args)
	   (apply #'py-static-method.__call__ f args)))



;;; Subscription of items (sequences, mappings)

(defgeneric py-subs (x item)
  (:method ((x t) (item t))
	   (let ((gi (recursive-class-dict-lookup (py-class-of x) '__getitem__)))
	     (if gi
		 (py-call gi x item)
	       (py-raise 'TypeError "Object ~S does not support item extraction" x)))))

(defgeneric (setf py-subs) (new-val x item)
  (:method (new-val x item)
	   #+(or)(warn "(setf py-subs) T T: ~S ~S" x item)
	   (let ((si (recursive-class-dict-lookup (py-class-of x) '__setitem__)))
	     (if si
		 (py-call si x item new-val)
	       (py-raise 'TypeError "Object ~S does not support item assignment" x)))))

(defun setf-py-subs (x item new-val)
  (setf (py-subs x item) new-val))

(defun setf-py-attr (x attr val)
  (declare (notinline (setf py-attr)))
  (setf (py-attr x attr) val))


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

(defun raise-invalid-operands (operation left &optional right)
  ;; XXX merge into one pretty printer string
  ;; XXX TypeError
  (error (if right
	     (format nil "Operation '~A' not supported for operands ~S and ~S."
		     operation left right)
	   (format nil "Operation '~S' not supported for operand ~S." operation left))))

(defvar *binary-op-funcs-ht* (make-hash-table :test #'eq))
(defvar *binary-iop-funcs-ht* (make-hash-table :test #'eq))
(defvar *binary-iop->op-ht* (make-hash-table :test #'eq))


  
(defun get-binary-op-func-name (op) (or (gethash op *binary-op-funcs-ht*)
				   (error "missing binary op func: ~A" op)))
(defun get-binary-iop-func-name (iop) (or (gethash iop *binary-iop-funcs-ht*)
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
(def-math-func +   py-+    __add__      __radd__       +=   py-+=   __iadd__      )
(def-math-func -   py--    __sub__      __rsub__       -=   py--=   __isub__      )
(def-math-func *   py-*    __mul__      __rmul__       *=   py-*=   __imul__      )
(def-math-func /t/ py-/t/  __truediv__  __rtruediv__   /t/  py-/t/= __itruediv__  )
(def-math-func //  py-//   __floordiv__ __rfloordiv__  //=  py-//=  __ifloordiv__ ) 
(def-math-func /   py-/    __div__      __rdiv__       /=   py-/=   __idiv__      )
(def-math-func %   py-%    __mod__      __rmod__       %=   py-%=   __imod__      )
(def-math-func <<  py-<<   __lshift__   __rlshift__    <<=  py-<<=  __ilshift__   )
(def-math-func >>  py->>   __rshift__   __rrshift__    >>=  py->>=  __irshift__   )
(def-math-func &   py-&    __and__      __rand__       &=   py-&=   __iand__      )
(def-math-func \|  py-\|   __or__       __ror__        \|=  py-\|=  __ior__       )
(def-math-func ^   py-^    __xor__      __rxor__       ^=   py-^=   __ixor__      )
(def-math-func <divmod> py-divmod __divmod__ __rdivmod__    nil  nil     nil           )

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
  (let* ((op-meth (recursive-class-lookup-and-bind x '__pow__))
	 (res (and op-meth (if z
			       (py-call op-meth y z)
			     (py-call op-meth y)))))
    
    (if (and res (not (eq res (load-time-value *the-notimplemented*))))
	res
      (raise-invalid-operands '** x y))))

(setf (gethash '** *binary-op-funcs-ht*) 'py-**)

;; **= has similar ugliness

(defun py-**= (x y &optional z)
  (let* ((x.class (py-class-of x))
	 (iop-meth (recursive-class-dict-lookup x.class '__ipow__))
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

(def-unary-op-func ~  py-unary-~  __invert__ )
(def-unary-op-func +  py-unary-+  __pos__    )
(def-unary-op-func -  py-unary--  __neg__    )

(defgeneric py-not (x)
  (:method ((x t))
	   (py-bool (not (py-val->lisp-bool x)))))

(setf (gethash 'not *unary-op-funcs-ht*) 'py-not)

;; Equality and membership testing:  a in b, a not in b, a is b, a is not b

(defvar *binary-test-funcs-ht* (make-hash-table :test #'eq))
(defun get-binary-test-func (op) (or (gethash op *binary-test-funcs-ht*)
				     (error "missing binary test func: ~A" op)))

(defgeneric py-in (x seq)
  (:method ((x t) (seq t))
	   #+(or)(warn "py-in T T: ~S ~S" x seq)
	   ;; use __contains__, fall back on iterator
	   (let ((contains-meth (recursive-class-dict-lookup
				 (py-class-of seq) '__contains__)))
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

(setf (gethash 'in *binary-op-funcs-ht*) 'py-in)
(setf (gethash '|not in| *binary-op-funcs-ht*) 'py-not-in)

(defgeneric py-is (x y)
  (:method ((x t) (y t))
	   (if (eq x y) *the-true* *the-false*)))

(defgeneric py-is-not (x y)
  (:method ((x t) (y t))
	   (if (eq x y) *the-false* *the-true*)))

(setf (gethash 'is *binary-op-funcs-ht*) 'py-is)
(setf (gethash '|is not| *binary-op-funcs-ht*) 'py-is-not)



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
(defun get-binary-comparison-func-name (op) (gethash op *binary-comparison-funcs-ht*))

(defmacro def-comparison (syntax func test-x-y)
  `(progn (defgeneric ,func (x y)
	    (:method ((x t) (y t))
		     (if ,test-x-y
			 (load-time-value *the-true*)
		       (load-time-value *the-false*))))
	  (setf (gethash ',syntax *binary-comparison-funcs-ht*) ',func)))
	    
;; pyb:cmp returns -1, 0 or 1 (or TypeError if user-supplied
;; method returns bogus comparison result; that TypeError is not
;; catched here but goes to user code.)

(def-comparison  <  py-<   (=  (the (integer -1 1) (pybf:cmp x y)) -1))
(def-comparison  >  py->   (=  (the (integer -1 1) (pybf:cmp x y))  1))
(def-comparison ==  py-==  (=  (the (integer -1 1) (pybf:cmp x y))  0))
(def-comparison !=  py-!=  (/= (the (integer -1 1) (pybf:cmp x y))  0)) ;; parser: <> -> !=
(def-comparison <=  py-<=  (<= (the (integer -1 1) (pybf:cmp x y))  0))
(def-comparison >=  py->=  (>= (the (integer -1 1) (pybf:cmp x y))  0))

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
			       `(py-raise 'TypeError
					  "Object ~A (a ~A) has no `~A' method"
					  x (class-name (py-class-of x))
					  ',method)))))))

(def-py-shortcut-func py-abs  __abs__ )
(def-py-shortcut-func py-repr __repr__)
(def-py-shortcut-func py-str  __str__ :error (py-repr x))
(def-py-shortcut-func py-hex  __hex__ )
(def-py-shortcut-func py-oct  __oct__ )
(def-py-shortcut-func py-len  __len__ )
(def-py-shortcut-func py-nonzero __nonzero__ )

#+(or)
(defun py-hash (x)
  (py-hash-1 (if (symbolp x)
		 (symbol-name x)
	       x)))

(def-py-shortcut-func py-hash __hash__)

(defun py-val->string (x)
  (if (symbolp x) ;; Symbols don't represent Python values, but this is just handy for ourselves
      (symbol-name x)
  (let ((s (deproxy x))) ;; deproxy, as it may be py-string subclass instance
    (if (stringp s)
	s
      (py-raise 'TypeError "Expected a string, but got: ~A" x)))))

(defun py-val->integer (x &key min)
  (let ((i (deproxy x)))
    (if (and (integerp i) (if min (>= i min) t))
	i
      (py-raise 'TypeError "Expected an integer ~@[>= ~A~]; got: ~S" min x))))

(defun py-val->number (x)
  (let ((n (deproxy x)))
    (if (numberp n)
	n
      (py-raise 'TypeError "Expected a number; got: ~S" x))))


(defun py-repr-string (x) (py-val->string (py-repr x)))
(defun py-str-string  (x) (py-val->string (py-str x)))

(defun py-string->symbol  (x &optional (package #.*package*))
  ;; {symbol,string} -> symbol
  (if (symbolp x) 
      x
    (let ((str (deproxy x)))
      (if (stringp str)
	  (or (find-symbol str package)
	      (intern str package))
	(py-raise 'TypeError "Object is not a string (or symbol): ~A" x)))))

(defun py-symbol->string (x)
  ;; {symbol,string} -> string
  (etypecase x
    (symbol (symbol-name x))
    (string x)))


(defun repr-fmt (stream argument &optional colon-p at-p &rest parameters)
  "Wrapper function for PY-REPR that is usable inside format string, using
the ~/.../ directive: ~/python:repr-fmt/"
  
  (when (or colon-p at-p parameters)
    (error "Format string function py-repr-fmt does not support colon, ~
            at or parameters"))
  
  (let ((s (py-repr-string argument)))
    (write-string s stream)))


(defgeneric py-del-subs (x item)
  (:method (x item) (let* ((x.cls (py-class-of x))
			   (__delitem__ (recursive-class-dict-lookup x.cls '__delitem__)))
		      (if __delitem__
			    (py-call __delitem__ x item)
			(py-raise 'TypeError
				  "Object ~A (a ~A) has no `__delitem__' method"
				  x (class-name (py-class-of x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Iteration

;; iterator from lambda

(defclass py-func-iterator (py-core-object)
  ((name        :initarg :name)
   (func        :initarg :func :type function)
   (stopped-yet :initform nil))
  (:metaclass py-core-type))

(defun make-iterator-from-function (&key func name)
  "Create an iterator that calls f again and again. F somehow has
to keep its own state. As soon as F returns NIL, it is considered
finished; F will then not be called again."
  (check-type func function)
  (make-instance 'py-func-iterator :func func :name name))

(def-py-method py-func-iterator.next (fi)
  (with-slots (stopped-yet func) fi
    (let ((res (cond (stopped-yet nil)
		     ((funcall func))
		     (t (setf stopped-yet t)
			nil))))
      (if res
	  (return-from py-func-iterator.next res)
	(raise-StopIteration)))))

(def-py-method py-func-iterator.__repr__ (fi)
  (with-output-to-string (s)
    (print-unreadable-object (fi s :identity t)
      (with-slots (name) fi
	(format s "generator ~@[~A~]" name)))))

(def-py-method py-func-iterator.__iter__ (f)
  f)

;; general useful iteration constructs

(defgeneric get-py-iterate-fun (x)
  (:documentation
   "Return a function that when called repeatedly returns VAL, T, where VAL is the
next value gotten by iterating over X. Returns NIL, NIL upon exhaustion.")
  (:method ((x t))
	   (let* ((x.cls       (py-class-of x))
		  (__iter__    (recursive-class-dict-lookup x.cls '__iter__))
		  (__getitem__ (and (not __iter__)
				    (recursive-class-dict-lookup x.cls '__getitem__))))

	     ;; TODO: binding __getitem__ using __get__ is not done at
	     ;; all yet.
	     
	     (cond (__iter__ ;; Preferable, use __iter__ to retrieve x's iterator
		    (let* ((iterator     (py-call __iter__ x))
			   (iterator.cls (py-class-of iterator))
			   (next-meth    (or (recursive-class-dict-lookup iterator.cls 'next)
					     (py-raise
					      'TypeError
					      "The value returned by ~A's `__iter__' method ~
                  		               is ~A, which is not an iterator (no `next' method)"
					      x iterator))))
		      
		      ;; Note that we just looked up the `next' method
		      ;; before the first value is demanded. This is
		      ;; semantically incorrect in an ignorable way.
		      
		      (excl:named-function (:py-iterate-fun using __iter__)
			(lambda ()
			  (handler-case (values (py-call next-meth iterator))
			    (StopIteration () (values nil nil))
			    (:no-error (val)  (values val t)))))))
		   
		   
		   (__getitem__ ;; Fall-back: call __getitem__ with successive integers
		    (let ((index 0))
		      (excl:named-function (:py-iterate-fun using __getitem__)
			(lambda ()
			  (handler-case (values (py-call __getitem__ x index))
			    
			    ;; ok if this happens when index = 0: then it's an empty sequence
			    (IndexError () (values nil nil)) 
			    
			    (:no-error (val) (progn (incf index)
						    (values val t))))))))
		   
		   (t
		    (py-raise 'TypeError "Iteration over non-sequence (got: ~A)" x))))))

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
   "Returns a Lisp list, that may not be modified destructively.")
  (:method ((x t))
	   (loop with it-fun = (get-py-iterate-fun x)
	       for val = (funcall it-fun)
	       while val collect val)))



(defun get-py-iterator-for-object (x)
  (let* ((x.cls       (py-class-of x))
	 (__iter__    (recursive-class-dict-lookup x.cls '__iter__)))
    
    (if __iter__
	
	(py-call __iter__ x)
      
      (let ((f (get-py-iterate-fun x)))
	(make-iterator-from-function :func f)))))


(defvar *stdout-softspace* 0 "should a space be printed in front of next arg?")

(defun py-print (dest items comma?)
  (let ((*print-pretty* nil))
    (let* ((write-func (if dest 
			   (py-attr dest 'write)
			 (lambda (s) (write-string s) (finish-output))))
	   
	   (softspace-val (if dest
			      (handler-case 
				  (py-attr dest 'softspace)
			      
				(Exception (c) (progn 
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
      
      (cond ((and comma? (not (char= last-char-written #\Newline))) ;; right logic? 
	     (if dest 
		 (setf (py-attr dest 'softspace) *the-true*)
	       (setf *stdout-softspace* *the-true*)))
	    
	    ((and comma? (char= last-char-written #\Newline))
	     (if dest
		 (setf (py-attr dest 'softspace) *the-false*)
	       (setf *stdout-softspace* *the-false*)))
	    
	    (t
	     (if dest
		 (setf (py-attr dest 'softspace) *the-false*)
	       (setf *stdout-softspace* *the-false*))
	     (py-call write-func (py-string-from-char #\Newline)))))))



