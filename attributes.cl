(in-package :python)

;; (declaim (optimize (debug 3)))

;; Attribute getting and setting
;; =============================
;; 
;; This description by Phillip J. Eby was very helpful in clarifying
;; the hairy behaviour:
;;  http://mail.python.org/pipermail/python-dev/2003-May/035732.html
;; 
;; 
;; Getting
;; -------
;; 
;; 
;; If the lookup is for an instance, and there is a method
;; `__getattribute__' defined in one of the classes in the CPL,
;; it intercepts the attribute lookup:
;;   
;;   Call __getattribute__ with (instance, attrname) as arguments.
;; 
;; 
;; Otherwise:
;;   
;;   The dictionaries of the classes and then the superclasses are
;;   searched for the attribute. Depending on the first value found,
;;   action is taken:
;;    
;;     If the value is a "data descriptor" and it has a __get__ method
;;     (i.e. it has both __get__ and __set__ methods):
;;       
;;       The __get__ method of the data descriptor is invoked. The
;;       arguments to __get__ (besides the implied `self') are either
;;       (inst, cls) or (None, cls); the former is used if the
;;       attribute is looked up on an instance; the latter if looked
;;       up on a class. [Actually, the documentation says that the
;;       __get__ function can be called with either 1 or 2 args, but
;;       CPython calls it always with 2 - we do the same.] [If the
;;       thing has no __get__ method, it is skipped in CPython - we
;;       copy that, instead of raising ian error.]
;;     
;;     Else, if the value is not a data descriptor:
;;       
;;       If the lookup is done for an instance: keep this result
;;       around, in case the instance dict doesn't contain the
;;       attribute. Goto instance dict search (if instance attr
;;       search); or return the value (if class attr search).
;;
;;     Otherwise, when no value is found:
;;       
;;       If this is an instance attribute search: continue with
;;       instance dict search, if that fails goto __getattr__ search,
;;       if that also fails raise AttributeError.
;;       
;;       If this is a class attribute search: raise AttributeError.
;;       
;; 
;; Instance dict search:
;;   
;;   Check the __dict__ or the slots of the instance. If the attribute
;;   is in there (and bound, in case of slots), return it. Otherwise,
;;   if we found something during the class search (above), return
;;   that.
;;   
;;   Objects in the instance dict are returned directly (no binding/calling).
;;
;;   Objects found and kept around during class search are returned as follows:
;;     
;;     If it is a descriptor with a __get__ method: call __get__ with
;;     (inst, class) as args and return that;
;;     
;;     If it is a function, return a bound method;
;;     
;;     Otherwise, just return the value.
;;     
;;     
;; __getattr__ search:
;;   
;;   Look in all classes in the CPL for an attribute __getattr__; if
;;   found: invoke it, using (inst, attrname) as values. If not found,
;;   raise AttributeError.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Directly setting the __dict__ attribute
;; ---------------------------------------
;; 
;; In CPython the regular __dict__ is writable, but the new value must
;; be of type `dict' (or a subclass).
;; 
;; CPython has that restriction, because it inlines the calls: instead
;; of calling __getitem__ etc, it dives straight into the dictionary
;; object itself. So, one can *not* overrule attribute handling with:
;; 
;;  class D(dict):
;;    def __getitem__..
;;    def __setitem__..
;;    del __delitem__..
;; 
;; We do allow all objects as dict: lookups will go through
;; __getitem__, sets to __setitem__, deletes to __delitem__.
;; 
;; XXX When the dict is of default type `namespace' (a subclass of
;; `py-dict'), we could special-case it and inline the rest of the
;; logic. (Not sure that would buy us much.)
;; 

(defvar *try-__getattr__* t)
(defvar *try-__getattribute__* t)
(defvar *bind-result* t)

;; temporary -- all calls should be changed to call py-get-attribute
(defun internal-get-attribute (x attr)
  (let ((res (py-get-attribute x attr)))
    (values res res)))


(defgeneric py-get-attribute (x attr)
  (:documentation
   "Get attribute ATTR of X; returns ATTR or NIL. ~@
    Does not raise exceptions (AttributeError) itself, but any Python ~
    exception could be raised in methods called during lookup."))


(defmethod py-get-attribute :around (x attr)
  (or (call-next-method)
      (cond ((and (eq attr '__name__) (eq x #'iterator-next)) ;; XXX hack
	     "next")
	    (t
	     (warn "Catch-all internal-get-attribute: ~S ~S" x attr)
	     nil))))
  
(defmethod py-get-attribute :around ((x class) (attr symbol))
  (cond ((eq attr '__class__)
	 (__class__ x))
	((eq attr '__mro__)
	 (make-tuple-from-list (py-class-mro x)))
	((eq attr '__name__)
	 (symbol-name (class-name x)))
	(t (call-next-method))))


(defmethod py-get-attribute ((x py-super) attr)
  (flet ((find-preceding-class-in-mro (mro cls)
	   (check-type mro list)
	   (check-type cls class)
	   (loop for sublist on mro 
	       when (eq (car sublist) cls)
	       do (if (cdr sublist)
		      (return-from find-preceding-class-in-mro (second sublist))
		    (py-raise 'TypeError "In mro ~A, there is no class before ~A"
			      mro cls))
	       finally
		 (error "find-preceding-class-in-mro strangeness"))))
    
    ;; XXX check when attribute is static method and super is bound to class
    (with-slots (object current-class) x
      (let ((klass (find-preceding-class-in-mro
		    (py-class-mro (if (typep object 'class) object (class-of object)))
		    current-class)))
	(let ((val (getattr-of-class-rec klass attr)))
	  (when val (maybe-bind val object klass)))))))


(defmethod py-get-attribute ((x number) attr)
  (getattr-of-number x attr))

(defmethod py-get-attribute ((x string) attr)
  (let* ((cls (load-time-value (find-class 'py-string)))
	 (res (py-get-attribute cls attr)))
    (when res (maybe-bind res x cls))))

(defmethod py-get-attribute ((x builtin-instance) attr)
  (getattr-of-instance-rec x attr))

(defmethod py-get-attribute ((x udc-instance) attr)
  (getattr-of-instance-rec x attr))

(defmethod py-get-attribute ((x builtin-class) attr)
  (getattr-of-class-rec x attr))

(defmethod py-get-attribute ((x user-defined-class) attr)
  (getattr-of-class-rec x attr))

(defmethod py-get-attribute ((x class) attr)
  (warn "i-g-a for x class: ~A ~A" x attr)
  (getattr-of-class-rec x attr))

(defmethod py-get-attribute ((x py-module) attr)
  (getattr-of-module x attr))

(defmethod py-get-attribute ((x udc-with-ud-metaclass) attr)
  ;; Situation:
  ;;   class M(type): pass
  ;;   class C: __metaclass__ = M
  ;; Now, C is of type udc-with-ud-metaclass.
  ;; 
  ;; All user-defined classes have a __dict__ attribute of type
  ;; NAMESPACE-PROXY. This includes user-defined classes with a
  ;; user-defined metaclass.
  
  (let ((res (namespace-lookup (slot-value x '__dict__) attr)))
    (if res 
	(maybe-bind res x (class-of x))
      (getattr-of-class-rec x attr))))

(defmethod getattr-of-number ((x number) attr)
  (dolist (num-cls (load-time-value (list (find-class 'number) ;; XXX hack
					  (find-class 'real)
					  (find-class 'integer)
					  (find-class 'complex)
					  (find-class 't))))
    (when (typep x num-cls)
      (let ((meth (lookup-bi-class-attr/meth num-cls attr)))
	(when meth
	  (return-from getattr-of-number (maybe-bind meth x (__class__ x))))))))


(defmethod getattr-of-module ((x py-module) (attr symbol))
  (namespace-lookup (slot-value x 'namespace) attr))
  
(defmethod getattr-of-module ((x py-module) attr)
  (getattr-of-module x (intern (py-string-designator-val attr) #.*package*)))


(defmethod getattr-of-instance-rec ((x builtin-instance) attr)
  (loop for cls in (mop:class-precedence-list (class-of x))
      while (typep cls 'builtin-class)
      do (let ((meth (lookup-bi-class-attr/meth cls attr)))
	   (when meth
	     (return-from getattr-of-instance-rec
	       (maybe-bind meth x (class-of x)))))))

(defmethod getattr-of-instance-rec ((x udc-instance) attr)
  (declare (optimize (debug 3)))
  ;; Check user-defined classes for __getattribute__, including X
  ;; itself. While we're doing that, keep an eye on the value of
  ;; `__getattr__' and the value of the attribute itself if we come
  ;; across them.
  ;; 
  ;; __getattribute__ overrides the other two, but in its absence,
  ;; one of the two others is used if present.
  
  (let* ((__getattr__ nil)
	 (attr-val    nil)
	 (cpl (mop:class-precedence-list (class-of x)))
	 (cls (pop cpl)))
    
    (declare (special *default-__getattribute__-running*)) ;; see builtin-classes.cl
    
    (loop while (typep cls 'user-defined-class)
		
	unless *default-__getattribute__-running*
	       ;; To avoid infinite recursion caused by the default
	       ;; (built-in) __getattribute__ calling a user-defined
	       ;; __getattribute__, set a flag stating this. XXX hackish
	do (let ((val (getattr-of-class-nonrec cls '__getattribute__)))
	     (when val
	       (return-from getattr-of-instance-rec
		 (py-call (maybe-bind val x (class-of x)) (list (string attr))))))
	   
	when (not attr-val)
	do (let ((val (getattr-of-class-nonrec cls attr)))
	     (when val 
	       (setf attr-val val)))

	when (and (not attr-val) (not __getattr__))
	do (let ((val (getattr-of-class-nonrec cls '__getattr__)))
	     (when val (setf __getattr__ val)))
	   
	do (setf cls (pop cpl)))
    
    ;; Arriving here means: no __getattribute__, but perhaps
    ;; __getattr__ or attr-val.
    ;; 
    ;; If no attr-val found yet, try the builtin-classes from which
    ;; this user-defined-class inherited (if any).
    
    (unless attr-val
      (loop while (typep cls 'builtin-class)
	  do (let ((val (lookup-bi-class-attr/meth cls attr)))
	       (when val
		 (return-from getattr-of-instance-rec
		   (maybe-bind val x (class-of x)))))
	     (setf cls (pop cpl))))
    
    ;; try built-in class method (like __str__ for user-defined subclass of py-string)
    (let ((val (lookup-bi-class-attr/meth (load-time-value (find-class 't)) attr)))
      (when val
	(return-from getattr-of-instance-rec
	  (maybe-bind val x (class-of x)))))
    
    ;; Arriving here means: no regular attribute value, no
    ;; __getattribute__, but perhaps __getattr__ or attr-val.

    ;; If an attribute value found, and it is a data descriptor
    ;; (i.e. has a `__set__' attribute), it takes precedence over the
    ;; attributes set on the instance.
    
    ;; Try __get__-able data descriptor
    (when (and attr-val (data-descriptor-p attr-val))
      (return-from getattr-of-instance-rec
	(maybe-bind attr-val x (class-of x))))
    
    ;; Look in instance dict and slots. These are returned unbound.
    (let ((val (getattr-of-ud-instance-nonrec x attr)))
      (when val (return-from getattr-of-instance-rec val)))
    
    ;; Fall back to a class attribute that is not a `data descriptor'.
    (when attr-val
      (return-from getattr-of-instance-rec
	(maybe-bind attr-val x (class-of x))))
    
    ;; Fall back to the __getattr__ hook.
    (when __getattr__
      (return-from getattr-of-instance-rec
	(py-call (maybe-bind __getattr__ x (class-of x)) (list (symbol-name attr)))))

    ;; Finally, give up.
    ))


(defmethod getattr-of-class-nonrec ((cls class) attr) ;; was: builtin-class
  (lookup-bi-class-attr/meth cls attr))

(defmethod getattr-of-class-nonrec ((cls user-defined-class) attr)
  (namespace-lookup (slot-value cls '__dict__) attr))
  
(defmethod getattr-of-class-nonrec ((cls (eql (load-time-value (find-class 't)))) attr)
  (lookup-bi-class-attr/meth cls attr))

(defmethod getattr-of-class-rec ((cls builtin-class) attr)
  (loop for c in (mop:class-precedence-list cls)
      while (typep c 'builtin-class)
      do (let ((val (getattr-of-class-nonrec c attr)))
	   (when val
	     (return-from getattr-of-class-rec
	       (maybe-bind val *None* cls)))))
  
  ;; fallback: methods specialized on class 'class or class 't
  (let ((val (getattr-of-class-nonrec (load-time-value (find-class 'class)) attr)))
    (when val
      (return-from getattr-of-class-rec
	(maybe-bind val *None* cls))))

  (let ((val (getattr-of-class-nonrec (load-time-value (find-class 't)) attr)))
    (when val
      (return-from getattr-of-class-rec
	(maybe-bind val *None* cls))))
 
  nil)


(defmethod getattr-of-class-rec :around ((cls user-defined-class) attr)
  (if (member attr '(__dict__ __name__)) ;; make dict proxy?
      (slot-value cls attr)
    (call-next-method)))

(defmethod getattr-of-class-rec ((cls user-defined-class) attr)
  ;; All udc's have a <py-dict> instance in slot named
  ;; __dict__; that dict contains class attribs and
  ;; methods are stored. TODO: metaclasses

  (let* ((cpl (mop:class-precedence-list cls))
	 (c (pop cpl)))
    
    (loop while (typep c 'user-defined-class)
	do (let ((val (getattr-of-class-nonrec c attr)))
	     (when val
	       (return-from getattr-of-class-rec
		 (maybe-bind val *None* cls))))
	   (setf c (pop cpl)))
    
    (assert (or (member c
			(load-time-value
			 (list (find-class 'user-defined-class)
			       ;; ^-- when it's an instance of a user-defined metaclass
			       
			       (find-class 'python-type)
			       ;; subclass of type
			       ;;  at the moment an instance of the subclass is created
			       
			       (find-class 'udc-instance-w/dict)
			       (find-class 'udc-instance-w/slots)
			       (find-class 'udc-instance-w/dict+slots))))
		(typep c 'builtin-class)))
    
    (loop while (typep c 'builtin-class)
	do (let ((val (getattr-of-class-nonrec c attr)))
	     (when val
	       (return-from getattr-of-class-rec
		 (maybe-bind val *None* cls))))
	   (setf c (pop cpl))))
    
  ;; fallback: methods specialized on class 'class and class 't
  (let ((val (getattr-of-class-nonrec (load-time-value (find-class 'class)) attr)))
    (when val
      (return-from getattr-of-class-rec
	(maybe-bind val *None* cls))))

  (let ((val (getattr-of-class-nonrec (load-time-value (find-class 't)) attr)))
    (when val
      (return-from getattr-of-class-rec
	(maybe-bind val *None* cls)))))

(defmethod getattr-of-class-rec (cls attr)
  ;;(break "though to be unused") 
  (assert (or (eq cls (load-time-value (find-class 'python-type)))
	      (subtypep cls 'Exception)))
  (when (and (subtypep cls 'Exception)
	     (eq attr '__name__))
    (return-from getattr-of-class-rec
      (symbol-name (class-name cls))))
  
  (let ((val (lookup-bi-class-attr/meth (load-time-value (find-class 't)) attr)))
    (when val 
      (return-from getattr-of-class-rec
	(maybe-bind val *None* cls)))))


(defmethod getattr-of-ud-instance-nonrec ((x udc-instance-w/dict+slots) attr)
  (if (and (slot-exists-p x attr)
	   (slot-boundp x attr))  ;; also if attr is `__dict__' or `__slots__'
      (progn
	(let ((val (slot-value x attr)))
	  (when (eq attr '__dict__)
	    (assert (typep val 'namespace-proxy)))
	  val))
	  
    (namespace-lookup (slot-value x '__dict__) attr)))


(defmethod getattr-of-ud-instance-nonrec ((x udc-instance-w/slots) attr)
  (cond ((eq attr '__slots__)
	 (slot-value (class-of x) '__slots__))
	
	((and (slot-exists-p x attr) (slot-boundp x attr))
	 (slot-value x attr))))

(defmethod getattr-of-ud-instance-nonrec ((x udc-instance-w/dict) attr)
  (let ((dict (slot-value x '__dict__)))
    (assert (typep dict 'namespace-proxy))
	     
    (if (eq attr '__dict__)
	dict
      (namespace-lookup dict attr))))
    

;; Shortcuts

(defun getattr-of-class (x attr)
  "Lookup ATTR attribute of X's class"
  (internal-get-attribute (__class__ x) attr))

(defmethod call-attribute-via-class ((x builtin-instance) (attr (eql '__get__))
				     &optional pos-args key-args)
  (assert (null key-args))
  (values (py-call #'__get__ (cons x pos-args)) t))
   
(defmethod call-attribute-via-class (x attr &optional pos-args key-args)
  "Lookup ATTR of the class of X, and call it for instance X. ~@
   Returns RES, FOUND-P: ~@
   RES is the result of calling ATTR (unless calling resulted in NotImplemented); ~@
   FOUND-P is T or NIL. ~
   Example use: `print' calls the class' __str__ method, not the ~@
   __str__ attribute of the instance."
  
  (let ((klass (__class__ x)))
    (let ((val (getattr-of-class-rec klass attr)))
      (cond ((not val)            nil)
	    ((eq val #'__get__)   (values (apply val x pos-args) t)) ;; XXX hack?
	    (t  (let* ((bound-val (maybe-bind val nil klass)) ;; nil was *None*
		       (res (py-call bound-val (cons x pos-args) key-args)))
		  (if (eq res *NotImplemented*)
		      nil
		    (values res t))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maybe-bind : bind result to instance or class

;; Remove use of unbound methods... XXX check if this doesn't remove too much...

(defmethod maybe-bind :around (x instance class)
  (declare (ignore instance class))
  (if *bind-result*
      (call-next-method)
    x))


(defmethod maybe-bind ((x function) instance class)
  (if (member instance (load-time-value (list nil *None*) :test 'eq))
      x ;; no unbound method anymore
    (__get__ x instance class)))

;; TODO: class-method ...

(defmethod maybe-bind ((x user-defined-function) instance class)
  (declare (ignore class))
  (if (member instance (load-time-value (list nil *None*) :test 'eq))
      x
    (call-next-method)))

#+(or) ;; there are no unbound methods anymore
(defmethod maybe-bind ((x unbound-method) instance class)
  (declare (ignore class))
  (with-slots ((cls class) func) x
    (if instance
	(cond ((typep instance cls)
	       (make-bound-method :func func :object instance))
	      ((eq instance *None*)
	       x)
	      (t
	       (py-raise 'TypeError "Unbound method ~A bound to instance ~A of wrong type"
			 x instance)))
      x)))

(defmethod maybe-bind (object instance class)
  (multiple-value-bind (bound-val get-found)
      (call-attribute-via-class object '__get__ (list instance class))
    (if get-found
	bound-val
      object)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Memoization for attribute lookups:

#+(or) ;; for later use
(define-compiler-macro py-get-attribute (x attr)
  (let ((cache (cons nil nil)))
    `(py-get-attribute-cached ,x ,attr ,cache)))

#+(or) ;; for later use
(defmethod py-get-attribute-cached ((x builtin-instance) (attr symbol) (cache cons))
  (let ((klass (class-of x)))
    (if (eq klass (car cons))
	(cdr cons)
      (let ((res (py-get-attribute x attr)))
	(when res (setf (car cache) klass
			(cdr cache) res))
	res))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET attribute
;; 
;; If the attribute is set on an instance:
;;   
;;   If there is a method `__setattr__' defined in one of the classes
;;   in the CPL, it intercepts the attribute setting. It is called
;;   with (instance, attrname, val) as arguments.
;;   
;;   Otherwise, the classes in the CPL are searched for the
;;   attribute. If found:
;;     
;;     If the value is a "data descriptor", its __set__ method of the
;;     is invoked. The arguments to __set__ (besides the implied
;;     `self') are (inst, val).
;;     
;;     Else, if the value is something else: store in instance
;;     __dict__/slot.
;;   
;;
;; If the lookup is set on a class:
;;   
;;   Update the class' __dict__.


(defgeneric internal-set-attribute (x attr val)
  (:documentation
   "Set attribute ATTR of X to VAL. ~@
    Returns nothing if success; raises AttributeError upon failure."))

(defmethod internal-set-attribute :around (x attr val)
  (declare (ignore val))
  ;; (format t "internal-set-attribute: ~S ~S~%" x attr)
  (check-type attr symbol)
  (verify-settable-attribute x attr)
  (call-next-method))

(defmethod internal-set-attribute (x attr val)
  (if (typep (class-of x) 'udc-with-ud-metaclass)
      (namespace-bind (slot-value x '__dict__) attr val)
    (break "internal-set-attribte: uncatched")))
    
(defmethod internal-set-attribute ((x user-defined-class) attr val)
  ;; Don't worry about metaclasses yet: assume all classes have a
  ;; attribute __dict__ which is a mapping that supports __setitem__
  
  (cond ((eq attr '__dict__)
	 (break "internal-set-attribute of __dict__: may be buggy")
	 (setf (slot-value x '__dict__) val))
	
	((eq attr '__name__)
	 (setf (class-name x) val))
	
	((eq attr '__bases__)
	 ;; change the direct-superclasses
	 (error "todo: change __bases__"))
	
	(t (let ((dict (slot-value x '__dict__)))
	     (unless (typep dict 'namespace-proxy)
	       (break "internal-set-attribute of udc: __dict__ not nsproxy"))
	     (namespace-bind dict attr val)))
	
	#+(or)(call-attribute-via-class (slot-value x '__dict__) '__setitem__
					(list attr val))))


(defmethod internal-set-attribute ((x udc-instance) attr val)
  ;; __dict__ is directly writable for instances

  ;; Look for __setattr__ in the classes, keeping an eye on the first
  ;; value of the attribute itself if we come across it.

  (let ((attr-val nil))
    (declare (special *default-__setattr__-running*))
   
    (loop for cls in (mop:class-precedence-list (__class__ x)) ;; XXX or class-of ?
		     ;; until (eq cls (load-time-value (find-class 'python-type)))
		     ;; XXX can we assume that none of the built-in
		     ;; classes have a 'data descriptor' attribute?!
		       
	with (is-udc is-bic)
	       
	do (setf is-udc (typep cls 'user-defined-class)
		 is-bic (typep cls 'builtin-class))
	     
	when (and is-udc (not *default-__setattr__-running*))  ;; Only UDC can have__setattr__ hook
	do (multiple-value-bind (setattr found)
	       (getattr-of-class-nonrec cls '__setattr__) ;; XXX bind?
	     (when found
	       (return-from internal-set-attribute
		 (py-call setattr (list x attr val)))))
	   
	when (and (not attr-val) (or is-udc is-bic))
	do (multiple-value-bind (val found)
	       (getattr-of-class-nonrec cls attr) ;; don't bind: __set__ is called later
	     (when found
	       (setf attr-val val))))
   
    ;; Call attr-val's __set__ if availabie (i.e. if it is a data
    ;; descriptor)
      
    (when attr-val
      (multiple-value-bind (res set-found)
	  (call-attribute-via-class attr-val '__set__ (list x val))
	(if set-found
	    (return-from internal-set-attribute res))))
    
    ;; Set attribute in the instance __dict__/slot
    (setattr-udi x attr val)))



(defmethod setattr-udi ((x udc-instance-w/dict) attr val)
  (if (eq attr '__dict__)
      (progn (break "setattr-udi __dict__: buggy")
	     (setf (slot-value x '__dict__) val))
    (let ((dict (slot-value x '__dict__)))
      (assert (typep dict 'namespace-proxy))
      (namespace-bind dict attr val)))) ;; was: __setitem__

;;    #+(or)(call-attribute-via-class (slot-value x '__dict__) '__setitem__
;;			      (list attr val))))

(defmethod setattr-udi ((x udc-instance-w/slots) attr val)
  (if (slot-exists-p x attr)
      (setf (slot-value x attr) val)
    (py-raise 'AttributeError 
	      "Instances of class ~A don't have a slot called ~A ~@
               (these are the available slots: ~A)"
	      (class-of x) attr (slot-value (class-of x) '__slots__))))

(defmethod setattr-udi ((x udc-instance-w/dict+slots) attr val)
  (if (slot-exists-p x attr)
      (setf (slot-value x attr) val)
    
    ;; inline?
    (let ((dict (slot-value x '__dict__)))
      (assert (typep dict 'namespace-proxy))
      (namespace-bind dict attr val)))
  #+(or)(call-attribute-via-class (slot-value x '__dict__) '__setitem__
				  (list attr val)))


(defmethod verify-settable-attribute (x attr)
  (if (typep (class-of x) 'udc-with-ud-metaclass)
      t
    (raise-setattr-error x attr)))

;; (setf (namespace-bind (slot-value x '__dict__) attr))

#+(or)
(defmethod verify-settable-attribute (x attr)
  ;; Can't set any attribute of built-in objects (whether classes or instances).
  ;; (Everything not of type USER-DEFINED-CLASS is a built-in type.)
  )

(defmethod verify-settable-attribute ((x user-defined-class) attr)
  ;; In CPython:
  ;;  - <user-defined-class>.__class__ can be set to another user-defined class.
  ;;  - class.__doc__ is not settable (I consider that a bug)
  ;; XXX todo
  (when (member attr '(__class__ __slots__) :test 'eq)
    (raise-setattr-error x attr)))

(defmethod verify-settable-attribute ((x udc-instance) attr)
  ;; Setting __slots__ is bogus. In CPython after class creation it
  ;; behaves as a regular attribute that's settable in both class and
  ;; instances. Let's not allow setting its value, so __slots__ can
  ;; only be used for one thing: specifying class slots, at class
  ;; creation time.
  (when (eq attr '__slots__)
    (raise-setattr-error x attr)))

(defun raise-setattr-error (x attr)
  (py-raise 'TypeError
	    "Attribute ~A of ~A is not settable (maybe TODO?)" x attr))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DELETE attribute
;; 
;; In case the object has a __dict__, __delitem__ is called on the
;; value of __dict__. If that leads to an exception (independent of
;; what kind), that exception is catched by internal-del-attribute and
;; AttributeError is raised instead.
;; 
;; We avoid setting up `handler-case' when dealing with a standard
;; dict (of type `hash-table') by special-casing it.

(defgeneric internal-del-attribute (x attr)
  (:documentation "Delete ATTR from X. ~@
                   Returns nothing. Raises AttributeError upon failure."))

(defmethod internal-del-attribute :around (x attr)
  (declare (ignore x))
  ;; Python catches any exception that occurs and returns
  ;; AttributeError in that case.
  (ensure-py-type attr attribute-name "Not a valid attribute name: ~S")
  (call-next-method))


(defmethod internal-del-attribute ((x builtin-object) attr)
  (py-raise 'AttributeError
	    "Attribute ~A of object ~A is not deletable, because it's a built-in object."
	    x attr))


(defmethod internal-del-attribute ((x user-defined-class) attr)
  (when (eq attr '__dict__)
    (break "TODO: i-del-attr udc __dict__"))
  (let ((dict (slot-value x '__dict__)))
    (typecase dict
      (namespace-proxy (or (namespace-delete dict attr)
			   (raise-delattr-error-no-such-att x attr)))
	  
      (t (warn "deleting by __delitme__")
	 (handler-case 
	     (__delitem__ dict attr)
	   (Exception () (raise-delattr-error-no-such-att x attr)))))))

(defmethod internal-del-attribute ((x udc-instance-w/dict+slots) attr)
  (if (slot-exists-p x attr)
      
      (if (slot-boundp x attr)
	  (slot-makunbound x attr) ;; todo: call __delete__ on val
	(raise-delattr-error-unbound x attr))
    
    (let* ((*bind-result* nil)
	   (curr-val (internal-get-attribute x attr))
	   (curr-val-delete (and curr-val (getattr-of-class curr-val '__delete__))))
      (if curr-val-delete
	  (progn (warn "calling delte method")
		 (py-call curr-val-delete (list curr-val x)))
	
	(let ((dict (slot-value x '__dict__)))
	  (typecase dict
	    (namespace-proxy (or (namespace-delete dict attr)
				 (raise-delattr-error-no-such-att x attr)))
	    (t (handler-case (__delitem__ dict attr)
		 (Exception () (raise-delattr-error-no-such-att x attr))))))))))


(defmethod internal-del-attribute ((x udc-instance-w/dict) attr)
  (let* ((*bind-result* nil)
	 (curr-val (internal-get-attribute x attr))
	 (curr-val-delete (and curr-val (getattr-of-class curr-val '__delete__))))
    (if curr-val-delete
	(progn (warn "calling delte method")
	       (py-call curr-val-delete (list curr-val x)))
      
      (let ((dict (slot-value x '__dict__)))
	(typecase dict
	  (namespace-proxy (or (namespace-delete dict attr)
			       (raise-delattr-error-no-such-att x attr)))
	  (t (handler-case (__delitem__ dict attr)
	       (Exception () (raise-delattr-error-no-such-att x attr)))))))))
  
(defmethod internal-del-attribute ((x udc-instance-w/slots) attr)
  (if (slot-exists-p x attr)
      (if (slot-boundp x attr)
	  (slot-makunbound x attr)
	(raise-delattr-error-unbound x attr))
    (raise-delattr-error-no-such-att x attr)))

(defun raise-delattr-error-no-such-att (x attr)
  (py-raise 'AttributeError
	    "Object ~A has no attribute ~A" x attr))

(defun raise-delattr-error-unbound (x attr)
  (py-raise 'AttributeError
	    "Cannot delete attribute ~A of object ~A: the slot exists but it is unbound."
	    x attr))

;; for debugging
(defun trace-attr ()
  (trace internal-get-attribute getattr-class-nonrec ud-instance-only call-attribute-via-class))
