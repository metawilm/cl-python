(in-package :python)

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
;;    (i.e. it has both __get__ and __set__ methods):
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
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 
;; Setting
;; -------
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
;; 
;; XXX Deleting...


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
;; When the dict is of default type `namespace' (a subclass of
;; `py-dict'), we could special-case it and inline the rest of the
;; logic. (Not sure that would buy us much.)
;; 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GET attribute

(defgeneric internal-get-attribute (x attr)
  (:documentation
   "Get attribute ATTR of X; returns VAL, FOUND-P. ~@
    Could raise a Python exception in the process (but does not ~@
    raise AttributeError itself)."))

(defmethod internal-get-attribute :around (x attr)
  ;; (format t "internal-get-attribute: ~S ~S~%" x attr)
  (ensure-py-type attr attribute-name "Not a valid attribute name: ~S")
  (cond ((eq attr '__class__)
	 (values (__class__ x) t))
	
	((and (typep x 'class)
	      (eq attr '__bases__))
	 (values (copy-list (mop:class-direct-superclasses x)) t)) ;; copy for safety
	
	(t
	 (call-next-method x attr))))

(defmethod internal-get-attribute ((x udc-instance) attr)
  (let ((__getattr__ nil)
	(attr-val    nil))
    
    ;; Check user-defined classes for __getattribute__, including x
    ;; itself. While we're doing that, keep an eye on the value of
    ;; `__getattr__' and the value of the attribute itself if we come
    ;; across them.
    
    (let (is-udc is-bic)
      (loop for cls in (mop:class-precedence-list (class-of x))
		       ;; until (eq cls (load-time-value (find-class 'python-type)))
		     
	  do (setf is-udc (typep cls 'user-defined-class)
		   is-bic (typep cls 'builtin-class)) ;; not mutually exclusive: subclass bi-cl
		      
	     ;; only UDC could have __getattribute__ hook
	  when is-udc 
	  do (multiple-value-bind (val found)
		 (getattr-class-nonrec cls '__getattribute__)
	       (when found (return-from internal-get-attribute
			     (values (__call__ val (list x attr))
				     t))))
	     
	  when (and (not attr-val) (or is-udc is-bic))
	  do (multiple-value-bind (val found)
		 (getattr-class-nonrec cls attr x)
	       (when found (setf attr-val val)))

	     ;; only UDC could have __getattr__ hook
	  when (and (not attr-val) (not __getattr__) is-udc)
	  do (multiple-value-bind (val found)
		 (getattr-class-nonrec cls '__getattr__)
	       (when found (setf __getattr__ val))))
    
      ;; Arriving here means: no __getattribute__, but perhaps __getattr__ or attr-val.
      
      (let ((attr-val-try-__get__ t))
	
	;; Try __get__-able data descriptor
	(when (and attr-val (data-descriptor-p attr-val))
	  (multiple-value-bind (get-meth found)
	      (internal-get-attribute attr-val '__get__)
	    (if found
		(return-from internal-get-attribute
		  (values (__call__ get-meth (list x (class-of x)))
			  t))
	      (setf attr-val-try-__get__ nil))))
	
	;; Look in instance dict
	(multiple-value-bind (val found)
	    (ud-instance-only x attr)
	  (when found (return-from internal-get-attribute
			(values val t))))
    
	;; Fall back to a class attribute
	(when attr-val
	  (if attr-val-try-__get__
	      
	      (multiple-value-bind (get-meth found)
		  (internal-get-attribute attr-val '__get__)
		(return-from internal-get-attribute
		  (values
		   (if found
		       (__call__ get-meth (list x (class-of x)))
		     attr-val)
		   t)))
	    (return-from internal-get-attribute
	      (values attr-val t)))))
      
      ;; __getattr__ hook
      (when __getattr__
	(return-from internal-get-attribute
	  (values (__call__ __getattr__ (list x attr))
		  t)))

      ;; give up
      (py-raise 'AttributeError
		"Object ~A has no attribute ~A" x attr))))


(defmethod internal-get-attribute ((x user-defined-class) attr)
  (loop for cls in (mop:class-precedence-list x)
		   ;; until (eq cls (load-time-value (find-class 'python-type)))
		   
      do (multiple-value-bind (val found)
	     (getattr-class-nonrec cls attr)
	   (when found 
	     (multiple-value-bind (get-meth get-found)
		 (internal-get-attribute val '__get__)
	       (return-from internal-get-attribute
		 (if get-found
		     (values (__call__ get-meth (list *None* x))
			     t)
		   (values val t)))))))
  (values nil nil))


(defmethod internal-get-attribute ((x py-module) attr)
  (namespace-lookup (slot-value x 'namespace) attr))

(defmethod internal-get-attribute (x attr)
  ;; XXX called when??
  (assert (typep x '(or builtin-object symbol string number)) ()
    "Object ~A is not (a designator for) a builtin-object." x)
  
  (loop for cls in (mop:class-precedence-list (__class__ x)) ;; not CLASS-OF because of designators
		   ;; until (eq cls (load-time-value (find-class 'python-type)))
		   
      do (multiple-value-bind (val found)
	     (getattr-class-nonrec cls attr x)
	   ;; VAL is by now already bound, if needed
	   (when found
	     (return-from internal-get-attribute
	       (values val t)))))
  (values nil nil))




(defmethod internal-get-attribute ((x builtin-class) attr)
  (loop for cls in (mop:class-precedence-list x)
		   ;; until (eq cls (load-time-value (find-class 'python-type)))
      do (multiple-value-bind (val found)
	     (getattr-class-nonrec cls attr)
	   (when found
	     (return-from internal-get-attribute
	       (values val found)))))
  (values nil nil))

(defgeneric getattr-class-nonrec (class attr &optional instance)
  (:documentation "Non-recursive attribute lookup in class. ~@
                   If INSTANCE is supplied, result is wrapped when appropriate. ~@
                   Returns VAL, FOUND"))

(defmethod getattr-class-nonrec (x attr &optional instance)
  (declare (ignore x attr instance))
  ;; default method
  (values nil nil))

(defmethod getattr-class-nonrec ((x user-defined-class) attr &optional instance)
  ;; All udc's have a <py-dict> instance in slot named
  ;; __dict__; that dict contains class attribs and
  ;; methods are stored
  (multiple-value-bind (val found)
      (__getitem__ (slot-value x '__dict__) attr)
    (when found
      (return-from getattr-class-nonrec
	(values (if (typep val 'user-defined-function)
		    (if instance
			(make-bound-method :func val :self instance)
		      (make-unbound-method :func val :class x))
		  val)
		t))))
  (values nil nil))


(defmethod getattr-class-nonrec ((x builtin-class) attr &optional instance)

  ;; magic methods `__xxx__' -> (un)bound methods for Generic Function
  (when (fboundp attr)
    (let ((gf (symbol-function attr)))
      (if (typep gf 'generic-function)

	(if (some (lambda (meth) 
		    (and (null (method-qualifiers meth)) ;; ignore the ':around' catch-all one
			 (subtypep x (car (mop:method-specializers meth)))))
		  (mop:generic-function-methods gf))
	    (return-from getattr-class-nonrec
	      (values (if instance
			  (make-bound-method :func gf :self instance)
			(make-unbound-method :func gf :class x))
		      t))))))

  ;; attribute: `<complex>.real -> value
  ;; 
  ;; ATTR is a symbol. Non-magic methods of built-in classes are
  ;; stored in the property list of the symbol that is the attribute
  ;; name, with the class object as key. For example, the `items'
  ;; method of class `py-dict', which is implemented by Generic
  ;; Function `dict-items', is stored as:
  ;; 
  ;; (setf (get 'items <py-dict class>) (cons 'meth #'dict-items)
  ;; 
  ;; So: (cons X #'func), where X is either 'meth or 'att
  ;;
  ;; And to get it back:
  ;; 
  ;; (get 'items <py-dict class>)
  ;; 
  ;; which returns either a generic function or NIL.
	     
  (let ((res (get attr x)))
    (when res
      (case (car res)
	('meth (return-from getattr-class-nonrec
		 (values (if instance
			     (make-bound-method :self instance :func (cdr res))
			   (make-unbound-method :class x :func (cdr res)))
			 t)))
	       
	;; An attribute is only ok when we are here for looking up
	;; an instance attribute. Otherwise, we're acting like the
	;; attribute is not found.
		       
	('att (if instance
		  (return-from getattr-class-nonrec
		    (values (funcall (cdr res) instance)
			    t)))))))
  (values nil nil))


(defmethod ud-instance-only ((x udc-instance-w/dict+slots) attr)
  (if (and (slot-exists-p x attr)
	   (slot-boundp x attr)) ;; also for attr=__dict__/__slots__ !
      (values (slot-value x attr) t)
    (__getitem__ (slot-value x '__dict__) attr)))

(defmethod ud-instance-only ((x udc-instance-w/slots) attr)
  (cond ((eq attr '__slots__)
	 (values (slot-value (class-of x) '__slots__) t))
	
	((and (slot-exists-p x attr) (slot-boundp x attr))
	 (values (slot-value x attr) t))
	
	(t (values nil nil))))

(defmethod ud-instance-only ((x udc-instance-w/dict) attr)
  (if (eq attr '__dict__)
      (values (slot-value x '__dict__) t)
    (handler-case 
	(__getitem__ (slot-value x '__dict__) attr)
      (KeyError ()
	(values nil nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET attribute

(defgeneric internal-set-attribute (x attr val)
  (:documentation
   "Set attribute ATTR of X to VAL. ~@
    Returns nothing if success; raises AttributeError upon failure."))

(defmethod internal-set-attribute :around (x attr val)
  (declare (ignore val))
  ;; (format t "internal-set-attribute: ~S ~S~%" x attr)
  (ensure-py-type attr attribute-name "Not a valid attribute name: ~S")
  (verify-settable-attribute x attr)
  (call-next-method))

(defmethod internal-set-attribute ((x user-defined-class) attr val)
  ;; Don't worry about metaclasses yet: assume all classes have a
  ;; attribute __dict__ which is a mapping that supports __setitem__
  
  (cond ((eq attr '__dict__)
	 (setf (slot-value x '__dict__) val))
	
	((eq attr '__name__)
	 (setf (class-name x) val))
	
	((eq attr '__bases__)
	 ;; change the direct-superclasses
	 (error "todo: change __bases__"))
	
	(t 
	 (__setitem__ (slot-value x '__dict__) attr val))))


(defmethod internal-set-attribute ((x udc-instance) attr val)
  ;; __dict__ is directly writable for instances

  ;; Look for __setattr__ in the classes, keeping an eye on the first
  ;; value of the attribute itself if we come across it.
  (let ((attr-val nil))
    
    (let (is-udc is-bic)
      
      (loop for cls in (mop:class-precedence-list (class-of x))
		       ;; until (eq cls (load-time-value (find-class 'python-type)))
		       ;; XXX can we assume that none of the built-in
		       ;; classes have a 'data descriptor' attribute?!
		     
	  do (setf is-udc (typep cls 'user-defined-class))
		      
	     ;; Only UDC could have__setattr__ hook?
	  when is-udc
	  do (multiple-value-bind (setattr found)
		 (internal-get-attribute cls '__setattr__)
	       (when found
		 (return-from internal-set-attribute
		   (__call__ setattr (list x attr val)))))
	   
	  when (and (not attr-val) (or is-udc is-bic))
	  do (multiple-value-bind (val found)
		 (internal-get-attribute cls attr)
	       (when found
		 (setf attr-val val))))
    
      ;; Call attr-val's __set__ if availabie (i.e. if it is a data
      ;; descriptor)
      (when attr-val
	(multiple-value-bind (__set__-meth found)
	    (internal-get-attribute attr-val '__set__)
	  (when found
	    (return-from internal-set-attribute
	      (__call__ __set__-meth (list x val))))))
  
      ;; Set attribute in the instance __dict__/slot
      (setattr-udi x attr val))))


(defmethod setattr-udi ((x udc-instance-w/dict+slots) attr val)
  (if (slot-exists-p x attr)
      (setf (slot-value x attr) val)
    (__setitem__ (slot-value x '__dict__) attr val)))
  
(defmethod setattr-udi ((x udc-instance-w/dict) attr val)
  (if (eq attr '__dict__)
      (setf (slot-value x '__dict__) val)
    (__setitem__ (slot-value x '__dict__) attr val)))

(defmethod setattr-udi ((x udc-instance-w/slots) attr val)
  (if (slot-exists-p x attr)
      (setf (slot-value x attr) val)
    (py-raise 'AttributeError 
	      "Instances of class ~A don't have a slot called ~A ~@
               (these are the available slots: ~A)"
	      (class-of x) attr (slot-value (class-of x) '__slots__))))


(defmethod verify-settable-attribute ((x builtin-object) attr)
  ;; Can't set anything.
  (raise-setattr-error x attr))

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
  (let ((dict (slot-value x '__dict__)))
    (handler-case (__delitem__ dict attr)
      (Exception () (raise-delattr-error-no-such-att x attr)))))


(defmethod internal-del-attribute ((x udc-instance-w/dict+slots) attr)
  (if (slot-exists-p x attr)
      
      (if (slot-boundp x attr)
	  (slot-makunbound x attr)
	(raise-delattr-error-unbound x attr))
    
    (let ((dict (slot-value x '__dict__)))
      (handler-case (__delitem__ dict attr)
	(Exception () (raise-delattr-error-no-such-att x attr))))))


(defmethod internal-del-attribute ((x udc-instance-w/dict) attr)
  (let ((dict (slot-value x '__dict__)))
    (handler-case (__delitem__ dict attr)
      (Exception () (raise-delattr-error-no-such-att x attr)))))


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