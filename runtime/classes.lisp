;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON; Readtable: PY-AST-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;; To have Emacs properly indent the DEF-PY-METHOD form, add to .emacs:
;;;  (put 'def-py-method 'fi:common-lisp-indent-hook (get 'defmethod 'fi:common-lisp-indent-hook))

(in-package :clpython)
(in-syntax *ast-user-readtable*)

;; TODO: ensure all items listed in this table are implemented
;; http://www.python.org/doc/current/library/inspect.html

;; Python metatype. Class `type' and subclasses thereof are instances
;; of py-meta-type.

(defun finalize-inheritance (c)
  (closer-mop:finalize-inheritance c))

(defmethod initialize-instance :after ((cls py-meta-type) &rest initargs)
  (declare (ignore initargs))
  (finalize-inheritance cls))

(defmethod initialize-instance :after ((cls py-type) &rest initargs)
  (declare (ignore initargs))
  (finalize-inheritance cls))

;; Lisp type/object
(defclass py-lisp-type (py-type)
  ()
  (:documentation "Metaclass for proxy classes")
  (:metaclass py-meta-type))

(defclass py-lisp-object (object)
  ((lisp-object :initarg :lisp-object :accessor proxy-lisp-val))
  (:metaclass py-lisp-type)
  (:documentation "Base class for proxy classes"))


(defun ensure-class (&rest args)
  (apply #'closer-mop:ensure-class args))

(defun ensure-class-using-class (&rest args)
  (apply #'closer-mop:ensure-class-using-class args))

(defun class-direct-superclasses (&rest args)
  (apply #'closer-mop:class-direct-superclasses args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This is a bit ugly, but method classes must be evaluated prior to their
;;; use. Therefore here are method classes, and below their methods.
;;;
;;; Note that PY-METHOD is not a Lisp FUNCTION.

(defclass py-method (object)
  ((func :initarg :func :accessor py-method-func))
  (:metaclass py-type))

(defclass py-class-method (py-method)
  ((class :initarg :class))
  (:metaclass py-type))

(defclass py-attribute-method (py-method)
  ()
  (:metaclass py-type))

(defclass py-class-attribute-method (py-attribute-method)
  ()
  (:metaclass py-type))

(defclass py-writable-attribute-method (py-attribute-method)
  ((write-func :initarg :writer :accessor attribute-writer))
  (:metaclass py-type))

(defun make-writable-attribute (reader writer)
  (make-instance 'py-writable-attribute-method :func reader :writer writer))

(defmethod print-object ((x py-writable-attribute-method) stream)
  (print-unreadable-object (x stream)
    (with-slots (func write-func) x
      (format stream ":func ~S :write-func ~S" func write-func))))

(defclass py-static-method (py-method)
  ()
  (:metaclass py-type))

(defparameter *writable-attribute-methods* (make-hash-table :test #'eq))

(eval-when (:compile-toplevel :load-toplevel :execute)

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
			  (ensure-user-symbol (format nil "~A-~A" cls.meth '#:writer))
			cls.meth)))
      
      (assert (<= (length modifiers) 1) ()
	"Multiple modifiers for a py-method: ~A. Todo?" modifiers)

      `(progn ,(destructuring-bind (func-args &body func-body) args
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
                                        (declare (ignorable ,real-name))
                                        ,body)))
                     else do (push sym real-args)
                          
                     finally (return (progn (setf real-args (nreverse real-args))
                                            `(defun ,func-name ,real-args
                                               ;; Make all args ignorable. Otherwise there will be warnings for
                                               ;; unused variables even if there is (declare (ignore ..)) in the
                                               ;; function body: function body is wrapped in a LOCALLY.
                                               ;; XXX Ugly, should parse declare-ignore declarations.
                                               ,(let ((sym-args (remove-if-not (lambda (s) (and (symbolp s)
                                                                                                (not (char= (aref (string s) 0) #\&))))
                                                                               real-args)))
                                                  `(declare (ignorable ,@sym-args)))
                                               (block ,cls.meth
                                                 ,body))))))
              (register-method ',cls ',meth
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
                                                        read-f ;; read function is already stored in dict
                                                        ))))))))

(defun register-method (cls-name attr val)
  (check-type attr symbol)
  (let* ((cls (or (find-class cls-name) (error "No such class: ~A" cls-name))))
    (assert (typep cls 'dict-mixin))
    (class.raw-attr-set cls attr val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; dynamic class creation

(defun make-py-class (&rest args)
  (declare (dynamic-extent args))
  (multiple-value-bind (cls kind)
      (apply #'make-py-class-1 args)
    (assert (member kind '(:metaclass :class :condition)))
    cls))

(defun apply-namespace-to-cls (namespace cls)
  (assert (eq-hash-table-p namespace)) ;; may not hold when using __prepare__ etc
  (loop for key being each hash-key in namespace
      using (hash-value val)
      do (assert (symbolp key))
         (class.raw-attr-set cls key val)))

(defun make-py-class-1 (&key name context-name namespace supers cls-metaclass mod-metaclass)
  (declare (ignore context-name)) ;; XXX for now
  (flet ((make-condition-exception ()
           (assert (not *exceptions-are-python-objects*))
           (values (let ((cond-class-name (apply #'define-exception-subclass
                                                 name (mapcar 'class-name supers))))
                     (check-type cond-class-name class)
                     cond-class-name)
                   :condition)))
    
    #+(or)(assert (symbolp name))
    #+(or)(assert (listp supers))
    #+(or)(assert (typep namespace 'dict))
  
    ;; XXX is this a true restriction?  Custom metaclasses may allow
    ;; more kinds of `bases' in their __new__(...) ?
  
    ;; either:
    ;;  1) all supers are subtype of 'py-type   (to create a new metaclass)
    ;;  2) all supers are subtype of 'object (to create new "regular user-level" class)
  
    (flet ((of-type-class (s) (typep s 'class))
           (subclass-of-py-dl-object-p (s) (subtypep s (ltv-find-class 'object)))
           (subclass-of-py-type-p (s)      (subtypep s (ltv-find-class 'py-type))))
    
      (unless (every #'of-type-class supers)
        (py-raise '{TypeError} "Not all superclasses are classes (got: ~A)." supers))

      ;; Handle the case where exceptions are not Python object instances.
      (when (and (not *exceptions-are-python-objects*)
                 (loop for s in supers thereis (subtypep s 'condition)))
        (unless (loop for s in supers always (subtypep s 'condition))
          (py-raise '{TypeError}
                    "Unsupported heterogeneous superclasses: some CONDITION, some not: ~A." supers))
        (return-from make-py-class-1 (make-condition-exception)))
      
      (loop for s in supers
          unless (or (subclass-of-py-type-p s)
                     (subclass-of-py-dl-object-p s))
          do   (error "BUG? Superclass ~A is neither sub of 'type nor sub of 'object~@[~A~]!"
                      s (unless *exceptions-are-python-objects* " not a subtype of 'condition")))
    
      #+(or)
      (let ((core-supers (remove-if-not (lambda (s) (typep s 'py-type)) supers)))
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
        ;; py-meta-type as metaclass.
        (when (or (eq metaclass (ltv-find-class 'py-lisp-type))
                  #+(or)(eq metaclass (ltv-find-class 'py-type)))
          (setf metaclass (ltv-find-class 'py-type)))
        #+(or)(warn "metaclass 2: ~A" metaclass)
        ;; Subclass of `type' has metaclass 'py-meta-type
        (when (eq metaclass (ltv-find-class 'py-meta-type))
          (let ((cls (ensure-class
                      (make-symbol (symbol-name name))
                      :direct-superclasses supers
                      :metaclass (ltv-find-class 'py-meta-type)
                      #+(or):dict #+(or)namespace)))
            (apply-namespace-to-cls namespace cls)
            (return-from make-py-class-1 (values cls :metaclass))))
            
        ;; Not a subclass of `type', so at the `object' level
        (let ((__new__ (class.attr-no-magic metaclass '{__new__})))
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
		
                (let ((__init__ (class.attr-no-magic metaclass '{__init__})))
                  #+(or)(warn "  __init__ method ~A is: ~A" metaclass __init__)
                  (when __init__
                    (py-call __init__ cls)))
	    
              #+(or)(warn "Not calling __init__ method, as class ~A is not instance of metaclass ~A"
                          cls metaclass))
	  
            (values cls :class)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +the-true+ 1)
(defconstant +the-false+ 0)

(defun py-bool (lisp-val)
  (if lisp-val +the-true+ +the-false+))

(define-compiler-macro py-bool (lisp-val)
  `(if ,lisp-val +the-true+ +the-false+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Built-in Python object types:
;;; 
;;; -- type ----------- repr ----- subclassable -- examples -------------------
;;;  py-lisp-object:  lisp value    yes       number, string, tuples, vectors, dict
;;;  object:  py cls inst   no        function, method
;;;  dicted-object:  py cls inst   yes       file, module, property
;;;
				       

;; (defun f (&rest args)
;;   (with-parsed-py-arglist ((a b) args)
;;     (+ a b)))
;;
;; (f 1 2) (f 1 :b 2) (f :a 1 :b 2)

(defmacro with-parsed-py-arglist ((formal-args actual-args) &body body)
  (let ((alist '#:alist))
    `(let* ((,alist (parse-poskey-arglist ',formal-args ,actual-args))
	    ,@(loop for f in formal-args
		  collect `(,f (cdr (assoc ',f ,alist :test #'eq)))))
       ,@body)))
    
(defun parse-poskey-arglist (formal-args actual-args)
  (setf formal-args (copy-list formal-args))
  (let (res)
    ;; handle all actual positional args
    (loop while (and formal-args actual-args (not (symbolp (car actual-args))))
        do (push (cons (pop formal-args) (pop actual-args)) res))
    ;; handle remaining keyword arguments
    (loop while actual-args
        for key = (pop actual-args)
        do (cond ((keywordp key)
                  (let ((formal-keyword-arg (or (find key formal-args :test 'string-equal)
                                                ;; FIXME case issue; (string= |:a| '|a|)
                                                (py-raise '{ValueError} "Invalid keyword argument supplied: ~S." key))))
                    (push (cons formal-keyword-arg (pop actual-args)) res)
                    (setf formal-args (delete formal-keyword-arg formal-args :test 'eq))))
                 (t
                  (py-raise '{ValueError} "Invalid positional arguement supplied: ~S." key))))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Core objects (function, method, None...; not subclassable by the user)
;;;

(def-py-method py-type.__class__ :attribute (x)
  (py-class-of x))

;; py-class-method

(def-py-method py-class-method.__new__ :static (cls func)
  (make-instance cls))

(defun classp (x)
  (checking-reader-conditionals
   #+allegro (excl::classp x)
   #+cmu (pcl::classp x)
   #+lispworks (clos::classp x)
   #+sbcl (sb-pcl::classp x)
   #-(or allegro cmu lispworks sbcl) (typep x 'class)))
  
(def-py-method py-class-method.__get__ (x inst class)
  (let ((arg (if (classp inst) inst (py-class-of inst))))
    (make-instance 'py-bound-method
      :func (slot-value x 'func)
      :instance arg)))

(def-py-method py-class-method.__init__ (x^ func)
  (setf (slot-value x 'func) func))

(def-py-method py-class-method.__call__ (x^ &rest args)
  (break "todo")
  #+(or)(apply #'py-call (py-method-func x) (slot-value x 'instance) args)
  #+(or)
  (let ((arg (if (classp obj) obj (py-class-of obj))))
    (apply #'py-call (py-method-func x) arg args)))


;; py-attribute-method

(def-py-method py-attribute-method.__get__ (x inst class)
  (if (and inst (not (none-p inst)))
      (py-call (slot-value x 'func) inst)
    nil))

(def-py-method py-attribute-method.__set__ (x obj val)
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
  (if (and inst (not (none-p inst)))
      (py-call (slot-value x 'func) inst)
    nil))

(defclass py-bound-method (py-method)
  ((instance :initarg :instance :accessor py-method-instance))
  (:metaclass py-type))

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
  
  (apply #'py-call (x.class-attr-no-magic.bind (py-method-func x) '{__get__}) args))

(def-py-method py-bound-method.__name__ :attribute (x)
  (py-bound-method.__repr__ x))	       
    
    
(defclass py-unbound-method (py-method)
  ((class :initarg :class :accessor py-method-class))
  (:metaclass py-type))

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
  (apply #'py-call (x.class-attr-no-magic.bind (py-method-func x) '{__get__}) args))


;; py-static-method

(def-py-method py-static-method.__new__ :static (cls func)
  (assert (eq cls (ltv-find-class 'py-static-method)))
  (make-instance 'py-static-method :func func))

(def-py-method py-static-method.__get__ (x inst class)
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

(defclass py-lisp-function (object)
  ()
  (:metaclass py-type))

(finalize-inheritance (find-class 'py-lisp-function))

(def-py-method py-lisp-function.__repr__ (func)
  (with-output-to-string (s)
    (print-object func s)))

(def-py-method py-lisp-function.__get__ (func inst cls)
  (assert inst)
  (let ((to-make (cond ((none-p inst)
			(if (and (typep cls 'class)
				 (eq (class-name cls) 'py-none))
			    :bound-method
			  :unbound-method))
		       ((none-p cls)
			(py-raise '{ValueError}
				  "function.__get__(None, None) : invalid args"))
		       (t
			:bound-method))))
    (ecase to-make
      (:bound-method   (make-instance 'py-bound-method :instance inst :func func))
      (:unbound-method (make-instance 'py-unbound-method :class cls :func func)))))

(def-py-method py-lisp-function.__name__ :attribute (func)
  (function-name func))

(defclass funcallable-python-class (closer-mop:funcallable-standard-class py-type)
  ()
  (:metaclass py-meta-type))

(defmethod closer-mop:validate-superclass ((class funcallable-python-class) superclass)
  (declare (ignorable class superclass))
  t)

;; Temporary (?) hack to get things running on SBCL 1.0.16, 
;; where instantiating a py-function leads to strange errors.
;; See <http://common-lisp.net/pipermail/clpython-devel/2008-May/000048.html>
(defparameter *create-simple-lambdas-for-python-functions*
    (checking-reader-conditionals
     #+(or allegro lispworks) nil
     #+sbcl t
     #-(or allegro lispworks sbcl) t)
     "Whether Python function are real CLOS funcallable instances, or just normal lambdas.
Note that in the latter case, functions miss their name and attribute dict, but should
otherwise work well.")

(defstruct (simple-function-data (:conc-name sfd-))
  func name attributes func-globals func-code)

(defparameter *simple-function-data* (make-hash-table :test 'eq)
  "Mapping from function to SIMPLE-FUNCTION-DATA")

(defun register-simple-function (func name func-globals func-code)
  (setf (gethash func *simple-function-data*)
    (make-simple-function-data :func func
                               :name name
                               :attributes (make-hash-table :test 'eq)
                               :func-globals func-globals
                               :func-code func-code)))

(defgeneric py-function-lambda (x)
  ;; XXX this function is also used when *create-simple-lambdas-for-python-functions*
  ;; which is not quite kosher.
  (:method ((x function)) x))

(defclass py-function (standard-generic-function dicted-object)
  ;; mop:funcallable-standard-class defines :name initarg, but how to to access it portably...
  ((fname        :initarg :fname        :initform nil :accessor py-function-name)
   (context-name :initarg :context-name :initform nil :accessor py-function-context-name)
   (lambda       :initarg :lambda       :initform nil :accessor py-function-lambda)
   (func-globals :initarg :func-globals                         :accessor py-function-func-globals)
   (func-code    :initarg :func-code                  :reader   py-function-code))
  (:metaclass funcallable-python-class))

;; XXX On LispWorks this is not guaranteed to work:
;;  http://article.gmane.org/gmane.lisp.lispworks.general/8999
;; Maybe switch to simple lambda's there too?

(defun set-funcallable-instance-function (inst func)
  (closer-mop:set-funcallable-instance-function inst func))

(defgeneric function-name (f)
  (:method ((f function))
           (let ((data (gethash f *simple-function-data*)))
             (when data
               (string (sfd-name data)))))
  (:method ((f py-function)) (string (py-function-name f))))

(defun make-py-function (&key name context-name lambda func-globals func-code)
  (if *create-simple-lambdas-for-python-functions*
      (progn (register-simple-function lambda name func-globals func-code)
             lambda)
    (let ((x (make-instance 'py-function
               :fname (string name)
               :lambda lambda
               :context-name context-name
               :func-globals func-globals
               :func-code func-code)))
      (set-funcallable-instance-function x lambda)
      ;; fill dict?
      x)))

(def-py-method py-function.__get__ (func inst cls)
  (py-lisp-function.__get__ func inst cls))

(def-py-method py-function.__hash__ (func)
  (sxhash func))
  
(def-py-method py-function.__repr__ (func)
  (with-output-to-string (s)
    (if (typep func 'py-function)
        (print-object func s)
      (format s "~A" (or (function-name func) func)))))

(defmethod print-object ((x py-function) stream)
  (print-unreadable-object (x stream :identity t)
    (format stream "python-function ~A~@[ (~A)~]"
            (py-function-name x)
            (unless (compiled-function-p (py-function-lambda x)) "interpreted"))
    (when (string/= (py-function-name x)
                    (py-function-context-name x))
      (format stream " (~A)" (py-function-context-name x)))))

(def-py-method py-function.__name__ :attribute-read (func)
  (or (and (typep func 'py-function)
           (py-function-name func))
      (whereas ((data (gethash func *simple-function-data*)))
        (or (whereas ((ht (sfd-attributes data))
                      (name (gethash '{__name__} ht)))
              (string name))
            (string (sfd-name data))))
      (py-raise '{AttributeError} "Function ~A has no attribute `__name__'." func)))

(def-py-method py-function.__name__ :attribute-write (func name)
  (if (typep func 'py-function)
      (setf (py-function-name func) name)
    (or (whereas ((data (gethash func *simple-function-data*)))
          (setf (sfd-name data) name))
        (py-raise '{AttributeError} "Cannot set attribute `__name__' of ~A." func))))
      
(def-py-method py-function._fif :attribute (x)
  "The funcallable instance function of X."
  ;; CLPython-specific.
  (when (typep x 'py-function)
    (setf x (py-function-lambda x)))
  (assert (functionp x))
  x)

(def-py-method py-function.func_globals :attribute-read (x)
  (etypecase x
    (py-function (py-function-func-globals x))
    (function (sfd-func-globals (or (gethash x *simple-function-data*)
                                    (error "No function metadata available"))))))

(def-py-method py-function.__setattr__ (func attr val)
  (when (stringp attr)
    (setf attr (ensure-user-symbol attr)))
  (check-type attr symbol)
  (when (eq attr '{__dict__})
    (when (typep val 'symbol-hash-table)
      (setf val (sht-ht val)))
    (check-type val hash-table) ;; XXX or custom ht
    (let ((ht (make-eq-hash-table)))
      (loop for key being the hash-key in val
          using (hash-value value)
          for key.sym = (typecase key
                          (symbol key)
                          (string (ensure-user-symbol key))
                          (t (py-raise '{TypeError} "Invalid key of type ~A in replacement dict (key: ~A)."
                                       (type-of key) key)))
          do (setf (gethash key.sym ht) value))
      (etypecase func
        (py-function (setf (dict func) ht))
        (function (let ((data (gethash func *simple-function-data*)))
                    (setf (sfd-attributes data) ht)))))
    (return-from py-function.__setattr__))
  (etypecase func
    (py-function (progn (let ((meth (funky-dict-get (dict (find-class 'py-function)) attr)))
                          (when (typep meth 'py-writable-attribute-method)
                            (return-from py-function.__setattr__
                              (py-writable-attribute-method.__set__ meth func val))))
                        (let ((d (or (dict func) (setf (dict func) (make-eq-hash-table)))))
                          (setf (gethash attr d) val))))
    (function (let ((ht (whereas ((data (gethash func *simple-function-data*)))
                          (or (sfd-attributes data)
                              (setf (sfd-attributes data) (make-eq-hash-table))))))
                (unless ht
                  (py-raise '{AttributeError} "Cannot set attribute on function ~A." func))
                (setf (gethash attr ht) val)))))

(def-py-method py-function.__getattribute__ (func attr)
  (when (stringp attr)
    (setf attr (ensure-user-symbol attr)))
  (check-type attr symbol)
  (whereas ((val (funky-dict-get (dict (find-class 'py-function)) attr)))
    (return-from py-function.__getattribute__
      (bind-val val func (find-class 'py-function))))
  (or (etypecase func
        (py-function (whereas ((d (or (dict func)
                                      (and (eq attr '{__dict__})
                                           (setf (dict func) (make-eq-hash-table))))))
                       (if (eq attr '{__dict__})
                           (make-symbol-hash-table d)
                         (values (gethash attr d)))))
        (function (whereas ((data (gethash func *simple-function-data*))
                            (ht (or (sfd-attributes data)
                                    (and (eq attr '{__dict__})
                                         (setf (sfd-attributes data) (make-eq-hash-table))))))
                    (if (eq attr '{__dict__})
                        (make-symbol-hash-table ht)
                      (values (gethash attr ht))))))
      (py-raise '{AttributeError} "Function ~A has no attribute `~A'." func attr)))

(def-py-method py-function.__delattr__ (func attr)
  (when (stringp attr)
    (setf attr (ensure-user-symbol attr)))
  (check-type attr symbol)
  (assert (not (eq attr '{__dict__})) () "Error: todo: function delattr of __dict__")
  (let ((ok (etypecase func
              (py-function (whereas ((d (dict func)))
                             (setf (py-subs d attr) nil)))
              (function (whereas ((data (gethash func *simple-function-data*))
                                  (ht (sfd-attributes data)))
                          (remhash attr ht))))))
    (unless ok
      (py-raise '{AttributeError} "Function ~A has no attribute `~A' to delete." func attr))))

(defclass func-code (clpython:object)
  ((name      :type string                :initarg :name      :reader func-code.name      :initform "")
   (arg-count :type (integer 0)           :initarg :arg-count :reader func-code.arg-count :initform 0)
   (nlocals   :type (integer 0)           :initarg :nlocals   :reader func-code.nlocals   :initform 0)
   (varnames  :type #1=(or py-tuple list) :initarg :varnames  :reader func-code.varnames  :initform *the-empty-tuple*)
   (cellvars  :type #1#                   :initarg :cellvars  :reader func-code.cellvars  :initform *the-empty-tuple*)
   (freevars  :type #1#                   :initarg :freevars  :reader func-code.freevars  :initform *the-empty-tuple*)
   (code      :type string                :initarg :code      :reader func-code.code      :initform "")
   (consts    :type #1#                   :initarg :consts    :reader func-code.consts    :initform *the-empty-tuple*)
   (names     :type #1#                   :initarg :names     :reader func-code.names     :initform *the-empty-tuple*)
   (filename  :type string                :initarg :filename  :reader func-code.filename  :initform "")
   (lnotab    :type string                :initarg :lnotab    :reader func-code.lnotab    :initform "")
   (stacksize :type (integer 0)           :initarg :stacksize :reader func-code.stacksize :initform 0)
   (flags     :type (integer 0)           :initarg :flags     :reader func-code.flags     :initform 0))
  (:metaclass clpython:py-type))
  
(def-py-method py-function.func_code :attribute (x)
  "Read-only attribute: the underlying lambda. (In CPython the bytecode vector.)"
  (etypecase x
    (py-function (py-function-code x))
    (function (sfd-func-code (or (gethash x *simple-function-data*)
                                 (error "No function metadata available"))))))

(progn (def-py-method func-code.co_name      :attribute (x) (func-code.name x))
       (def-py-method func-code.co_argcount  :attribute (x) (func-code.arg-count x))
       (def-py-method func-code.co_nlocals   :attribute (x) (func-code.nlocals x))
       (def-py-method func-code.co_varnames  :attribute (x) (func-code.varnames x))
       (def-py-method func-code.co_cellvars  :attribute (x) (func-code.cellvars x))
       (def-py-method func-code.co_freevars  :attribute (x) (func-code.freevars x))
       (def-py-method func-code.co_code      :attribute (x) (func-code.code x))
       (def-py-method func-code.co_consts    :attribute (x) (func-code.consts x))
       (def-py-method func-code.co_names     :attribute (x) (func-code.names x))
       (def-py-method func-code.co_filename  :attribute (x) (func-code.filename x))
       (def-py-method func-code.co_lnotab    :attribute (x) (func-code.lnotab x))
       (def-py-method func-code.co_stacksize :attribute (x) (func-code.stacksize x))
       (def-py-method func-code.co_flags     :attribute (x) (func-code.flags x)))

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

#+clpython-source-level-debugging
(def-py-method py-function._src :attribute (x)
  (when (typep x 'py-function)
    (setf x (py-function-lambda x)))
  (when (excl::closurep x) ;; needed?
    (setq x (excl::cl_shared x)))
  (let ((*print-level* 2))
    (excl::dump-lisp-source x)))

(defmethod py-function-name ((x function))
  (declare (ignorable x))
  #+allegro (format nil "~A" (excl::func_name x))
  #-allegro (call-next-method))

(defmethod py-function-name ((x t))
  ;; fall-back
  (format nil "~A" x))

(def-py-method py-function.__call__ (func &rest args)
  (apply func args))

(defun function-arglist (f)
  (check-type f function)
  #+allegro (excl:arglist f)
  #-allegro nil)

(def-py-method py-function.__doc__ :attribute-read (func^)
  (let ((fname (py-function-name func))
        (arglist (function-arglist func))
        (documentation (documentation func 'function)))
    (with-output-to-string (s)
      (when fname (write-string fname s))
      (format s "~A" (or arglist "(unknown args)"))
      (when (and documentation (or fname arglist))
        (terpri s))
      (when documentation (write-string documentation s)))))

(def-py-method py-function.__doc__ :attribute-write (func^ doc)
  (setf (documentation func 'function) doc))

;; Enumerate (core object)

(defclass py-enumerate (object)
  ((gener :initarg :gener))
  (:metaclass py-type))

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


(defclass py-slice (object)
  ((start :initarg :start :accessor slice-start)
   (stop  :initarg :stop  :accessor slice-stop)
   (step  :initarg :step  :accessor slice-step))
  (:metaclass py-type))

(defmethod print-object ((x py-slice) stream)
  (print-unreadable-object (x stream)
    (with-slots (start stop step) x
      (format stream ":start ~A :stop ~A :step ~A"
              start stop step))))
            
(defun make-slice (start stop step)
  (make-instance 'py-slice
    :start (or start (load-time-value *the-none*))
    :stop  (or stop  (load-time-value *the-none*))
    :step  (or step  (load-time-value *the-none*))))

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

(defun slice-indices (x length)
  "Return three integers: START, STOP, STEP.
START and END are _inclusive_, absolute indices >= 0. STEP is != 0."
  (check-type x py-slice)
  (check-type length integer)
  (let* ((start      (or (slice-start x) (load-time-value *the-none*)))
	 (stop       (or (slice-stop x)  (load-time-value *the-none*)))
	 (step       (or (slice-step x)  (load-time-value *the-none*)))
	 reversed-p)
    (setf step  (if (none-p step) 1 (py-val->integer step))
	  reversed-p (minusp step)
	  start (if (none-p start)
		    (if reversed-p (1- length) 0)
		  (py-val->integer start))
	  stop  (if (none-p stop)
		    (if reversed-p -1 length)
		  (py-val->integer stop)))
    (assert (every #'integerp (list start stop step)))
    (when (minusp start) (incf start length))
    (when (minusp stop) (unless (and reversed-p (= stop -1)) ;; XXX right?
                          (incf stop length)))
    (cond ((= step 0)
	   (py-raise '{ValueError} "Slice step cannot be zero (got: ~S)." x))
          ((or (and (plusp step) (or (> start length)
                                     (minusp stop)
                                     (> start stop)))
	       (and (minusp step) (or (minusp start)
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
	   (cond ((= start stop) 
                  (values :empty-slice-bogus))
                 ((and (plusp step) (< start stop))
		  (let* ((start (max start 0))
			 (stop  (min (1- stop) (1- length)))
			 (num-increments (floor (- stop start) step))
			 (real-stop (+ start (* step num-increments))))
                    (assert (<= 0 start real-stop stop (1- length)) ()
		      "not (<= 0 start real-stop stop (1- length)): ~A"
		      `(<= 0 ,start ,real-stop ,stop (1- ,length)))
                    (values :nonempty-stepped-slice
			    start real-stop step (1+ num-increments))))
                 ((and (minusp step) (> start stop))
		  (let* ((start (min start (1- length)))
			 (stop  (max (incf stop) 0))
			 (num-increments (floor (- stop start) step))
			 (real-stop (- start (* (- step) num-increments))))
                    (assert (<= 0 stop real-stop start (1- length)) ()
		      "not (<= 0 stop real-stop start (1- length)): ~A"
		      `(<= 0 ,stop ,real-stop ,start (1- ,length)))
                    (values :nonempty-stepped-slice
			    start real-stop step (1+ num-increments))))
                 (t (break "unexpected")))))))

;; super( <B class>, <C instance> ) where C derives from B:
;;   :object = <C instance>
;;   :current-class = <B class>
;; 
;; A typical use for calling a cooperative superclass method is:
;; 
;;  class C(B):
;;    def meth(self, arg):
;;      super(C, self).meth(arg)

(defclass py-super (object) ;; subclassable?
  ((object        :initarg :object)
   (current-class :initarg :current-class))
  (:metaclass py-type))

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
	     (val (loop for cls in remaining-mro
                      thereis (class.raw-attr-get cls attr.sym))))
	
	(if val
	    (bind-val val
		      (if (eq object class) (load-time-value *the-none*) object)
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

(defclass py-xrange (object)
  (start stop step)
  (:metaclass py-type))

(finalize-inheritance (find-class 'py-xrange))

(def-py-method py-xrange.__new__ :static (cls &rest args)
  (or args) ;; XX fix warning
  (make-instance cls))
                                  
(def-py-method py-xrange.__init__ (x &rest args)
  (multiple-value-bind (start stop step)
      (ecase (length args)
        (0 (py-raise '{TypeError} "xrange: >= 1 arg needed"))
        (1 (unless (>= (car args) 0)
             (break "xrange: invalid 1-arg: ~A" (car args)))
           (values 0 (car args) 1))
        (2 (destructuring-bind (start end) args
             (if (< start end)
                 (values start end 1)
               (values 0 0 1))))
        (3 (destructuring-bind (start end step) args
             (if (or (and (< start end) (plusp step))
                     (and (> start end) (minusp step)))
                 (values start end step)
               (values 0 0 1)))))
    (setf (slot-value x 'start) start
          (slot-value x 'stop) stop
          (slot-value x 'step) step))
  (load-time-value *the-none*))

(defun xrange-iter-func (x)
  (with-slots (start stop step) x
    (let ((i start))
      (lambda ()
        (unless (or (and (<= start stop) (>= i stop))
                    (and (<= stop start) (<= i stop)))
          (prog1 i
            (incf i step)))))))

(def-py-method py-xrange.__iter__ (x)
  (make-iterator-from-function :name :xrange-iterator
                               :func (xrange-iter-func x)))

(def-py-method py-xrange.__str__ (x^)
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t :type t)
      (with-slots (start stop step) x
	(format s ":start ~A :stop ~A :step ~A" start stop step)))))

;; None

(def-py-method py-none.__hash__ (x)
  12345)

(def-py-method py-none.__repr__ (x)
  "None")

(def-py-method py-none.__nonzero__ (x)
  +the-false+)

;; Ellipsis

(defclass py-ellipsis (object) () (:metaclass py-type))
(defvar *the-ellipsis* (make-instance 'py-ellipsis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User objects (Object, Module, File, Property)

;; Object (User object)

;; ACL 7.0 has an issue with compiler-macro + notinline.
#+(or)
;;#+(and allegro-version>= (version>= 8 0))
(define-compiler-macro bind-val (val x x.class)
  `(locally (declare (notinline bind-val))
     (let ((.val ,val)
	   (.x ,x))
       (if (and (functionp .val)
		(not (none-p .x)))
	   (progn 
	     #+(or)(warn "bind-val ~S ~S -> bound method" val x)
	     (make-instance 'py-bound-method :instance .x :func .val))
	 (bind-val .val .x ,x.class)))))

(def-py-method object.__new__ :static (cls &rest attr)
  (make-instance cls))

(def-py-method object.__init__ (&rest attr)
  )

(def-py-method object.__class__ :attribute-read (x)
  (py-class-of x))

(def-py-method object.__class__ :attribute-write (x new-class)
  (assert (classp new-class))
  (change-class x new-class))

(def-py-method object.__str__ (x)
  (py-repr x))

(def-py-method object.__repr__ (x)
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t :type nil)
      (format s "~A" (type-of x)))))

(defvar *print-object-does-repr* t)
(defvar *print-object-with-repr-show-errors* nil)
(defvar *print-object-with-repr-error-level* 0)

(defmethod print-object ((x object) stream)
  (multiple-value-bind (result error)
      (when *print-object-does-repr*
        (ignore-errors (values (py-repr x))))
    (when result
      (if (stringp result)
          (write-string result stream)
        (print-unreadable-object (x stream)
          (format stream "Unprintable object of type ~A ~
                            [repr() gave non-string result of type ~A]"
                  (class-name (class-of x)) (class-name (class-of result)))))
      (return-from print-object))
    (let ((*print-object-does-repr* nil))
      (print-unreadable-object (x stream)
        (format stream "Unprintable object of type ~A" (class-name (class-of x)))
        (when (and error *print-object-with-repr-show-errors*)
          (let ((*print-object-with-repr-error-level* (1+ *print-object-with-repr-error-level*)))
            (format stream " -- repr() gave [~A]"
                    ;; Skip error details at lower levels, especially useful when __repr__ faulty.
                    (if (<= *print-object-with-repr-error-level* 1)
                        error
                      (class-name (class-of error))))))))))

(def-py-method object.__nonzero__ (x)
  "Objects are nonzero ('true') by default."
  (py-bool t))

(def-py-method object.__get__ (value instance class)
  value)

;; Type (User object)

(def-py-method py-type.__new__ :static (metacls name supers^ dict)
  "Create a new type with the given metaclass, name, superclasses and attributes.
Basically the Python equivalent of ENSURE-CLASS."
  ;; deproxy supers, as it may be *the-empty-tuple* instance
  (unless (symbolp name)
    (let ((str (deproxy name)))
      (if (stringp str)
	  (setf name (ensure-user-symbol str))
	(py-raise '{TypeError} "Invalid class name: ~A" name))))
  
  (let* ((cls-type (if (and (some (lambda (s) (subtypep s (ltv-find-class 'py-type)))
                                  supers)
			    (eq metacls 'py-type)) ;; XXX (subtypep meta pytype)?
		       :metaclass
		     :class))
         (c (ensure-class
	     (make-symbol (symbol-name name)) 
	     
	     :direct-superclasses (if supers
				      (let ((res (subst (ltv-find-class 'dicted-object)
							(ltv-find-class 'object)
							supers)))
					(unless (member (ltv-find-class 'dicted-object) res :test #'eq)
					  (nconc res (load-time-value (list (find-class 'dicted-object)))))
					res)
				    (load-time-value (list (find-class 'dicted-object))))
	     
	     :metaclass (ecase cls-type
			  (:metaclass (ltv-find-class 'py-meta-type))
			  (:class     metacls))
             #+(or) :dict #+(or) dict)))
    (apply-namespace-to-cls dict c)
    (finalize-inheritance c)
    c))

(def-py-method py-type.__init__ (cls &rest args)
  nil)

(def-py-method py-type.__name__ :attribute (cls)
  ;; XXX remove prefix `py-' etc
  (symbol-name (class-name cls)))

(def-py-method py-type.__nonzero__ (cls)
  ;; to make e.g. "if str: ..." work
  (declare (ignore cls))
  +the-true+)

(def-py-method py-type.__dict__ :attribute-read (cls)
  (make-instance 'funky-dict-wrapper
    :getter (lambda () (dict cls))
    :setter (lambda (new) (setf (dict cls) new))))

(def-py-method py-type.__dict__ :attribute-write (cls new-dict)
  ;; XXX check NEW-DICT is of (sub)type DICT.
  (setf (dict cls) new-dict))

(def-py-method py-type.__bases__ :attribute-read (cls)
  (assert (classp cls))
  (let* ((supers (class-direct-superclasses cls)))
    (assert supers () "Class ~A has no direct superclasses?!" cls)
    (make-tuple-from-list (remove (ltv-find-class 'dicted-object) supers))))

(def-py-method py-type.__bases__ :attribute-write (cls bases^)
  (assert (classp cls))
  (let ((bases (py-iterate->lisp-list bases)))
    (ensure-class-using-class cls (class-name cls) 
                              :direct-superclasses bases)
    cls))

(def-py-method py-type.__hash__ (x)
  (sxhash x))

(defvar *mro-filter-implementation-classes* t
  "Whether to filter reasonable intermediate classes from the returned MRO.
For example, `py-int' derives in this implementation from `py-real' and `py-number',
but the latter two classes are not in CPython.")

(def-py-method py-type.__mro__ :class-attribute (x)
  ;; We could filter out the Lisp implementation classes.
  (make-tuple-from-list
   (loop for cls in (closer-mop:class-precedence-list x)
       until (or (eq cls (ltv-find-class 'standard-class))
		 (eq cls (ltv-find-class 'standard-object)))
       unless (member (class-name cls) '(dicted-object
                                         py-lisp-object
                                         object
                                         dict-mixin
                                         t))
       unless (and *mro-filter-implementation-classes*
                   (member (class-name cls) '(py-real py-number)))
       collect cls into res
       finally (let ((base-cls (if (subtypep x 'py-meta-type)
				   'py-type
				 'object)))
		 (unless (member base-cls res :key #'class-name)
		   (nconc (last res) (list (find-class base-cls))))
		 (return res)))))

(defun class-direct-subclasses (c)
  (closer-mop:class-direct-subclasses c))

(def-py-method py-type.__subclasses__ (x)
  (make-py-list-from-list (class-direct-subclasses x)))

(defvar *class-display-without-py-prefix* t
  "Whether to show class names without `py-' prefix: `int' instead of `py-int' etc.")

(defmethod print-object ((x py-type) stream)
  (write-string (py-type.__repr__ x) stream))

(def-py-method py-type.__repr__ (x) ;; XXX deproxy not needed
  (check-type x class)
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
                name)
      (when (symbol-package (class-name x))
        (let ((*package* #.(find-package :common-lisp)))
          (format s " (~S)" (class-name x))))))))

(def-py-method py-type.__str__ (x) ;; XXX deproxy not needed
  (py-type.__repr__ x))

(defun class-finalizer-p (cls)
  ;; Could perhaps be optimized by keeping track of whether any class has defined __del__. 
  (not (null (class.attr-no-magic cls '{__del__}))))

(defun generic-finalizer (x)
  (whereas ((__del__ (x.class-attr-no-magic.bind x '{__del__})))
    (py-call __del__)))
  
(defun maybe-add-finalizer (x)
  (let ((cls (class-of x)))
    (when (and (typep cls (ltv-find-class 'py-type))
               (class-finalizer-p cls))
      (schedule-finalization x #'generic-finalizer)
      t)))

(def-py-method py-type.__call__ (cls &rest args)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           #+(or)(dynamic-extent args)) ;; XXX this declaration ends up in the wrong place
  (cond ((and (eq cls (ltv-find-class 'py-type))
	      args
	      (not (cdr args)))
         ;; type(x) -> the type of x
	 (return-from py-type.__call__
	   (py-class-of (car args))))
	
	((eq cls (ltv-find-class 'object))
         ;; object() -> an instance without __dict__
	 (return-from py-type.__call__
	   (make-instance 'object)))
	
	((eq cls (ltv-find-class 'py-type))
         ;; Inline common case: creating new classes with TYPE as requested metaclass
	 (apply #'py-type.__new__ cls args))
        
        (t
         ;; Object instantiation: x = C(..)
         (let ((__new__ (class.attr-no-magic cls '{__new__})))
           (unless __new__
             (break "Class ~A lacks __new__ method." cls))
           #+(or)(warn "__new__: ~A" __new__)
           (let* ((bound-new (if (eq (class-of __new__) (ltv-find-class 'py-static-method))
                                 __new__ ;; inline common case
                               (bind-val __new__ cls (py-class-of cls))))
                  (inst    (apply #'py-call bound-new cls args))) ;; including CLS as arg!
             
             (when (or (eq (class-of inst) cls) ;; <- Efficiency optimization
                       (subtypep (py-class-of inst) cls)) ;; <- real test
               ;; Don't run __init__ when inst is not of type cls
               (let ((__init__-unbound (class.attr-no-magic cls '{__init__})))
                 ;;(format t "__init__-unbound: ~A~%" __init__-unbound)
                 (if (functionp __init__-unbound)
                     (apply __init__-unbound inst args) ;; inline common case
                   (let ((__init__ (x.class-attr-no-magic.bind inst '{__init__})))
                     (apply #'py-call __init__ args)))))
             inst)))))

;;; Module

(defclass module (dicted-object)
  ((namespace-ht   :initarg :namespace-ht :accessor module-ht :initform (make-eq-hash-table))
   (name           :initarg :name           
		   :type string
		   :initform (error "module name required")
		   :accessor module-name
		   :documentation "The (dotted) module name")
   (builtinp       :initarg :builtin   :initform nil :accessor module-builtin-p)
    ;; PACKAGEP <-> a Python package, i.e. a directory with __init__.py file
   (packagep       :initarg :package                 :accessor module-package-p)
   ;; Keep track of original files
   (src-pathname   :initarg :src-pathname :initform nil :accessor module-src-pathname
                   :type (or pathname null))
   (bin-pathname   :initarg :bin-pathname :initform nil :accessor module-bin-pathname
                   :type (or pathname null))
   (src-file-write-date :initarg :src-file-write-date :initform nil :accessor module-src-file-write-date)
   (bin-file-write-date :initarg :bin-file-write-date :initform nil :accessor module-bin-file-write-date))
  (:metaclass py-type))

(defun init-module-namespace (module-globals module-name)
  ;; Called by compiler
  ;; should dispatch on namespace type?
  (etypecase module-globals
    (hash-table (setf (gethash '{__name__} module-globals) module-name
                      (gethash '{__debug__} module-globals) +the-true+))
    (package (setf (symbol-value (intern (symbol-name '{__name__}) module-globals)) module-name
                   (symbol-value (intern (symbol-name '{__debug__}) module-globals)) +the-true+))))

;;; Keep a registry of all loaded modules.

(defvar *all-modules* (make-weak-key-hash-table :test 'eq))

(defmethod initialize-instance :after ((m module) &rest args)
  (declare (ignore args))
  (with-slots (name src-pathname src-file-write-date bin-pathname bin-file-write-date)
      m
    (when (and src-pathname (not src-file-write-date))
      (if (careful-probe-file src-pathname)
          (setf src-file-write-date (file-write-date src-pathname))
        (warn "Cannot set module SRC-FILE-WRITE-DATE: probe-file ~S failed, for module ~A"
              src-pathname m)))
    (when (and bin-pathname (not bin-file-write-date))
      (unless (string-equal (namestring bin-pathname) "__main__") ;; XXX ugly
        (assert (careful-probe-file bin-pathname))
        (setf bin-file-write-date (file-write-date bin-pathname))))
    (when (string= name "__main__")
      (setf name (pathname-name (or src-pathname bin-pathname)))))
  (with-slots (src-pathname bin-pathname packagep) m
      (cond (src-pathname
             (locally
                 (declare (special *package-indicator-filename*))
               (setf packagep (equal (pathname-name src-pathname :case :common) *package-indicator-filename*))))
            ((equal bin-pathname #P"__main__")
             ;; REPL
             (setf packagep nil))
            (t
             (break "Can't determine :packagep for ~S: no src-pathname" m))))
  (check-type (module-ht m) hash-table) ;; XXX or custom ht
  (setf (gethash m *all-modules*) t))

(defun find-module-fuzzy (x)
  (etypecase x
    (module
     x)
    (string
     (find-module-fuzzy (pathname x)))
    (pathname
     (or (find-module :src-pathname x)
         (find-module :bin-pathname x)))))

(defun find-module (&key src-pathname bin-pathname)
  (loop for m being the hash-key in *all-modules*
      when (or (and src-pathname (equalp (module-src-pathname m) src-pathname))
               (and bin-pathname (equalp (module-bin-pathname m) bin-pathname)))
      return m))

(defun ensure-module (&rest args)
  "Returns MODULE, NEW-P"
  (or (apply #'find-module :allow-other-keys t args)
      (values (apply #'make-instance 'module args) t)))

(defmethod make-load-form ((m module) &optional environment)
  (declare (ignore environment))
  `(ensure-module :name ',(module-name m)
                  :src-pathname ,(module-src-pathname m)
                  :bin-pathname ,(module-bin-pathname m)
                  :packagep ,(module-package-p m)))


(def-py-method module.__repr__ (x^)
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t)
      (with-slots (name builtinp src-pathname bin-pathname src-file-write-date bin-file-write-date) x
        (if builtinp
            (format s "builtin module `~A'" name)
          (format s "module `~A'~_ :src ~S~_ :binary ~S~_ :src-time ~A :bin-time ~A"
                  name src-pathname bin-pathname
                  src-file-write-date bin-file-write-date))))))

(def-py-method module.path (x)
  (string (or (module-src-pathname x) (module-bin-pathname x) "<unknown source location>")))

(defgeneric dir-items (item &rest args))

(defmethod dir-items ((x package) &key use-all)
  (declare (ignore use-all))
  (loop for s being each external-symbol in x
      for value = (bound-in-some-way s)
      when value
      collect (cons (symbol-name s) value)))

(defmethod dir-items ((x module) &key (use-all t))
  (whereas ((all (and use-all (instance.attr-no-magic x '{__all__}))))
    (return-from dir-items
      (loop for k in (py-iterate->lisp-list all)
          collect (cons k (attr x (ensure-user-symbol k))))))
  (loop for key being each hash-key in (module-ht x)
      using (hash-value val)
      collect (cons (string key) val)))

(defmethod dir-items ((x class) &rest args)
  (declare (ignore args))
  (let (res)
    (do-cpl (c x)
      (class.raw-attr-map c (lambda (k v)
                              (pushnew (cons (symbol-name k) v) res 
                                       ;; take the first entry: subclass overrides superclass
                                       :test 'eq :key 'car))))
    res))

(defmethod dir-items (x &rest args)
  (nconc (when (dict x)
           (let (res)
             (funky-dict-map (dict x) (lambda (k v) (push (cons (symbol-name k) v) res)))
             res))
         (apply #'dir-items (py-class-of x) args)))

(defun copy-module-contents (&key from to)
  (check-type from module)
  (check-type to module)
  (loop for k being the hash-key in (module-ht from) using (hash-value v)
      do (setf (gethash k (module-ht to)) v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Detecting names and values of built-ins

(defun builtin-name-p (x)
  (find-symbol (string x) (load-time-value (find-package :clpython.user.builtin))))

(defun builtin-value (x)
  (whereas ((builtin-sym (builtin-name-p x)))
    (bound-in-some-way builtin-sym)))

;;;

(def-py-method module.__getattribute__ (x^ attr)
  (let ((attr.sym (py-string-val->symbol attr)))
    (or (gethash attr.sym (module-ht x))
        ;; should be coupled to the namespace
        (builtin-value attr)
        (py-import (list attr.sym)
                   :within-mod-name (module-name x)
                   :within-mod-path (module-src-pathname x)
                   :if-not-found-value nil)
        (py-raise '{AttributeError} "Module ~A~_ has no attribute `~A'." x attr))))

(def-py-method module.__setattr__ (x^ attr val)
  (when (slot-value x 'builtinp)
    (warn "Setting attribute '~A' on built-in module ~A." attr x))
  (setf (gethash (py-string-val->symbol attr) (module-ht x)) val))

(def-py-method module.__delattr__ (x^ attr)
  (when (slot-value x 'builtinp)
    (warn "Deleting attribute '~A' on built-in module ~A." attr x))
  (unless (remhash (py-string-val->symbol attr) (module-ht x))
    (py-raise '{AttributeError} "Module ~A has no attribute ~A (to delete)." x attr))
  (load-time-value *the-none*))

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

(defconstant-once +impl-statuses+ '((t           . "complete")
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

(defclass lisp-package (object)
  ()
  (:metaclass py-type))

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

(def-py-method lisp-package.__repr__ (pkg)
  (with-output-to-string (s)
    (print-unreadable-object (pkg s :identity t :type nil)
      (format s "module ~A (package ~S with ~A external symbols)"
              (relative-package-name pkg)
              (package-name pkg)
              (loop for s being the external-symbol in pkg count s)))))

(defun bind-in-some-way (sym value)
  (check-type sym symbol)
  (typecase value
    (function (setf (symbol-function sym) value))
    (class    (setf (find-class sym) value))
    (t        (setf (symbol-value sym) value))))

(defun bound-in-some-way (sym)
  (check-type sym symbol)
  (cond ((boundp sym)  (symbol-value sym))
        ((fboundp sym) (symbol-function sym))
        ((find-class sym nil))
        (t nil)))

(defun find-symbol-value (symbol pkg-designator)
  (let* ((pkg (or (find-package pkg-designator)
                  (error "No such package: ~A." pkg-designator)))
         (sym (or (find-symbol (symbol-name symbol) pkg)
                  (error "Symbol ~S not found in package ~A." symbol pkg))))
    (or (bound-in-some-way sym)
        (error "Symbol ~A not bound in any way." sym))))

(def-py-method lisp-package.__setattr__ (pkg name new-val)
  ;; Only allowed when symbol denotes writable attribute.
  (let ((cur-val (lisp-package.__getattribute__ pkg name :writable-attr-ok t)))
    (if (typep cur-val 'py-writable-attribute-method)
        (funcall (attribute-writer cur-val) new-val)
      (py-raise '{AttributeError} "Attribute `~A' of package ~A is not writable." name pkg))))
         
(def-py-method lisp-package.__getattribute__ (pkg name &key writable-attr-ok)
  (declare (special *inside-import-from-stmt*))
  (check-type name string)
  (when (string= name "__dict__")
    (return-from lisp-package.__getattribute__
      (lisp-package.__dict__ pkg)))
  
  (flet ((todo-error ()
           (if writable-attr-ok
               (return-from lisp-package.__getattribute__ nil)
             (py-raise '{NotImplementedError} 
                       "Attribute `~A' of module `~A' is not implemented yet."
                       name (relative-package-name pkg))))
         
         (n/a-error ()
           (if writable-attr-ok
               (return-from lisp-package.__getattribute__ nil)
             (py-raise '{NotImplementedError}
                       "Attribute `~A' of module `~A' is not applicable for this ~
 implementation, therefore it is not implemented."
                       name (relative-package-name pkg))))
         
         (unbound-error ()
           (if writable-attr-ok
               (return-from lisp-package.__getattribute__ nil)
             (py-raise (if *inside-import-from-stmt* '{ImportError} '{AttributeError})
                       "Attribute `~A' of module `~A' is unbound."
                       name (relative-package-name pkg))))
         
         (no-attr-error ()
           (if writable-attr-ok
               (return-from lisp-package.__getattribute__ nil)
             (py-raise (if *inside-import-from-stmt* '{ImportError} '{AttributeError})
                       "Module `~A' (Lisp package ~A) has no attribute `~A'."
                       (relative-package-name pkg) pkg name)))

         (incomplete-warning ()
           (unless writable-attr-ok
             (warn "Attribute `~A' of module `~A' was accessed. ~
 The implementation of this attribute is still incomplete, therefore ~
 unexpected results may occur."
                   name (relative-package-name pkg)))))
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
        (if (and (not writable-attr-ok)
                 (typep val 'py-writable-attribute-method))
            (funcall (py-method-func val))
          val)))))

(def-py-method lisp-package.__dict__ (pkg)
  ;; TODO: keep package and dict in sync
  (check-type pkg package)
  (loop with dict = (make-py-hash-table)
      for s being each external-symbol in pkg
      for skipped = (member (impl-status s) '(:todo :n/a))
      count skipped into num-skipped
      unless skipped
      do (setf (py-subs dict (symbol-name s))
           (lisp-package.__getattribute__ pkg (symbol-name s)))
      finally
        (when (plusp num-skipped)
          (break "Module (Lisp package) ~S has some attributes with :TODO or :N/A status, excluded from returned __dict__."
                pkg))
        (return dict)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property (User object)

(defclass py-property (dicted-object)
  (fget fset fdel doc)
  (:metaclass py-type))

;; standard __new__ behaviour

(def-py-method py-property.__init__ (x &rest args)
  (with-parsed-py-arglist ((fget fset fdel doc) args)
    (setf (slot-value x 'fget) (or fget (load-time-value *the-none*))
	  (slot-value x 'fset) (or fset (load-time-value *the-none*))
	  (slot-value x 'fdel) (or fdel (load-time-value *the-none*))
	  (slot-value x 'doc)  (or doc  ""))))

(def-py-method py-property.__get__ (x obj class)
  (with-slots (fget) x
    (if (none-p fget)
	(py-raise '{AttributeError} "Cannot get attribute")
      (py-call fget obj))))

(def-py-method py-property.__set__ (x obj val)
  (with-slots (fset) x
    (if (none-p fset)
	(py-raise '{AttributeError} "Cannot set attribute")
      (py-call (slot-value x 'fset) obj val)))) ;; bind?

(def-py-method py-property.__del__ (x obj)
  (with-slots (fdel) x
    (if (none-p fdel)
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
	
	(t (break "unexpected"))))

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
      (load-time-value *the-notimplemented*))))

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
	   (error "unexpected")) ;; due to CL simplification rules
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

(def-py-method py-int.__new__ :static (cls &optional (arg 0) (base 0 base-provided))
  ;; If base = 0, then derive base from literal ARG, or use base = 10.
  (labels ((invalid-arg-error (a)
             (py-raise '{TypeError} "Invalid arg for int.__new__: ~S ~@[with base ~S~]."
                       a (when base-provided base)))
           (coerce-arg ()
             (typecase arg
               (integer (return-from coerce-arg arg))
               (float   (return-from coerce-arg (truncate arg))))
             (let ((coerce-meth (x.class-attr-no-magic.bind arg '{__int__})))
               (when coerce-meth
                 (return-from coerce-arg (py-call coerce-meth))))
             (setf arg (py-val->string arg)
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
                      (when (and base-provided (/= base 16))
                        (invalid-arg-error arg))
                      (read-arg (subseq arg 2) 16))
                     
                     ((and (= (length arg) 1))
                      (or (digit-char-p (aref arg 0) 10)
                          (invalid-arg-error arg)))
                     
                     ((and (>= (length arg) 1)
                           (char= (aref arg 0) #\0)
                           (zerop base))
                      (read-arg (subseq arg 1) 8))
                     
                     ((= base 0)
                      (read-arg arg 10))
                     
                     ((/= base 0)
                      (check-type base (integer 2 36))
                      (read-arg arg base))))))
    (let ((val (coerce-arg)))
      (if (eq cls (ltv-find-class 'py-int))
	  val
	(make-instance cls :lisp-object val)))))

(def-py-method py-int.__init__ (&rest args) nil)

(defun careful-floor-1ret (x y)
  "ANSI requires second arg to be non-zero. Test for that."
  (when (zerop y)
    (restart-case (py-raise '{ZeroDivisionError} "Attempt to divide ~A by zero." x)
      (use-another-divisor (new-divisor)
	  :report "Use another divisor instead of zero"
	  :interactive (lambda ()
			 (loop with divisor = 0
			     while (zerop divisor)
			     do (format *query-io* "New divisor value: ")
				(setf divisor (eval (read *query-io*)))
			     finally (return (list divisor))))
	(setf y new-divisor))))
  (values (floor x y)))

(def-py-method py-int.__floordiv__ (x^ y^)
  (careful-floor-1ret x y))

(def-py-method py-int.__div__ (x^ y^)
  (cond ((and (integerp x) (integerp y))
	 (careful-floor-1ret x y))
	((and (numberp x) (numberp y))
	 (/ x y))
	(t (load-time-value *the-notimplemented*))))
       
(def-py-method py-int.__lshift__ (x^ y^)  (ash x y))
(def-py-method py-int.__rshift__ (x^ y^)  (ash x (- y)))

(def-py-method py-int.__xor__ (x^ y^) (logxor x y))
(def-py-method py-int.__and__ (x^ y^) (logand x y))
(def-py-method py-int.__or__  (x^ y^) (logior x y))
(def-py-method py-int.__ror__  (x y) (py-int.__or__ y x))

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

(def-py-method py-int.__rmul__ (x y) (py-int.__mul__ y x))

;; Bool

(def-proxy-class py-bool (py-int))

(def-py-method py-bool.__new__ :static (cls &optional (val 0))
  (let ((bool-val (if (py-val->lisp-bool val) +the-true+ +the-false+)))
    (if (eq cls (find-class 'py-bool))
	bool-val
      (make-instance cls :lisp-object bool-val))))

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

;;; Dict

(def-proxy-class dict)

(def-py-method dict.__new__ :static (cls &rest kwargs)
  (let ((ht (make-py-hash-table)))
    (if (eq cls (ltv-find-class 'dict))
        ht
      (make-instance cls :lisp-object ht))))

(def-py-method dict.__init__ (x &rest kwargs)
  (when kwargs
    (if (cadr kwargs)
        (loop with setitem-meth = 
              (or (x.class-attr-no-magic.bind x '{__setitem__})
                  (py-raise '{TypeError}
                            "Dict.__init__ called on object without __setitem__: ~A." x))
            while kwargs
            do (let ((key (pop kwargs))
                     (val (pop kwargs)))
                 (unless (keywordp key)
                   (py-raise '{TypeError} "dict.__new__: invalid key (not a symbol): ~S" key))
                 (py-call setitem-meth (symbol-name key) val)))
      (let* ((iterable (car kwargs))
             (items-meth (x.class-attr-no-magic.bind iterable '{items}))
             (items (if items-meth
                        (py-call items-meth)
                      (py-iterate->lisp-list iterable)))
             (setitem-meth (or (x.class-attr-no-magic.bind x '{__setitem__})
                               (py-raise '{TypeError}
                                         "dict.__init__ called on object without __setitem__ (~A)"
                                         x))))
        (map-over-object (lambda (obj)
                           (let ((lst (py-iterate->lisp-list obj)))
                             (if (= (length lst) 2)
                                 (py-call setitem-meth (pop lst) (pop lst))
                               (py-raise '{ValueError}
                                         "Got more than 2 values for element in .items() of ~A."
                                         iterable))))
                         items)))))

(def-py-method dict.__delitem__ (dict k)
  (with-py-dict 
      (or (remhash k dict)
          (py-raise '{KeyError} "Dict ~A has no such key: ~A." dict k))))

(def-py-method dict.__cmp__ (dict1 dict2)
  (if (py-val->lisp-bool (dict.__eq__ dict1 dict2))
      0
    -1)) ;; XXX -1/+1 now arbitrary: fix later

(def-py-method dict.__eq__ (dict1 dict2)
  (with-py-dict 
      (py-bool (cond ((eq dict1 dict2)
                      t)
                     ((not (hash-table-p dict2))
                      nil)
                     ((/= (hash-table-count dict1) (hash-table-count dict2))
                      nil)
                     (t 
                      (with-hash-table-iterator (next dict1)
                        (loop named iter
                            do (multiple-value-bind (entry-p k v) (next)
                                 (cond ((not entry-p)
                                        (return-from iter t))
                                       ((not (py-==->lisp-val (gethash k dict2) v))
                                        (return-from iter nil)))))))))))

(def-py-method dict.__getitem__ (dict k)
  "KEY may be symbol (converted to string)"
  (with-py-dict
      (or (gethash k dict)
          (py-raise '{KeyError} "Dict ~A has no such key: ~A." dict k))))

(def-py-method dict.__iter__ (dict)
  (dict.iterkeys dict))

(def-py-method dict.__len__ (dict)
  (with-py-dict
      (hash-table-count dict)))

(def-py-method dict.__nonzero__ (dict)
  (with-py-dict
      (py-bool (plusp (hash-table-count dict)))))

(def-py-method dict.__repr__ (x)
  (with-py-dict
      (with-output-to-string (s)
        (write-char #\{ s)
        (with-hash-table-iterator (next x)
          (loop named iter
              with first = t
              do (multiple-value-bind (entry-p key val) (next)
                   (cond ((not entry-p) (return-from iter))
                         (t (cond (first (setf first nil))
                                  (t (write-string ", " s)))
                            (repr-fmt s key)
                            (write-string ": " s)
                            (repr-fmt s val))))))
        (write-char #\} s))))

(def-py-method dict.__setitem__ (x key val)
  (with-py-dict
      (setf (gethash key x) val)))

(def-py-method dict.clear (d^)
  (with-py-dict (clrhash d)))

(def-py-method dict.copy (d1)
  (with-hash-table-iterator (next d1)
    (loop named iter
        with d2 = (make-py-hash-table)
        do (multiple-value-bind (entry-p k v) (next)
             (cond ((not entry-p) (return-from iter d2))
                   (t (setf (gethash k d2) v)))))))

(def-py-method dict.fromkeys :static (seq &optional (val (load-time-value *the-none*)))
  (with-py-dict
      (let ((d (make-py-hash-table)))
        (map-over-object (lambda (key) (setf (gethash key d) val)) seq)
        d)))

(def-py-method dict.get (x k &optional (default (load-time-value *the-none*)))
  (with-py-dict
      (or (gethash k x) default)))
      
(def-py-method dict.has_key (x k)
  (with-py-dict
      (multiple-value-bind (val presentp)
          (gethash k x)
        (declare (ignore val))
        (py-bool presentp))))

(def-py-method dict.items (x)
  (with-py-dict
      (with-hash-table-iterator (next x)
        (loop named iter
            with tuples
            do (multiple-value-bind (entry-p k v) (next)
                 (cond ((not entry-p)
                        (return-from iter (make-py-list-from-list tuples)))
                       (t
                        (push (make-tuple-from-list (list k v)) tuples))))))))

(defparameter *hash-table-iterator-indefinite-extent*
    (checking-reader-conditionals
     #+allegro t
     #+ecl t
     #+lispworks nil
     #+sbcl t
     #-(or allegro ecl lispworks sbcl) nil)
     "Whether the iterator created by WITH-HASH-TABLE-ITERATOR has indefinite extent.
ANSI states for WITH-HASH-TABLE-ITERATOR:  \"It is unspecified what happens if any
of the implicit interior state of an iteration is returned outside the dynamic extent
of the with-hash-table-iterator form such as by returning some closure over the
invocation form.\"")

(defun make-dict-iterator (hash-table func)
  (if *hash-table-iterator-indefinite-extent*
      (with-py-dict
          (with-hash-table-iterator (next-fn hash-table)
            (make-iterator-from-function
             :func (lambda () (multiple-value-bind (ok key val) (next-fn)
                                (when ok (funcall func key val)))))))
    (progn
      #+custom-hash-table-fallback ;; from library CL-CUSTOM-HASH-TABLE
      (error "This LOOP is not supported by CUSTOM-HASH-TABLE-FALLBACK") 
      (let ((vec 
             (loop with vec = (make-array (* 2 (hash-table-count hash-table)))
                 with i = -1
                 for key being each hash-key in hash-table
                 using (hash-value val)
                 do (setf (svref vec (incf i)) key
                          (svref vec (incf i)) val)
                 finally (return vec)))
            (i 0)
            (count (* 2 (hash-table-count hash-table))))
        (make-iterator-from-function
         :func (lambda ()
                 (when (< i count)
                   (let ((key (svref vec i))
                         (val (svref vec (incf i))))
                     (declare (ignorable key val))
                     (prog1 (funcall func key val)
                       (incf i))))))))))

(def-py-method dict.iteritems (x)
  (make-dict-iterator x (lambda (k v) (make-tuple-from-list (list k v)))))

(def-py-method dict.iterkeys (x)
  (make-dict-iterator x (lambda (k v) (declare (ignore v)) k)))

(def-py-method dict.itervalues (x)
  (make-dict-iterator x (lambda (k v) (declare (ignore k)) v)))

(def-py-method dict.keys (x)
  (with-py-dict
      (with-hash-table-iterator (next x)
        (loop with keys
            do (multiple-value-bind (entry-p k v) (next)
                 (declare (ignore v))
                 (cond ((not entry-p)
                        (return-from dict.keys (make-py-list-from-list keys)))
                       (t
                        (push k keys))))))))

(def-py-method dict.pop (x key &optional default)
  (with-py-dict
      (let ((val (gethash key x)))
        (if val
            (prog1 val (remhash key x))
          (or default (py-raise '{KeyError} "Dict has no key `~A' to pop()." key))))))

(def-py-method dict.popitem (x)
  (with-py-dict
      (maphash (lambda (k v)
                 (remhash k x)
                 (return-from dict.popitem 
                   (make-tuple-from-list (list k v))))
               x))
  (py-raise '{KeyError} "Dict is empty, can not popitem()."))

(def-py-method dict.setdefault (x key &optional default)
  (with-py-dict
      (or (gethash key x)
          (setf (gethash key x) default))))

(def-py-method dict.update (x y &rest kv-items)
  (with-py-dict
      (when (or (keywordp y) (and kv-items (not (keywordp (car kv-items)))))
        (py-raise '{ValueError}
                  "Invalid arguments to dict.update: expected (x, k=v, k2=v2, ..)."))
    (if (hash-table-p y)
        (maphash (lambda (k v) (setf (gethash k x) v)) y)
      (loop for entry in (py-iterate->lisp-list y)
          for subentries = (py-iterate->lisp-list entry)
          do (unless (= (length subentries) 2)
               (py-raise '{ValueError} "Invalid update subentry: expected (k, v), got: ~A."
                         (py-repr-string subentries)))
             (destructuring-bind (key val)
                 subentries
               (setf (gethash key x) val))))
    (assert (evenp (length kv-items)))
    (loop for (key val) on kv-items by #'cddr
        do (setf (gethash (symbol-name key) x) val)))
  *the-none*)
   
(def-py-method dict.values (x)
  ;; XXX should err in ECL
  (with-py-dict
      (make-py-list-from-list (loop for v being the hash-value in x collect v))))

;; Dicts used for namespaces

(defclass symbol-hash-table (object)
  ((hash-table :initarg :hash-table :accessor sht-ht))
  (:metaclass py-type))

(defvar *ht->symbol-hash-table* (make-hash-table :test 'eq))
;; Is there a reason to make the mapping unique?

(defun make-symbol-hash-table (ht)
  (with-py-dict 
      (or (gethash ht *ht->symbol-hash-table*)
          (setf (gethash ht *ht->symbol-hash-table*)
            (make-instance 'symbol-hash-table :hash-table ht)))))

(def-py-method symbol-hash-table.__setitem__ (d key val)
  (let ((key.sym (py-string-val->symbol key)))
    (setf (gethash key.sym (sht-ht d)) val)))

(def-py-method symbol-hash-table.__getitem__ (d key)
  (let ((key.sym (py-string-val->symbol key)))
    (or (gethash key.sym (sht-ht d))
        (py-raise '{KeyError} "Symbol hash table ~A has no such key: ~A." d key.sym))))

(def-py-method symbol-hash-table.__delitem__ (d key)
  (let ((key.sym (py-string-val->symbol key)))
    (or (remhash key.sym (sht-ht d))
        (py-raise '{KeyError} "Symbol hash table ~A has no such key: ~A." d key.sym))))

(def-py-method symbol-hash-table.has_key (d key)
  (py-bool (gethash (ensure-user-symbol (py-val->string key)) (sht-ht d))))

(def-py-method symbol-hash-table.__repr__ (d)
  (with-output-to-string (s)
    (let ((*print-circle* t))
      (print-unreadable-object (d s :type t)
        (format s "{~_~{~A: ~S~^,~_ ~}~_}" (loop for k being the hash-key in (sht-ht d)
                                               using (hash-value v)
                                               nconc (list (string k) v)))))))

(def-py-method symbol-hash-table.items (d)
  (loop for key being each hash-key in (sht-ht d)
      using (hash-value value)
      collect (make-tuple-from-list (list (string key) value))))

 
;; TODO: add the other dict methods

;;; Proxies for funky dicts

(defclass funky-dict-wrapper (object)
  ((getter :initarg :getter :accessor fdw-getter)
   (setter :initarg :setter :accessor fdw-setter))
  (:metaclass py-type))

(defun funky-dict-wrapper.alist (w)
  (let (items)
    (funky-dict-map (funcall (fdw-getter w)) (lambda (k v) (push (cons k v) items)))
    items))

(def-py-method funky-dict-wrapper.__repr__ (w)
  (with-output-to-string (s)
    (let ((*print-escape* t))
      (print-unreadable-object (w s :type t)
        (format s "{~_ ~{~A: ~S~^,~_ ~}~_}" (loop for (k . v) in (funky-dict-wrapper.alist w)
                                                nconc (list (string k) v)))))))

(def-py-method funky-dict-wrapper.keys (w)
  (make-py-list-from-list (loop for (k . nil) in (funky-dict-wrapper.alist w) collect (string k))))

;; List (Lisp object: adjustable array)

(def-proxy-class py-list)

(defun make-py-list (&optional (size 0))
  (make-array size :adjustable t :fill-pointer 0))

(def-py-method py-list.__new__ :static (cls &optional iterable)
  (let ((vec (make-py-list)))
    (if (eq cls (ltv-find-class 'py-list))
        vec
      (make-instance cls :lisp-object vec))))
		    
(def-py-method py-list.__init__ (x^ &optional iterable)
  (when iterable
    (let* ((items (py-iterate->lisp-list iterable))
	   (len (length items)))
      (setq x (adjust-array x len))
      (loop for i from 0 below len
	  do (setf (aref x i) (pop items)))
      (setf (fill-pointer x) len)
      x)))

(def-py-method py-list.__iadd__ (x^ y^)
  ;; not efficient, but correct
  (let ((res (py-list.__add__ x y)))
    (setf (fill-pointer x) 0)
    (loop for item across res
        do (vector-push-extend item x))
    x))
          
(def-py-method py-list.__add__ (x^ y^)
  (when (stringp y)
    (setf y (map 'vector #'py-string-from-char y)))
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

(def-py-method py-list.__cmp__ (x^ y^)
  (unless (and (vectorp x) (vectorp y))
    (return-from py-list.__cmp__ (load-time-value *the-notimplemented*)))
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
    (integer (when (minusp item)
	       (incf item (length x)))
	     (unless (<= 0 item (1- (length x)))
	       (py-raise '{ValueError}
			 "del <list>[i] : i outside range (got ~A, length list = ~A)"
			 item (length x)))
	     (replace x x :start1 item :start2 (1+ item))
	     (decf (fill-pointer x)))
    (py-slice (with-slots (start stop step) item
		(cond ((and (none-p start) (none-p stop) (none-p step)) ;; del x[:]
		       (fill x nil)
		       (setf (fill-pointer x) 0))
		      (t (destructuring-bind (kind &rest args)
                             (multiple-value-list (slice-indices item (length x)))
                           (ecase kind
                             ((:empty-slice-bogus :empty-slice-before :empty-slice-after :empty-slice-between))
                             ;; nothing to do
                             (:nonempty-slice
                              (destructuring-bind (start-incl stop-incl num) args
                                (replace x x :start1 start-incl :start2 (1+ stop-incl))
                                (decf (fill-pointer x) num)))
                             (:nonempty-stepped-slice
                              (destructuring-bind (start-incl stop-incl step times) args
                                stop-incl
                                (when (minusp step)
                                  (setf step (- step))
                                  (rotatef start-incl stop-incl))
                                (dotimes (i times)
                                 (let ((start-ix (- (+ start-incl (* step i)) i)))
                                   (replace x x :start1 start-ix :start2 (1+ start-ix))))
                                (decf (fill-pointer x) times)))))))))))

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
			 "Sequence subscript outside range (got ~A, length = ~A)"
			 item (length x)))
	     (funcall make-seq-func (aref x item) t))
    
    (py-slice (destructuring-bind (kind &rest args)
		  (multiple-value-list (slice-indices item (length x)))
                (ecase kind
                  ((:empty-slice-bogus :empty-slice-before :empty-slice-after :empty-slice-between)
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
                            (values (if (plusp step)
                                        (loop for i from start upto stop by step collect (aref x i))
                                      (loop for i from start downto stop by (- step) collect (aref x i)))
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
  (typecase item
    (integer
     (when (< item 0)
       (incf item (length x)))
     (unless (<= 0 item (1- (length x)))
       (py-raise '{ValueError}
                 "<list>[i] = x : i outside range (got ~A, length list = ~A)"
                 item (length x)))
     (setf (aref x item) val))
    (py-slice
     (let ((values (py-iterate->lisp-list val)))
       (destructuring-bind (kind &rest args)
           (multiple-value-list (slice-indices item (length x)))
         (ecase kind
           (:empty-slice-bogus (py-raise '{ValueError} "Invalid slice for assignment: ~A." item))
           (:empty-slice-before (let ((copy (copy-seq x)))
                                  (setf (fill-pointer x) 0)
                                  (loop for v in values do (vector-push-extend v x))
                                  (loop for v across copy do (vector-push-extend v x))
                                  (setf (fill-pointer x) (+ (length copy) (length values)))))
           (:empty-slice-after (loop for v in values do (vector-push-extend x v)))
           (:empty-slice-between (let ((n-before (pop args)))
                                   (let ((copy (subseq x (1+ n-before))))
                                     (setf (fill-pointer x) n-before)
                                     (loop for v in values do (vector-push-extend v x))
                                     (loop for v across copy do (vector-push-extend v x))
                                     (setf (fill-pointer x) (+ (length copy) (length values))))))
           (:nonempty-slice (destructuring-bind (start-incl stop-incl num) args
                              num
                              (let ((copy-tail (subseq x (1+ stop-incl))))
                                (setf (fill-pointer x) start-incl)
                                (loop for v in values do (vector-push-extend v x))
                                (loop for v across copy-tail do (vector-push-extend v x)))))
           (:nonempty-stepped-slice (destructuring-bind (start-incl stop-incl step times) args
                                      stop-incl
                                      (unless (= times (length values))
                                        (py-raise '{ValueError}
                                                  "Attempt to assign sequence of ~A items to stepped slice of ~A items."
                                                  (length values) times))
                                      (when (minusp step)
                                        (setf step (- step))
                                        (rotatef start-incl stop-incl)
                                        (setf values (nreverse values)))
                                      (dotimes (i times)
                                        (let ((ix (+ start-incl (* step i))))
                                          (setf (aref x ix) (pop values)))))))))))
  (load-time-value *the-none*))

(defvar *py-print-safe* nil)

(def-py-method py-list.__str__ (x^)
  (if *py-print-safe*
      (with-output-to-string (s)
	(print-unreadable-object (x s :type nil :identity t)
	  (format s "list with ~A items" (length x))))
    (py-list.__repr__ x)))


(def-py-method py-list.append (x^ y)
  (vector-push-extend y x)
  (load-time-value *the-none*))

(defmacro real-py-list.append (list item)
  "For internal use, e.g. list comprehensions."
  `(vector-push-extend ,item ,list))

(def-py-method py-list.index (list item)
  (or (position item list :test #'py-==->lisp-val)
      (py-raise '{ValueError} "Item ~S not in list." item)))
  
(def-py-method py-list.insert (list index object)
  "Insert OBJECT before index INDEX"
  (setf index (min (max index 0) (length list)))
  (vector-push-extend nil list)
  (do ((i (1- (length list)) (1- i)))
      ((<= i index))
    (setf (aref list i) (aref list (1- i))))
  (setf (aref list index) object)
  (load-time-value *the-none*))

(def-py-method py-list.extend (x^ iterable)
  (loop for item in (py-iterate->lisp-list iterable)
      do (py-list.append x item))
  (load-time-value *the-none*))
       
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
	(py-raise '{IndexError}
		  "list.pop(x, i): ix wrong (got: ~A; x.len: ~A)"
		  ix x.len)))))

(def-py-method py-list.reverse (x^)
  (nreverse x))

(def-py-method py-list.sort (x^ &optional fn)
  (let* ((sort-fun (if fn
		       (lambda (x y) (< (signum (deproxy (py-call fn x y))) 0))
		     (lambda (x y) (< (signum (py-cmp x y)) 0))))
	 (res (sort x sort-fun)))

    ;; It's not guaranteed by ANSI that (eq res x).
    (unless (eq res x)
      (replace x res))
    
    (load-time-value *the-none*)))


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

(defvar *py-char-strings*
    (make-array 256
                :element-type 'string
                :initial-contents (loop for i from 0 to 255
                                      collect (string (code-char i))))
  "Mapping from character to corresponding length-1 string.")

(defun py-string-from-char (ch)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (character ch))
  (let ((code (char-code ch))
        (char-strings (load-time-value (coerce (loop for i from 0 to 255
                                                   collect (string (code-char i)))
                                               'vector))))
    (if (<= 0 code 255)
        (svref char-strings code)
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
    (values :utf8 #x0010FFFF 255))
   
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
    (declare (ignorable ex-format)
             (ignore max-octet-code))
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


(def-py-method py-string.__add__ (x^ y^)
  (if (stringp y)
      (concatenate 'string x y)
    (load-time-value *the-notimplemented*)))

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

(def-py-method py-string.__rmul__ (string n) (py-string.__mul__ n string))

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
  (assert (= (length fillchar) 1))
  (error "todo"))
   
(def-py-method py-string.decode (x^ &optional encoding^ errors)
  (py-decode-unicode x encoding errors))

(def-py-method py-string.encode (x^ &optional encoding^ errors)
  (py-encode-unicode x encoding errors))

(def-py-method py-string.endswith (x^ suffix &optional start end)
  (when (or start end) (error "todo"))
  (py-bool (string= (subseq x (- (length x) (length suffix))) suffix)))
  
(def-py-method py-string.find (x^ item &rest args)
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
  (setf old (py-val->string old)
        new (py-val->string new))
  (when count
    (setf count (py-val->integer count 0)))
  (cond ((zerop (length old))
         (py-raise '{ValueError} "Can't string.replace with old having length 0"))
        ((and (= (length old) 1)
              (= (length new) 1))
         (substitute (aref new 0) (aref old 0) x :count count))
        (t
         (apply #'concatenate 'string
                (loop with find-start-ix = 0
                    with coll = ()
                    for find-ix = (search old x :start2 find-start-ix)
                    for replacements from 0
                    while find-ix
                    while (or (not count) (< replacements count))
                    do (push (subseq x find-start-ix find-ix) coll)
                       (push new coll)
                       (setf find-start-ix (+ find-ix (length old)))
                    finally (push (subseq x find-start-ix) coll)
                            (return (nreverse coll)))))))
 
(def-py-method py-string.split (x^ &optional (sep (load-time-value *the-none*))
                                             (max-splits most-positive-fixnum))
  (let ((sep-sequence (cond ((none-p sep) (list #\Space #\Tab #\Return)) ;; definition of whitespace?
                            ((and (stringp sep)
                                  (= (length sep) 1)) (coerce sep 'list))
                            (t (error "Todo: string.split() with this as seperator: ~S." sep)))))
    (let (res
          (ix 0)
          (num-splits 0))
        (block iter
          (dotimes (i max-splits)
            (when (>= ix (length x))
              (return-from iter))
            (unless (whereas ((start-collect (position-if-not (lambda (x) (member x sep-sequence)) x :start ix))
                              (end-collect (when start-collect
                                             (or (position-if (lambda (x) (member x sep-sequence)) x :start (1+ start-collect))
                                                 (length x)))))
                      (push (subseq x start-collect end-collect) res)
                      (incf num-splits)
                      (setf ix (1+ end-collect))
                      t)
              (return-from iter))))
        (unless (>= ix (length x))
          (push (subseq x ix) res))
        (make-py-list-from-list (nreverse res)))))
  
(def-py-method py-string.startswith (x^ prefix &optional start end)
  (when (or start end) (error "todo"))
  (py-bool (and (>= (length x) (length prefix))
                (string= (subseq x 0 (length prefix)) prefix))))

(def-py-method py-string.upper (x^)
  (string-upcase x))
   
;; Symbols

(def-proxy-class py-symbol)

(def-py-method py-symbol.__repr__ (x)
  (with-output-to-string (s)
    (print-unreadable-object (x s :type nil :identity nil)
      (format s "symbol ~S" x))))

;; Alist

(def-proxy-class py-alist)

(def-py-method py-alist.__getattribute__ (x attr)
  (check-type x list)
  (let ((attr-variants (list attr (substitute #\- #\_ attr))))
    (loop for item in x
        when (and (listp item)
                  (let ((key (car item)))
                    (and (symbolp key)
                         (member key attr-variants :test 'string-equal))))
        do (return-from py-alist.__getattribute__
             (or (cadr item)
                 *the-none*)))) ;; can't have NIL as Python value
  (py-raise '{AttributeError} "Assoc-list ~S has no attribute `~A'." x attr))

(def-py-method py-alist.__repr__ (x)
  (check-type x list)
  (with-standard-io-syntax
    (format nil "~A" x)))

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

(defun make-py-list-from-tuple (tuple)
  (make-py-list-from-list (if (eq tuple *the-empty-tuple*) () tuple)))

(defun make-lisp-list-from-tuple (tuple)
  (if (eq tuple *the-empty-tuple*) () tuple))

(defun make-tuple-from-list (list)
  (or list *the-empty-tuple*))

(defmacro make-tuple-unevaled-list (items)
  `(make-tuple-from-list (list ,@items)))

;; Efficient tuple functions

(def-py-method py-tuple.__add__ (x^ y^)
  (make-tuple-from-list (append x y)))

(def-py-method py-tuple.__cmp__ (x^ y^)
  (unless (and (listp x) (listp y))
    (return-from py-tuple.__cmp__ (load-time-value *the-notimplemented*)))
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
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-stack-list (tmp (py-hash (or (first x) 1))
                        (py-hash (or (second x) 2))
                        (py-hash (or (third x) 3))
                        (py-hash (or (fourth x) 4)))
    (sxhash tmp)))

(def-py-method py-tuple.__iter__ (x^)
  (make-iterator-from-function
   :name :tuple-iterator
   :func (let ((i -1))
	   (lambda ()
	     (nth (incf i) x)))))

(def-py-method py-tuple.__len__ (x^)
  (length x))

(def-py-method py-tuple.__mul__ (x^ n)
  (let* ((n (py-val->integer n :min 0))
         (x-list (make-lisp-list-from-tuple x))
         (repeats (loop repeat n collect x-list)))
    (make-tuple-from-list (apply #'concatenate 'list repeats))))

(def-py-method py-tuple.__rmul__ (tuple n) (py-tuple.__mul__ n tuple))
                                 
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
  (:method ((x hash-table)) (declare (ignorable x)) (ltv-find-class 'dict))
  (:method ((x integer)) (declare (ignorable x)) (ltv-find-class 'py-int    ))
  (:method ((x real))    (declare (ignorable x)) (ltv-find-class 'py-float  ))
  (:method ((x complex)) (declare (ignorable x)) (ltv-find-class 'py-complex))
  (:method ((x string))  (declare (ignorable x)) (ltv-find-class 'py-string ))
  (:method ((x vector))  (declare (ignorable x)) (ltv-find-class 'py-list   ))
  
  #+ecl
  (:method ((x cl-custom-hash-table:custom-hash-table))
           (declare (ignorable x))
           (ltv-find-class 'dict))
                               
  (:method ((x list))    (cond ((null x)
                                (break "PY-CLASS-OF of NIL"))
                               ((and (listp (car x)) (symbolp (caar x)))
                                (ltv-find-class 'py-alist))
                               (t
                                (ltv-find-class 'py-tuple))))
  (:method ((x symbol))  (declare (ignorable x))
                         (ltv-find-class 'py-symbol ))
  (:method ((x function))    (declare (ignorable x))
                             (ltv-find-class 'py-function))
  (:method ((x py-function)) (declare (ignorable x))
                             (ltv-find-class 'py-function))
  (:method ((x package))     (declare (ignorable x))
                             (ltv-find-class 'lisp-package))
  
  #+(or)(:method ((x py-type)) (ltv-find-class 'py-type))
    
  (:method ((x py-meta-type)) ;; metatypes (including `type')
                              ;;  fake being of type `type'
                              (declare (ignorable x))
                              (ltv-find-class 'py-type))
  
  (:method ((x py-type)) (class-of x))
  
  (:method ((x (eql (find-class 'py-meta-type))))
           ;; the metatypes is posing as `type'
           (declare (ignorable x))
           (ltv-find-class 'py-type))

  (:method ((x t))       (class-of x)))

(defun (setf py-class-of) (new-cls x)
  (change-class x new-cls))

;;; Attributes are a fundamental thing: getting, setting, deleting

#+(or)
;;#+(and allegro-version>= (version>= 8 0))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generator

(defclass generator (object)
  ;; XXX make funcallable instance? but should not be allowed by py-call it...
  ((state :initarg :state :accessor generator-state))
  (:metaclass py-type))

(def-py-method generator.next (g)
  "Returns the next yielded value."
  (%generator.send g *the-none* t))

(def-py-method generator.send (g value)
  "Returns the next yielded value."
  (%generator.send g value t))

(def-py-method generator.throw (g &optional (value *the-none*) (traceback *the-none*))
  "Raises exception in the generator at the current point of execution."
  (%generator.send g (list :exception value traceback) nil))

(def-py-method generator.__iter__ (g)
  g)

(def-py-method generator.close (g)
  (%generator.close g))
  
#+(or) ;; todo -- will be the first built-in class with a del method?
(defun generator.__del__ (g)
  (generator.close g))

;; iterator from lambda

(defclass py-func-iterator (object)
  ((name        :initarg :name :initform "(unnamed)")
   (func        :initarg :func :type function)
   (stopped-yet :initform nil))
  (:metaclass py-type))

(defclass py-func-iterator-sendable (py-func-iterator)
  ()
  (:metaclass py-type))

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
    (loop (when stopped-yet
            (funcall 'raise-StopIteration))
      (let ((val (apply func args)))
        (if val
            (return-from py-func-iterator.next-or-send val)
          (setf stopped-yet t))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Calling objects (functions, classes, instances)

(defun py-apply (f &rest args)
  "Like APPLY, but accepting any python sequence as last arg, and using PY-CALL instead of FUNCALL."
  (apply #'py-call f (nconc (butlast args) (py-iterate->lisp-list (car (last args))))))

(define-compiler-macro py-apply (f &rest args)
  `(apply #'py-call ,f ,@(butlast args) (py-iterate->lisp-list ,(car (last args)))))

(defgeneric py-call (f &rest args)
  ;; XXX Speed up slot value lookup by using
  ;; MOP:SLOT-DEFINITION-LOCATION, MOP:STANDARD-INSTANCE-ACCESS.
  (:method ((f null) &rest args)
           (declare (ignore args) (ignorable f) (dynamic-extent args))
	   (error "PY-CALL of NIL"))
  
  (:method ((f class) &rest args)
           (declare (dynamic-extent args)
                    (ignorable f args)
                    (optimize (speed 3) (safety 0) (debug 0)))
           #-clpython-exceptions-are-python-objects
           (when (subtypep f '{Exception})
             #+cmu (when (typep f 'class)
                     ;; CMUCL Snapshot 2009-01 bug: 
                     ;;  make-condition does not work with class arguments.
                     (setf f (class-name f)))
             (return-from py-call (make-condition f :args (copy-list args))))
           (call-next-method))

  (:method ((x t) &rest args) ;; objects
           (let ((__call__ (class.attr-no-magic (py-class-of x) '{__call__})))
             (cond ((functionp __call__)
                    (apply __call__ x args))
                   (__call__
                    (apply #'py-call (bind-val __call__ x (py-class-of x)) args))
                   (t 
                    (error "Don't know how to call: ~S (args: ~A)" x args)))))

  ;; XXX For bound/unbound/static method: need to check if object is
  ;; not instance of (user-defined) subclass?
  
  (:method ((f py-bound-method) &rest args)
           (declare (dynamic-extent args)
                    (optimize (speed 3) (safety 0) (debug 0)))
	   (apply 'py-bound-method.__call__ f args))
  
  (:method ((f py-unbound-method) &rest args)
           (declare (dynamic-extent args)
                    (optimize (speed 3) (safety 0) (debug 0)))
	   (apply 'py-unbound-method.__call__ f args))
  
  (:method ((f py-static-method) &rest args)
           (declare (dynamic-extent args)
                    (optimize (speed 3) (safety 0) (debug 0)))
	   (apply 'py-static-method.__call__ f args))
  
  ;; Avoid infinite recursion:
  (:method ((f function) &rest args)
           (declare (dynamic-extent args)
                    (optimize (speed 3) (safety 0) (debug 0)))
           (apply f args))

  #+sbcl
  (:method ((x sb-pcl::condition-class) &rest args)
	   (declare (ignore args))
           (assert (not *exceptions-are-python-objects*) ()
             "Should not come in (py-call <condition-type> ..) if ~A = T."
             '*exceptions-are-python-objects*)
           (make-condition x)))

(defun py-classlookup-bind-call (x attr &rest args)
  "Returns RESULT, METH-FOND-P"
  (let ((m (x.class-attr-no-magic.bind x attr)))
    (if m
	(values (apply #'py-call m args) t)
      (values nil nil))))


;; Compiler macros are defined in optimize.cl; PY-CALL is exceptional
;; in that this function is used many times in this module already,
;; and we benefit from inlining it.

#+(or) ;;(and allegro-version>= (version>= 8 0))
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
               (let ((attr-sym (second attr)))
                 `(let* ((.x ,x)
                         (val (class.attr-no-magic (py-class-of .x) ',attr-sym)))
                    (if (and (functionp val) (not (instance.attr-no-magic .x ',attr-sym)))
                        (funcall val .x ,@args)
                      (py-call .x ,@args)))))
             #+(or)
             `(multiple-value-bind (.a .b .c)
                  (py-attr ,x ,attr :bind-class-attr nil)
                (if (eq .a :class-attr)
                    (progn #+(or)(assert (functionp .b))
                           #+(or)(warn "saving bound method ~A ~A" .b .c)
                           (funcall .b .c ,@args))
                  (py-call .a ,@args))))
            
	    #+(or)
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

            #+(or)
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

(defparameter *most-positive-slice-index* most-positive-fixnum)

(defun normalize-slice-for-get/set (x slice methname)
  (check-type slice py-slice)
  (check-type methname symbol)
  (let* ((start (slice-start slice))
         (stop (slice-stop slice))
         (step (slice-step slice))
         (try-@etslice (or (none-p step) (equal step 1))))
    (when try-@etslice
      (whereas ((m (x.class-attr-no-magic.bind x methname)))
        ;; __getslice__ can be called on objects that don't implement __len__
        ;; as long as the indices are not negative.
        ;; None start defaults to 0
        ;; None end defaults to most-positive-index, which is 0x7fffffff in CPython.
        (let (len) ;; lazy
          (cond ((none-p start)
                 (setf start 0))
                ((and (numberp start) (minusp start))
                 (setf len (py-len x))
                 (incf start len)))
          (cond ((none-p stop)
                 (setf stop *most-positive-slice-index*))
                ((and (numberp stop) (minusp stop))
                 (or len (setf len (py-len x)))
                 (incf stop len))))
        (return-from normalize-slice-for-get/set (values m start stop))))))

(defgeneric py-subs (x item))

(defmethod py-subs :around (x (item py-slice))
  ;; First try __getslice__ (with normalized indices)
  (multiple-value-bind (meth start stop)
      (normalize-slice-for-get/set x item '{__getslice__})
    (when meth
      (return-from py-subs (py-call meth start stop))))
  (call-next-method))
  
(defmethod py-subs (x item)
  (whereas ((gi (x.class-attr-no-magic.bind x '{__getitem__})))
    (return-from py-subs (py-call gi item)))
  (py-raise '{TypeError} "Object ~S does not support item extraction." x))

(defgeneric (setf py-subs) (new-val x item))

(defmethod (setf py-subs) :around (new-val x (item py-slice))
  ;; First try __getslice__ (with normalized indices)
  (multiple-value-bind (meth start stop)
      (normalize-slice-for-get/set x item '{__setslice__})
    (when meth
      (return-from py-subs (py-call meth start stop new-val))))
  (call-next-method))

(defmethod (setf py-subs) (new-val x item)
  "New-val = NIL means deletion."
  (if (null new-val)
      (let* ((x.cls (py-class-of x))
             (__delitem__ (class.attr-no-magic x.cls '{__delitem__})))
        (if __delitem__
            (py-call __delitem__ x item)
          (py-raise '{TypeError}
                    "Object ~A (a ~A) has no `__delitem__' method."
                    x (class-name (py-class-of x)))))
    (let ((si (x.class-attr-no-magic.bind x '{__setitem__})))
      (if si
          (py-call si item new-val)
        (py-raise '{TypeError} "Object ~S does not support item assignment" x)))))

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

(defun generic-binary-op (x y l-meth r-meth op-syntax)
  (let ((x.class (py-class-of x))
        (y.class (py-class-of y)))
    (multiple-value-bind (x-result x-val-class)
        (class.attr-no-magic x.class l-meth)
      (multiple-value-bind (y-result y-val-class)
          (class.attr-no-magic y.class r-meth)
        (let ((res (cond ((and x-result y-result)
                          ;; The __rxxx__ goes before __xxx__, unless __xxx__ is defined in
                          ;; a subclass of where __rxxx__ is defined.
                          (let ((try-y-first (and (not (eq x-val-class y-val-class))
                                                  (subtypep y-val-class x-val-class)))
                                (ni (load-time-value *the-notimplemented*))
                                res)
                            (if try-y-first
                                (progn (setf res (py-call y-result y x))
                                       (when (eq res ni)
                                         (setf res (py-call x-result x y))))
                              (progn (setf res (py-call x-result x y))
                                     (when (eq res ni)
                                       (setf res (py-call y-result y x)))))
                            res))
                         (x-result (py-call x-result x y))
                         (y-result (py-call y-result y x)))))
          (if (or (null res)
                  (eq res (load-time-value *the-notimplemented*)))
              (raise-invalid-operands op-syntax x y)
            res))))))

(defun generic-binary-iop (x val i-meth)
  (whereas ((iop-meth (class.attr-no-magic (py-class-of x) i-meth))
            (res (py-call iop-meth x val)))
    (unless (eq res (load-time-value *the-notimplemented*))
      res)))

(defmacro def-math-func (op-syntax op-func l-meth r-meth iop-syntax iop-func i-meth)
  `(progn
     
     (defgeneric ,op-func (x y)
       (:method ((x t) (y t))
                (generic-binary-op x y ',l-meth ',r-meth ',op-syntax)))
     
     ,(when op-syntax
	`(setf (gethash ',op-syntax *binary-op-funcs-ht*) ',op-func))
     
     ,(when iop-func
	`(defgeneric ,iop-func (x val)
	   (:method ((x t) (val t))
                    (generic-binary-iop x val ',i-meth))))
     
     ,(when iop-syntax
	`(setf (gethash ',iop-syntax *binary-iop-funcs-ht*) ',iop-func))
     
     ,(when (and iop-syntax op-syntax)
	`(setf (gethash ',iop-syntax *binary-iop->op-ht*) ',op-syntax))))

;; /t/ is not Python syntax, but a hack to support __future__ feature
;; `true division'
(progn (def-math-func [+]   py-+    {__add__}      {__radd__}       [+=]   py-+=   {__iadd__}      )
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
       (def-math-func [<divmod>] py-divmod {__divmod__} {__rdivmod__}   nil    nil     nil         ))

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

(defgeneric py-** (x y &optional z))

(defmethod py-** (x y &optional z)
  (if z
      ;; XXX should also do __rpow__ for three-arg case...
      (let* ((op-meth (x.class-attr-no-magic.bind x '{__pow__})) 
             (res (and op-meth (py-call op-meth y z))))
        (if (and res (not (eq res (load-time-value *the-notimplemented*))))
            res
          (raise-invalid-operands '** x y)))
    (generic-binary-op x y '{__pow__} '{__rpow__} 'clpython.ast.operator:**)))

(setf (gethash '[**] *binary-op-funcs-ht*) 'py-**)

;; **= has similar ugliness

(defun py-**= (x y &optional z)
  (let* ((x.class (py-class-of x))
	 (iop-meth (class.attr-no-magic x.class '{__ipow__}))
	 (res (and iop-meth (if z
				(py-call iop-meth x y z)
			      (py-call iop-meth x y)))))
    
    (if (and iop-meth (not (eq res (load-time-value *the-notimplemented*))))
	res
      nil)))

(setf (gethash '[**=] *binary-iop-funcs-ht*) 'py-**=)
(setf (gethash '[**=] *binary-iop->op-ht*) '[**])


(defvar *unary-op-funcs-ht* (make-hash-table :test #'eq))
(defun get-unary-op-func-name (op) (or (gethash op *unary-op-funcs-ht*)
				  (error "missing unary op func: ~A" op)))

(defmacro def-unary-op-func (syntax fname meth)
  `(progn (defgeneric ,fname (x)
	    (:method ((x t))
		     (let* ((x.class (py-class-of x))
			    (op-meth (class.attr-no-magic x.class ',meth))
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
	   (let ((contains-meth (class.attr-no-magic (py-class-of seq) '{__contains__})))
	     (if contains-meth
		 (let ((res (py-call contains-meth seq x)))
		   (cond ((eq res t)   +the-true+)
			 ((eq res nil) +the-false+)
			 (t            res)))
	       (loop with f = (get-py-iterate-fun seq)
		   for seq-item = (funcall f)
		   while seq-item
		   when (py-==->lisp-val x seq-item) return +the-true+
		   finally (return +the-false+))))))
	     
(defgeneric py-not-in (x seq)
  (:method ((x t) (seq t))
	   (py-not (py-in x seq))))

(setf (gethash '[in] *binary-op-funcs-ht*) 'py-in)
(setf (gethash '[not in] *binary-op-funcs-ht*) 'py-not-in)

(defgeneric py-is (x y)
  (:method ((x t) (y t))
	   (if (eq x y) +the-true+ +the-false+)))

(defgeneric py-is-not (x y)
  (:method ((x t) (y t))
	   (if (eq x y) +the-false+ +the-true+)))

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
			 +the-true+
		       +the-false+)))
	  (setf (gethash ',syntax *binary-comparison-funcs-ht*) ',func)))

#+ecl
(defvar *py-id-entries* (make-weak-key-hash-table))

(defun py-id (x)
  "Return pointer address. This might change during the life time of the object,
e.g. due to moving by the GC. Python has reference counting, and guarantees a
fixed id during the object's lifetime."
  (checking-reader-conditionals
   #+allegro (excl:lispval-to-address x)
   #+ccl (ccl:%address-of x)
   #+cmu (kernel:get-lisp-obj-address x)
   #+ecl (or #1=(gethash x *py-id-entries*)
             (setf #1# (hash-table-count *py-id-entries*)))
   #+lispworks (system:object-address x)
   #+sbcl (sb-kernel:get-lisp-obj-address x)))

(defgeneric py-cmp (x y)
  (:documentation
   "Compare two objects, of which at least one is a user-defined-object.
Returns one of (-1, 0, 1): -1 iff x < y; 0 iff x == y; 1 iff x > y")
  
  (:method ((x class) (y class))
           ;; Added, perhaps not in CPython. Invalid for sufficiently hairy metaclasses.
           ;; Without this method, the method (x t) (y t) tries to compare IDs
           (cond ((eq x y) 0)
                 ((string< (class-name x) (class-name y)) -1)
                 (t 1)))
  
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
		 (let* ((__cmp__ (class.attr-no-magic x.class '{__cmp__})) ;; XXX bind
			(cmp-res (when __cmp__ (py-call __cmp__ x y))))
		   (when (and cmp-res
			      (not (eq cmp-res (load-time-value *the-notimplemented*))))
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
		     do (let* ((meth (class.attr-no-magic (if y-sub-of-x y.class x.class) meth-name))
			       (res (when meth
				      (py-call meth
					       (if y-sub-of-x y x)
					       (if y-sub-of-x x y)))))
		 
			  (when (and res (not (eq res (load-time-value *the-notimplemented*))))
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

	       (let* ((meth (class.attr-no-magic x.class '{__cmp__}))
		      (res (when meth
			     (py-call meth x y))))
		 (when (and res (not (eq res (load-time-value *the-notimplemented*))))
		   (let ((norm-res (normalize res)))
		     (return-from py-cmp norm-res))))

	       (let* ((meth (class.attr-no-magic y.class '{__cmp__}))
		      (res (when meth
			     (py-call meth y x))))
		 (when (and res (not (eq res (load-time-value *the-notimplemented*))))
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
      
	       (cond ((none-p x) (return-from py-cmp -1))
		     ((none-p y) (return-from py-cmp  1)))
      
	       ;; Instances of different class are compared by class name, but
	       ;; numbers are always smaller.
      
	       ;; Probably, when we arrive here, there is a bug in the logic
	       ;; above. Therefore print a warning.
      
	       #+(or)(warn "[debug] CMP can't properly compare ~A and ~A." x y)
      
	       (return-from py-cmp
		 (if (string< (class-name x.class) (class-name y.class))
		     -1
		   1))))))

;; XXX these functions should call __le__ etc, not __cmp__ ?!
(def-comparison  [<]  py-<   (=  (the (integer -1 1) (py-cmp x y)) -1))
(def-comparison  [>]  py->   (=  (the (integer -1 1) (py-cmp x y))  1))
(def-comparison [==] py-==  (=  (the (integer -1 1) (py-cmp x y))  0))
(def-comparison  [!=] py-!=  (/= (the (integer -1 1) (py-cmp x y))  0)) ;; lexer: <> --> !=
(def-comparison  [<=] py-<=  (<= (the (integer -1 1) (py-cmp x y))  0))
(def-comparison  [>=] py->=  (>= (the (integer -1 1) (py-cmp x y))  0))

;; Necessary for bootstrapping (dict + def-py-method)
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
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (notinline py-==)) ;; skip compiler macro
  (py-== y x))

(defmethod py-== ((x symbol) y)
  (py-== (symbol-name x) y))

(defmethod py-== (x (y symbol))
  (py-== x (symbol-name y)))

#+(or) ;; moved to classdefs
(defun py-==->lisp-val (x y)
  (py-val->lisp-bool (py-== x y))) 

(defgeneric py-val->lisp-bool (x)
  (:method ((x number)) (/= x 0))
  (:method ((x string)) (> (length x) 0))
  (:method ((x vector)) (> (length x) 0))
  (:method (x)          (py-val->lisp-bool (py-nonzero x))))


;; Shortcut functions

(defmacro def-py-shortcut-func (funcname method &key not-found-clause (defgeneric t))
  `(progn ,@(when defgeneric
	      `((defgeneric ,funcname (x))))
	  (defmethod ,funcname ((x t))
	      (let ((,method (x.class-attr-no-magic.bind x ',method)))
                (if ,method
                    (py-call ,method)
                  ,(or not-found-clause
                       `(py-raise '{TypeError}
                                  "Object ~A (a ~A) has no `~A' method."
                                  x (class-name (py-class-of x))
                                  ',method)))))))

(def-py-shortcut-func py-abs  {__abs__} )
(def-py-shortcut-func py-repr {__repr__} 
  :not-found-clause ;; it's not a Python object, otherwise object.__repr__ would kick in
  (with-output-to-string (s)
    (format s "~A" x)))
(def-py-shortcut-func py-str  {__str__} :not-found-clause (py-repr x))
(def-py-shortcut-func py-hex  {__hex__} )
(def-py-shortcut-func py-oct  {__oct__} )
(def-py-shortcut-func py-len  {__len__} )
(def-py-shortcut-func py-nonzero {__nonzero__} )
(def-py-shortcut-func py-float {__float__})
(def-py-shortcut-func py-hash  {__hash__} :defgeneric nil) ;; defgeneric in metaclass.lisp
(def-py-shortcut-func py-index {__index__})

(defun py-contains (x item)
  ;; Whether ITEM is in X (note reversed args!)
  (let ((method (x.class-attr-no-magic.bind x '{__contains__})))
    (if method
        (py-call method item)
      (py-raise '{TypeError}
                "Object ~A (a ~A) has no `~A' method."
                x (class-name (py-class-of x)) '{__contains__}))))

(defmethod py-hash ((x symbol))
  ;; Returning (py-hash (symbol-name x)) leads to infinite recursion.
  (break "py-hash of SYMBOL")
  (or (get x 'py-hash)
      (let ((hash (py-string.__hash__ (symbol-name x))))
        (setf (get x 'py-hash) hash))))

(defmethod py-repr ((x (eql nil)))
  (declare (ignorable x))
  "#<the symbol NIL>")

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
            "Object ~A (a ~A) has no `~A' method."
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
      (py-raise '{TypeError} "Expected an integer~@[ >= ~A~]; got: ~S" min x))))

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
    (checking-reader-conditionals
     #+allegro :hash-table
     #+(or cmu sbcl lispworks) :level
     #-(or allegro cmu lispworks sbcl) :level))

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


(defun py-string->symbol  (x &optional (package #+cmu(load-time-value (find-package :clpython.user))
                                                #-cmu #.(find-package :clpython.user))) ;; cmu errs on this
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
                  (__iter__    (class.attr-no-magic x.cls '{__iter__}))
                  (__getitem__-unb (unless __iter__
                                     (class.attr-no-magic x.cls '{__getitem__})))
                  (__getitem__ (when __getitem__-unb
                                 (bind-val __getitem__-unb x x.cls))))
             
             ;; TODO: binding __getitem__ using __get__ is not done at all yet.
             
             #+(or)(warn "GET-PY-ITERATE-FUN ~A (a ~A)~% -> __iter__ = ~A; __getitem = ~A"
                         x x.cls __iter__ __getitem__)
             
             (cond (__iter__ ;; Preferable, use __iter__ to retrieve x's iterator
                    (let* ((iterator     (py-call (bind-val __iter__ x x.cls)))
                           (iterator.cls (py-class-of iterator))
                           (next-meth-unbound
                            (or (class.attr-no-magic iterator.cls '{next})
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
                      (flet ((using-__iter__ ()
                               (handler-case (values (if next-meth-bound
                                                         (py-call next-meth-bound)
                                                       (funcall next-meth-unbound iterator)))
                                 ({StopIteration} () (values nil nil))
                                 (:no-error (val)  (values val t)))))
                        #'using-__iter__)))
                   
                   (__getitem__ ;; Fall-back: call __getitem__ with successive integers
                    (let ((index 0))
                      ;; (excl:named-function (:py-iterate-fun using __getitem__)
                      (flet ((using-__getitem__ ()
                               (handler-case (values (py-call __getitem__ index))
                                 ({IndexError} ()
                                   ;; can even happen index = 0: then it's an empty sequence
                                   (values nil nil))
                                 (:no-error (val)
                                   (incf index)
                                   (values val t)))))
                        #'using-__getitem__)))
                   (t
                    (py-raise '{TypeError} "Iteration over non-sequence: ~A." x))))))

(defgeneric map-over-object (func object)
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
	 (__iter__    (class.attr-no-magic x.cls '{__iter__})))
    
    (if __iter__
	(py-call __iter__ x)
      (let ((f (get-py-iterate-fun x)))
	(make-iterator-from-function :func f)))))


(defvar *stream-softspaces* (make-hash-table :test 'eq)
  "should a space be printed in front of next arg?")

(defun py-print (dest items comma?)
  (declare (special *habitat*))
  (setf dest (cond (dest)
                   (*habitat* (clpython:habitat-stdout *habitat*))
                   (t
                    ;; e.g. when a __del__ method tries to print after habitat is gone
                    *standard-output*)))
  (assert dest)
  (let* ((*print-pretty* nil)
         (write-func (if (streamp dest)
                         (lambda (s) (write-string s dest)) ;; No FINISH-OUTPUT: hurts performance
                       (attr dest '{write})))
         (softspace-val (if (streamp dest)
                            (or (gethash dest *stream-softspaces*) 0)
                          (handler-case 
                              (attr dest '{softspace})
                            (error (c)
                              (warn "PY-PRINT: error retrieving attribute `softspace' of ~S: ~S"
                                    dest c)
                              0))))
         (softspace-p (py-val->lisp-bool softspace-val))
         (last-char-written nil))
    (loop
        for x in items
        for i from 0
        do (when (or (plusp i)
                     (and (zerop i) softspace-p))
             (py-call write-func " "))
           (let ((s (if (stringp x) x (py-str-string x))))
             (check-type s string)
             (py-call write-func s)
             (when (plusp (length s))
               (setf last-char-written (aref s (1- (length s)))))))
    (unless comma?
      (py-call write-func #.(string #\Newline)))
    (let* ((printed-newline-already (or (not comma?)
                                        (and last-char-written
                                             (char= last-char-written #\Newline))))
           (must-print-newline-next-time (py-bool (not printed-newline-already))))
      (if (streamp dest)
          (setf (gethash dest *stream-softspaces*) must-print-newline-next-time)
        (setf (attr dest '{softspace}) must-print-newline-next-time))))
  (values))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional attributes

(defgeneric attr-hook (object attr)
  (:documentation "Hook to define additional attributes for (non-Python) objects")
  (:method (object attr)
           (declare (ignore object attr))
           nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make Lisp streams available to Python

(defmethod attr-hook ((stream stream) (attr (eql '{write})))
  (if (output-stream-p stream)
      (flet ((stream.write (string)
               (check-type string string)
               (write-string string stream)))
        #'stream.write)
    (call-next-method)))
