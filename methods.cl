(in-package :python)

;;; Registering methods and attributes of built-in classes


;; Class-specific methods, both magic and non-magic, like the `clear'
;; methods of dicts, and the `append' method of lists, and the
;; `__repr__' method of all objects, are stored in a hashtable, where
;; the key is the method name as a symbol, and the value is an alist
;; where the class is the key, and the method for that class is the value.

(defparameter *builtin-class-attr/meths* (make-hash-table :test #'eq))

(defmethod register-bi-class-attr/meth ((class class) (meth-name symbol) attr-value)
  "Puts the method in the hash table. TYPE is either :ATTR or :METH."
  (when (eq attr-value '__and__)
    (warn "Registering __and__"))
  (let* ((alist (gethash meth-name *builtin-class-attr/meths*))
	 (kons  (cons class attr-value))
	 (assval (assoc class alist)))
    (when assval
      (warn "builtin-class-methods already had a func for method ~A of class ~A"
	    meth-name class))
    (push kons alist)
    (setf (gethash meth-name *builtin-class-attr/meths*) alist)))

(defmethod lookup-bi-class-attr/meth ((class class) (meth-name symbol))
  "Returns METHOD, TYPE. Both are NIL if not found."
  (cdr (assoc class (gethash meth-name *builtin-class-attr/meths*))))

(defmethod view-register ((meth-name symbol))
  (gethash meth-name *builtin-class-attr/meths*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attributes of instances (not methods)

(defclass bi-class-attribute (builtin-instance)
  ((func :initarg :func))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'bi-class-attribute))

(defun make-bi-class-attribute (func)
  (make-instance 'bi-class-attribute :func func))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods
;;
;; Normal methods; classmethods; staticmethods.

;; bound method

(defclass bound-method (builtin-instance)
  ((func   :initarg :func)
   (object :initarg :object))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'bound-method))

(defun make-bound-method (&key func object)
  (make-instance 'bound-method :func func :object object))

(defmethod print-object ((x bound-method) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (with-slots (func object) x
      (format stream ":func ~A ~_:object ~A" func object))))

(defmethod __name__ ((x bound-method))
  (internal-get-attribute (slot-value x 'func) '__name__))

(register-bi-class-attr/meth (find-class 'bound-method) '__name__
			     (make-bi-class-attribute #'__name__))
			     

(defmethod __repr__ ((x bound-method))
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t :type t)
      (with-slots (func object) x
	(pprint-logical-block (s nil)
	  (format s ":func ~A ~_:object ~A"
		  (call-attribute-via-class func '__repr__)
		  (call-attribute-via-class object '__repr__)))))))


;; unbound method
;; XXX remove unbound methods, just as CPython did 20050117

(defclass unbound-method (builtin-instance)
  ((func   :initarg :func)
   (class  :initarg :class))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'unbound-method))

(defun make-unbound-method (&key func class)
  (make-instance 'unbound-method :func func :class class))

(defmethod print-object ((x unbound-method) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (pprint-logical-block (stream nil)
      (with-slots (func class) x
	(format stream ":func ~A ~_:class ~A" func class)))))

(defmethod __repr__ ((x unbound-method))
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t :type t)
      (with-slots (func class) x
	(pprint-logical-block (s nil)
	  (format s ":func ~A ~_:class ~A"
		  (call-attribute-via-class func '__repr__)
		  (call-attribute-via-class class '__repr__)))))))

(defmethod __name__ ((x unbound-method))
  (internal-get-attribute (slot-value x 'func) '__name__))

(register-bi-class-attr/meth (find-class 'unbound-method) '__name__
			     (make-bi-class-attribute #'__name__))
			     

;; static method

(defclass static-method (builtin-instance)
  ((func :initarg :func))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'static-method))

(defun make-static-method (func)
  (make-instance 'static-method :func func))

(defmethod print-object ((x static-method) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (pprint-logical-block (stream nil)
      (with-slots (func) x
	(format stream ":func ~A" func)))))

(defmethod static-method-__new__ ((cls class) func)
  (assert (subtypep cls 'static-method))
  (make-instance cls :func func))

(register-bi-class-attr/meth (find-class 'static-method) '__new__
			     (make-static-method #'static-method-__new__))

(defmethod __name__ ((x static-method))
  (internal-get-attribute (slot-value x 'func) '__name__))

(register-bi-class-attr/meth (find-class 'static-method) '__name__
			     (make-bi-class-attribute #'__name__))
			     


;; class method

(defclass class-method (builtin-instance)
  ((func :initarg :func))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'class-method))

(defmethod class-method-__new__ ((cls class) func)
  (assert (subtypep cls 'class-method))
  (make-instance cls :func func))

(register-bi-class-attr/meth (find-class 'class-method) '__new__
			     (make-static-method #'class-method-__new__))


(defmethod __name__ ((x class-method))
  (internal-get-attribute (slot-value x 'func) '__name__))

(register-bi-class-attr/meth (find-class 'class-method) '__name__
			     (make-bi-class-attribute #'__name__))
			     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A special kind of built-in function

(defclass bi-function-accepting-kw-args (builtin-instance)
  ((func :initarg :func))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'bi-function-accepting-kw-args))

(defun make-bi-function-accepting-kw-args (func)
  (make-instance 'bi-function-accepting-kw-args :func func))

