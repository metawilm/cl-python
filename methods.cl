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
  (let* ((alist (gethash meth-name *builtin-class-attr/meths*))
	 (kons  (cons class attr-value))
	 (assval (assoc class alist)))
    (when assval
      (warn "builtin-class-methods already had a func for method ~A of class ~A"
	    meth-name class))
    (if alist
	(push kons alist)
      (setf (gethash meth-name *builtin-class-attr/meths*) (list kons)))))

(defmethod lookup-bi-class-attr/meth ((class class) (meth-name symbol))
  "Returns METHOD, TYPE. Both are NIL if not found."
  (cdr (assoc class (gethash meth-name *builtin-class-attr/meths*))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attributes of instances (not methods)

(defclass bi-class-attribute (builtin-instance)
  ((func :initarg :func))
  (:metaclass builtin-class))

(defun make-bi-class-attribute (func)
  (make-instance 'bi-class-attribute :func func))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods
;;
;; Normal methods; classmethods; staticmethods.

(defclass bound-method (builtin-instance)
  ((func   :initarg :func)
   (object :initarg :object))
  (:metaclass builtin-class))

(defun make-bound-method (&key func object)
  (make-instance 'bound-method :func func :object object))

(defclass unbound-method (builtin-instance)
  ((func   :initarg :func)
   (class  :initarg :class))
  (:metaclass builtin-class))

(defun make-unbound-method (&key func class)
  (make-instance 'unbound-method :func func :class class))

(defclass class-method (builtin-instance)
  ((func :initarg :func))
  (:metaclass builtin-class))

(defclass static-method (builtin-instance)
  ((func :initarg :func))
  (:metaclass builtin-class))

(defun make-static-method (func)
  (make-instance 'static-method :func func))

