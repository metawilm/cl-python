(in-package :python)

;; The Python Reference Manual says:
;; 
;;   3.3.2.2 Implementing Descriptors
;; 
;;   In general, a descriptor is an object attribute with ``binding
;;   behavior'', one whose attribute access has been overridden by
;;   methods in the descriptor protocol: \method{__get__()},
;;   \method{__set__()}, and \method{__delete__()}.  If any of those
;;   methods are defined for an object, it is said to be a descriptor.
;; 
;; A descriptor is called a "data descriptor" iff it implements
;; `__set__'. Whether this is the case influences the attribute lookup
;; order.

(defun descriptor-p (x)
  (or (internal-get-attribute x '__get__)
      (internal-get-attribute x '__set__)
      (internal-get-attribute x '__delete__)))

(defun data-descriptor-p (x)
  "Returns DES-P, __SET__"
  (multiple-value-bind (val found)
      (internal-get-attribute x '__set__)
    (if found
	(values t val)
      nil)))

;; Property is a built-in class; it's instances implement all three of
;; the `descriptor' methods (even if not all three arguments are
;; filled by arguments given in the constructor).
;; 
;; Usage is like:
;; 
;;   class C:
;;     def getx(self): ..
;;     def setx(self, val): ..
;;     def delx(self): ..
;;     x = property(getx, setx, delx, "doc")
;; 
;; Relevant documentation on how CPython behaves:
;;  http://mail.python.org/pipermail/python-dev/2003-May/035791.html

(defclass py-property (builtin-object)
  ((get    :initarg :get)
   (set    :initarg :set)
   (delete :initarg :delete)
   (doc    :initarg :doc))
  (:documentation "The PROPERTY built-in class. User-defined `properties' ~
                   are instances of this class.")
  (:metaclass builtin-class))


;; Because this might be called directly from within Python, it's a `pyfun'.
#+(or) ;; not yet: have to think about name binding etc
(def-pyfun make-property (&key (fget *None*) (fset *None*) (fdel *None*) (doc *None*))
  (make-instance 'py-property :get fget :set fset :delete fdel :doc doc))


(defmethod fget ((x py-property))
  (slot-value x 'get))

(defmethod fset ((x py-property))
  (slot-value x 'set))

(defmethod fdel ((x py-property))
  (slot-value x 'delete))

(register-bi-class-attr/meth (find-class 'py-property) 'fget (make-bi-class-attribute #'fget))
(register-bi-class-attr/meth (find-class 'py-property) 'fset (make-bi-class-attribute #'fset))
(register-bi-class-attr/meth (find-class 'py-property) 'fdel (make-bi-class-attribute #'fdel))
