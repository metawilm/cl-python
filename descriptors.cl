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
      (getattr-of-class x '__set__)
    (if (and found (not (eq val *None*)))
	(values t val)
      nil)))
