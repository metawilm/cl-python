;;; CLPython: Some interesting code fragments


;; This `Exception' class is a Python class, a regular Python object,
;; and a Lisp condition type: CLOS is impressive (although this is
;; "non-portable").

(defclass Exception (python-object condition)
  ((args :initarg :args :documentation "Exception arguments (as tuple)"))
  (:documentation "The Exception type.")
  (:metaclass python-type))