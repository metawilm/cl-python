(in-package :python)

(defparameter *sys.modules* (make-dict)) ;; not a namespace, as keys can be dotted names
(defparameter *sys.path* (make-py-list-from-list (list ".")))

(defparameter *__builtin__-module* nil)
(defparameter *__builtin__-module-namespace* nil)


(defun def-std-module (name &optional bindings)
  (assert (symbolp name))
  (let* ((ns (make-namespace :builtins t))
	 (mod (make-py-module :namespace ns)))
    (py-dict-sethash *sys.modules* (symbol-name name) mod)
    (namespace-bind ns '__name__ (symbol-name name))
    (loop for (k v) in bindings
	do (namespace-bind ns k v))
    (values mod ns)))
