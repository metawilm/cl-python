(in-package :python)

(defparameter *sys.modules* (make-dict)) ;; not a namespace, as keys can be dotted names
(defparameter *sys.path* (make-py-list-from-list (list ".")))

(defparameter *__builtin__-module* nil)
(defparameter *__builtin__-module-namespace* nil)


(defmacro make-std-module (name &optional bindings)
  `(let* ((ns (make-namespace :builtins t))
	  (mod (make-py-module :namespace ns)))
     (py-dict-sethash *sys.modules* (string ',name) mod)
     (namespace-bind ns '__name__ (string ',name))
     ,@(loop for (k v) in bindings
	   collect `(namespace-bind ns ',k ,v))
     (values mod ns)))
