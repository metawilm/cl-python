(in-package :python)

(defparameter *sys.modules* (make-namespace))
(defparameter *sys.path* nil)

(defparameter *__builtin__-module* nil)
(defparameter *__builtin__-module-namespace* nil)


(defmacro make-std-module (name &optional bindings)
  `(let* ((ns (make-namespace :builtins t))
	  (mod (make-module :name ,(string name) :namespace ns)))
     (namespace-bind *sys.modules* ',name mod)
     (namespace-bind ns '__name__ (string ',name))
     ,@(loop for (k v) in bindings
	   collect `(namespace-bind ns ',k ,v))
     (values mod ns)))


;; The Python `sys' module: keeps status of the Python interpreter

;; XXX check that this is evaluated at the right time 

(let ((sys-path-list (make-py-list-from-list '("")))) ;; "" is current dir
  (make-std-module sys
		   ((path    sys-path-list)
		    (modules *sys.modules*)))
  (setf *sys.path* sys-path-list))


;; The `__builtin__' module: built-in functions, types and some special variables

(multiple-value-bind (mod ns)
    (make-std-module __builtin__)

  (setf *__builtin__-module* mod
	*__builtin__-module-namespace* ns)
    
  (do-external-symbols (s 'python-builtin-functions)
    (namespace-bind ns (symbol-name s) (symbol-function s)))
  
  (do-external-symbols (s 'python-builtin-types)
    (if (boundp s) ;; check needed, as some symbols are TODO
	(namespace-bind ns (symbol-name s) (symbol-value s))))
  
  (loop for (key val) in
	`((None ,*None*)(Ellipsis ,*Ellipsis*)(NotImpemented ,*NotImplemented*)(True ,*True*)(False ,*False*))
      do (namespace-bind ns key val))
  
  (loop for (name . exc) in *python-exceptions*
      do (namespace-bind ns name exc)))

