(in-package :user)

(if (find-package :python)
    (delete-package :python))

(defpackage :python
  (:documentation "An implementation of the Python programming language.")
  (:use :common-lisp)
  (:export :python-type :python-object
	   :python-class :def-python-class :python-class-p
	   :python-class-instance-p
	   :python-function :make-function :python-function-p
	   :python-unbound-method :make-unbound-method
	   :python-bound-method :make-bound-method
	   :python-none  :*None*
	   :set-attribute :get-attribute
	   :def-class-method :call :dir :dir2
	   :test)
  (:shadow ))

(in-package :python)

(defun compy ()
  (with-compilation-unit ()
    (dolist (f '("classes" "exceptions" "pythonic" "magicmeths" "builtin-classes"
		 "call" "builtin-funcs" "builtin-types" "mathops"
		 "descriptors" "attributes" "pyeval"
		 #+(or)"gener" "parsepython" "walk" "repl"))
      (excl::compile-file-if-needed f)
      (load f)))
  'done)


(defpackage :python-builtin-functions
  (:nicknames :pyb)
  (:use )
  (:export 
   :__import__ :abs :apply :callable :chr :cmp :coerce
   :compile :delattr :dir :divmod :eval :execfile :filter
   :getattr :globals :hasattr :hash :hex :id :input :intern :isinstance
   :issubclass :iter :len :locals :map :ord :pow :range
   :raw_input :reduce :reload :repr :round :setattr :sorted
   :sum #+(or):type :unichr :vars :zip))

(defpackage :python-builtin-types
  (:nicknames :pyt)
  (:use )
  (:export
   :bool :complex :dict :enumerate :float :int :list :long
   :slice :str :tuple :xrange
   ;;; todo:
   :classmethod :staticmethod :property :object :type :unicode))

(defpackage :python-module-sys
  (:use)
  (:export :modules))