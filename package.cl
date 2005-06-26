(when (eq *package* (find-package :python))
  (error "You must load PACKAGE.CL in another package than PYTHON, because ~
          the PYTHON package will be deleted and then recreated."))

(in-package :user)

(when (find-package :python)
  (delete-package :python))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package definitions

(defpackage :python
  (:documentation "An implementation of the Python programming language.")
  (:use :common-lisp)
  #+(or)(:export :python-type :python-object
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
    (dolist (f '("parser" "lexer" "pyprint" "walk" ;; AST generation and manipulation
		 ;; classes" "exceptions" "pythonic" "functions" 
		 ;; "methods" "magicmeths" "builtin-classes" "formatstring"
		 ;; "call" "builtin-funcs" "builtin-types" "mathops"
		 ;; "descriptors" "attributes" "modules" "pyeval"
		 ;; "parsepython" "walk" "gener" "repl" "trace"
		 ;; "bi-modules" "pyprint"
		 ))
      #+allegro(excl::compile-file-if-needed f)
      #-allegro(compile-file (concatenate 'string f ".cl"))
      (load f)))
  (values))

#+(or)
(defun compy ()
  (with-compilation-unit ()
    (dolist (f '("classes" "exceptions" "pythonic" "functions" 
		 "methods" "magicmeths" "builtin-classes" "formatstring"
		 "call" "builtin-funcs" "builtin-types" "mathops"
		 "descriptors" "attributes" "modules" "pyeval"
		 "parsepython" "walk" "gener" "repl" "trace"
		 "bi-modules" "pyprint"))
      #+allegro(excl::compile-file-if-needed f)
      #-allegro(compile-file (concatenate 'string f ".cl"))
      (load f)))
  'done)


(defpackage :python-builtin-functions
  (:nicknames :pyb)
  (:use )
  (:export 
   :__import__ :abs :apply :callable :chr :cmp :coerce :compile
   :delattr :dir :divmod :eval :execfile :filter :getattr :globals
   :hasattr :hash :hex :id :input :intern :isinstance :issubclass
   :iter :len :locals :map :max :min :oct :ord :pow :range :raw_input
   :reduce :reload :repr :round :setattr :sorted :sum #+(or):type
   :unichr :vars :zip))

(defpackage :python-builtin-types
  (:nicknames :pyt)
  (:use )
  (:export
   :bool :complex :dict :enumerate :float :int :list :long :slice :str
   :super :tuple :xrange :classmethod :staticmethod :property :object
   :type :unicode))

(defpackage :python-module-sys
  (:use)
  (:export :modules))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Features to be included
;;  
;;  Some of them aid debugging of Lisp or Python code, but slow down
;;  execution.

(defvar *track-exception-stack* t)
;; Slightly violates Python semantics, because exception names are
;; evaluated immediately, not only in case an exception does happen.
