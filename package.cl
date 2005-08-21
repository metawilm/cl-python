(when (eq *package* (find-package :python))
  (error "You must load PACKAGE.CL in another package than PYTHON, because ~
          the PYTHON package will be deleted and then recreated."))

(in-package :user)

(when (find-package :python)
  (delete-package :python))


(defpackage :python
  (:documentation "An implementation Python in Common Lisp.")
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


(defpackage :python-builtin-functions
  (:nicknames :pybf)
  (:use )
  (:export 
   :__import__ :abs :apply :callable :chr :cmp :coerce :compile
   :delattr :dir :divmod :eval :execfile :filter :getattr :globals
   :hasattr :hash :hex :id :input :intern :isinstance :issubclass
   :iter :len :locals :map :max :min :oct :ord :pow :range :raw_input
   :reduce :reload :repr :round :setattr :sorted :sum :unichr :vars
   :zip ))


(defpackage :python-builtin-types
  (:nicknames :pybt)
  (:use )
  (:export
   :basestring :bool :complex :dict :enumerate :float :int :list :long
   :slice :str :super :tuple :xrange :classmethod :staticmethod :property
   :object :type :unicode
   
   ;; BUILTIN-TYPES.CL adds the names of the exceptions classes to the
   ;; exported symbols.
   ))

(defpackage :python-builtin-values
  (:nicknames :pybv)
  (:use )
  (:export
   :None :Ellipsis :True :False :NotImplemented))


