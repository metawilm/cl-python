(in-package :python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function
;; 
;;  - Lisp functions
;;  - Python functions
;;
;; There are two types of Python functions:
;; 
;; BUILTIN-FUNCTION represents the functions present in de __builtin__
;; module (implemented as Lisp functions)
;; 
;; USER-DEFINED-FUNCTION is used for representing all functions
;; defined while running Python.

(defclass python-function (builtin-instance)
  ((ast             :initarg :ast   
		    :documentation "AST of the function code")
   (params          :initarg :params
		    :documentation "Formal parameters, e.g. '((a b) ((c . 3)(d . 4)) args kwargs)")
   (call-rewriter   :initarg :call-rewriter
		    :documentation "Function that normalizes actual arguments")
   (namespace       :initarg :namespace
		    :documentation "The namespace in which the function code is ~
                                    executed (the lexical scope -- this is not ~
                                    func.__dict__)")
   (name :initarg :name :initform "<no name>")
   
   ;; update internal data structure that keeps track of function calling
   #+(or)(id :initform (new-python-function-defined)))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'python-function))


;; namespace reuse optimization hack

#+(or)
(defvar *python-function-counter* 0)

#+(or)
(defun new-python-function-defined ()
  (prog1 *python-function-counter*
    (incf *python-function-counter*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A special kind of built-in function

(defclass lisp-function-accepting-kw-args (python-function) ;; WWW was: builtin-instance
  ((func :initarg :func))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'lisp-function-accepting-kw-args))

(defun make-lisp-function-accepting-kw-args (func)
  (make-instance 'lisp-function-accepting-kw-args :func func))


;; Lambda

(defclass py-lambda-function (python-function)
  ()
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-lambda-function))

;; TODO: __new__, __init__


;; Regular function

(defclass user-defined-function (python-function)
  ()
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'user-defined-function))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function returning a generator

;;; XXX should subclass from function or something

(defclass python-function-returning-generator (builtin-instance)
  ((call-rewriter :initarg :call-rewriter)
   (generator-creator :initarg :generator-creator)
   (name :initarg :name))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'python-function-returning-generator))

