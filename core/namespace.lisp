;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)

;;; Namespaces

(defparameter *debug-no-locals-dict* nil
  "A hack to analyze compiler output; don't use.")

(defparameter *debug-dummy-outer-namespace* nil
  "A debugging aid; don't use.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +clpython-namespaces-are-classes+ t)
  (register-feature :clpython-namespaces-are-classes +clpython-namespaces-are-classes+))

(defun my-make-load-form (x)
  (declare (ignorable x))
  ;; Work around Allegro 8.2 beta bug, where make-load-form results are not evaluated
  ;; correctly at fasl load time.
  (values :unavailable-compile-time-namespace))

(defgeneric ns.expand-with (namespace body-form environment))
(defgeneric ns.read-form (namespace name))
(defgeneric ns.write-form (namespace name val-form))
(defgeneric ns.write-runtime-form (namespace name-form val-form))
(defgeneric ns.del-form (namespace name))
(defgeneric ns.locals-form (namespace))

(defmacro with-namespace ((ns &key (define-%locals (not *debug-no-locals-dict*))
                                   (define-%globals nil))
                          &body body &environment e)
  ;; XXX not always %locals needed (e.g. when no call expr in body)
  (let ((new-body `(locally ,@body)))
    (when define-%globals
      (assert define-%locals () "define-%globals implies define-%locals")
      (setf new-body `(flet ((%globals () (%locals)))
                        (declare (ignorable #'%globals))
                        ,new-body)))
    (when define-%locals
      (setf new-body `(flet ((%locals () ,(ns.locals-form ns)))
                        (declare (ignorable #'%locals))
                        ,new-body)))
    `(with-pydecl ((:namespace ,ns))
       ,(ns.expand-with ns new-body e))))

(defun ns.find-form (func ns &rest args)
  (declare (dynamic-extent args))
  (check-type func function)
  (loop named lookup
      for try-ns = ns then (ns.parent try-ns)
      for form = (cond ((not try-ns) nil)
                       ((and (not (eq ns try-ns))
                             (eq (ns.scope try-ns) :class))
                        ;; class namespaces can't be closed over / referenced like function namespaces
                        nil)
                       (t (apply func try-ns args)))
      while try-ns
      when form
      do (return-from lookup (values form try-ns))
      finally (if *debug-dummy-outer-namespace*
		  (return-from lookup `(ns-dummy-form ,func ,ns ,@args))
		(error "No namespace for handling ~A ~A, from ~A." func args ns))))

(defmacro namespace-set (key val &environment e)
  (check-type key symbol)
  (let ((ns (get-pydecl :namespace e)))
    (ns.write-form ns key val)))

(defmacro namespace-set-runtime (key-form val &environment e)
  (let ((ns (get-pydecl :namespace e)))
    (ns.write-runtime-form ns key-form val)))

(defmacro namespace-get (key &environment e)
  (check-type key symbol)
  (let ((ns (get-pydecl :namespace e)))
    (ns.read-form ns key)))

(defmacro module-namepace-get (key &environment e)
  (check-type key symbol)
  (let ((ns (get-module-namespace e)))
    (ns.read-form ns key)))

#+clpython-namespaces-are-classes
(defclass namespace ()
  ((parent :accessor ns.parent :initarg :parent :initform nil)
   (scope :accessor ns.scope :initarg :scope 
          :initform #+(or)(error "missing arg :scope") :missing-initform ;; hack for my-make-load-form
          )))

#-clpython-namespaces-are-classes
(defstruct (namespace (:conc-name ns.))
  parent scope)

(defmethod print-object ((namespace namespace) stream)
  (print-unreadable-object (namespace stream :type t)
    (pprint-logical-block (stream nil)
      (unless (member 'ns.attributes (trace))
        (format stream "~{~S ~S~^ ~:_~}" (ns.attributes namespace))))))

(defgeneric ns.attributes (namespace)
  (:method-combination append))

(defmethod ns.attributes append ((namespace namespace))
  (list :context (loop for ns = namespace then (ns.parent ns)
                     while ns
                     collect (cons (ns.scope ns) (type-of ns)))))

(defmethod make-load-form ((ns namespace) &optional environment)
  (declare (ignore environment))
  (my-make-load-form ns))
  
(defmethod ns.expand-with ((ns namespace) body-form environment)
  (declare (ignorable ns environment))
  body-form)

#+clpython-namespaces-are-classes
(progn
  (defclass excl-ns (namespace)
    ((excluded-names :accessor ns.excluded-names :initarg :excluded-names)))
  
  (defmethod ns.read-form :around ((ns excl-ns) (s symbol))
    (unless (member s (ns.excluded-names ns))
      (call-next-method)))
  
  (defmethod ns.write-form :around ((ns excl-ns) (s symbol) val-form)
    (declare (ignore val-form))
    (unless (member s (ns.excluded-names ns))
      (call-next-method)))
  
  (defmethod ns.del-form :around ((ns excl-ns) (s symbol))
    (unless (member s (ns.excluded-names ns))
      (call-next-method))))

(defmethod ns.attributes append ((namespace excl-ns))
  (list :excluded-names (ns.excluded-names namespace)))
  
#+clpython-namespaces-are-classes
(progn
  (defclass mapping-ns (namespace)
    ((mapping-form :accessor ns.mapping-form :initarg :mapping-form)))
  
  (defun make-mapping-ns (&rest args)
    (apply #'make-instance 'mapping-ns args)))

#-clpython-namespaces-are-classes
(defstruct (mapping-ns (:include namespace) (:conc-name ns.))
  mapping-form)

(defmethod make-load-form ((ns mapping-ns) &optional environment)
  (declare (ignore environment))
  (my-make-load-form ns))

(defmethod ns.read-form ((ns mapping-ns) (s symbol))
  `(or (py-subs ,(ns.mapping-form ns) ,(symbol-name s))
       ,(when (ns.parent ns)
          (ns.read-form (ns.parent ns) s))))

(defmethod ns.write-form ((ns mapping-ns) (s symbol) val-form)
  `(setf (py-subs ,(ns.mapping-form ns) ,(symbol-name s)) ,val-form))

(defmethod ns.del-form ((ns mapping-ns) (s symbol))
  `(prog1 (py-subs ,(ns.mapping-form ns) ,(symbol-name s))
     (setf (py-subs ,(ns.mapping-form ns) ,(symbol-name s)) nil)))
 
(defmethod ns.locals-form ((ns mapping-ns))
  (ns.mapping-form ns))

#+clpython-namespaces-are-classes
(progn 
  (defclass mapping-w/excl-ns (mapping-ns excl-ns)
    ())
  (defun make-mapping-w/excl-ns (&rest args)
    (apply #'make-instance 'mapping-w/excl-ns args)))

#-clpython-namespaces-are-classes
(progn 
  (defstruct (mapping-w/excl-ns (:include mapping-ns) (:conc-name ns.))
    excluded-names)

  (defmethod ns.read-form :around ((ns mapping-w/excl-ns) (s symbol))
    (unless (member s (ns.excluded-names ns))
      (call-next-method)))
  
  (defmethod ns.write-form :around ((ns mapping-w/excl-ns) (s symbol) val-form)
    (declare (ignore val-form))
    (unless (member s (ns.excluded-names ns))
      (call-next-method)))
  
  (defmethod ns.del-form :around ((ns mapping-w/excl-ns) (s symbol))
    (unless (member s (ns.excluded-names ns))
      (call-next-method))))

(defmethod make-load-form ((ns mapping-w/excl-ns) &optional environment)
  (declare (ignore environment))
  (my-make-load-form ns))

#+clpython-namespaces-are-classes
(defclass let-ns (namespace)
  ((names :accessor ns.names :initarg :names :initform :missing-initform))) ;; hack

#-clpython-namespaces-are-classes
(defstruct (let-ns (:include namespace) (:conc-name ns.))
  names)

(defmethod make-load-form ((ns let-ns) &optional environment)
  (declare (ignore environment))
  (my-make-load-form ns))

(defmethod ns.read-form ((ns let-ns) (s symbol))
  (when (member s (ns.names ns)) s))

(defmethod ns.write-form ((ns let-ns) (s symbol) val-form)
  (when (member s (ns.names ns))
    `(setf ,s ,val-form)))

(defmethod ns.del-form ((ns let-ns) (s symbol))
  (when (member s (ns.names ns))
    `(prog1 ,s (setf ,s nil))))

(defmethod ns.expand-with ((ns let-ns) body-form environment)
  (declare (ignore environment))
  `(let ,(ns.names ns)
     ,body-form))

(defmethod ns.locals-form ((ns let-ns))
  `(make-locals-dict ',(ns.names ns) (list ,@(loop for name in (ns.names ns)
                                                 collect (ns.read-form ns name)))))

(defun make-locals-dict (name-list value-list)
  (loop with ht = (make-eq-hash-table "locals-dict")
      for name in name-list
      for val in value-list
      unless (null val) ;; unbound var
      do (setf (gethash name ht) val)
      finally (return ht)))

#+clpython-namespaces-are-classes
(progn
  (defclass let-w/locals-ns (let-ns)
    ((locals-names :accessor ns.locals-names :initarg :locals-names :initform :missing-initform) ;; hack
     (let-names :accessor ns.let-names :initarg :let-names :initform :missing-initform)))
  
  (defun make-let-w/locals-ns (&rest args)
    (apply #'make-instance 'let-w/locals-ns args)))

#-clpython-namespaces-are-classes
(defstruct (let-w/locals-ns (:include let-ns) (:conc-name ns.))
  locals-names let-names)

(defmethod make-load-form ((ns let-w/locals-ns) &optional environment)
  (declare (ignore environment))
  (my-make-load-form ns))

(defmethod ns.attributes append ((x let-w/locals-ns))
  (list :locals-names (mapcar #'symbol-name (ns.locals-names x)) :let-names (ns.let-names x)))

(defmethod ns.expand-with ((ns let-w/locals-ns) body-form environment)
  (declare (ignore environment))
  `(let ,(ns.let-names ns)
     ,body-form))

(defmethod ns.locals-form ((ns let-w/locals-ns))
  `(make-locals-dict ',(ns.locals-names ns) (list ,@(loop for name in (ns.names ns)
                                                       collect (ns.read-form ns name)))))

#+clpython-namespaces-are-classes
(progn
  (defclass hash-table-ns (namespace)
    ((dict-form :accessor ns.dict-form :initarg :dict-form :initform :dict-missing #+(or) (error "required")))) ;;hack
  
  (defun make-hash-table-ns (&rest args)
    (apply #'make-instance 'hash-table-ns args)))

#-clpython-namespaces-are-classes
(defstruct (hash-table-ns (:include namespace) (:conc-name ns.))
  dict-form)

(defmethod make-load-form ((ns hash-table-ns) &optional environment)
  (declare (ignore environment))
  (my-make-load-form ns))
           
(defmethod ns.read-form ((ns hash-table-ns) (s symbol))
  `(or (gethash ',s ,(ns.dict-form ns))
       ,(when (ns.parent ns)
          (ns.read-form (ns.parent ns) s))))

(defmethod ns.write-form ((ns hash-table-ns) (s symbol) val-form)
  `(setf (gethash ',s ,(ns.dict-form ns)) ,val-form))

(defmethod ns.write-runtime-form ((ns hash-table-ns) name-form val-form)
  `(setf (gethash ,name-form ,(ns.dict-form ns)) ,val-form))
       
(defmethod ns.del-form ((ns hash-table-ns) (s symbol))
  `(remhash ',s ,(ns.dict-form ns)))

(defmethod ns.locals-form ((ns hash-table-ns))
  (ns.dict-form ns))

#+clpython-namespaces-are-classes
(progn
  (defclass hash-table-w/excl-ns (hash-table-ns excl-ns)
    ())
  (defun make-hash-table-w/excl-ns (&rest args)
    (apply #'make-instance 'hash-table-w/excl-ns args)))

#-clpython-namespaces-are-classes
(progn 
  (defstruct (hash-table-w/excl-ns (:include hash-table-ns) (:conc-name ns.))
    excluded-names)
  
  (defmethod ns.read-form :around ((ns hash-table-w/excl-ns) (s symbol))
    (unless (member s (ns.excluded-names ns))
      (call-next-method)))
  
  (defmethod ns.write-form :around ((ns hash-table-w/excl-ns) (s symbol) val-form)
    (declare (ignore val-form))
    (unless (member s (ns.excluded-names ns))
      (call-next-method)))
  
  (defmethod ns.del-form :around ((ns hash-table-w/excl-ns) (s symbol))
    (unless (member s (ns.excluded-names ns))
      (call-next-method))))

(defmethod ns.attributes append ((x hash-table-w/excl-ns))
  (list :excluded-names (ns.excluded-names x)))

(defmethod make-load-form ((ns hash-table-w/excl-ns) &optional environment)
  (declare (ignore environment))
  (my-make-load-form ns))

#|| ;; no use case for this yet
(defclass package-ns (namespace)
  ((package :accessor ns.package :initarg :package)
   (incl-builtins :accessor ns.incl-builtins :initarg :incl-builtins :initform nil)))

(defmethod package-ns-intern ((ns package-ns) (s symbol))
  (intern (symbol-name s) (ns.package ns)))

(defmethod ns.read-form ((ns package-ns) (s symbol))
  (let ((ps (package-ns-intern ns s)))
    `(or (and (boundp ',ps) (symbol-value ',ps))
         ,(when (ns.incl-builtins ns)
            `(builtin-value ',s)))))

(defmethod ns.write-form ((ns package-ns) (s symbol) val-form)
  (let ((ps (package-ns-intern ns s)))
    `(setf (symbol-value ',ps) ,val-form)))

(defmethod ns.del-form ((ns package-ns) (s symbol))
  (let ((ps (package-ns-intern ns s)))
    `(prog1 (boundp ',ps)
       (makunbound ',ps))))

(defmethod ns.locals-form ((ns package-ns))
  `(loop for x being the symbols in ,(ns.package ns)
       when (and (eq (symbol-package x)  ,(ns.package ns))
                 (boundp x))
       collect (progn (warn "name: ~A" x) x) into names
       and collect (symbol-value x) into values
       finally (return (make-locals-dict names values))))
||#

#+clpython-namespaces-are-classes
(progn
  (defclass builtins-ns (namespace)
    ())
  
  (defun make-builtins-ns (&rest args)
    (apply #'make-instance 'builtins-ns args)))

#-clpython-namespaces-are-classes
(defstruct (builtins-ns (:include namespace)))

(defmethod make-load-form ((ns builtins-ns) &optional environment)
  (declare (ignore environment))
  (my-make-load-form ns))

(defun make-builtins-namespace ()
  (make-builtins-ns :scope :builtins))

(defmethod ns.read-form ((ns builtins-ns) (s symbol))
  (when (builtin-value s)
    `(load-time-value (builtin-value ',s))))

(defmethod ns.locals-form ((ns builtins-ns))
  `(error "Namespace ~A does not have locals()." ,ns))

(defmethod ns.write-form ((ns builtins-ns) (s symbol) val-form)
  (declare (ignore val-form))
  `(error "Namespace ~A not writable (attempt to set ~A)." ,ns ,s))

(defmethod ns.del-form ((ns builtins-ns) (s symbol))
  `(error "Namespace ~A not writable (attempt to delete ~A)." ,ns ',s))


(defgeneric get-module-namespace (context)
  (:method (environment)
           #+allegro (check-type environment sys::augmentable-environment)
           (get-module-namespace (get-pydecl :namespace environment)))
  (:method ((namespace namespace))
           (loop for ns = namespace then (ns.parent ns)
               while ns
               when (member (ns.scope ns) '(:module :exec-globals))
               return ns
               finally (break "No global namespace found among parents, for: ~A" namespace))))
           
                         
