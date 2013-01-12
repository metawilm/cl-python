;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.OPERATOR -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module.operator)

;; Documentation (comments and docstring) are taken from
;;  <http://docs.python.org/library/operator.html#module-operator>.

(eval-when (:compile-toplevel :execute)

  (defun magicify (meth)
    (let ((name (symbol-name meth)))
      (intern (format nil "__~A~A_"
                      (string-downcase meth)
                      (if (equal (aref name (1- (length name))) #\_) "" #\_)) #.*package*)))

  (defun sym->op (op)
    (with-standard-io-syntax
      (let ((name (format nil "~A-~A" '#:py op)))
        (or (find-symbol name :clpython)
            (break "No such symbol in pkg clpython: ~A" name)))))

  (defun sym->iop (op)
    (with-standard-io-syntax
      (let ((name (format nil "~A-~A=" '#:py op)))
        (or (find-symbol name :clpython)
            (break "No such symbol in pkg clpython: ~A" name)))))
  
  (defmacro def-magic-twins (name params &body body)
    `(progn (defun ,name ,params ,@body)
            (defun ,(magicify name) ,params (,name ,@params))
            (eval-when (:load-toplevel :execute)
              (export ',name #.*package*)
              (export ',(magicify name) #.*package*))))
  )

;; Binary, magic

#.`(progn ,@(loop for (meth op) in
                  '((lt <)
                    (le <=)
                    (eq ==)
                    (ne !=)
                    (ge >=)
                    (gt >)
                    (add +)
                    (and_ &)
                    (div /)
                    (floordiv //)
                    (lshift <<)
                    (mod %)
                    (mul *)
                    (or_ |\||)
                    (pow **)
                    (rshift >>)
                    (sub  -)
                    (truediv /t/)
                    (xor ^)
                    (concat +) ;; ?
                    (contains contains))
                collect `(def-magic-twins ,(intern (string-downcase meth)) (x y) (,(sym->op op) x y))))

;; Unary, magic

#.`(progn ,@(loop for (meth op) in 
                  '((abs     abs)
                    (not     not)
                    (inv     unary-~)
                    (invert  unary-~)
                    (neg     unary--)
                    (pos     unary-+)
                    (index index))
                collect `(def-magic-twins ,(intern (string-downcase meth)) (x) (,(sym->op op) x))))

;; Unary, no magic

#.`(progn ,@(loop for (meth op) in
                  '((truth bool))
                for name = (intern (string-downcase meth))
                collect `(defun ,name (x) (,(sym->op op) x))
                collect `(eval-when (:load-toplevel :execute)
                           (export ',name))))

;; Binary, no magic
#.`(progn ,@(loop for (meth op) in
                  '((is_ is)
                    (is_not is-not))
                for name = (intern (string-downcase meth))
                collect `(defun ,name (x y) (,(sym->op op) x y))
                collect `(eval-when (:load-toplevel :execute)
                           (export ',name))))

(defun |countOf| (a b)
  "Return the number of occurrences of b in a."
  (loop for x in (clpython:py-iterate->lisp-list a) count (clpython:py-==->lisp-val x b)))

(eval-when (:load-toplevel :execute)
  (export '|countOf|))

(def-magic-twins |delitem| (a b)
  "Remove the value of a at index b."
  (setf (clpython:py-subs a b) nil))

(def-magic-twins |delslice| (a b c)
  "Delete the slice of a from index b to index c-1."
  (setf (clpython:py-subs a (clpython:make-slice b c nil)) nil))

(def-magic-twins |getitem| (a b)
  "Return the value of a at index b."
  (clpython:py-subs a b))

(def-magic-twins |getslice| (a b c)
  "Return the slice of a from index b to index c-1."
  (clpython:py-subs a (clpython:make-slice b c nil)))

(defun |indexOf| (a b)
  "Return the index of the first of occurrence of b in a."
  (or (position b a :test 'clpython:py-==->lisp-val)
      (py-raise '{ValueError} "Item ~A not in sequence ~A." b a)))

(eval-when (:load-toplevel :execute)
  (export '|indexOf|))

(def-magic-twins |repeat| (a b)
  "Return a * b where a is a sequence and b is an integer."
  (clpython:py-* a b))

(eval-when (:load-toplevel :execute)
  (export '|repeat|))

;; sequenceIncludes(...) : Alias for contains().
;; Deprecated since version 2.0: Use contains() instead.

(def-magic-twins |setitem| (a b c)
  "Set the value of a at index b to c."
  (setf (clpython:py-subs a b) c))

(def-magic-twins |setslice| (a b c v)
  "Set the slice of a from index b to index c-1 to the sequence v."
  (setf (clpython:py-subs a (clpython:make-slice b c nil)) v))

;; "Many operations have an 'in-place' version. The following functions
;; provide a more primitive access to in-place operators than the usual
;; syntax does; for example, the statement
;;   x += y
;; is equivalent to
;;   x = iadd(x, y).
;; 
;; Another way to put it is to say that
;;   z = iadd(x, y) is equivalent to
;; the compound statement
;;   z = x; z += y."

#.`(progn ,@(loop for (name op) in
                  '((|iadd| +)
                    (|iand| &)
                    (|iconcat| +)
                    (|idiv| /) ;; check __future__.division
                    (|ifloordiv| //)
                    (|ilshift| <<)
                    (|imod| %)
                    (|imul| *)
                    (|ior| |\||)
                    (|ipow| *)
                    (|irepeat| *)
                    (|irshift| >>)
                    (|isub| -)
                    (|itruediv| /t/)
                    (|ixor| ^))
                collect `(def-magic-twins ,name (x y) (or (,(sym->iop op) x y)
                                                          (,(sym->op op) x y)))))
 

;;; Not-very-reliable "duck type" tests:

;; isCallable(obj)
;; Deprecated since version 2.0: Use the callable() built-in function instead.

;; isMappingType(obj)
;; Returns true if the object obj supports the mapping interface. This
;; is true for dictionaries and all instance objects defining
;; __getitem__().

;; isNumberType(obj)
;; Returns true if the object obj represents a number. This is true
;; for all numeric types implemented in C.

;; isSequenceType(obj)
;; Returns true if the object obj supports the sequence protocol. This
;; returns true for all objects which define sequence methods in C,
;; and for all instance objects defining __getitem__().

(defun |attrgetter| (&rest args)
"Return a callable object that fetches attr from its operand. If more than one attribute
is requested, returns a tuple of attributes. After,
   f = attrgetter('name'),
the call f(b) returns b.name.
After,
   f = attrgetter('name', 'date'),
the call f(b) returns (b.name, b.date).

The attribute names can also contain dots; after
  f = attrgetter('date.month'),
the call f(b) returns b.date.month."
  (dolist (x args) (check-type x string))
  (error "todo"))

(eval-when (:load-toplevel :execute)
  (export '|attrgetter|))

(defun |itemgetter| (&rest items)
  "Return a callable object that fetches item from its operand using the operand’s
__getitem__() method. If multiple items are specified, returns a tuple of lookup
values. Equivalent to:

    def itemgetter(*items):
        if len(items) == 1:
            item = items[0]
            def g(obj):
                return obj[item]
        else:
            def g(obj):
                return tuple(obj[item] for item in items)
        return g

The items can be any type accepted by the operand’s __getitem__() method. Dictionaries
accept any hashable value. Lists, tuples, and strings accept an index or a slice."
  (cond ((null items)
         (py-raise '{ValueError} "One or more items must be provided."))
        ((not (cdr items))
         (named-function :itemgetter
             (lambda (obj &aux (item (car items)))
               (py-subs obj item))))
        (t
         (named-function :itemgetter
           (lambda (obj)
             (make-tuple-from-list (loop for item in items collect (py-subs obj item))))))))

(eval-when (:load-toplevel :execute)
  (export '|itemgetter|))

(defun |methodcaller| (name args)
  "Return a callable object that calls the method name on its operand. If additional
arguments and/or keyword arguments are given, they will be given to the method as well.
After f = methodcaller('name'), the call f(b) returns b.name().
After f = methodcaller('name', 'foo', bar=1), the call f(b) returns b.name('foo', bar=1)."
  (declare (ignore name args))
  (error "todo"))

(eval-when (:load-toplevel :execute)
  (export '|methodcaller|))
