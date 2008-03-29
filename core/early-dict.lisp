;; -*- package: clpython -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Python dictionary

(in-package :clpython)

(defparameter *magic-methods*
    '#.(sort (loop for s being the external-symbol in :clpython.user
                 for sn = (symbol-name s)
                 when (and (> (length sn) 0)
                           (or (char= (char sn 0) #\_)
                               (string= sn "next")))
                 collect s)
             #'string<
             :key #'symbol-name)
    "All methods names for the form `__xxx__' that have special ~
meaning in the language, plus the (oddly named) method name `next'.
All these symbols are in the clpython.user package.")

(defconstant +attr-cache-ix-property+ 'attr-cache-ix)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +dikt-hash-vector-size+ 32))

(deftype attr-hash-type () '(integer 0 #.+dikt-hash-vector-size+))

(defparameter *attr-cache-iter* -1)
(declaim (type fixnum *attr-cache-iter*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *dict-optimize-settings* 
      '(optimize (speed 3) (safety 1) (debug 0))))

(defun get-sym-attr-hash (x)
  (declare #.*dict-optimize-settings*
           (symbol x))
  (let ((plist (symbol-plist (the symbol x))))
    (if (eq (pop plist) +attr-cache-ix-property+)
        (pop plist)
      ;; Not set as first property, or not set at all
      (or (get x +attr-cache-ix-property+)
          (setf (get x +attr-cache-ix-property+)
            (mod (the fixnum (incf (the fixnum *attr-cache-iter*))) +dikt-hash-vector-size+))))))

(defun set-magic-methods-attr-hashes ()
  (mapcar #'get-sym-attr-hash *magic-methods*)
  (let ((sym '{__init__})
        (hash 14)) ;; hardcoded, yeah
    (assert (= (get-sym-attr-hash sym) hash) ()
      "Sanity check for attribute hash codes failed: hash of ~S is ~A, expected ~A."
      sym (get-sym-attr-hash sym) hash)))

(set-magic-methods-attr-hashes)

(defun get-attr-hash (x)
  "X either symbol, string, or user-defined subclass of string"
  (declare #.*dict-optimize-settings*)
  (typecase x
    (symbol (get-sym-attr-hash x))
    (string (let ((s (find-symbol x :clpython.user)))
              (and s (get-sym-attr-hash s))))
    (t (mod (py-hash x) +dikt-hash-vector-size+))))

;;; Py-dict 

(defstruct (dikt (:type vector))
  (vector (make-array +dikt-hash-vector-size+))
  (hash-table nil))

;; Invariant:
;;  VECTOR is NIL <=> HASH-TABLE is used.
;;  VECTOR not NIL => all keys are symbols (representing Python strings)

(defclass py-dict ()
  ((dikt :accessor py-dict-dikt :initform (make-dikt))))

(defun make-dict ()
  (declare #.*dict-optimize-settings*)
  (make-instance 'py-dict))

(defun dict-map (x func)
  (check-type x py-dict)
  (check-type func function)
  (dikt-map (py-dict-dikt x) func))

(defun py-==->lisp-val (x y)
  (declare #.*dict-optimize-settings*
           (notinline py-==))
  (if (and (stringp x) (stringp y))
      (if (and (char= (aref x 0) (aref y 0))
               (string= x y))
          t
        nil)
    (not (zerop (the fixnum (py-== x y))))))

#+(or)
(define-compiler-macro py-==->lisp-val (x y)
  ;; A user can define a class whose instances are not equal to themselves.
  ;; Because that leads to problems in hashtable lookup, and this function
  ;; is used a lot for hash tables, we assume EQ => py-==.
  `(let ((.x ,x)
         (.y ,y))
     (if (eq .x .y)
         t
       (/= (the fixnum (py-== .x .y)) 0))))
	 
;; The functions PY-HASH and PY-== are needed for bootstrapping; the
;; definitions here are temporarily.

(unless (fboundp 'py-hash)
  (defun py-hash (x)
    (declare (ignore x))
    0))

(unless (fboundp 'py-==)
  (defun py-== (x y)
    "Returns Python equality value: True = 1, False = 0."
    #+(or)(warn "old py-== ~A ~A" x y)
    (if (equalp x y) 1 0)))

(defun make-py-hash-table ()
  #+(or allegro lispworks)
  (make-hash-table :test 'py-==->lisp-val :hash-function 'py-hash)
  #-(or allegro lispworks)
  (erorr "Creating python dict not suported in this environment."))

(defun convert-dikt (x)
  (declare #.*dict-optimize-settings*)
  #+(or)(break "converting dikt ~A" x)
  (do ((d (make-py-hash-table))
       (i 0 (1+ i)))
      ((= i +dikt-hash-vector-size+)
       (setf (dikt-hash-table x) d
             (dikt-vector x) nil)
       x)
    (let ((entries (svref (dikt-vector x) i)))
      (do ((entry (pop entries) (pop entries)))
          ((null entry) )
        (setf (gethash (car entry) d) (cdr entry))))))

;;; DIKT functions

(defun dikt-del (x key)
  ;; Returns whether found.
  (declare #.*dict-optimize-settings*)
  (cond ((dikt-hash-table x) ;; use hash-table
         (remhash key (dikt-hash-table x)))
        ((symbolp key)
         (dikt-del-sym-with-hash x key (get-sym-attr-hash key)))
        (t
         (dikt-del (convert-dikt x) key))))

(defun dikt-del-sym-with-hash (x key-sym hash)
  ;; Assumes: HASH == (get-sym-attr-hash KEY-SYM)
  (declare #.*dict-optimize-settings*
           (symbol key-sym)
           (type attr-hash-type hash))
  (if (dikt-hash-table x)
      (dikt-del x key-sym)
    (let ((entries (svref (dikt-vector x) hash)))
      (do ((entry (pop entries) (pop entries))
           (prev-entry nil entry))
          ((null entry) nil)
        (when (eq (car entry) key-sym)
          (if prev-entry
              (setf (cdr prev-entry) entries)
            (setf (svref (dikt-vector x) hash) entries))
          (return-from dikt-del-sym-with-hash t))))))

(define-compiler-macro dikt-del (&whole whole x key)
  ;; Try to move lookup of symbol hash code to compile time or load time.
  (let ((key.sym (cond ((stringp key) (intern key :clpython.user))
                       ((and (listp key) 
                             (eq (car key) 'quote)
                             (symbolp (second key)))
                        (second key)))))
    (cond ((null key.sym) 
           whole)
          ((member key.sym *magic-methods*)
           `(dikt-del-sym-with-hash ,x ',key.sym ,(get-sym-attr-hash key.sym)))
          (t
           `(dikt-del-sym-with-hash ,x ',key.sym (load-time-value (get-sym-attr-hash ',key.sym)))))))

;;; DIKT-SET

(defun dikt-set (x key val)
  (declare #.*dict-optimize-settings*)
  (cond ((dikt-hash-table x) ;; use hash-table
         (setf (gethash key (dikt-hash-table x)) val))
        ((symbolp key)
         (dikt-set-sym-with-hash x key (get-sym-attr-hash key) val))
        (t
         (dikt-set (convert-dikt x) key val))))

(defun dikt-set-sym-with-hash (x key-sym hash val)
  ;; Assumes: HASH == (get-sym-attr-hash KEY-SYM)
  (declare #.*dict-optimize-settings*
           (symbol key-sym)
           (type attr-hash-type hash))
  (if (dikt-hash-table x)
      (dikt-set x key-sym val)
    (let ((entries (svref (dikt-vector x) hash)))
      (do ((entry (pop entries) (pop entries)))
          ((null entry) (push (cons key-sym val) (svref (dikt-vector x) hash)))
        (when (eq key-sym (car entry))
          (setf (cdr entry) val)
          (return-from dikt-set-sym-with-hash))))))

(define-compiler-macro dikt-set (&whole whole x key val)
  ;; Try to move lookup of symbol hash code to compile time or load time.
  (let ((key.sym (cond ((stringp key) (intern key :clpython.user))
                       ((and (listp key) 
                             (eq (car key) 'quote)
                             (symbolp (second key)))
                        (second key)))))
    (cond ((null key.sym) 
           whole)
          ((member key.sym *magic-methods*)
           `(dikt-set-sym-with-hash ,x ',key.sym ,(get-sym-attr-hash key.sym) ,val))
          (t
           `(dikt-set-sym-with-hash ,x ',key.sym (load-time-value (get-sym-attr-hash ',key.sym)) ,val)))))
  
;;; DIKT-GET

(defun dikt-get (x key)
  "Returns value or NIL"
  (declare #.*dict-optimize-settings*)
  (cond ((dikt-hash-table x) ;; use hash-table
         (gethash key (dikt-hash-table x)))
        ((symbolp key)
         (dikt-get-sym-with-hash x key (get-sym-attr-hash key)))
        (t 
         (let ((user-sym (and (stringp key)
                              (find-symbol key #.(find-package :clpython.user)))))
           (when user-sym
             (return-from dikt-get (dikt-get-sym-with-hash x user-sym (get-sym-attr-hash user-sym)))))
         (warn "dikt-get: non-{symbol,string} lookup key ~S for symbol-only dikt?" key)
         (dikt-get (convert-dikt x) key))))

(defun dikt-get-sym-with-hash (x key-sym hash)
  ;; Assumes: HASH == (get-sym-attr-hash KEY-SYM)
  (declare #.*dict-optimize-settings*
           (symbol key-sym)
           (type attr-hash-type hash))
  (if (dikt-hash-table x)
      (dikt-get x key-sym)
    (let ((entries (svref (dikt-vector x) hash)))
      (do ((entry (pop entries) (pop entries)))
          ((null entry) )
        (when (eq key-sym (car entry))
          (return-from dikt-get-sym-with-hash (cdr entry)))))))

(define-compiler-macro dikt-get (&whole whole x key)
  ;; Try to move lookup of symbol hash code to compile time or load time.
  (let ((key.sym (cond ((stringp key) (intern key :clpython.user))
                       ((and (listp key) 
                             (eq (car key) 'quote)
                             (symbolp (second key)))
                        (second key)))))
    (cond ((null key.sym) 
           whole)
          ((member key.sym *magic-methods*)
           `(dikt-get-sym-with-hash ,x ',key.sym ,(get-sym-attr-hash key.sym)))
          (t
           `(dikt-get-sym-with-hash ,x ',key.sym (load-time-value (get-sym-attr-hash ',key.sym)))))))

;;; Other methods

(defun dikt-equal (d1 d2)
  (declare #.*dict-optimize-settings*
           (notinline py-==))
  (cond ((eq d1 d2) 
         t)
        
        ((and (dikt-hash-table d1)
              (dikt-hash-table d2))
         (let ((ht1 (dikt-hash-table d1))
               (ht2 (dikt-hash-table d2)))
           (and (= (the fixnum (hash-table-count ht1)) (the fixnum (hash-table-count ht2)))
                (loop for d1.k being the hash-key in ht1 using (hash-value d1.v)
                    for d2.v = (gethash d1.k ht2)
                    when (not d2.v) return nil
                    when (py-val->lisp-bool (py-!= d1.v d2.v)) return nil
                    finally (return t)))))
        
        ((not (or (dikt-hash-table d1) (dikt-hash-table d2)))
         (do ((i 0 (1+ i)))
             ((= i +dikt-hash-vector-size+) t)
           (let ((entries.1 (svref (dikt-vector d1) i))
                 (entries.2 (svref (dikt-vector d2) i)))
             (unless (= (length entries.1) (length entries.2))
               (return-from dikt-equal nil))
             (do* ((entry.1 (pop entries.1) (pop entries.1))
                   (entry.2 (and entry.1 (assoc (car entry.1) entries.2 :test 'eq))))
                 ((null entry.1) )
               (unless (eq (cdr entry.2) (cdr entry.1))
                 (return-from dikt-equal nil))))))
        
        (t (unless (dikt-hash-table d1)
             (rotatef d1 d2))
           ;; Now d1 is the hash-table; d2 is the vector
           (and (= (dikt-count d2) (dikt-count d1))
                (loop for d1.k being the hash-key in (dikt-hash-table d1)
                    always (or (symbolp d1.k) (stringp d1.k)))
                (loop for d1.k being the hash-key in (dikt-hash-table d1)
                    using (hash-value d1.v)
                    always (py-== (dikt-get d2 d1.k) d1.v))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +hash-table-iterator-indefinite-extent+
      #+allegro nil
      #+lispworks nil
      #-(or allegro lispworks) nil
    "Whether the iterator created by WITH-HASH-TABLE-ITERATOR has indefinite extent.
This is not guaranteed due to the following in ANSI:
  It is unspecified what happens if any of the implicit interior state of
  an iteration is returned outside the dynamic extent of the
  with-hash-table-iterator form such as by returning some closure over
  the invocation form."))

(defun hashtable-nth-entry (ht i)
  "Return the I-th entry in the hashtable (I >= 0), using its internal order."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop for ix from 0 to i
      for k being the hash-key in ht
      when (= ix i) return (values k (gethash k ht))
      finally (return (values nil nil))))

(defmacro def-dikt-iter-func (name args doc key-var val-var action-form)
  ;; BLOCK is named 'iter.
  ;; Modifying the value of the most recently returned item by the iterator
  ;; should be okay; removing it is not yet supported.
  `(defun ,name (.d ,@args)
     (declare (optimize (debug 3) (safety 3) (speed 0)))
     ,doc
     (if (dikt-hash-table .d)
         ,(if +hash-table-iterator-indefinite-extent+
              `(with-hash-table-iterator (.next-func (dikt-hash-table .d))
                 (lambda ()
                   (block iter
                     (multiple-value-bind (.ret .k .v)
                         (.next-func)
                       (declare (ignorable .k .v))
                       (let (,@(when key-var `((,key-var .k)))
                             ,@(when val-var `((,val-var .v))))
                         (when .ret
                           ,action-form))))))
            `(let ((.i 0)
                   (.ht (dikt-hash-table .d)))
               (lambda ()
                 (block iter
                   (multiple-value-bind (.k .v)
                       (hashtable-nth-entry .ht .i)
                     (declare (ignorable .k .v))
                     (let (,@(when key-var `((,key-var .k)))
                           ,@(when val-var `((,val-var .v))))
                       (when (and .k .v)
                         (incf .i)
                         ,action-form)))))))
       (let ((.hash-ix 0)
             (.nth-item 0))
         (lambda ()
           (loop named iter
               do (let* ((.entries (svref (dikt-vector .d) .hash-ix))
                         (.entry-var (nth .nth-item .entries)))
                    (cond (.entry-var
                           (incf .nth-item)
                           (let (,@(when key-var `((,key-var (car .entry-var))))
                                 ,@(when val-var `((,val-var (cdr .entry-var)))))
                             ,action-form))
                          ((< (incf .hash-ix) +dikt-hash-vector-size+)
                           (setf .nth-item 0))
                          (t (return-from iter nil))))))))))

(def-dikt-iter-func dikt-iter-keys-func ()
    "Returns the keys one by one, and NIL when finished."
  key nil (return-from iter key))

(def-dikt-iter-func dikt-iter-values-func ()
    "Returns the keys one by one, and NIL when finished."
  nil val (return-from iter val))

(def-dikt-iter-func dikt-iter-keys-values-func ()
    "Returns the key, value as two values; and NIL when finished."
  key val (return-from iter (values key val)))

(defun dikt-map (d func)
  (let ((mapper (dikt-map-func d func)))
    (loop while (funcall mapper))))
   
(def-dikt-iter-func dikt-map-func (func)
  "Func, a function of two args, is called for every key-value pair."
  key val (progn (funcall func key val)
                 t))

(defun dikt-count (d)
  (if (dikt-hash-table d)
      (hash-table-count (dikt-hash-table d))
    (do ((i 0 (1+ i))
         (count 0))
        ((= i +dikt-hash-vector-size+) count)
      (incf count (length (svref (dikt-vector d) i))))))

(defun dikt-clear (d)
  (when (dikt-hash-table d)
    (setf (dikt-hash-table d) nil))
  (cond ((dikt-vector d)
         (do ((i 0 (1+ i)))
             ((= i +dikt-hash-vector-size+) )
           (setf (svref d i) nil)))
        (t (setf (dikt-vector d)
             (make-array +dikt-hash-vector-size+))))
  d)
