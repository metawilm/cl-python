;; -*- package: clpython -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Python dictionary - early definition

(in-package :clpython)


;;; Special handling of `magic methods'

(defparameter *magic-methods*
    '#.(sort (loop for s being the external-symbol in :clpython.user
                 for sn = (symbol-name s)
                 when (and (> (length sn) 0)
                           (or (char= (schar sn 0) #\_)
                               (string= sn "next")))
                 collect s)
             #'string<
             :key #'symbol-name)
    "All methods names for the form `__xxx__' that have special ~
meaning in the language, plus the (oddly named) method name `next'.
All these symbols are in the clpython.user package.")

(defconstant +num-dynamic-magic-methods+ 25)
(defparameter *num-magic-methods* 
    (+ (length *magic-methods*) +num-dynamic-magic-methods+))
(defparameter *next-vacant-magic-method-ix* (length *magic-methods*))

(defconstant +magic-ix-prop+ 'magic-ix)

(defun set-magic-method-props ()
  (loop for s in *magic-methods*
      for i from 0
      do (setf (get s +magic-ix-prop+) i)))

(set-magic-method-props)

(defun get-magic-method-index (meth)
  "Returns: index.
For unseen symbols, might create index.
Non-{string,symbol} names never have a magic ix."
  (typecase meth
    (symbol (or (get meth +magic-ix-prop+)
                ;; If vacant method ix available: use for this symbol
                (when *next-vacant-magic-method-ix*
                  (prog1 
                      (setf (get meth +magic-ix-prop+) *next-vacant-magic-method-ix*)
                    #+(or)(warn "Making ~A next magic method (~A)" 
                                meth *next-vacant-magic-method-ix*)
                    (when (>= (incf *next-vacant-magic-method-ix*) *num-magic-methods*)
                      (setf *next-vacant-magic-method-ix* nil))))))
    (string (let ((x (find-symbol meth :clpython.user)))
              (and x (get x +magic-ix-prop+))))))


;;; Py-dict 

(defconstant +dikt-vector-size+ 4)

(defstruct (dikt (:type vector))
  (vector-key (make-array +dikt-vector-size+))
  (vector-val (make-array +dikt-vector-size+))
  (hash-table nil)
  (magic-methods-bitarr (make-array *num-magic-methods* :element-type 'bit
                                    :initial-element 0)))

;; Invariants:
;;  VECTOR is NIL if HASH-TABLE is used.
;;  HASH-TABLE is used if more than 

(defclass py-dict ()
  ((dikt :accessor py-dict-dikt :initform (make-dikt))))

(defun make-dict ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (make-instance 'py-dict))

(defun dict-map (x func)
  (check-type x py-dict)
  (check-type func function)
  (dikt-map (py-dict-dikt x) func))

(defun py-==->lisp-val (x y)
  (declare (optimize (speed 3) (safety 1) (debug 0))
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


;;; DIKT functions

(defun dikt-del (x key)
  ;; Returns whether found
  (declare (optimize (safety 3) (debug 3)))
  (cond ((dikt-hash-table x) ;; use hash-table
         
         ;; Work around Allegro 8.0 bug: sometimes wrong return value for REMHASH.
         #+allegro
         (let ((count-before (hash-table-count (dikt-hash-table x))))
           (remhash key (dikt-hash-table x))
           (let ((count-after (hash-table-count (dikt-hash-table x))))
             (return-from dikt-del (< count-after count-before))))
         #-allegro
         (return-from dikt-del (remhash key (dikt-hash-table x))))
        
        (t ;; use vector
         (let ((c0 (typecase key
                     (string (when (> (length key) 0)
                               (schar key 0)))
                     (symbol (setf key (symbol-name key))
                             (schar key 0)))))
           (do ((i 0 (1+ i)))
               ((>= i +dikt-vector-size+) (return-from dikt-del nil))
             (let ((k (svref (dikt-vector-key x) i)))
               (when (and k
                          (if c0
                              (and (char= (schar k 0) (schar key 0))
                                   (string= k key))
                            (or (eq k key)
                                (py-val->lisp-bool (py-== k key)))))
                 (setf (svref (dikt-vector-key x) i) nil
                       (svref (dikt-vector-val x) i) nil)
                 (return-from dikt-del t))))))))

(defun make-py-hash-table ()
  (make-hash-table :test 'py-==->lisp-val :hash-function 'py-hash))

(defun convert-dikt (x)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (do* ((d (make-py-hash-table))
        (i 0 (1+ i)))
      ((>= i +dikt-vector-size+) d)
    (let ((k (svref (dikt-vector-key x) i)))
      (when k
        (setf (gethash k d) (svref (dikt-vector-val x) i)
              (svref (dikt-vector-key x) i) nil
              (svref (dikt-vector-val x) i) nil)))
    (setf (dikt-hash-table x) d)))

(defun dikt-set (x key val) ;; &optional magic-ix)
  (declare (optimize (speed 3) (safety 1) (debug 0)))

  ;; Update bitarr, in case of string/symbol
  (whereas ((bitarr (dikt-magic-methods-bitarr x))
            (magic-ix (or (get-magic-method-index key)
                          (setf (dikt-magic-methods-bitarr x) nil))))
    (setf (sbit bitarr magic-ix) 1))
  
  #+(or)
  (typecase key
    (string (get-magic-method-index key))
    (symbol (get key +magic-ix-prop+))
    (t      ;; remove bitarr upon insertion of non-string key
     (setf (dikt-magic-methods-bitarr x) nil)))

  (cond ((dikt-hash-table x) ;; use hash-table
         (return-from dikt-set (setf (gethash key (dikt-hash-table x)) val)))

        ((not (or (stringp key) (symbolp key)))
         (progn (convert-dikt x)
                (dikt-set x key val))) ;;(setf (gethash key (dikt-hash-table x)) val)))
        
        (t ;; use vector, if enough room left
         (let ((c0 (typecase key
                     (string (when (> (length key) 0)
                               (schar key 0)))
                     (symbol (setf key (symbol-name key))
                             (schar key 0)))))
           (do ((i 0 (1+ i))
                (ins-pos nil))
               
               ((>= i +dikt-vector-size+)
                (if ins-pos
                    (setf (svref (dikt-vector-key x) ins-pos) key
                          (svref (dikt-vector-val x) ins-pos) val)
                  (progn (convert-dikt x)
                         (setf (gethash key (dikt-hash-table x)) val)))
                (return-from dikt-set nil))
             
             (let ((k (svref (dikt-vector-key x) i)))
               (cond (k
                      (when (if c0
                                (and (char= (schar k 0) (schar key 0))
                                     (string= k key))
                              (or (eq k key)
                                  (py-val->lisp-bool (py-== k key))))
                        (setf (svref (dikt-vector-val x) i) val)
                        (return-from dikt-set)))
                     (t
                      (unless ins-pos
                        (setf ins-pos i))))))))))

(defun dikt-get (x key) ;; &optional (magic-ix (get-magic-method-index key)))
  (declare (optimize (speed 3) (safety 1) (debug 0)))

  ;; Only in case of symbols
  (when (symbolp key)
    (whereas ((bitarr (dikt-magic-methods-bitarr x))
              (magic-ix (get key +magic-ix-prop+)))
      (declare (type (integer 0 10000) magic-ix))
      (when (zerop (sbit bitarr magic-ix))
        (return-from dikt-get nil))))
  
  (cond ((dikt-hash-table x) ;; use hash-table
         (return-from dikt-get (gethash key (dikt-hash-table x))))
        
        (t ;; use vector
         (let ((c0 (typecase key
                     (string (when (> (length (the string key)) 0)
                               (schar key 0)))
                     (symbol (setf key (symbol-name key))
                             (schar key 0)))))
           (do ((i 0 (1+ i)))
               ((>= i +dikt-vector-size+) (return-from dikt-get nil))
             (declare (fixnum i))
             (whereas ((k (svref (dikt-vector-key x) i)))
               (when (if c0
                         (and (char= (schar k 0) (schar key 0))
                              (string= k key))
                       (or (eq k key)
                           (py-==->lisp-val k key)))
                 (return-from dikt-get
                   (svref (dikt-vector-val x) i)))))))))

#+(or)
(define-compiler-macro dikt-get (&whole whole x key &optional (magic-ix nil magic-ix-p))
  ;; Calculate magic-ix at compile time.
  (declare (ignore magic-ix))
  (if (and (typep key '(or string symbol))
           (not magic-ix-p))
      `(dikt-get ,x ,key ,(get-magic-method-index key))
    whole))


(defun dikt-equal (d1 d2)
  (declare (optimize (speed 3) (safety 1) (debug 0))
           (notinline py-==))
  (cond ((eq d1 d2) (return-from dikt-equal t))
        ((and (dikt-hash-table d1)
              (dikt-hash-table d2))
         (let ((ht1 (dikt-hash-table d1))
               (ht2 (dikt-hash-table d2)))
           (and (= (the fixnum (hash-table-count ht1)) (the fixnum (hash-table-count ht2)))
                (loop for d1.k being the hash-key in ht1 using (hash-value d1.v)
                    for d2.v = (gethash d1.k ht2)
                    when (py-val->lisp-bool (py-!= d1.v d2.v)) return nil
                    finally (return-from dikt-equal t)))))

        ((not (or (dikt-hash-table d1) (dikt-hash-table d2)))
         (unless (= (loop for k across (dikt-vector-key d1) count k)
                    (loop for k across (dikt-vector-key d2) count k))
           (return-from dikt-equal nil))
         (loop with d1.count fixnum = 0
             with d1.keys = (dikt-vector-key d1)
             with d1.vals = (dikt-vector-val d1)
             for i from 0 below +dikt-vector-size+
             for d1.key = (svref d1.keys i)
             when d1.key
             do (incf d1.count)
                (loop with d1.val = (svref d1.vals i)
                    with d2.keys = (dikt-vector-key d2)
                    with d2.vals = (dikt-vector-key d2)
                    for j from 0 below +dikt-vector-size+
                    for d2.key = (svref d2.keys i)
                    for d2.val = (svref d2.vals i)
                    when (or (eq d1.key d2.key)
                             (py-val->lisp-bool (py-== d1.key d2.key)))
                    unless (or (eq d1.val d2.val)
                               (py-val->lisp-bool (py-== d1.val d2.val)))
                    do (return-from dikt-equal nil))
             finally (return-from dikt-equal t)))
        
        (t (unless (dikt-hash-table d1)
             (rotatef d1 d2))
           ;; D1 is the HASH-TABLE
           ;; D2 is the vector
           (cond ((/= (loop for k across (dikt-vector-key d2) count k)
                      (the fixnum (hash-table-count (dikt-hash-table d1))))
                  (return-from dikt-equal nil))
                 ((= (hash-table-count (dikt-hash-table d1))
                     0
                     (loop for i from 0 below +dikt-vector-size+
                         count (svref (dikt-vector-key d2) i)))
                  (return-from dikt-equal t)))
           (loop for d1.key being the hash-key in (dikt-hash-table d1)
               using (hash-value d1.val)
               do (loop
                      with d2.keys = (dikt-vector-key d2)
                      with d2.vals = (dikt-vector-val d2)
                      with d2-matching-key-found
                      for i from 0 below +dikt-vector-size+
                      for d2.key = (svref d2.keys i)
                      for d2.val = (svref d2.vals i)
                      when d2.key
                      when (or (eq d1.key d2.key)
                               (py-val->lisp-bool (py-== d1.key d2.key)))
                      do (setf d2-matching-key-found t)
                      unless (or (eq d1.val d2.val)
                                 (py-val->lisp-bool (py-== d1.val d2.val)))
                      do (return-from dikt-equal nil)
                      finally (if d2-matching-key-found
                                  (return-from dikt-equal t)
                                (return-from dikt-equal nil)))))))

(defun dikt-iter-keys-func (d)
  "Returns the keys one by one, and NIL when finished."
  (if (dikt-hash-table d)
      (with-hash-table-iterator (next-func (dikt-hash-table d))
        (lambda () (multiple-value-bind (ret key val) 
                       (next-func)
                     (declare (ignore val))
                     (when ret key))))
    (let ((i -1))
      (lambda ()
        (loop with keys = (dikt-vector-key d)
            for j fixnum from (1+ i) below +dikt-vector-size+
            for key = (svref keys j)
            when key
            do (setf i j)
            return key)))))

(defun dikt-iter-values-func (d)
  "Returns the keys one by one, and NIL when finished."
  (if (dikt-hash-table d)
      (with-hash-table-iterator (next-func (dikt-hash-table d))
        (lambda () (multiple-value-bind (ret key val) 
                       (next-func)
                     (declare (ignore key))
                     (when ret val))))
    (let ((i -1))
      (lambda ()
        (loop with keys = (dikt-vector-key d)
            for j fixnum from (1+ i) below +dikt-vector-size+
            for key = (svref keys j)
            when key
            do (setf i j)
            return (svref (dikt-vector-val d) j))))))

(defun dikt-iter-keys-values-func (d)
  "Returns key, value as two values, and NIL when finished."
  (if (dikt-hash-table d)
      (with-hash-table-iterator (next-func (dikt-hash-table d))
        (lambda () (multiple-value-bind (ret key val) 
                       (next-func)
                     (when ret
                       (values key val)))))
    (let ((i -1))
      (lambda ()
        (loop with keys = (dikt-vector-key d)
            for j fixnum from (1+ i) below +dikt-vector-size+
            for key = (svref keys j)
            when key
            do (setf i j)
            return (values key (svref (dikt-vector-val d) j)))))))

(defun dikt-map (d func)
  "Returns the keys one by one, and NIL when finished."
  (check-type func function)
  (if (dikt-hash-table d)
      (loop for key being the hash-key in (dikt-hash-table d)
          using (hash-value val)
          do (funcall func key val))
    (loop with keys = (dikt-vector-key d)
        for j fixnum from 0 below +dikt-vector-size+
        for key = (svref keys j)
        when key
        do (funcall func key (svref (dikt-vector-val d) j)))))

(defun dikt-count (d)
  (if (dikt-hash-table d)
      (hash-table-count (dikt-hash-table d))
    (loop for k in (dikt-vector-key d) count k)))



