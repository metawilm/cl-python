;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.ARRAY; Readtable: PY-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module.array)
(in-syntax clpython:*user-readtable*)

(defclass |array| (clpython:object)
  ((kind    :type character :initarg :kind    :accessor py-array-kind)
   (array   :type cl:array  :initarg :array   :accessor py-array-array)
   (elmtype :type t         :initarg :elmtype :accessor py-array-elmtype)
   (elmsize :type fixnum    :initarg :elmsize :accessor py-array-elmsize))
  (:metaclass clpython:py-type))

(defparameter *py-array-types*
    ;; code  type              item size
    '(( #\c  (unsigned-byte 8) nil)  ;; char  XXX 7 or 8 chars?
      ( #\b  (signed-byte   8) nil)  ;; signed char
      ( #\B  (unsigned-byte 8) nil)  ;; unsigned char
      ( #\u  character         nil)  ;; Unicode char
      ( #\h  (signed-byte   16) nil) ;; signed short
      ( #\H  (unsigned-byte 16) nil) ;; signed short
      ( #\i  (signed-byte   16) nil) ;; signed int
      ( #\I  (unsigned-byte 16) nil) ;; unsigned int
      ( #\l  (signed-byte   32) nil) ;; signed long
      ( #\L  (unsigned-byte 32) nil) ;; unsigned long
      ( #\f  single-float       nil) ;; float
      ( #\f  double-float       nil) ;; double
      ))

(def-py-method |array.__new__| :static (cls typecode &optional initializer)
  ;; Both creation and initialization, as array size is dependent on initializer.
  (assert (eq cls (find-class '|array|)) () "Subclassing ARRAY not yet supported...") 
  (let* ((type-code (let ((s (py-val->string typecode)))
		      (if (= (length s) 1)
			  (aref typecode 0)
			(py-raise 'TypeError "Type indicator must be char (got: ~S)" typecode))))
	 (kind (or (find type-code *py-array-types* :test #'char= :key #'car)
		   (py-raise 'ValueError "Unknown array type indicator: ~S" (aref typecode 0))))
	 (lisp-type (second kind))
	 (item-size (or (third kind)
			;; Determine item size by writing to file and reading file size.
			;; XXX Is there a better way?
			(setf (third kind)
			  (let ((fname (format nil "__tmp_~A" (gensym))))
			    (with-open-file (f fname
					     :direction :output
					     :element-type lisp-type
					     :if-does-not-exist :create
					     :if-exists :supersede)
			      (write-byte 0 f))
			    (with-open-file (f fname
					     :direction :input
					     :element-type '(unsigned-byte 8)
					     :if-does-not-exist :error)
			      (file-length f)))))))
    #+(or)(warn "code ~A = type ~A = ~A bytes per item" type-code lisp-type item-size)
    (flet ((create-array (&optional (size 0))
	     (make-instance '|array|
	       :kind type-code
	       :array (make-array size :element-type lisp-type :adjustable t :fill-pointer 0)
	       :elmtype lisp-type
	       :elmsize item-size)))
      (if initializer
	  (typecase initializer
	    ;; XXX how about user-defined subclasses?
	    ((or string array)
	     (let ((arr (create-array (length initializer))))
	       (funcall (if (stringp initializer) #'|array.fromstring| #'|array.fromlist|)
			arr initializer)
	       arr))
	    (t
	     (let ((arr (create-array)))
               (|array.extend| arr initializer)
               arr))) ;; XXX could take __len__
	(create-array)))))

(def-py-method |array.__getitem__| (x item)
  (check-type item (integer 0 #.most-positive-fixnum))
  (let ((arr (py-array-array x)))
    (if (< item (length arr))
        (aref arr item)
      (py-raise '{IndexError} "Index ~A outside the range of array ~A (num elements = ~A)."
                item arr (length arr)))))

(def-py-method |array.__len__| (x)
  (length (py-array-array x)))

(def-py-method |array.__repr__| (x)
  (with-output-to-string (s)
    (print-unreadable-object (x s :type t :identity t)
      (format s "typecode '~A', elm-type ~A, elm-size ~A bytes, ~A items~@[~A~]"
	      (py-array-kind x)
	      (py-array-elmtype x)
	      (py-array-elmsize x)
	      (length (py-array-array x))
	      (when (< (length (py-array-array x)) 10)
		(format nil " [~{~A~^ ~}]" (loop for i across (py-array-array x) collect i)))))))

(def-py-method |array.__setitem__| (x item value)
  (setf (aref (py-array-array x) item) value))

(def-py-method |array.fromstring| (py-arr string)
  (loop with vec = (py-array-array py-arr)
      for ch across (py-val->string string)
      do (vector-push-extend (string ch) vec))
  *the-none*)

(def-py-method |array.fromlist| (py-arr list)
  (|array.fromstring| py-arr list))

(def-py-method |array.extend| (py-arr iterable)
  (loop with vec = (py-array-array py-arr)
      for item in (py-iterate->lisp-list iterable)
      do (vector-push-extend item vec))
  *the-none*)
