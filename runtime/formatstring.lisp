;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON; Readtable: PY-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)
(in-syntax *user-readtable*)

(defstruct (format-string (:conc-name fs-) (:constructor make-fs))
  (string        ""    :type string)
  (type-of-arg   :list :type (member :mapping :list) :read-only t)
  (recipes       #()   :type vector)
  (list-num-args nil   :type (or fixnum null)))

(defun fs-extend-vec (additional-str str)
  (loop for ch across additional-str do (vector-push-extend ch str)))

(defun make-formatted-string (fs arg)
  (check-type fs format-string)
  (let ((is-mapping-fs (ecase (fs-type-of-arg fs)
			 (:mapping t)
			 (:list    nil))))
    (with-stack-list (one-arg-list arg)
      (loop
	  with list-args = (unless is-mapping-fs
			     (let ((args (deproxy arg)))
			     
			       ;; ".." % x  is same as  ".." % (x,)  when x used for list of args
			       (unless (listp args) (setf args one-arg-list))
			     
			       (unless (= (length args) (fs-list-num-args fs))
				 (py-raise 
				  '{ValueError}
                                  "Wrong number of arguments for format string (wanted ~A, got ~A)"
				  (fs-list-num-args fs) (length args)))
			       args))
				 
	  with mapping-getitem-unb = (and (not (stringp arg))
                                          is-mapping-fs
                                          (or (class.attr-no-magic (py-class-of arg) '{__getitem__})
                                              (error "Not suitable as format string mapping: ~A ~S" (py-class-of arg) arg)))
	  with mapping-getitem-bound = (when mapping-getitem-unb
					 ;; for efficiency, skip making bound method
					 (unless (functionp mapping-getitem-unb)
					   (bind-val mapping-getitem-unb arg (py-class-of arg))))
				     
	  with collected-strings = ()
			       
	  for rec across (fs-recipes fs)
	  do (ecase (pop rec)
	       (:literal (push (car rec) collected-strings))
	       (:format  (destructuring-bind
			     (map-key conv-flags min-field-width precision conv-type) rec

			   #+(or)
			   (when precision
			     (warn "Formatting: ignoring 'precision' field value (~S)." precision))
			 
			   (when (eq min-field-width :arg) (setf min-field-width (pop list-args)))
			 
			   (when (eq precision :arg) (setf precision (pop list-args)))
			 
			   (let* ((obj (if map-key
					   (if mapping-getitem-bound
					       (py-call mapping-getitem-bound map-key)
					     (funcall mapping-getitem-unb arg map-key))
					 (pop list-args)))
				
				  (obj.f (format-object conv-type obj))
				
				  (obj.f2 (if (or conv-flags min-field-width precision)
					      (apply-fmt-ops conv-type obj obj.f
							     conv-flags min-field-width)
					    obj.f)))
			   
			     (push obj.f2 collected-strings)))))
	   
	  finally (setf collected-strings (nreverse collected-strings))
		  #+(or)(warn "collected: ~S" collected-strings)
		  (loop for s in collected-strings
		      sum (length s) into s.len
		      finally (return-from make-formatted-string
				(apply #'concatenate 'string collected-strings)))))))

(defun format-object (conv-type obj)
  (ecase conv-type
    (#\s (py-str-string obj))
    (#\r (py-repr-string obj))
    (#\c (setf obj (deproxy obj))
	 (typecase obj
	   (integer (string (code-char obj))) ;; convert as by {chr}
	   (string  (unless (= (length obj) 1)
		      (py-raise '{TypeError}
				"The %c formatting code wants 1-char string (got: ~S)."
				obj))
		    obj)
	   (t (py-raise '{TypeError} "Invalid object for %c format convertion: ~S." obj))))
    (#\i (format nil "~D" (deproxy obj)))
    (#\o (format nil "~O" (deproxy obj)))
    
    (#\x (nstring-downcase (format nil "~X" (deproxy obj))))
    (#\X (nstring-upcase   (format nil "~X" (deproxy obj))))
    
    (#\e (nstring-downcase (format nil "~e" (deproxy obj))))
    (#\E (nstring-upcase   (format nil "~e" (deproxy obj))))
    
    (#\f (nstring-downcase (format nil "~f" (deproxy obj))))
    (#\F (nstring-upcase   (format nil "~f" (deproxy obj))))
    
    (#\g (nstring-downcase (format nil "~g" (deproxy obj))))
    (#\G (nstring-upcase   (format nil "~g" (deproxy obj))))))
									      

(defun apply-fmt-ops (conv-type obj obj.f conv-flags min-field-width)
  (setf obj (deproxy obj))
  
  (let* ((alt-form-p     (member #\# conv-flags))
	 (right-padded-p (member #\- conv-flags))
	 (zero-padded-p  (unless right-padded-p (member #\0 conv-flags)))
	 (sign-p         (member #\+ conv-flags))
	 (blank-prefix-for-pos-num (unless sign-p (member #\Space conv-flags)))
	 
	 (stuff-before ())
	 (stuff-after ()))
    
    (when (or alt-form-p sign-p blank-prefix-for-pos-num)
      (unless (numberp obj)
	(py-raise '{TypeError}
		  "The `#', `+' and ` ' (space) conversion flags may only be used ~
                   for numeric arguments (got: ~S)." obj)))
    
    (when (and alt-form-p (/= 0 obj))
      (push (case conv-type
	      (#\o #\0)
	      (#\x "0x")
	      (#\X "0X"))
	    stuff-before))
    
    (when (and sign-p (< obj 0))
      #+(or)(push #\- stuff-before)
      (setf obj (- obj)))
    
    (when blank-prefix-for-pos-num
      (cond ((>= obj 0) (push #\Space stuff-before))
	    ((<  obj 0) #+(or)(push #\- stuff-before)
			(setf obj (- obj)))))
    
    (flet ((count-extras (e) (loop for x in e
				 for x.len = (etypecase x
					       (string    (length x))
					       (character 1))
				 sum x.len)))
      (let* ((extra-len-before (count-extras stuff-before))
	     (extra-len-after (count-extras stuff-after))
	     (core-length (+ extra-len-before (length obj.f) extra-len-after))
	     (num-pad-char (if min-field-width
			       (max 0 (- min-field-width core-length))
			     0))
	     (tot-len (+ core-length num-pad-char))
	     (pad-char (if zero-padded-p #\0 #\Space)))
	
	
	(dotimes (i num-pad-char)
	  (if right-padded-p
	      (push pad-char stuff-after)
	    (push pad-char stuff-before)))
	
	(let ((res (make-array tot-len :element-type 'character))
	      (i 0))
	  (loop for item in (nconc stuff-before (cons obj.f stuff-after)) 
	      do (etypecase item
		   (string (loop for ch across item
			       do (setf (aref res i) ch) 
				  (incf i)))
		   (character (setf (aref res i) item)
			      (incf i))))
	  (assert (= i tot-len))
	  
	  ;; Minus sign must be in front of padding with 0's.  Because
	  ;; the minus sign is part of "%d" etc, need to manually fix
	  ;; that here.
	  
	  (when zero-padded-p
	    (let ((minus-ix     (position #\- res))
		  (first-zero-p (char= (aref res 0) #\0)))
	      (when (and minus-ix first-zero-p)
		(rotatef (aref res minus-ix) (aref res 0)))))
			   
	  
	  res)))))

	     
(defvar *parsed-format-strings* (make-hash-table :test #'equal))

(defun ensure-parsed-format-string (string)
  "Returns FS struct"
  (check-type string string)
  
  (or (gethash string *parsed-format-strings*)
      (setf (gethash string *parsed-format-strings*)
	(parse-format-string string))))

(defun parse-format-string (string)
  "Returns a FORMAT-STRING struct that can be called in MAKE-FORMATTED-STRING."
  (check-type string string)
  (let ((s.len (length string))
	(res ())
	(i 0))
    (labels ((next-ch-nil ()
               (prog1 (and (< i s.len) (char string i))
                 (incf i)))
	     (next-ch-error ()
               (or (next-ch-nil)
                   (py-raise '{ValueError} "Unfinished format string (~S)." string)))
	     (unread-ch () 
               (decf i)
               (assert (and (>= i 0)))))
      (loop
	(let ((dispatch-c (next-ch-nil)))
          (unless dispatch-c
            (return))
          (if (char= dispatch-c #\%)
              (let ((next (next-ch-nil)))
                (cond ((not next)
                       (py-raise '{ValueError} "Unfinished format string (~S)." string))
                      ((char= next #\%)
                       (push (list :literal "%") res))
                      (t (unread-ch)
                         (let* ((mapping-key
                                 (if (char= (next-ch-error) #\( ) ;; "%(name)s" % {'name': 'john'}
                                     (coerce (loop for c = (next-ch-error) until (char= c #\)) collect c)
                                             'string)
                                   (progn (unread-ch)
                                          nil)))
                                (conversion-flags
                                 (loop for c = (next-ch-error)
                                     while (member c '( #\# #\0 #\- #\Space #\+ ) :test #'char=)
                                     collect c into flags
                                     finally (unread-ch)
                                             (return flags)))
                                (minimum-field-width
                                 (let ((c (next-ch-error)))
                                   (cond ((char= c #\*) ;; to be supplied as argument
                                          :arg) 
                                         ((digit-char-p c 10)
                                          (loop with res = 0
                                              while (digit-char-p c 10) 
                                              do (setf res (+ (* 10 res) (digit-char-p c 10))
                                                       c (next-ch-error))
                                              finally (unread-ch)
                                                      (return res)))
                                         (t (unread-ch)
                                            nil))))
                                (precision
                                 (if (char= (next-ch-error) #\.)
                                     (let ((c (next-ch-error)))
                                       (cond ((digit-char-p c)
                                              (loop with res = 0
                                                  while (digit-char-p c 10) 
                                                  do (setf res (+ (* 10 res) (digit-char-p c 10))
                                                           c (next-ch-error))
                                                  finally (unread-ch)
                                                          (return res)))
                                             
                                             ((char= c #\*) ;; to be supplied as argument
                                              :arg)
                                             (t (py-raise '{ValueError}
                                                          "Format string contains illegal precision ~@
                                               (got ~A after dot; expected number)." c))))
                                   (progn (unread-ch)
                                          nil)))
                                (ignored-C-synax-length-modifier
                                 (unless (member (next-ch-error) '( #\h #\l #\L ) :test #'char=)
                                   (unread-ch)))
                                (conversion-type
                                 (let ((c (next-ch-error)))
                                   (unless (position c "diouxXeEfFgGcrs%" :test #'char=)
                                     (py-raise '{ValueError}
                                               "In format string, unrecognized conversion type found: `~A'." c))
                                   (case c ;; Some codes are redundant
                                     ((#\d #\u) #\i)  
                                     (#\F       #\f)
                                     (t         c)))))
                           (declare (ignore ignored-C-synax-length-modifier))
                           (push (list :format mapping-key conversion-flags minimum-field-width
                                       precision conversion-type)
                                 res)))))
            ;; string literal
            (let ((end-ix (position #\% string :start i)))
              (push (list :literal (subseq string (1- i) end-ix)) 
                    res)
              (setf i (or end-ix (length string))))))))
    (let* ((fmt-ops (remove-if-not (lambda (res) (eq (car res) :format)) res))
           (kind (let ((num-not-map (count nil (mapcar #'second fmt-ops))))
		   (cond ((= num-not-map 0)                :mapping)
			 ((= num-not-map (length fmt-ops)) :list)
			 (t (py-raise '{ValueError}
				      "Both mapping and non-mapping formatting operations ~
                                       found in format string (~S)." string)))))
           (num-args (ecase kind
                       (:list (loop for (format nil nil min-fld-width prec nil) in fmt-ops
				  do (assert (eq format :format))
				  count t
				  count (eq min-fld-width :arg)
				  count (eq prec :arg)))
		       (:mapping (loop for (format nil nil min-fld-width prec nil) in fmt-ops
				     do (assert (eq format :format))
				     when (or (eq min-fld-width :arg)
					      (eq prec :arg))
				     do (py-raise '{ValueError}
						  "This format string uses mapping arg. Therefore ~
                                                   it cannot refer (using `*') to an argument in ~
                                                   the field-width or precision (~S)." string)
				     finally (return nil))))))
      
      (make-fs :string string
	       :type-of-arg kind
	       :recipes (coerce (nreverse res) 'vector)
	       :list-num-args num-args))))
