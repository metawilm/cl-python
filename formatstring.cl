(in-package :python)

(defstruct (format-string (:type vector) (:conc-name fs-) (:constructor make-fs))
  (string        :type string)
  (type-of-arg   :type (or (eql :mapping) (eql :list)) :read-only t)
  (recipes       :type list)
  (list-num-args :type (or fixnum null)))

(defun fs-extend-vec (str vec)
  (loop for ch across str do (vector-push-extend ch vec)))

(defun make-formatted-string (fs arg)
  #+(or)(check-type fs format-string)
  (let ((is-mapping-fs  (ecase (fs-type-of-arg fs)
			  (:mapping t)
			  (:list    nil))))
    #+(or)
    (when (and (not is-mapping-fs)
	       (not (subtypep (py-class-of arg) 'py-tuple)))
      (setf arg (make-tuple-from-list (list arg))))
    
    (loop
	with string = (make-array 20 :element-type 'character :adjustable t :fill-pointer 0)
	
	with list-args = (unless is-mapping-fs
			   (let ((args (deproxy arg)))
			     
			     ;; ".." % x  is same as  ".." % (x,)  when x used for list of args
			     (unless (listp args) (setf args (list args)))
			     
			     (unless (= (length args) (fs-list-num-args fs))
			       (py-raise 
				'ValueError "Wrong number of arguments for format string ~
                                             (wanted ~A, got ~A)"
				(fs-list-num-args fs) (length args)))
			     args))
				 
	with mapping-getitem = (when is-mapping-fs
				 (recursive-class-lookup-and-bind arg '__getitem__))
			       
	for rec across (fs-recipes fs)
	do (ecase (pop rec)
	     (:literal (fs-extend-vec (car rec) string))
	     (:format  (destructuring-bind
			   (map-key conv-flags min-field-width precision conv-type) rec
			 
			 (when (or conv-flags min-field-width precision)
			   #+(or)(warn "Ignoring some format string params (make-formatted-string)"))
			 
			 (when (eq min-field-width :arg) (setf min-field-width (pop list-args)))
			 
			 (when (eq precision :arg) (setf precision (pop list-args)))
			 
			 (let* ((obj (if map-key
					 (py-call mapping-getitem map-key) 
				       (pop list-args)))
				
				(obj.str (case conv-type
					   (#\s (py-str-string obj))
					   (#\r (py-repr-string obj))
					   (t (warn "ignoring conv-type ~A (make-formatted-string)"
						    conv-type)
					      (py-str-string obj)))))
			   
			   (fs-extend-vec obj.str string)))))
	   
	finally (return string))))


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
    
    (labels ((next-ch-nil   () (prog1 (and (< i s.len) (char string i))
				 (incf i)))
	     (next-ch-error () (or (next-ch-nil)
				   (py-raise 'ValueError "Unfinished format string (~S)." 
					     string)))
	     (unread-ch     () (progn (decf i)
				      (assert (and (>= i 0))))))
      
      (loop
	(let ((dispatch-c (next-ch-nil)))
	  
	  (case dispatch-c
	    
	    ((nil) (return))
	    
	    (#\% 
	     (let* ((mapping-key (if (char= (next-ch-error) #\( ) ;; "%(name)s" % {'name': 'john'}
				     (coerce (loop for c = (next-ch-error)
						 until (char= c #\) )
						 collect c)
					     'string)
				   (progn (unread-ch)
					  nil)))
		    
		    (conversion-flags (loop for c = (next-ch-error)
					  while (member c '( #\# #\0 #\- #\Space #\+ )
							:test #'char=)
					  collect c into flags
					  finally (unread-ch)
						  (return flags)))
		    
		    (minimum-field-width (let ((c (next-ch-error)))
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
		    
		    (precision (if (char= (next-ch-error) #\.)
				   (progn
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
					     
					     (t (py-raise
						 'ValueError
						 "Format string contains illegal precision ~@
                                                  (got ~A after dot; expected number)." c)))))
				 
				 (progn (unread-ch)
					nil)))
		    
		    
		    (ignored-C-synax-length-modifier (unless (member (next-ch-error) '( #\h #\l #\L )
								     :test #'char=)
						       (unread-ch)))
		    
		    (conversion-type (let ((c (next-ch-error)))
				       (unless (position c "diouxXeEfFgGcrs%" :test #'char=)
					 (py-raise 'ValueError
						   "In format string, unrecognized conversion ~
                                                    type found: `~A'." c))
				       c)))
	       
	       (declare (ignore ignored-C-synax-length-modifier))
	       
	       (push (list :format mapping-key conversion-flags minimum-field-width
			   precision conversion-type)
		     res)))
	    
	    (t ;; string literal
	     (let ((end-ix (position #\% string :start i)))
	       (push (list :literal (subseq string (1- i) end-ix))
		     res)
	       (setf i (or end-ix (length string)))))))))
    
    
    (let* ((fmt-ops (remove-if-not (lambda (res) (eq (car res) :format)) 
				   res))
	   
	   (kind (let ((num-not-map (count nil (mapcar #'second fmt-ops))))
		   (cond ((= num-not-map 0)                :mapping)
			 ((= num-not-map (length fmt-ops)) :list)
			 (t (py-raise 'ValueError
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
				     do (py-raise 'ValueError
						  "This format string uses mapping arg. Therefore ~
                                                   it cannot refer (using `*') to an argument in ~
                                                   the field-width or precision (~S)." string)
				     finally (return nil))))))
      
      (make-fs :string string
	       :type-of-arg kind
	       :recipes (coerce (nreverse res) 'vector)
	       :list-num-args num-args))))
   

#+(or)
(defmethod py-format-string ((x string) arg)
  
  ;; String formatting; similar to C's sprintf().
  ;; http://docs.python.org/lib/typesseq-strings.html
  
  ;; If ARG is a tuple, the items it contains are the values to
  ;; use.
  ;; 
  ;; If ARG is not a tuple, ARG is the one object to use as argument:
  ;; either by including a string representation of ARG itself, or
  ;; using ARG as mapping; this is determined by the format string.
  ;; 
  ;; Note that if ARG is an iterable other than a tuple, it is still
  ;; treated as the one argument to use. Thus only with tuples, a list
  ;; of arguments can be supplied.
  ;; 
  ;; The number of string directives and the number of args must
  ;; match.
  
  (multiple-value-bind (args is-mapping-arg)
      (if (subtypep (py-class-of arg) 'py-tuple)
	  (values (deproxy arg) nil)
	(values (list arg) arg))
    
    (let* ((res (make-array 20 :element-type 'character :adjustable t :fill-pointer 0))
	   (next-ch-index 0)
	   (max-index (1- (length x)))
	   (mapping-arg-used nil)
	   (arg-vector-i (if (vectorp args) 0 nil)))
      
      (labels ((next-ch-nil () (if (> next-ch-index max-index) nil
				   (prog1 (aref x next-ch-index) (incf next-ch-index))))
	       (next-ch-error () (or (next-ch-nil)
				       (py-raise 'ValueError "Unfinished format string")))
	       (next-arg () (if mapping-arg-used
				(py-raise 'ValueError "In format string, both non-mapping ~@
                                                       and mapping operators found")
			      (typecase args
				(list (or (pop args)
					  (py-raise 'TypeError
						    "Not enough arguments for format string")))
				(vector (if (<= arg-vector-i (length args))
					    (prog1 (aref args arg-vector-i)
					      (incf arg-vector-i))
					  (py-raise 'TypeError
						    "Not enough arguments for format string")))))))
	(let ((c (next-ch-nil)))
	  (tagbody start-parsing
	    
	    (loop while (and c (not (char= c #\%))) ;; collect regular characters
		do (vector-push-extend c res)
		   (setf c (next-ch-nil)))
	    
	    (unless c (return-from py-format-string res)) ;; eof
	    
	    (assert (char= c #\%))
	    (setf c (next-ch-error))
	    
	    (let* ((mapping-key
		    (when (char= c #\( )  ;; example: "%(name)s" % {'name': 'john'}
		      (loop with p = (next-ch-error)
			  with mapping-key = (make-array 4 :element-type 'character
							 :adjustable t :fill-pointer 0)
			  until (char= p #\) )
			  do (vector-push-extend p mapping-key)
			     (setf p (next-ch-error))
			  finally (assert (char= p #\) ))
				  (setf c (next-ch-error)
					mapping-arg-used t)
				  (return mapping-key))))
		   
		   (conversion-flags
		    (loop while (member c `( #\# #\0 #\- #\Space #\+ ))
			collect c
			do (setf c (next-ch-error))))
		   
		   (minimum-field-width
		    (cond ((char= c #\*)
			   (let ((w (next-arg)))
			     (ensure-py-type w integer
					     "Format string: field width must be ~@
                                             integer (got: ~A)")
			     (setf c (next-ch-error))
			     w))
			  
			  ((digit-char-p c 10)
			   (loop with res = 0
			       while (digit-char-p c 10) 
			       do (setf res (+ (* 10 res) (digit-char-p c 10))
					c (next-ch-error))
			       finally (return res)))
			  
			  (t 0)))
		   
		   (precision
		    (if (char= c #\.)
			(progn
			  (setf c (next-ch-error))
		      
			  (cond  ((digit-char-p c) 			  
				  (loop with res = 0
				      while (digit-char-p c 10) 
				      do (setf res (+ (* 10 res) (digit-char-p c 10))
					       c (next-ch-error))
				      finally (return res)))
			     
				 ((char= c #\*)
				  (let ((w (next-arg)))
				    (ensure-py-type w integer
						    "Format string: precision must be ~@
                                                 integer (got: ~A)")
				    (setf c (next-ch-error))
				    w))
			     
				 (t (py-raise 'ValueError
					      "Format string contains illegal precision ~@
                                           (got ~A after dot; expecting number)" c))))
		      0))
		   
		   ;; skip useless length modifiers (a C thing)
		   (ignored (loop while (member c '( #\h #\l #\L ))
				do (setf c (next-ch-error))))
		   
		   (conversion-type
		    (if (position c "diouxXeEfFgGcrs%")
			c
		      (py-raise 'ValueError "Unrecognized conversion type: ~A" c))))
	      
	      (declare (ignore ignored))
	      
	      #+(or)(warn "mapping-key: ~S conversion-flags: ~S minimum-field-width: ~A"
			  mapping-key conversion-flags minimum-field-width)
	      #+(or)(warn "precision: ~A  conversion-type: ~A" precision conversion-type)

	      (when (and mapping-key (not mapping-arg))
		(py-raise 'TypeError "Format string requiers mapping arg (got: ~A)" arg))
	      
	      (let ((converted-object
		     (py-format
		      :flags conversion-flags
		      :minimum-field-width minimum-field-width
		      :precision precision
		      :type conversion-type
		      :object
		      (if mapping-key
			  (call-attribute-via-class mapping-arg '__getitem__ mapping-key)
			(next-arg)))))
		(loop for ch across converted-object
		    do (vector-push-extend ch res))))
	    
	    (setf c (next-ch-nil))
	    (go start-parsing)))))))

#+(or)
(defun py-format (&key type flags minimum-field-width precision object)
  (declare (ignorable flags minimum-field-width precision))
  
  ;; Normalize a bit

  (when (member #\- flags)
    (setf flags (delete #\0 flags)))
  
  (when (member #\+ flags)
    (setf flags (delete #\Space flags)))
  
  (let ((conv-upcase nil))
    
    (setf type (case type
		 ;; There are no unsigned integers in Python although
		 ;; 0xFFFFFFFF used to be -1 until v. 2.4
		 ((#\u #\i)  #\d)
		 ( #\X       (setf conv-upcase t) #\x)
		 ( #\F       #\f)
		 (#\G        (setf conv-upcase t) #\g)
		 (t          type)))
    
    (ecase type
      ((#\d #\o #\x) ;; d=decimal o=octal  x=hex
       (ensure-py-type object integer
		       (format nil "Format string conversion type requires int (got: ~A)"))

       ;; Because of differences between Lisp and Python padding
       ;; semantics etc, need to do this manually...
       
       (let* ((res (format nil (ecase type  ;; XXX remove sign from here later on
				 (#\d "~D")
				 (#\o "~O")
				 (#\x "~X")) object)))
	 (return-from py-format
	   (if conv-upcase (string-upcase res) res))))
      
      
      ((#\e #\f #\g)  ;; floating point exponential; floating point
       (ensure-py-type object real
		       (format nil "Format string conversion requires real (got: ~A)"))
       (let ((res (format nil (ecase type
				(#\e "~e")
				(#\f "~f")
				(#\g "~g")) object)))
	 (return-from py-format
	   (if conv-upcase (string-upcase res) res))))
      
      ((#\r #\s)
       (let ((res (call-attribute-via-class object
					    (if (char= type #\r) '__repr__ '__str__))))
	 (return-from py-format
	   (if conv-upcase (string-upcase res) res)))))))
      
;; Flag `0': prefix zeros
;; (These can later be overwritten with `0x' etc.)
#+(or)
(progn (num-zeros-padded (if (member #\0 flags)
			     (loop for i from 0
				 while (< (length res) minimum-field-width)
				 do (setf res (concatenate 'string "0" res))
				 finally (return i))
			   0))

       ;; Flag `#': add `0' or `0x' when result doesn't start with `0' already
       (when (and (char/= (aref res 0) #\0)
		  (member #\# flags))
	 (ecase type
	   (#\0 (setf res (concatenate 'string "0" res)))
	   (#\x (setf res (concatenate 'string "0x" res)))
	   (#\d (warn "In format string: ignored flag `#' for %d"))))
       
       ;; Flags `+' and ` ': Add sign or space
       (cond ((and (< object 0) (member #\+ flags))
	      (setf res (concatenate 'string "-" res)))
	     
	     ((and (> object 0) (member #\Space flags))
	      (setf res (concatenate 'string " " res)))
	     
	     ((and (> object 0) (member #\+ flags))
	      (setf res (concatenate 'string "+" res))))
       
       ;; Flag `-': add space padding to the right
       (when (member #\- flags)
	 (loop while (< (length res) minimum-field-width)
	     do (setf res (concatenate 'string res " ")))))
