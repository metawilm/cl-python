(in-package :python)

(defstruct (format-string (:type vector) (:conc-name fs-) (:constructor make-fs))
  (string        :type string)
  (type-of-arg   :type (or (eql :mapping) (eql :list)) :read-only t)
  (recipes       :type list)
  (list-num-args :type (or fixnum null)))

(defun fs-extend-vec (str vec)
  (loop for ch across str do (vector-push-extend ch vec)))

(defmethod make-formatted-string (fs arg)
  (check-type fs format-string)
  (let ((is-mapping-fs (ecase (fs-type-of-arg fs)
			 (:mapping t)
			 (:list    nil))))
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
			 
			 (when precision
			   (warn "Formatting: ignoring 'precision' field value (~S)." precision))
			 
			 (when (eq min-field-width :arg) (setf min-field-width (pop list-args)))
			 
			 (when (eq precision :arg) (setf precision (pop list-args)))
			 
			 (let* ((obj (if map-key
					 (py-call mapping-getitem map-key) 
				       (pop list-args)))
				
				(obj.f (format-obj conv-type obj))
				(obj.f2 (if (some #'identity (cdr rec)
						  (apply-fmt-ops conv-type obj obj.f
								 conv-flags min-field-width))
					    obj.f)))
			   
			   (fs-extend-vec string obj.f2)))))
	   
	finally (return string))))


(defun format-object (conv-type obj)
  (ecase conv-type
    (#\s (py-str-string obj))
    (#\r (py-repr-string obj))
    (#\c (setf obj (deproxy obj))
	 (typecase obj
	   (integer (pybf:chr obj))
	   (string  (unless (= (length obj) 1)
		      (py-raise 'TypeError
				"The %c formatting code wants 1-char string (got: ~S)."
				obj))
		    obj)
	   (t (py-raise 'TypeError "Invalid object for %c format convertion: ~S." obj))))
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
  (flet ((pad (str from-ix times char)
	   (dotimes (i times)
	     (setf (aref str (+ from-ix i)) char))))
    
    (let* ((alt-form-p     (member #\# conv-flags))
	   (right-padded-p (member #\- conv-flags))
	   (zero-padded-p  (unless right-padded-p (member #\0 conv-flags)))
	   (sign-p         (member #\+ conv-flags))
	   (blank-prefix-for-pos-num (unless sign-p (member #\Space conv-flags)))
	   
	   (stuff-before ())
	   (stuff-after ()))
      
      (when (or alt-form-p sign-p blank-prefix-for-pos-num)
	(unless (numberp obj)
	  (py-raise 'TypeError
		    "The `#', `+' and ` ' (space) conversion flags may only be used ~
                     for numeric arguments (got: ~S)." obj)))
      
      (when (and alt-form-p (/= 0 obj))
	(push (case conv-type
		(#\o #\0)
		(#\x "0x")
		(#\X "0X"))
	      stuff-before))
      
      (when (and sign-p (< obj 0))
	(push #\- stuff-before))
      
      (when (and blank-prefix-for-pos-num (> obj 0))
	(push #\Space stuff-before))

      (flet ((count-extras (e) (loop for x in e
				   for x.len = (etypecase x
						 (string    (length x))
						 (character 1))
				   sum x.len)))
	(let* ((extra-len-before (count-extras stuff-before))
	       (extra-len-after (count-extras stuff-after))
	       (core-length (+ extra-len-before (length obj.f) extra-len-after))
	       (num-pad-char (max 0 
				  (- min-field-width core-length)))
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
	    res))))))

	     
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
				 
				 (t (py-raise 'ValueError
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
			 (py-raise 'ValueError
				   "In format string, unrecognized conversion type found: `~A'." c))
		       
		       (case c ;; Some codes are redundant
			 ((#\d #\u) #\i)  
			 (#\F       #\f)
			 (t         c)))))
	       
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
