(in-package :python)

(defmethod py-format-string ((x string) arg)
  
  ;; String formatting; similar to C's sprintf().
  ;; http://docs.python.org/lib/typesseq-strings.html
  
  ;; If ARG is a tuple, its content are the values to use. If it is
  ;; not, ARG is the one object to use as argument (either by
  ;; including a string representation of ARG itself, or using ARG as
  ;; mapping).
  
  (multiple-value-bind (args mapping-arg)
      (if (typep arg 'py-tuple) (values (slot-value arg 'list) nil) (values (list arg) arg))
    
    (let* ((res (make-array 20 :element-type 'character :adjustable t :fill-pointer 0))
	   (next-char-index 0)
	   (max-index (1- (length x)))
	   (mapping-arg-used nil))
      
      (labels ((next-char-nil () (if (> next-char-index max-index) nil
				   (prog1 (aref x next-char-index) (incf next-char-index))))
	       (next-char-error () (or (next-char-nil)
				       (py-raise 'ValueError "Unfinished format string")))
	       (next-arg () (if mapping-arg-used
				(py-raise 'ValueError "In format string, both non-mapping ~@
                                                       and mapping operators found")
			      (or (pop args)
				  (py-raise 'TypeError
					    "Not enough arguments for format string")))))
	(let ((c (next-char-nil)))
	  (tagbody start-parsing
	    
	    (loop while (and c (not (char= c #\%))) ;; collect regular characters
		do (vector-push-extend c res)
		   (setf c (next-char-nil)))
	    
	    (unless c (return-from py-format-string res)) ;; eof
	    
	    (assert (char= c #\%))
	    (setf c (next-char-error))
	    
	    (let* ((mapping-key
		    (when (char= c #\( )  ;; `mapping key' : "%(name)s" % {'name': 'john'}
		      (loop with p = (next-char-error)
			  with mapping-key = (make-array 4 :element-type 'character
							 :adjustable t :fill-pointer 0)
			  until (char= p #\) )
			  do (vector-push-extend p mapping-key)
			     (setf p (next-char-error))
			  finally (assert (char= p #\) ))
				  (setf c (next-char-error)
					mapping-arg-used t)
				  (return mapping-key))))
		   
		   (conversion-flags
		    (loop while (member c `( #\# #\0 #\- #\Space #\+ ))
			collect c
			do (setf c (next-char-error))))
		   
		   (minimum-field-width
		    (cond ((char= c #\*)
			   (let ((w (next-arg)))
			     (ensure-py-type w integer
					     "Format string: field width must be ~@
                                             integer (got: ~A)")
			     (setf c (next-char-error))
			     w))
			  
			  ((digit-char-p c 10)
			   (loop with res = 0
			       while (digit-char-p c 10) 
			       do (setf res (+ (* 10 res) (digit-char-p c 10))
					c (next-char-error))
			       finally (return res)))
			  
			  (t 0)))
		   
		   (precision
		    (if (char= c #\.)
			(progn
			  (setf c (next-char-error))
		      
			  (cond  ((digit-char-p c) 			  
				  (loop with res = 0
				      while (digit-char-p c 10) 
				      do (setf res (+ (* 10 res) (digit-char-p c 10))
					       c (next-char-error))
				      finally (return res)))
			     
				 ((char= c #\*)
				  (let ((w (next-arg)))
				    (ensure-py-type w integer
						    "Format string: precision must be ~@
                                                 integer (got: ~A)")
				    (setf c (next-char-error))
				    w))
			     
				 (t (py-raise 'ValueError
					      "Format string contains illegal precision ~@
                                           (got ~A after dot; expecting number)" c))))
		      0))
		   
		   ;; skip useless length modifiers  (C syntax?!)
		   (ignored (loop while (member c '( #\h #\l #\L ))
				do (setf c (next-char-error))))
		   
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
	    
	    (setf c (next-char-nil))
	    (go start-parsing)))))))


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
