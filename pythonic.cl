(in-package :python)

;;; Pythonic: some utility functions to do Pythonic things


(defun py-raise (exc-type string &rest format-args)
  "Raise a Python exception with given format string"
  (if (find-class exc-type)
      (error exc-type :args (apply #'format nil string format-args))
    (apply #'error string format-args)))
  


(defmethod get-py-iterate-fun (object)
  "Return a function that when called returns VAL, T, where VAL is the next value ~@
   gotten by iterating over OBJECT. Returns NIL, NIL upon exhaustion."
  
  (when (member (class-name (class-of object)) '(py-tuple py-list))
    (return-from get-py-iterate-fun 
      (__iter__-fun object)))
  
  ;; Using __iter__ is the prefered way.
  (multiple-value-bind (iter-meth found)
      (getattr-of-class object '__iter__)
      
    (if found
	  
	(let* ((iterator (py-call iter-meth (list object)))
	       (next-meth (getattr-of-class iterator 'next)))
	    
	  ;; One could argue that looking up the `next' method should
	  ;; only happen when the first value is requested. Doing it
	  ;; earlier, as it is now, will detect the error below sooner.
	    
	  (unless next-meth
	    (py-raise 'TypeError "Got invalid iterator (no `next' method): ~A" iterator))	 
	  (labels ((get-next-val-fun ()
		     (let ((li (list iterator)))
		       (declare (dynamic-extent li))
		       (handler-case (values (py-call next-meth li))
			 (StopIteration () (values nil nil))
			 (:no-error (val)  (values val t))))))
	    (lambda () (get-next-val-fun))))
	
      ;; Fall-back: __getitem__ with successive integers, starting from 0.
      ;; 
      ;; There is the theoretical possibility that the object's class,
      ;; or just the __getitem__ method, is changed or removed while
      ;; we do this iteration.  By storing the __getitem__ method we
      ;; evade that; however, it might be semantically wrong,
      ;; theoretically speaking.
      
      (multiple-value-bind (getitem-meth found)
	  (getattr-of-class object '__getitem__)
	(if found

	    (let ((index 0))
	      (labels ((get-next-val-fun ()
			 (handler-case (py-call getitem-meth (list object index))
			   (IndexError () (values nil nil))  ;; even ok if index = 0 (empty sequence)
			   (:no-error (val)
			     (incf index)
			     (values val t)))))
		(lambda () (get-next-val-fun))))
	  
	  ;; If nothing works...
	  
	  (py-raise 'TypeError
		    "Iteration over non-sequence (got: ~A)" object))))))


(defmethod py-iterate->lisp-list (object)
  "Returns a Lisp list, that may not be modified destructively."
  (let ((res ()))
    (map-over-py-object (lambda (x) (push x res)) object)
    (nreverse res)))

(defmethod map-over-py-object (fun object)
  "Iterate over OBJECT, calling Lisp function FUN on each value. Returns nothing."
  (loop with f = (get-py-iterate-fun object)
      for val = (funcall f)
      while val do (funcall fun val))
  (values))


(defun make-py-iterator-for-object (object)
  (let ((f (get-py-iterate-fun object)))
    (make-iterator-from-function f)))

(defgeneric py-iterate-n-values (object n &key error)
  (:documentation 
   "Return N values from iterable Python object in the form of a Lisp vector.
   If ERROR, than when there are more values in the object, an error of type
   TypeError is raised; if not ERROR, NIL is returned in that situation."))

#+(or) ;; needed?
(eval-when (:execute) ;; TODO: change source code loading order and remove EVAL-WHEN
  (defmethod py-iterate-n-values ((x py-list/tuple) (n fixnum) &key (error t))
    (assert (> n 0))
    (with-slots (vec) x
      (if (= (length vec) n)
	  vec
	(when error
	  (py-raise 'ValueError "Wanted ~A values from iterable ~A, but got ~A"
		    n x (length vec)))))))

(defmethod py-iterate-n-values (x (n fixnum) &key (error t))
  (assert (> n 0))
  (let* ((f (get-py-iterate-fun x))
	 (vec (make-array n)))
    (declare (dynamic-extent vec))
    (loop with val = (funcall f)
	for i from 0 below n
	do (if val
	       (setf (aref vec i) val
		     val (funcall f))
	     (if error
		 (py-raise 'ValueError
			   "Wanted ~A values from iterable, but got less: ~A"
			   n i)
	       (return-from py-iterate-n-values nil)))
	finally
	  (let ((v (funcall f)))
	    (when v
	      (py-raise 'ValueError
			"Wanted ~A values from iterable ~A, but got 1 more already: ~A"
			n x v)))
	  (return vec))))



(defmacro ensure-py-type (vars cl-type err-str)
  "Ensure that all vars in VARS are a designator for CL-TYPE, if so SETF all vars
   to the CL-TYPE value they designate. Raise TypeError if check fails; ERR-STR
   may take one argument (the value of the first VAR that failed the type check)."
  (let ((des-p '#:des-p)
	(val   '#:val)
	(designator-function (cdr
			      (assoc cl-type
				     '((integer . py-int-designator-p)
				       (number  . py-number-designator-p)
				       (real    . py-real-designator-p)
				       (attribute-name . attribute-name-designator-p)
				       (string  . py-string-designator-p)
				       #+(or)(python-object . python-object-designator-p)))))
	(vars (if (listp vars) vars (list vars))))
    
    (assert designator-function ()
      "Designator for ~A unknown [macro ENSURE-PY-TYPE]" cl-type)
    
    `(progn ,@(loop for var in vars
		  collect `(multiple-value-bind (,des-p ,val)
			       (,designator-function ,var)
			     (if ,des-p
				 (setf ,var ,val)
			       (py-raise 'TypeError ,err-str ,var)))))))


(defun convert-to-py-object (x)
  "Return PYTHON-OBJECT, CONVERTED-P"
  (typecase x
    (python-object (values x nil))
    (number        (values (make-py-number x) t))
    (string        (values (make-py-string x) t))
    (symbol        (values (make-py-string (string x)) t))
    ((eql (load-time-value (find-class 'python-type))) (values x nil))
    (t (error "Not a recognized Python object: ~A" x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bridging the gap: making Lisp functions callable from within
;;; Python

(defmacro def-pyfun (name (&rest args) &body body)
  "Define a Lisp function that is callable from within Python. ~@
   It basically defines (defun NAME ...) and (defmethod py-call ((eql #'NAME) ...)."
  (multiple-value-bind (pos-args key-args key-vals)
      (lispargs->pyargs args)
    ;; For example args:
    ;;  (a b  &key (key1 . val1) (key2 . val2))
    ;; variables now have these values:
    ;;  pos-args: (a b) 
    ;;  key-args: (key1 key2)
    ;;  key-vals: ((key1 . val1) (key2 . val2))
    (let ((res '#:res))
      `(progn
	 (defun ,name (,@args)
	   ,@body)
	 (let ((call-rewriter
		(make-call-rewriter ',pos-args
				    (list ,@(mapcar (lambda (kv) `(cons ',(car kv) ,(cdr kv)))
						key-vals))
				    nil nil)))
	   (defmethod py-call ((f (eql #',name)) &optional pos-args key-args)
	     (let* ((,res (funcall call-rewriter pos-args key-args))
		    ,@(mapcar (lambda (posarg)
				`(,posarg (cdr (assoc ',posarg ,res))))
			      pos-args)
		    ,@(mapcar (lambda (key)
				`(,key (cdr (assoc ',key ,res))))
			      key-args))
	       (,name ,@pos-args ,@(loop for key in key-args
				       append `(,(intern key 'keyword)
						,key))))))
	 (unless (ignore-errors (namespace-bind *scope* ',name #',name)
				t)
	   (warn "Couldn't bind name ~A to function in current namespace (*scope*)"
		 ',name))))))

#+(or) ;; test
(def-pyfun foo (x y &key (z 0))
  (+ x y z))


(defun lispargs->pyargs (list)
  ;; Handles only positional and keywords (&key ..) for now.
  (let ((pos ())
	(keys ())
	(kvs ()))
    (block collect
      (loop
	(when (null list)
	  (return))
	(let ((x (pop list)))
	  (cond ((eq x '&key) (loop for (k v) in list
				  collect k into keys-temp
				  collect `(,k . ,v) into kvs-temp
				  finally (setf kvs kvs-temp
						keys keys-temp)
					  (return-from collect)))
		(t (push x pos))))))
    (setf pos (nreverse pos))
    (values pos keys kvs)))


#+(or) ;; test
(assert (equal (multiple-value-list (lispargs->pyargs '(a b &key (key1 val1) (key2 val2))))
	       (list '(a b) '(key1 key2) '((key1 . val1) (key2 . val2)))))

#+(or) ;; test case
(progn (def-pyfun foo (a b &key (key1 *None*) (key2 *None*))
	 (format t "FOO got:  a: ~S  b: ~S  key1: ~S  key2: ~S~%" a b key1 key2))
       (foo 1 2 :key2 3)
       (py-call #'foo '(1 2) '((key2 . 3))))

