(in-package :python)

;; Pythonic:  some nice macros to do Pythonic things


(defmacro py-raise (exc-type string &rest format-args)
  "Raise Python exception of type EXC-TYPE, ~@
   where STRING with FORMAT-ARGS is the exception argument."
  `(error ,exc-type :args (format nil ,string ,@format-args)))


;; XXX All uses of PY-ITERATE should be changed to use GET-PY-ITERATE-FUN instead.
;; There's no need for this macro, and this macro is even incorrect in that it calls __iter__.

(defmacro py-iterate ((val object) &body body)
  "Iterate over OBJECT, successively binding VAL to the new value
   and executing BODY.
   This works is OBJECT implements either __iter__ or __getitem__."
  ;; XX assumes OBJECT is a first-class object.
  (let ((iterator '#:iterator))
    `(block py-iterate
       (tagbody 
	 (handler-case (__iter__ ,object)
	   (AttributeError () (go try-getitem))
	   (:no-error (,iterator) (loop (handler-case (next ,iterator)
					  (StopIteration () (return-from py-iterate))
					  (:no-error (,val) ,@body)))))
	try-getitem
	 ,(let ((index '#:index))
	    `(let ((,index 0))
	       (loop 
		 (handler-case (__getitem__ ,object ,index)
		   (AttributeError () (if (= ,index 0)
					  (go error)
					(return-from py-iterate)))
		   (IndexError () ;; even ok if index = 0 (empty sequence)
		     (return-from py-iterate))
		   (:no-error (,val)
		     ,@body
		     (incf ,index))))))
	error
	 (py-raise 'TypeError
		   "Iteration over non-sequence (got: ~A)" ,object)))))
	      

(defun get-py-iterate-fun (object)
  "Return a function that when called returns VAL, T, where VAL is the next value ~@
   gotten by iterating over OBJECT. Returns NIL, NIL upon exhaustion."

  ;; Using __iter__ is the prefered way.
  (multiple-value-bind (iter-meth found)
      (getattr-of-class object '__iter__)
	     
    (if found
	
	(let ((iterator (__call__ iter-meth (list object))))
	  (labels ((get-next-val-fun ()
		     (handler-case (next iterator)
		       (StopIteration () (values nil nil))
		       (:no-error (val)  (values val t)))))
	    (lambda () (get-next-val-fun))))
      
      ;; Fall-back: __getitem__ with successive integers, starting from 0.
      ;; 
      ;; XXX There is the theoretical possibility that the object's
      ;; class, or just the __getitem__ method, changes while we do
      ;; this. By storing the __getitem__ method we just found, we
      ;; don't take that into account. Might be semantically wrong,
      ;; but changing __getitem__ while iterating over the object is
      ;; insane.
      
      (multiple-value-bind (getitem-meth found)
	  (getattr-of-class object '__getitem__)
	(if found

	    (let ((index 0))
	      (labels ((get-next-val-fun ()
			 (handler-case (__call__ getitem-meth (list object index))
			   (IndexError () (values nil nil))  ;; even ok if index = 0 (empty sequence)
			   (:no-error (val)
			     (incf index)
			     (values val t)))))
		(lambda () (get-next-val-fun))))
	  
	  (py-raise 'TypeError
		    "Iteration over non-sequence (got: ~A)" object))))))

(defun py-iterate->lisp-list (object)
  (map-over-py-object #'identity object))

(defun map-over-py-object (fun object)
  "Iterate over OBJECT, calling Lisp function FUN on each value."
  (declare (optimize (speed 3)(debug 0)(safety 0))
	   (:explain :calls))
  (loop with f = (get-py-iterate-fun object)
      with val = (funcall f)
      when val collect (funcall fun val)
      while val do (setf val (funcall f))))


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
				       (python-object . python-object-designator-p)))))
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
    ((eql (find-class 'python-type)) (values x nil))
    (t (error "Not a recognized Python object: ~A" x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bridging the gap: making Lisp functions callable from within
;;; Python

(defmacro def-pyfun (name (&rest args) &body body)
  "Define a Lisp function that is callable from within Python. ~@
   It basically defines (defun NAME ...) and (defmethod __call__ #'NAME ...)."
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
	   (defmethod __call__ ((f (eql #',name)) &optional pos-args key-args)
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
       (__call__ #'foo '(1 2) '((key2 . 3))))

