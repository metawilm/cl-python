(in-package :python)

;; Pythonic:  some nice macros to do Pythonic things


(defmacro py-raise (exc-type string &rest format-args)
  "Raise Python exception of type EXC-TYPE, ~@
   where STRING with FORMAT-ARGS is the exception argument."
  `(error ,exc-type :args (format nil ,string ,@format-args)))

(defmacro py-raise-simple (exc-type val)
  "Raise Python exception with simple value"
  `(error ,exc-type :args ,val))

(defmacro py-iterate ((val object) &body body)
  "Iterate over OBJECT, successively binding VAL to the new value
   and executing BODY.
   This works is OBJECT implements either __iter__ or __getitem__."
  ;; XX assumes OBJECT is a first-class object.
  (let ((iterator '#:iterator))
    `(block py-iterate
       (tagbody 
	 (handler-case (__iter__ ,object)
	   ((or AttributeError %magic-method-missing%) ()
	     (go try-getitem))
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
		   (%magic-method-missing% ()
		     (go error))
		   (:no-error (,val)
		     ,@body
		     (incf ,index))))))
	error
	 (py-raise 'TypeError
		   "Iteration over non-sequence (got: ~A)" ,object)))))
	      
  
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
;;; if, while

(defmacro py-if (test &optional then else)
  `(if (py-val->lisp-bool ,test)
       ,then
     ,else))

(defmacro py-while (test &body body)
  `(loop
     (if (not (py-val->lisp-bool ,test))
	 (return))
     ,@body))

#+(or) ;; test
(let ((i (make-int 3)))
  (py-while i
	    (format t "i = ~A~%" i)
	    (setf i (__sub__ i 1))))


;;; Standard logical operators: OR, AND, NOT
;;;
;;; XXX They expand into nested binary operations.
;;; (there's no *-ary OR/.. in Python)

(defmacro py-or (&rest clauses)
  "Returns generalized Lisp boolean"
  (if clauses
      `(or (py-val->lisp-bool ,(car clauses))
	   (py-or ,@(cdr clauses)))
    nil))
	   
(defmacro py-and (&rest clauses)
  "Returns generalized Lisp boolean"
  (if clauses
      `(and (py-val->lisp-bool ,(car clauses))
	    (py-and ,@(cdr clauses)))
    t))

(defmacro py-not (x)
  "Returns generalized Lisp boolean"
  `(not (py-val->lisp-bool ,x)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bridging the gap: making Lisp functions callable from within Python

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

(assert (equal (multiple-value-list (lispargs->pyargs '(a b &key (key1 val1) (key2 val2))))
	       (list '(a b) '(key1 key2) '((key1 . val1) (key2 . val2)))))

#+(or) ;; test case
(progn (def-pyfun foo (a b &key (key1 *None*) (key2 *None*))
	 (format t "FOO got:  a: ~S  b: ~S  key1: ~S  key2: ~S~%" a b key1 key2))
       (foo 1 2 :key2 3)
       (__call__ #'foo '(1 2) '((key2 . 3))))
