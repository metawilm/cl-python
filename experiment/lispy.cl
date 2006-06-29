;;; Lisp-Python syntax

(in-package :python)

(defparameter *the-lispy-module* (make-module) "Python module")

#||
(defvar *the-lispy-package* nil "Lisp package")
||#

(defmacro in-lisp-py-syntax (&key module)
  (declare (ignore module))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *readtable* (let ((rt (copy-readtable)))
			 (loop for i from 0 below 256
			     do (set-macro-character (code-char i) #'lispy-parser t rt))
			 rt))))

(defun lispy-accolade-reader (stream char)
  (assert (char= char #\{))
  
  (let* ((string (coerce (loop with chars = ()
			     for ch = (read-char stream nil nil)
				      
			     do (cond ((null ch)      (lispy-eof-error))
				      
				      ((char= ch #\}) ;; don't unread #\}
						      (return (nreverse chars)))
				      
				      (t (push ch chars))))
			 'string))
	 
	 (python-ast (parse-python-string string)))
    
    (destructuring-bind (module-stmt (suite-stmt items)) python-ast
      (assert (eq module-stmt 'module-stmt))
      (assert (eq suite-stmt 'suite-stmt))
      `(in-lispy-python ,@items))))

(defun lispy-eof-error ()
  (py-raise 'EOFError "Got unexpected end-of-file"))

(defparameter *lispy-lisp-readtable* 
    (let ((r (copy-readtable nil)))
      (set-macro-character #\{ #'lispy-accolade-reader nil r)
      r))
				
(defun lispy-parser (stream ch)
  ;; Heuristic: 
  ;; 
  ;;  - A new paragraph is started if the first non-blank character
  ;;    of the line is located in the left-most column.
  ;;
  ;;  - Every paragraph that can be parsed as Python code, is
  ;;    treated as Python code.
  ;;
  ;;  - Each paragraph can be either in Lisp or Python 

  (unread-char ch stream)
  (let* ((lines (loop for line = (read-line stream nil nil t)
		    while line
		    collect (concatenate 'string line #.(format nil "~%"))))
	 
	 ;; Group lines in paragraphs
	 (paragraphs (let ((groups ())
			   (acc ()))
		       (loop for line = (pop lines)
			   do (when (or (and (> (length line) 0)
					     (not (member (aref line 0)
							  '(#\Space #\Newline #\Tab) :test 'char=)))
					(null line))
				
				;; End of previous paragraph
				(push (apply #'concatenate 'string (nreverse acc)) groups)
				(setf acc ()))
			      
			      (if line
				  (push line acc)
				(return (nreverse groups))))))
	 
	 (res ()))

    ;; Fill RES
    (block :fill-res
      (let (p)
	(tagbody
	  #+(or)(warn "tagbody")
	  
	 :start
	  
	  (setf p (pop paragraphs))
	  #+(or)(warn "p: ~A" p)
	  
	  (unless p
	    (return-from :fill-res))
	  
	  (when (= (length (py-string.strip p)) 0)
	    (go :start))
	  
	  
	 :again
	  
	  (handler-case
	      (values (parse-python-string p))
	    
	    (py-syntax-eof-condition () 
	      #+(or)(warn "py: syntax-eof error")
	      (setf p (concatenate 'string p (or (pop paragraphs)
						 (lispy-eof-error))))
	      (go :again))
	    
	    (SyntaxError ()
	      #+(or)(warn "py: syntax error")
	      (let ((lisp-form (ignore-errors (with-standard-io-syntax
						(let ((*package* #.*package*)
						      (*readtable* *lispy-lisp-readtable*))
						  (read-from-string p nil nil))))))
		(if lisp-form
		    (progn #+(or)(warn "ok, Lisp: ~A" lisp-form)
			   (push `(in-lispy-lisp ,lisp-form) res))
		  
		  (if (or (= (length p) 0)
			  (char/= (aref p 0) #\;))
		      (warn "Oops: ~A" p)
		    #+(or)(warn #| 'SyntaxError|# "Cannot parse (neither Lisp nor Python):~%~S" p)))))
	    
	    (:no-error (python-ast)
	      (destructuring-bind (module-stmt (suite-stmt items)) python-ast
		(assert (eq module-stmt 'module-stmt))
		(assert (eq suite-stmt 'suite-stmt))
		#+(or)(warn "ok, Python: ~A" python-ast)
		(push `(in-lispy-python ,@items) res))))
	  
	  (go :start))))
    
    (setf res (nreverse res))
    
    `(progn ,@res)))

(defmacro with-bridges ((py-module) &body body)
  `(handler-bind ((py-unbound-variable
		   (lambda (c)
		     (let* ((r (find-restart 'use-value))
			    (varname (py-unbound-variable-varname c))
			    (lispfunc (symbol-function varname)))
		       (if (and r lispfunc)
			   (use-value lispfunc)
			 (warn "in-lispy-python: py-unbound-variable ~A unresolved" varname)))))
		  
		  (cell-error (lambda (c)
				(let* ((varname (cell-error-name c))
				       (pyval   (py-attr ,py-module varname)))
				  (if pyval
				      (use-value pyval)
				    (warn "in-lispy-lisp: cell-error ~A unresolved" varname)))))
		  
		  ;; Muffly warnings about undefined functions, that might in fact be Python functions
		  ;; defined at runtime.
		  (style-warning (lambda (c) (muffle-warning c))))
     
     ,@body))

(defun replb ()
  (with-bridges (*repl-mod*)
    (repl)))

#+(or)
(defmacro in-lispy-python (&body body)
  `(let ((*py-signal-conditions* t)
	 (*in-lispy-python* t)
	 (*level* (1+ *level*)))
     
     (declare (special *py-signal-conditions*))
     
     (warn "entering in-lispy-python [~A]" *level*)
     
     (prog1 (handler-bind ((py-unbound-variable
			    (lambda (c)
			      (let* ((r (find-restart 'use-value))
				     (varname (py-unbound-variable-varname c))
				     (lispfunc (symbol-function varname)))
				(if (and r lispfunc)
				    (use-value lispfunc)
				  (warn "in-lispy-python: py-unbound-variable ~A unresolved" varname))))))
       
	      (let ((f (compile nil `(lambda () (with-this-module-context (,*the-lispy-module*)
						  ,@',body)))))
		(funcall f)))
	     
       (warn "exiting in-lispy-python [~A]" *level*))))

(defmacro in-lispy-python (&body body)
  `(let ((*py-signal-conditions* t)
	 (*level* (1+ *level*)))
     
     (with-bridges (*the-lispy-module*)
	 (let ((f (compile nil `(lambda () (with-this-module-context (,*the-lispy-module*)
					     ,@',body)))))
	   (funcall f)))))

(defvar *level* 0)


(defmacro in-lispy-lisp (&body body)
  `(let ((*level* (1+ *level*)))
     
     (with-bridges (*the-lispy-module*) 
	 (let ((f (compile nil `(lambda () (with-this-module-context (,*the-lispy-module*)
					     ,@',body)))))
	   (funcall f)))))


#||
(defmacro with-py-context ((mod) &body body)
  `(
||#