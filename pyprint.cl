(in-package :python)

;;; Pretty printer for parsed Python code
;;;
;;; Of course this is fully dependent on the form of the AST that the
;;; parser yields.
;;;
;;;
;;; TODO: 
;;;  - insert line ends for too long lines

(defvar *py-pprint-dispatch* (copy-pprint-dispatch nil))

(defun ast->python-code (ast)
  (let ((*print-pprint-dispatch* *py-pprint-dispatch*))
    (with-output-to-string (*standard-output*)
      (pprint ast))))

(defun parse-and-print (str)
  (write-string (ast->python-code (parse-python-string str))))

(defgeneric py-pprint (stream ast)
  (:documentation "Print AST as Python source code to STREAM"))

(set-pprint-dispatch t #'py-pprint 0 *py-pprint-dispatch*)


;;; Number

(defmethod py-pprint (stream (x integer))
  (with-standard-io-syntax
    (format stream "~D" x)))

(defmethod py-pprint (stream (x float))
  (with-standard-io-syntax
    (format stream "~G" x)))

(defmethod py-pprint (stream (x complex))
  (assert (= 0 (realpart x)))
    (with-standard-io-syntax
      (format stream "~Gj" (imagpart x))))

#+(or) ;; needed?
(defmethod py-pprint (stream (x character)) 
  (with-standard-io-syntax
    (write-char x stream)))

(defmethod py-pprint (stream (x symbol))
  (with-standard-io-syntax
    (write-string (symbol-name x) stream)))


(defmethod py-pprint (stream (x string))  
  ;; see also READ-STRING in PARSEPYTHON.CL

  (multiple-value-bind (delim-quote other-quote unicode?)
      (loop for ch across x
	  counting (char= ch #\') into single-qs
	  counting (char= ch #\") into double-qs
	  counting (> (char-code ch) 255) into unicode
	  finally (let* ((delim-quote (if (<= single-qs double-qs) #\' #\"))
			 (other-quote (if (char= delim-quote #\') #\" #\'))
			 (unicode? (> unicode 0)))
		    (return (values delim-quote other-quote unicode?))))
    
    (with-standard-io-syntax ;; no recursion please
      
      (when unicode? ;; unicode prefix 'u'
	(write-char #\u stream))
      
      (write-char delim-quote stream) ;; starting quote
      
      (loop for ch across x
	  do (cond ((char= ch delim-quote)  (write-char #\\ stream)
					    (write-char ch stream))
		   
		   ((char= ch other-quote)  (write-char ch stream))
		   
		   ((char= ch #\\)  (write-char #\\ stream)
				    (write-char #\\ stream))
			 
		   ;; printable ASCII character
		   ((and (<= (char-code ch) 127) 
			 (graphic-char-p ch))     (write-char ch stream))
		   
		   ((> (char-code ch) 255)
		    (format stream "\u~v,vX"
			    (if (> (char-code ch) #16xFFFF) 8 4)
			    #\0 (char-code ch)))
		   
		   ((alphanumericp ch)  (write-char ch stream))
			 
		   (t (loop for ch across
			    (case ch
			      (#\Bell      "\\a") 
			      (#\Backspace "\\b")
			      (#\Page      "\\f")
			      (#\Newline   "\\n")
			      (#\Return    "\\r")
			      (#\Tab       "\\t")
			      (#\VT        "\\v")
			      (#\Space     " " )
			      
			      ;; Maybe there are more cases to catch before
			      ;; we encode the character in octal code?
			      
			      (t (format nil "\\0~3,vO" #\0 (char-code ch))))
			    
			  do (write-char ch stream)))))
      
      (write-char delim-quote stream)))) ;; closing quote


(defvar *precedence-level* -1)

;; XXX check prec levels
(defvar *binary-op-precedence*
    '((or . 0)(and . 1)(in . 3)(|not in| . 3)(is . 4)(|is not| . 4)
      (< . 5)(<= . 5)(> . 5)(>= . 5)(!= . 5) (== . 5)
      (|\|| . 6)(^ . 7)(& . 8)(<< . 9)(>> . 9)
      (+ . 10)(- . 10) (* . 11)(/ . 11)(% . 11)(// . 11)
      (** . 14)))

(defvar *unary-op-precedence* ;; XXX check
    '((not . 2)(+ . 12)(- . 12)(~ . 13)))

(defmethod py-pprint (stream (x list))
  (case (car x)
    (assert (let ((data (cdr x)))
	      (format stream "assert ~A" (first data))
	      (when (second data)
		(format stream ", ~A" (second data)))))
    
    (assign-expr (format stream "~{~A = ~^~}~A" (third x) (second x)))
    
    (attributeref (format stream "~A.~A" (second x) (third x)))
    
    (augassign-expr (format stream "~A ~A ~A" (second x) (third x) (fourth x)))
    (backticks   (format stream "`~{~A~^, ~}`" (second x)))
    
    ((binary binary-lazy)
     (let* ((lev (cdr (assoc (second x) *binary-op-precedence*)))
	    (brackets? (< lev *precedence-level*)))
       #+(or) ;; debug
       (with-standard-io-syntax 
	 (warn "brackets: ~A=~A, old=~A -> ~A"
	       (second x) lev *precedence-level* brackets?))
       (when brackets? (write-char #\( stream))
       (let ((*precedence-level* lev))
	 (format stream "~A ~A ~A"
		 (third x) (second x) (fourth x)))
       (when brackets? (write-char #\) stream))))
    
    (break (format stream "~A" (first x)))
    
    (call (destructuring-bind (primary args) (cdr x)
	    (format stream "~A(" primary)
	    (let ((first-arg t))
	      (dolist (a args)
		(if first-arg
		    (setf first-arg nil)
		  (format stream ", "))
		(ecase (car a)
		  (pos (format stream "~A" (second a)))
		  (key (format stream "~A=~A" (second a) (third a)))
		  (*   (format stream "*~A" (second a)))
		  (**  (format stream "**~A" (second a))))))
	    (format stream ")")))

    (classdef (destructuring-bind (name supers suite) (cdr x)
		(format stream "~&class ~A ~A: ~A" name supers suite)))
    
    (comparison (format stream "~A ~A ~A" (third x) (second x) (fourth x)))

    (continue (format stream "~A" (first x)))
    
    (del (format stream "del ~A" (second x)))
    
    (dict    (progn (format stream "{")
		    (loop for (k . v) in (second x)
			do (format stream "~A: ~A, ~_" k v))
		    (format stream "}")))
    (ellipsis (format stream "..."))
    (file-input (dolist (s (second x))
		  (format stream "~A~%" s)))
    
    (for-in (destructuring-bind (targets source suite else-suite)
		(cdr x)
	      (format stream "for ~A in ~A: ~A" targets source suite)
	      (when else-suite
		(format stream "else: ~A " else-suite))))

    (funcdef (destructuring-bind (name vars suite) (cdr x)
	       (format stream "def ~A(" name)
	       (let ((first-arg t))
		 (dolist (var (first vars))
		   (if first-arg
		       (setf first-arg nil)
		     (format stream ", "))
		   (if (symbolp var)
		       (format stream "~A" var)
		     (format stream "~A=~A" (car var) (cdr var))))
		 (when (second vars)
		   (if first-arg
		       (setf first-arg nil)
		     (format stream ", "))
		   (format stream "*~A" (second vars)))
		 (when (third vars)
		   (if first-arg
		       (setf first-arg nil)
		     (format stream ", "))
		   (format stream "**~A" (third vars))))
	       (format stream "): ")
	       (format stream "~A" suite)))
    
    (global (format stream "global ~{~A~^, ~A~}" (second x)))
    
    (identifier (format stream "~A" (second x)))
    
    (if (destructuring-bind ((ftest fsuite) &rest other-tests)
	    (second x)
	  (format stream "if ~A: ~A" ftest fsuite)
	  (loop for (test suite) in other-tests
	      do (format stream "elif ~A: ~A" test suite))
	  (when (third x)
	    (format stream "else: ~A" (third x)))))
    
    (import (format stream "import ")
	    (let ((first t))
	      (dolist (mod (second x))
		(if first
		    (setf first nil)
		  (format stream ", "))
		(case (car mod)
		  (not-as (format stream "~A" (second mod)))
		  (as (format stream "~A as ~A" (second mod) (third mod)))))))
    
    (import-from (format stream "from ~A import " (second x))
		 (let ((first t))
		   (dolist (mod (third x))
		     (if first
			 (setf first nil)
		       (format stream ", "))
		     (assert (eq (car mod) 'as)) ;; TODO: keep this AST format?
		     (if (eq (second mod) (third mod))
			 (format stream "~A" (second mod))
		       (format stream "~A as ~A" (second mod) (third mod))))))
    
    (lambda (destructuring-bind ((params *-arg **-arg) expr) (cdr x)
	      (format stream "lambda")
	      (let ((first-arg t))
		(flet ((maybe-first-comma ()
			 (if first-arg
			     (progn (setf first-arg nil)
				    (format stream " "))
			   (format stream ", "))))
		  (dolist (p params)
		    (maybe-first-comma)
		    (if (symbolp p)
			(format stream "~A" p)
		      (format stream "~A=~A" (car p) (cdr p))))
		  (when *-arg
		    (maybe-first-comma)
		    (format stream "*~A" *-arg))
		  (when **-arg
		    (maybe-first-comma)
		    (format stream "**~A" **-arg))
		  (format stream ": ~A" expr)))))

    (list  (format stream "[~{~A~^, ~}]" (second x)))
    (list-compr (format stream "[~A" (second x))
		(loop for clause in (third x)
		    do (case (first clause)
			 (list-for-in (format stream " for ~A in ~A"
					      (second clause) (third clause)))
			 (list-if     (format stream " if ~A" (second clause)))))
		(format stream "]"))
    
    (pass (format stream "pass"))
    
    (print (format stream "print ~{~A~^, ~}" (second x))
	   (when (third x)
	     (format stream ",")))
    
    (print->> (format stream "print >>~A, ~{~A~^, ~}" (second x) (third x))
	      (when (third x)
		(format stream ",")))
    
    (raise (format stream "raise ~{~A~^, ~}"
		   (loop for y in (cdr x) while y collect y)))
    
    (return (format stream "return ")
	    (when (second x)
	      (format stream "~A" (second x))))
    
    (slice (destructuring-bind (a b c) (cdr x)
	     (unless (null a)
	       (format stream "~A" a))
	     (format stream ":")
	     (unless (null b)
	       (format stream "~A" b))
	     (when c
	       (format stream ":~A" c))))

    (subscription (format stream "~A[~A]" (second x) (third x)))

    (suite (format stream "~&~<   ~@;~@{~A~^~&~}~:>~&" (second x)))
	   
    (tuple (let ((brackets? (/= *precedence-level* -1)))
	     (when brackets? (format stream "("))
	     (let ((items (second x)))
	       (format stream "~{~A~^, ~}" items)
	       (unless (cdr items)
		 (format stream ",")))
	     (when brackets? (format stream ")"))))
    
    (try-except (destructuring-bind (try-suite except-suites else-suite)
		    (cdr x)
		  (format stream "try: ~A" try-suite)
		  (loop for ((exc-class param) suite) in except-suites
		      do (cond ((null exc-class)
				(format stream "except: ~A" suite))
			       ((null param)
				(format stream "except ~A: ~A"
					exc-class suite))
			       (t
				(format stream "except ~A, ~A: ~A"
					exc-class param suite))))
		  (when else-suite
		    (format stream "else: ~A" else-suite))))
    
    (try-finally (format stream "try: ~Afinally: ~A"
			 (second x) (third x)))
    
    (tuple   (format stream "(~{~A~^, ~},)" (second x)))
    
    (unary (let* ((lev (cdr (assoc (second x) *unary-op-precedence*)))
		  (brackets? (< lev *precedence-level*)))
	     #+(or) ;; debug
	     (with-standard-io-syntax 
	       (warn "brackets: ~A=~A, old=~A -> ~A"
		     (second x) lev *precedence-level* brackets?))     
	     (when brackets? (write-char #\( stream))
	     (let ((*precedence-level* lev))
	       (format stream "~A ~A" (second x) (third x)))
	     (when brackets? (write-char #\) stream))))
    
    (while (destructuring-bind (test suite else-suite) (cdr x)
	     (format stream "while ~A: ~A" test suite)
	     (when else-suite
	       (format stream "else: ~A" else-suite))))
    
    (yield (format stream "yield ~A" (second x)))
    
    (t (error "uncatched in py-pprint: ~A" x))))



    