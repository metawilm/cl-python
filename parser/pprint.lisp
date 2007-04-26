;; -*- package: clpython.parser; readtable: py-ast-readtable -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Pretty printer for Python ASTs

;; Of course this is fully dependent on the form of the AST that the
;; parser yields.
;;
;; TODO: 
;;  - insert line breaks for too long lines

(in-package :clpython.parser)
(in-syntax *ast-readtable*)

(defvar *py-pprint-dispatch* (copy-pprint-dispatch nil))

(defun py-pprint (ast &optional stream)
  "Print AST as Python source code to STREAM. STREAM defaults to standard output"
  (flet ((do-print (stream)
	   (let ((*print-pprint-dispatch* *py-pprint-dispatch*))
	     (pprint ast stream))))
    (if stream
	(do-print stream)
      (with-output-to-string (s)
	(do-print s)))))

(defgeneric py-pprint-1 (stream ast)
  (:documentation "Print AST as Python source code to STREAM"))

(set-pprint-dispatch t #'py-pprint-1 0 *py-pprint-dispatch*)

(defmethod py-pprint-1 (stream object)
  ;; Everything not explicitly overridden gets default behaviour
  (with-standard-io-syntax
    (format stream "~A" object)))

(defmethod py-pprint-1 (stream (x integer))
  (with-standard-io-syntax
    (format stream "~D" x)))

(defmethod py-pprint-1 (stream (x float))
  (with-standard-io-syntax
    (format stream "~G" x)))

(defmethod py-pprint-1 (stream (x complex))
  (assert (= 0 (realpart x)))
    (with-standard-io-syntax
      (format stream "~Gj" (imagpart x))))

(defmethod py-pprint-1 (stream (x string))  
  (multiple-value-bind (delim-quote other-quote unicode?)
      (loop for ch character across x
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
      
      (loop for ch character across (the string x)
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
			      (#\Page      "\\p")
			      (#\Newline   "\\n")
			      (#\Return    "\\r")
			      (#\Tab       "\t")
			      (#\VT        "\v")
			      (#\Space     " " )
			      
			      ;; Maybe there are more cases to catch before
			      ;; we encode the character in octal code?
			      
			      (t (format nil "\0~3,vO" #\0 (char-code ch))))
			    
			  do (write-char ch stream)))))
      
      (write-char delim-quote stream)))) ;; closing quote


(defvar *precedence-level* -1)

;; XXX check prec levels
(defvar *binary-op-precedence*
    '(([or] . 0) ([and] . 1) ([in] . 3) ([not in] . 3) ([is] . 4) ([is not] . 4)
      ([<] . 5) ([<=] . 5) ([>] . 5) ([>=] . 5) ([!=] . 5) ([==] . 5)
      ([\|] . 6) ([^] . 7) ([&] . 8) ([<<] . 9) ([>>] . 9)
      ([+] . 10) ([-] . 10) ([*] . 11) ([/] . 11) ([%] . 11) ([//] . 11)
      ([**] . 14)))

(defvar *unary-op-precedence*
    '(([not] . 2) ([+] . 12) ([-] . 12) ([~] . 13)))

(defvar *suite-no-newline* nil)

(defmethod py-pprint-1 (stream (x list))
  (case (car x)

    ([assert-stmt]       (format stream "assert ~A~@[, ~A~]" (second x) (third x)))
    ([assign-stmt]       (format stream "~{~A = ~^~}~A" (third x) (second x)))
    ([attributeref-expr] (format stream "~A.~A"         (second x) (third x)))
    ([augassign-stmt]    (format stream "~A ~A ~A"      (third x) (second x) (fourth x)))
    ([backticks-expr]    (format stream "`~{~A~^, ~}`"  (second x)))
    
    (([binary-expr] [binary-lazy-expr])
     (let* ((lev (cdr (assoc (second x) *binary-op-precedence*)))
	    (brackets? (< lev *precedence-level*)))
       
       (let ((*precedence-level* lev))
	 (format stream "~:[~;(~]~A ~A ~A~:[~;)~]"
		 brackets?
		 (third x) (second x) (fourth x)
		 brackets?))))
    
    ([break-stmt] (format stream "break"))
    
    ([call-expr] (destructuring-bind (primary args) (cdr x)
		 (format stream "~A(" primary)
		 (apply #'print-arg-list stream args)
		 (format stream ")")))

    ([classdef-stmt] (destructuring-bind (name supers suite) (cdr x)
		     (format stream "~&class ~A ~A: ~A" name supers suite)))
    
    ([comparison-expr] (format stream "~A ~A ~A" (third x) (second x) (fourth x)))
    ([continue-stmt]   (format stream "continue"))
    ([del-stmt]        (format stream "del ~A" (second x)))
    ([dict-expr]       (format stream "{~{~A: ~A~^, ~}}"
			     (loop for (k . v) in (second x) collect k collect v)))
    
    ([for-in-stmt] (destructuring-bind (targets source suite else-suite)
		     (cdr x)
		   #+(or)(format stream "for ~A in ~A: ~A~@[else: ~A~]"
				 targets source suite else-suite)
		   (let ((*suite-no-newline* t))
		     (format stream "for ~A in ~A: ~A~@[~&else: ~A~]"
			     targets source suite else-suite))))

    ([funcdef-stmt] (destructuring-bind (decorators fname args suite)
		      (cdr x)
		    (declare (ignore decorators))
		    (format stream "def ~A(" fname)
		    (apply #'print-arg-list stream args)
		    (format stream "): ~A" suite)))

    ([generator-expr] (format stream "(~A" (second x))
		    (loop for clause in (third x)
			do (ecase (first clause)
			      ([for-in-clause] (format stream " for ~A in ~A"
						       (second clause) (third clause)))
			      ([if-clause]     (format stream " if ~A" (second clause)))))
		    (format stream ")"))
     
    ([global-stmt]     (format stream "global ~{~A~^, ~}" (second x)))
    ([identifier-expr] (format stream "~A" (second x)))
    
    ([if-stmt] (destructuring-bind (clauses else-suite) (cdr x)
	       #+(or)(format stream "if ~{~A: ~A~}~@[~:{elif ~A: ~A~}~]~@[else: ~A~]"
			     (car clauses) (cdr clauses) else-suite)
	       
 	       (let ((*suite-no-newline* t))
		 (format stream "if ~{~A: ~A~}~@[~:{~&elif ~A: ~A~}~]~@[~&else: ~A~]"
			 (car clauses) (cdr clauses) else-suite))))
    
    
    ([import-stmt] (format stream "import ~{~:[~A~*~;~*~{~A~^.~}~]~:[~*~; as ~A~]~^, ~}"
			 (loop for m in (second x)
			     append (list (eq (caadr m) 'dotted)
					  (second m)
					  (cdadr m)
					  (eq (car m) 'as)
					  (third m)))))
    
    ([import-from-stmt] (format stream "from ~A import ~{~A~:[~*~; as ~A~]~^, ~}"
			      (second x)
			      (loop for i in (third x)
				  append (list (second i)
					       (eq (first i) 'as)
					       (third i)))))
		      
    
    ([lambda-expr] (destructuring-bind (args expr) (cdr x)
		   (format stream "lambda~:[~; ~]" (some #'identity args))
		   (apply #'print-arg-list stream args)
		   (format stream ": ~A" expr)))

    ([list-expr]  (format stream "[~{~A~^, ~}]" (second x)))
    ([listcompr-expr] (format stream "[~A" (second x))
		    (loop for clause in (third x)
			do (ecase (first clause)
			      ([for-in-clause] (format stream " for ~A in ~A"
						       (second clause) (third clause)))
			      ([if-clause]     (format stream " if ~A" (second clause)))))
		    (format stream "]"))
    
    ([module-stmt]  (let ((suite (cadr x)))
		    (assert (eq (first suite) '[suite-stmt]))
		    (format stream "~{~A~%~}" (second suite))))
    
    ([pass-stmt]    (format stream "pass"))
    
    ([print-stmt] (destructuring-bind (dest items comma?) (cdr x)
		  (format stream "print ~@[>> ~A, ~]~{~A~^, ~}~:[~;,~]"
			  dest items comma?)))
    
    ([raise-stmt] (format stream "raise ~{~A~^, ~}"
			(loop for y in (cdr x) while y collect y)))
    
    ([return-stmt] (format stream "return ~@[~A~]" (second x)))
    
    ([slice-expr]  (destructuring-bind (start stop step) (cdr x)
		   (format stream "~@[~A~]:~@[~A~]~@[:~A~]" start stop step)))

    ([subscription-expr] (format stream "~A[~A]" (second x) (third x)))
    
    #+(or) ;; this version does not treat docstrings specially
    ([suite-stmt]        (format stream "~&~<    ~@;~@{~A~^~&~}~:>~&" (second x)))
    
    ([suite-stmt] #+(or)
		;; docstring at the head of the suite: print as:  """docstring"""
		;; There are still issues with escaping; let's leave it out for now.
		(destructuring-bind (item-1 &rest items) (second x)
		  (if (stringp item-1)
		      (let ((body-s (with-standard-io-syntax
				      (format nil "\"\"\"~A\"\"\"~&" item-1)))
			    (items-s (mapcar (lambda (x) (format nil "~A" x)) items)))
			
			(with-standard-io-syntax
			  (break "body-s: ~S item-s: ~S" body-s items-s))
			
			;; standard syntax, otherwise strings are
			;; printed with Python string escapes
			
			(with-standard-io-syntax
			  (format stream "~&~@<    ~@;~A~{~A~^~&~}~:>~&" body-s items-s)))))
		
		#+(or)
		(format stream "~&~<    ~@;~@{~A~^~&~}~:>~&" (second x))
		
		(progn (let ((*suite-no-newline* nil))
			 (format stream "~&~<    ~@;~@{~A~^~&~}~:>" (second x)))
		       (unless *suite-no-newline*
			 (format stream "~&"))))
	   
    ([tuple-expr] (let* ((items (second x)))
		  (if items
		      (let ((brackets? t #+(or)(/= *precedence-level* -1))
			    (post-comma? (not (cdr items))))
			(format stream "~@[(~*~]~{~A~^, ~}~@[,~*~]~@[)~*~]"
				brackets? items post-comma? brackets?))
		    (format stream "()"))))
    
    ([try-except-stmt] (destructuring-bind (try-suite except-suites else-suite)
			       (cdr x)
		       (let ((*suite-no-newline* t))
			 (format stream "try: ~A~:{~&except~@[ ~A~@[, ~A~]~]: ~2@*~A~}~@[~&else: ~A~]"
				     try-suite except-suites else-suite))))
    
    ([try-finally-stmt] (let ((*suite-no-newline* nil))
			(format stream "try: ~Afinally: ~A" (second x) (third x))))
    
    ([unary-expr] (let* ((lev (cdr (assoc (second x) *unary-op-precedence*)))
		       (brackets? (< lev *precedence-level*)))
		  
		  (let ((*precedence-level* lev))
		    (format stream "~@[(~]~A ~A~@[)~]"
			    brackets? (second x) (third x) brackets?))))
    
    ([while-stmt] (destructuring-bind (test suite else-suite) (cdr x)
		  (let ((*suite-no-newline* t))
		    (format stream "while ~A: ~A~@[else: ~A~]" test suite else-suite))))
    
    ([yield-stmt] (format stream "yield ~A" (second x)))
    
    (t (with-standard-io-syntax
	 #+(or)(warn "uncatched in py-pprint-1: ~A" x)
	 (format stream "~A" x)))))

  
(defun print-arg-list (stream pos-args key-args *-arg **-arg)
  (format stream "~@[~{~A~^, ~}~:[~;, ~]~]"
	  pos-args (or key-args *-arg **-arg))
  
  (format stream "~@[~{~A=~A~^, ~}~:[~;, ~]~]"
	  (loop for (k v) in key-args collect k collect v)
	  (or *-arg **-arg))
  
  (format stream "~@[*~A~:[~;, ~]~]" *-arg **-arg)
  (format stream "~@[**~A~]"         **-arg))


;; Utils

(defun parse-and-print (str)
  (py-pprint (parse-python-string str)))