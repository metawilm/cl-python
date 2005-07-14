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


;; Everything not explicitly overridden gets default behaviour

(defmethod py-pprint (stream object)
  (with-standard-io-syntax
    (format stream "~A" object)))


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
    '((or . 0) (and . 1) (in . 3) (|not in| . 3) (is . 4) (|is not| . 4)
      (< . 5) (<= . 5) (> . 5) (>= . 5) (!= . 5) (== . 5)
      (|\|| . 6) (^ . 7) (& . 8) (<< . 9) (>> . 9)
      (+ . 10) (- . 10) (* . 11) (/ . 11) (% . 11) (// . 11)
      (** . 14)))

(defvar *unary-op-precedence*
    '((not . 2) (+ . 12) (- . 12) (~ . 13)))

(defmethod py-pprint (stream (x list))
  (case (car x)

    (assert-stmt       (format stream "assert ~A~@[, ~A~]" (second x) (third x)))
    (assign-expr       (format stream "~{~A = ~^~}~A" (third x) (second x)))
    (attributeref-expr (format stream "~A.~A"         (second x) (third x)))
    (augassign-expr    (format stream "~A ~A ~A"      (third x) (second x) (fourth x)))
    (backticks-expr    (format stream "`~{~A~^, ~}`"  (second x)))
    
    ((binary-expr binary-lazy-expr)
     (let* ((lev (cdr (assoc (second x) *binary-op-precedence*)))
	    (brackets? (< lev *precedence-level*)))
       
       (let ((*precedence-level* lev))
	 (format stream "~:[~;(~]~A ~A ~A~:[~;)~]"
		 brackets?
		 (third x) (second x) (fourth x)
		 brackets?))))
    
    (break-stmt (format stream "break"))
    
    (call-expr (destructuring-bind (primary args) (cdr x)
		 (format stream "~A(" primary)
		 (apply #'print-arg-list stream args)
		 (format stream ")")))

    (classdef-stmt (destructuring-bind (name supers suite) (cdr x)
		     (format stream "~&class ~A ~A: ~A" name supers suite)))
    
    (comparison-expr (format stream "~A ~A ~A" (third x) (second x) (fourth x)))
    (continue-stmt   (format stream "continue"))
    (del-stmt        (format stream "del ~A" (second x)))
    (dict-expr       (format stream "{~{~A: ~A~^, ~}}"
			     (loop for (k . v) in (second x) collect k collect v)))
    
    (for-in-stmt (destructuring-bind (targets source suite else-suite)
		     (cdr x)
		   (format stream "for ~A in ~A: ~A~@[else: ~A~]"
			   targets source suite else-suite)))

    (funcdef-stmt (destructuring-bind (decorators fname args suite)
		      (cdr x)
		    (declare (ignore decorators))
		    (format stream "def ~A(" fname)
		    (apply #'print-arg-list stream args)
		    (format stream "): ~A" suite)))
    
    (global-stmt     (format stream "global ~{~A~^, ~}" (second x)))
    (identifier-expr (format stream "~A" (second x)))
    
    (if-stmt (destructuring-bind (clauses else-suite) (cdr x)
	       (format stream "if ~{~A: ~A~}~@[~:{elif ~A: ~A~}~]~@[else: ~A~]"
		       (car clauses) (cdr clauses) else-suite)))
    
    (import-stmt (format stream "import ~{~:[~A~*~;~*~{~A~^.~}~]~:[~*~; as ~A~]~^, ~}"
			 (loop for m in (second x)
			     append (list (eq (caadr m) 'dotted)
					  (second m)
					  (cdadr m)
					  (eq (car m) 'as)
					  (third m)))))
    
    (import-from-stmt (format stream "from ~A import ~{~A~:[~*~; as ~A~]~^, ~}"
			      (second x)
			      (loop for i in (third x)
				  append (list (second i)
					       (eq (first i) 'as)
					       (third i)))))
		      
    
    (lambda-expr (destructuring-bind (args expr) (cdr x)
		   (format stream "lambda~:[~; ~]" (some #'identity args))
		   (apply #'print-arg-list stream args)
		   (format stream ": ~A" expr)))

    (list-expr  (format stream "[~{~A~^, ~}]" (second x)))
    (list-compr-expr (format stream "[~A" (second x))
		     (loop for clause in (third x)
			 do (ecase (first clause)
			      (list-for-in (format stream " for ~A in ~A"
						   (second clause) (third clause)))
			      (list-if     (format stream " if ~A" (second clause)))))
		     (format stream "]"))
    (module-stmt  (format stream "~A" (second x)))
    (pass-stmt    (format stream "pass"))
    
    (print-stmt (destructuring-bind (dest items comma?) (cdr x)
		  (format stream "print ~@[>> ~A, ~]~{~A~^, ~}~:[~;,~]"
			  dest items comma?)))
    
    (raise-stmt (format stream "raise ~{~A~^, ~}"
			(loop for y in (cdr x) while y collect y)))
    
    (return-stmt (format stream "return ~@[~A~]" (second x)))
    
    (slice-expr  (destructuring-bind (start stop step) (cdr x)
		   (format stream "~@[~A~]:~@[~A~]~@[:~A~]" start stop step)))

    (subscription-expr (format stream "~A[~A]" (second x) (third x)))
    (suite-stmt        (format stream "~&~<   ~@;~@{~A~^~&~}~:>~&" (second x)))
	   
    (tuple-expr (let ((brackets? t #+(or)(/= *precedence-level* -1)))
		  (format stream "~:[~;(~]~{~A~^, ~}~:[~;,~]~:[~;)~]"
			  brackets? (second x) (cdr (second x)) brackets?)))
    
    (try-except-stmt (destructuring-bind (try-suite except-suites else-suite)
			 (cdr x)
		       (format stream "try: ~A~:{except~@[ ~A~@[, ~A~]~]: ~2@*~A~}~@[else: ~A~]"
			       try-suite except-suites else-suite)))
    
    (try-finally-stmt (format stream "try: ~Afinally: ~A" (second x) (third x)))
    
    (tuple-expr   (format stream "(~{~A~^, ~}~@[,~])" (second x) (not (cdr (second x)))))
    
    (unary-expr (let* ((lev (cdr (assoc (second x) *unary-op-precedence*)))
		       (brackets? (< lev *precedence-level*)))
		  
		  (let ((*precedence-level* lev))
		    (format stream "~@[(~]~A ~A~@[)~]"
			    brackets? (second x) (third x) brackets?))))
    
    (while-stmt (destructuring-bind (test suite else-suite) (cdr x)
		  (format stream "while ~A: ~A" test suite)
		  (when else-suite
		    (format stream "else: ~A" else-suite))))
    
    (yield-stmt (format stream "yield ~A" (second x)))
    
    (t (with-standard-io-syntax (warn "uncatched in py-pprint: ~A" x)
				(format stream "~A" x)))))

  
(defun print-arg-list (stream pos-args key-args *-arg **-arg)
  ;; using:  ~@[, ~]  as a shortcut for:  ~:[~;, ~]
  (format stream "~@[~{~A~^, ~}~:[~;, ~]~]"
	  pos-args (or key-args *-arg **-arg))
  
  (format stream "~@[~{~A=~A~^, ~}~:[~;, ~]~]"
	  (loop for (k . v) in key-args collect k collect v)
	  (or *-arg **-arg))
  
  (format stream "~@[*~A~:[~;, ~]~]" *-arg **-arg)
  (format stream "~@[**~A~]"         **-arg))
