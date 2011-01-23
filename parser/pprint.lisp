;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.PARSER; Readtable: PY-AST-USER-READTABLE -*-
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
(in-syntax *ast-user-readtable*)

(defvar *py-pprint-dispatch* (copy-pprint-dispatch nil))

(defun py-pprint (ast &optional stream)
  "Print AST as Python source code to STREAM.
If STREAM is not supplied, output goes to a string.
If output goes to a stream, then the first character will be a newline;
output to a string does not start with a newline."
  (flet ((do-print (stream)
	   (let ((*print-pprint-dispatch* *py-pprint-dispatch*)
                 (*print-level* nil)) ;; no limit
	     (pprint ast stream))))
    (if stream
	(do-print stream)
      (let ((str (with-output-to-string (s)
                   (do-print s))))
        (when (plusp (length str))
          (assert (char= (aref str 0) #\Newline))
          (make-array (1- (length str)) 
                      :element-type (array-element-type str)
                      :displaced-to str
                      :displaced-index-offset 1))))))

(defgeneric py-pprint-1 (stream ast)
  (:documentation "Print AST as Python source code to STREAM"))

(set-pprint-dispatch t #'py-pprint-1 0 *py-pprint-dispatch*)

(defmethod py-pprint-1 (stream object)
  ;; Everything not explicitly overridden gets default behaviour
  (with-standard-io-syntax
    (format stream "~A" object)))

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
(defvar *tuple-must-have-brackets* nil)
(defvar *one-item-tuple-gets-comma* t)

(defvar *in-comparison-expr* nil)
(defvar *in-bracketed-expr* nil)

(defmethod py-pprint-1 (stream (x list))
  (case (car x)

    ([assert-stmt]       (let ((*tuple-must-have-brackets* t))
			   (format stream "assert ~A~@[, ~A~]" (second x) (third x))))
    ([assign-stmt]       (format stream "~{~A = ~^~}~A" (third x) (second x)))
    ([attributeref-expr] (format stream "~A.~A"         (second x) (third x)))
    ([augassign-stmt]    (format stream "~A ~A ~A"      (third x) (second x) (fourth x)))
    ([backticks-expr]    (format stream "`~A`"  (second x)))
    
    (([binary-expr] [binary-lazy-expr])
     (destructuring-bind (op left right)
         (cdr x)
       (let* ((lev (cdr (assoc op *binary-op-precedence*)))
              (brackets? (< lev *precedence-level*)))
         (let ((*precedence-level* lev))
           (format stream "~@[(~*~]~A ~A ~A~@[)~*~]" brackets? left op right brackets?)))))
    
    ([bracketed-expr] (let ((*in-bracketed-expr* t))
                        ;; Ignore superfluous brackets.
                        (format stream "~A" (second x))))
    ([break-stmt] (format stream "break"))
    
    ([call-expr] (destructuring-bind (primary pos key *a **a) (cdr x)
		 (format stream "~A(" primary)
		 (print-arg-list stream pos key *a **a)
		 (format stream ")")))

    ([classdef-stmt] (destructuring-bind (name supers suite) (cdr x)
                       (destructuring-bind (tuple-expr super-items)
                           supers
                         (assert (eq tuple-expr '[tuple-expr]))
                         (format stream "~&class ~A" name)
                         (when super-items
                           (let ((*tuple-must-have-brackets* t))
                             (format stream "~A" supers)))
                         (format stream ":~A" suite))))
    
    ([comparison-expr] (when (and *in-comparison-expr* *in-bracketed-expr*) ;; needed for "1 < (2 < 3)"
                         (write-char #\( stream))
                       (let ((*in-comparison-expr* t))
                         (format stream "~A ~A ~A" (third x) (second x) (fourth x)))
                       (when (and *in-comparison-expr* *in-bracketed-expr*)
                         (write-char #\) stream)))
    ([continue-stmt]   (format stream "continue"))
    ([del-stmt]        (format stream "del ~A" (second x)))
    
    ([dict-expr]      (let ((*tuple-must-have-brackets* t))
			(format stream "{~{~A: ~A~}}"
				(loop for (v k) on (second x) by #'cddr
				    collect k
				    collect v))))
		      
    ([exec-stmt]   (destructuring-bind (code globals locals)
                       (cdr x)
                     (let ((*tuple-must-have-brackets* t))
                       (format stream "exec ~A" code)
                       (when globals
                         (format stream " in ~A" globals)
                         (when locals
                           (format stream ", ~A" locals))))))
    
    ([for-in-stmt] (destructuring-bind (targets source suite else-suite)
		     (cdr x)
		   (let ((*suite-no-newline* t))
		     (format stream "for ~A in ~A:~A~@[~&else:~A~]"
			     targets source suite else-suite))))

    ([funcdef-stmt] (destructuring-bind (decorators fname args suite)
		      (cdr x)
		    (declare (ignore decorators))
		    (format stream "def ~A(" fname)
		    (apply #'print-arg-list stream args)
		    (format stream "):~A" suite)))

    ([generator-expr] (format stream "(~A" (second x))
		    (loop for clause in (third x)
			do (ecase (first clause)
			      ([for-in-clause] (format stream " for ~A in ~A"
						       (second clause) (third clause)))
			      ([if-clause]     (format stream " if ~A" (second clause)))))
		    (format stream ")"))
     
    ([global-stmt]     (let ((*one-item-tuple-gets-comma* nil))
                         (format stream "global ~A" (second x))))
    ([identifier-expr] (let ((name (second x)))
                         (if (eq name '{Ellipsis})
                             (format stream "...")
                           (format stream "~A" name))))
    
    ([if-expr] (destructuring-bind (cond then else) (cdr x)
                 (format stream "(~A if ~A else ~A)" then cond else)))
                                                         
    ([if-stmt] (destructuring-bind (clauses else-suite) (cdr x)
 	       (let ((*suite-no-newline* t))
		 (format stream "if ~{~A:~A~}~@[~:{~&elif ~A:~A~}~]~@[~&else:~A~]"
			 (car clauses) (cdr clauses) else-suite))))
    
    
    ([import-stmt] (format stream "import ")
		   (with-standard-io-syntax
		     (format stream "~{~A~^, ~}"
			     (loop for (mod-name as-name) in (second x)
				 collect (format nil "~{~A~^.~}~@[ as ~A~]"
						 mod-name as-name)))))

    ([import-from-stmt] (destructuring-bind (dotted-name items)
			    (cdr x)
			  (format stream "from ~{~A~^.~} import " dotted-name)
			  (if (eq items '[*])
			      (format stream "*")
			    (with-standard-io-syntax
			      (format stream "~{~A~^, ~}"
				      (loop for (item bind-name) in items
					  collect (format nil "~A~@[ as ~A~]" item bind-name)))))))
    
    ([lambda-expr] (destructuring-bind (args expr) (cdr x)
		   (format stream "lambda~:[~; ~]" (some #'identity args))
		   (apply #'print-arg-list stream args)
		   (format stream ": ~A" expr)))

    ([list-expr]   (let ((*tuple-must-have-brackets* t))
		     (format stream "[~{~A~^, ~}]" (second x))))
    
    ([listcompr-expr] (format stream "[")
                      (let ((*tuple-must-have-brackets* t))
                        (format stream "~A" (second x)))
                      (loop for clause in (third x)
                          do (ecase (first clause)
                               ([for-in-clause] (format stream " for ~A in ~A"
                                                        (second clause) (third clause)))
                               ([if-clause]     (format stream " if ~A" (second clause)))))
                      (format stream "]"))
    
    ([literal-expr] (destructuring-bind (kind value) (cdr x)
                      (py-pprint-literal stream kind value)))
    
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
		      (let ((brackets? (or (/= *precedence-level* -1)
					   *tuple-must-have-brackets*))
			    (post-comma? (and (not (cdr items))
                                              *one-item-tuple-gets-comma*)))
			(let ((*tuple-must-have-brackets* t)
                              (*one-item-tuple-gets-comma* t))
			  (format stream "~@[(~*~]~{~A~^, ~}~@[,~*~]~@[)~*~]"
				  brackets? items post-comma? brackets?)))
		    (format stream "()"))))
    
    ([try-except-stmt] (destructuring-bind (try-suite except-suites else-suite)
			       (cdr x)
		       (let ((*suite-no-newline* t))
			 (format stream "try:~A~:{~&except~@[ ~A~@[, ~A~]~]:~2@*~A~}~@[~&else:~A~]"
				     try-suite except-suites else-suite))))
    
    ([try-finally-stmt] (let ((*suite-no-newline* nil))
			(format stream "try:~Afinally:~A" (second x) (third x))))
    
    ([unary-expr] (destructuring-bind (op arg)
                      (cdr x)
                    (let* ((lev (cdr (assoc op *unary-op-precedence*)))
                           (brackets? (< lev *precedence-level*)))
                      
                      (let ((*precedence-level* lev))
                        (format stream "~@[(~*~]" brackets?)
			(format stream "~A" op)
			(when (eq op '[not])
			  (format stream " "))
			(format stream "~A~@[)~*~]" arg brackets?)))))
    
    ([while-stmt] (destructuring-bind (test suite else-suite) (cdr x)
		  (let ((*suite-no-newline* t))
		    (format stream "while ~A:~A~@[else: ~A~]" test suite else-suite))))
    
    ([yield-expr] (format stream "(yield~@[ ~A~])" (second x)))
    ([yield-stmt] (format stream "yield~@[ ~A~]" (second x)))
    
    (t (format stream "(~{~S~^ ~})" x))))
  
(defun print-arg-list (stream pos-args key-args *-arg **-arg)
  (let ((*precedence-level* -1)
	(*tuple-must-have-brackets* t))
    (format stream "~@[~{~A~^, ~}~:[~;, ~]~]"
	    pos-args (or key-args *-arg **-arg))
  
    (format stream "~@[~{~A=~A~^, ~}~:[~;, ~]~]"
	    (loop for (k v) in key-args collect k collect v)
	    (or *-arg **-arg))
    
    (format stream "~@[*~A~:[~;, ~]~]" *-arg **-arg)
    (format stream "~@[**~A~]"         **-arg)))


(defgeneric py-pprint-literal (stream kind value))

(defmethod py-pprint-literal :around (stream kind value)
  (declare (ignore stream kind value))
  (with-standard-io-syntax
    (call-next-method)))

(defmethod py-pprint-literal (stream (kind (eql :number)) (x integer))
  (declare (ignorable kind))
  (format stream "~D" x))

(defmethod py-pprint-literal (stream (kind (eql :number)) (x complex))
  (declare (ignorable kind))
  (assert (= 0 (realpart x)))
  (format stream "~Gj" (imagpart x)))

(defmethod py-pprint-literal (stream (kind (eql :number)) (x float))
  (declare (ignorable kind))
  (format stream "~G" x))

(defmethod py-pprint-literal (stream (kind (eql :bytes)) (x vector))
  (declare (ignorable kind))
  (write-string "b'" stream)
  (loop for item across x
      do (when (characterp item)
           (setf item (char-code item)))
         (check-type item (unsigned-byte 8))
         (format stream "\\x~2,'0x" item))
  (write-string "'" stream))

(defmethod py-pprint-literal (stream (kind (eql :string)) (x string))
  (declare (ignorable kind))
  (multiple-value-bind (delim-quote other-quote unicode?)
      (loop for ch of-type character across x
	  counting (char= ch #\') into single-qs
	  counting (char= ch #\") into double-qs
	  counting (> (char-code ch) 255) into unicode
	  finally (let* ((delim-quote (if (<= single-qs double-qs) #\' #\"))
			 (other-quote (if (char= delim-quote #\') #\" #\'))
			 (unicode? (> unicode 0)))
		    (return (values delim-quote other-quote unicode?))))
    (when unicode? ;; unicode prefix 'u'
      (write-char #\u stream))
    (write-char delim-quote stream) ;; starting quote
    (loop for ch of-type character across (the string x)
        do (cond ((char= ch delim-quote)  (write-char #\\ stream)
                                          (write-char ch stream))
                 ((char= ch other-quote)  (write-char ch stream))
                 ((char= ch #\\)  (write-char #\\ stream)
                                  (write-char #\\ stream))
                 ;; printable ASCII character
                 ((and (<= (char-code ch) 127) 
                       (graphic-char-p ch))     (write-char ch stream))
		 ((> (char-code ch) 255)
                  (format stream "\\u~v,vX"
                          (if (> (char-code ch) #xFFFF) 8 4)
                          #\0 (char-code ch)))
		 ((alphanumericp ch)  (write-char ch stream))
		 (t (loop for ch across
                          ;; Cross-reference: #'(read-kind (string) ..) does the inverse.
                          (case ch
                            (#\Bell      "\\a") 
                            (#\Backspace "\\b")
                            (#\Page      "\\p")
                            (#\Newline   "\\n")
                            (#\Return    "\\r")
                            (#\Tab       "\\t")
                            (#.(code-char 11) "\\v") ;; #\VT or #\PageUp
                            (#\Space     " " )
                            ;; Maybe there are more cases to catch before
                            ;; we encode the character in octal code?
                            (t (format nil "\\0~3,vO" #\0 (char-code ch))))
                        do (write-char ch stream)))))
    (write-char delim-quote stream))) ;; closing quote

(defmethod py-pprint-literal (stream (kind (eql :lisp)) x)
  (declare (ignorable kind))
  (format stream "~A" x))

;; Utils

(defun parse-and-print (str)
  (py-pprint (parse str)))

(defmacro with-python-pprinter (() &body body)
  `(let ((*print-pprint-dispatch* *py-pprint-dispatch*))
     ,@body))
