;; -*- package: clpython.parser; readtable: py-ast-user-readtable -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.parser)
(in-syntax *ast-user-readtable*)

;;;; Parser for Python code

(defun handle-parser-condition (c)
  ;; When a SyntaxError is thrown by us in the lexer, the parser
  ;; first signals the SyntaxError, then it raises a GRAMMAR-PARSE-ERROR.
  (declare (special clpython:*exceptions-loaded*))
  (cond ((and clpython:*exceptions-loaded* (typep c '{SyntaxError}))
	 (error c)) ;; Converting SIGNAL to ERROR
	
	((typep c 'excl.yacc:grammar-parse-error)
	 (let* ((pos (excl.yacc:grammar-parse-error-position c))
		(line (second (assoc :line-no pos)))
                (eof-seen (second (assoc :eof-seen pos)))
		(token (excl.yacc:grammar-parse-error-token c))
		(encl-error (excl.yacc::grammar-parse-error-enclosed-error c)))

	   (when (and (integerp token)
		      *include-line-numbers*)
	     (setf token '[newline]))
	   
	   (cond (encl-error ;; Error in one of our grammar rules
		  (when clpython:*exceptions-loaded*
		    (assert (not (typep encl-error '{SyntaxError}))
			() "CLPython: Strange: Parser raises EXCL.YACC:GRAMMAR-PARSE-ERROR ~
                              with a SyntaxError enclosed, without first signalling that ~
                              SyntaxError (~A)." c))
		  (raise-syntax-error
		   (format nil "Parse error at line ~A~@[, at token `~S'~].~%[inner error: ~A]"
			   line token encl-error)))
		 
		 ((or (eq token 'excl.yacc:eof)
                      eof-seen)
		  (raise-unexpected-eof)
		  (assert nil () "unreachable"))
		 
		 (t (raise-syntax-error
		     (format nil "At line ~A, parser got unexpected token: `~A'."
			     line token))
		    (assert nil () "unreachable")))))))

(defun parse-python-with-lexer (&rest lex-options)
  (let* ((lexer (apply #'make-py-lexer lex-options))
	 (grammar (make-instance 'python-grammar :lexer lexer)))
    
    (handler-bind 
	((condition #'handle-parser-condition))
      (excl.yacc:parse grammar))))

(defun parse-python-file (file &rest options)
  "Parse given file (either path or stream), return AST."
  (apply #'parse-python-string (clpython.package::slurp-file file) options))

(defgeneric parse-python-string (string &rest options)
  (:documentation "Parse given string, return AST.")
  
  (:method :around ((s string) &rest options &key (incl-module t))
	   (let ((res (apply #'call-next-method s (sans options :incl-module))))
	     (if incl-module
		 res
	       (destructuring-bind (module-stmt (suite-stmt rest)) res
		 (assert (eq module-stmt '[module-stmt]))
		 (assert (eq suite-stmt '[suite-stmt]))
		 rest))))
	   
  (:method ((s string) &rest options)
	   (declare (ignore incl-module))
	   (let ((next-i 0)
		 (max-i (length s)))
	     
	     (apply #'parse-python-with-lexer
		    :read-chr (lambda ()
				(when (< next-i max-i)
				  (prog1 (char s next-i) (incf next-i))))
		    
		    :unread-chr (lambda (c)
				  (assert (and c (> next-i 0) (char= c (char s (1- next-i)))))
				  (decf next-i))
		    
		    options))))

(defun parse-python-one-expr (string)
  (check-type string string)
  (let ((res (parse-python-string string :incl-module nil)))
    (case (length res)
      (0 (error "String ~S cannot be parsed into a value" string))
      (1 (car res))
      (t (error "String ~S parses into multiple (~A) expressions: ~{~A~^, ~}."
		string (length res) res)))))

(defmacro with-python-code-reader (() &body body)
  ;; The Python parser handles all reading.
  `(let ((*readtable* (load-time-value
                       (setup-omnivore-readmacro #'parse-python-file
                                                 (copy-readtable nil)))))
     ,@body))
