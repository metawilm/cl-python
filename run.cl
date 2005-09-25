(in-package :python)

;;; Interface to the parser and lexer.

(defun parse-python-with-lexer (&rest lex-options)
  (let* ((lexer (apply #'make-py-lexer lex-options))
	 (grammar (make-instance 'python-grammar :lexer lexer)))
    
    (handler-case 
	(parse grammar)
      
      ;; When a SyntaxError S is thrown by us in the lexer, the parser
      ;; first signals S, then it raises a GRAMMAR-PARSE-ERROR.
      
      (SyntaxError (e) ;; signaled
	(error e))
      
      (grammar-parse-error (c)
	(let* ((pos (grammar-parse-error-position c))
	       (line (second (assoc :line-no pos)))
	       (token (yacc:grammar-parse-error-token c))
	       (encl-error (yacc::grammar-parse-error-enclosed-error c)))
	  
	  (when encl-error
	    (assert (not (typep encl-error 'SyntaxError)))
	    (error encl-error))
	  
	  (py-raise 'SyntaxError
		    (if encl-error
			(format nil "Parse error at line ~A~@[, at token `~S'~].~%[inner error: ~A]"
				line token encl-error)
		      (format nil "At line ~A, parser got unexpected token `~S'."
			      line token))))))))

(defgeneric parse-python-file (source)
  (:method ((s stream))
	   (parse-python-with-lexer
	    :read-chr   (lambda ()
			  (declare (optimize (speed 3) (safety 1) (debug 0)))
			  (read-char s nil nil))
	    :unread-chr (lambda (c)
			  (declare (optimize (speed 3) (safety 1) (debug 0)))
			  (unread-char c s))))
  (:method ((filename t))
	   (with-open-file (f (string filename) :direction :input)
	     (parse-python-file f))))


(defmethod parse-python-string ((s string))
  (let ((next-i 0)
	(max-i (length s)))
    
    (parse-python-with-lexer
     :read-chr (lambda ()
		  (when (< next-i max-i)
		    (prog1 (char s next-i) (incf next-i))))
     
     :unread-chr (lambda (c)
		    (assert (and c (> next-i 0) (char= c (char s (1- next-i)))))
		    (decf next-i)))))
