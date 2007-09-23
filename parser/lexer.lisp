;; -*- package: clpython.parser; readtable: py-ast-readtable -*-
;; 
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Lexer for Python code

(in-package :clpython.parser)
(in-syntax *ast-readtable*)

(defvar *lex-debug* nil "Print the tokens returned by the lexer")

(defvar *include-line-numbers* nil "Include line number tokens in AST?")

(defvar *tab-width-spaces* 8
  "One tab is equivalent to this many spaces, when it comes to indentation levels.")

(defconstant +whitespace+ '(#\Space #\Tab #\Newline #\Return #\Page))

;; internal state
(defvar *curr-src-line*)
(defvar *lex-read-char*)
(defvar *lex-unread-char*)

(defun make-py-lexer (&key (read-chr    (lambda () (read-char *standard-input* nil nil t)))
			   (unread-chr  (lambda (c) (unread-char c *standard-input*)))
			   (tab-width-spaces     *tab-width-spaces*)
			   (debug                *lex-debug*)
			   (include-line-numbers *include-line-numbers*))
  "Return a lexer for the Python grammar.
READ-CHR is a function that returns either a character or NIL (it should not signal ~
an error on eof).
UNREAD-CHR is a function of one character that ensures the next call to READ-CHAR
returns the given character. UNREAD-CHAR is called at most once after a call of
READ-CHR."
  (let ((tokens-todo ())
	(indentation-stack (list 0))
	(open-brackets ())
	(curr-src-line 1)
	(curr-char 0)) 

    (excl:named-function (make-py-lexer closure)
      (lambda (grammar &optional op)
        (declare (ignore grammar))
        (block lexer
	
          (when (eq op :report-location)  ;; used when GRAMMAR-PARSE-ERROR occurs
            (return-from lexer `((:line-no ,curr-src-line)
                                 (:eof-seen ,(member 'excl.yacc:eof tokens-todo :key #'second)))))

          (let ((*lex-read-char* (lambda () (let ((ch (funcall read-chr)))
                                              (incf curr-char)
                                              (when (and ch (char= ch #\Newline))
                                                (incf curr-src-line))
                                              ch)))
	      
                (*lex-unread-char* (lambda (ch) (progn (assert ch)
                                                       (funcall unread-chr ch)
                                                       (decf curr-char)
                                                       (when (char= ch #\Newline)
                                                         (decf curr-src-line)))))
	      
                (*curr-src-line*    curr-src-line)
                (*tab-width-spaces* tab-width-spaces)
                (*lex-debug*        debug)
                (*include-line-numbers* include-line-numbers))
	  
            (when (= curr-char 0)
              ;; Detect leading whitespace. This will go unnoticed by the lexer otherwise.
              (let ((ch (read-chr-nil)))
                (if (char-member ch +whitespace+)
                    (progn (unread-chr ch)
                           (multiple-value-bind (newline new-indent eof-p)
                               (read-whitespace)
                             (declare (ignore newline))
                             (unless eof-p
                               (when (> new-indent 0)
                                 (restart-case
                                     (raise-syntax-error
                                      "Leading whitespace on first non-blank line.")
                                   (cl-user::continue ()
                                       :report "Continue parsing, ignoring ~@
                                                the leading whitespace."))))))
                  (when ch (unread-chr ch)))))
          
            (when tokens-todo
              (let ((item (pop tokens-todo)))
                (when *lex-debug*
                  (format t "lexer returns: ~s  (from todo)~%" (second item)))
                (return-from lexer (apply #'values item))))

            (excl.yacc:with-terminal-codes (python-grammar)
	    
              (macrolet ((lex-todo (token-name value)
                           `(let ((val ,value))
                              (when *lex-debug*
                                (format t "lexer todo: ~s~%" val))
                              (push (list (excl.yacc:tcode ,token-name) val) tokens-todo)))
		       
                         (lex-return (token-name value) ;; (lex-return name <value>)
                           `(let ((val ,value))
                              (when *lex-debug*
                                (format t "lexer returns: ~s ~s~%" ',token-name val))
                              (return-from lexer (values (excl.yacc:tcode ,token-name) val))))
		       
                         (find-token-code (token-name)
                           `(excl.yacc:tcode-1 (load-time-value (find-class 'python-grammar))
                                               ,token-name)))

                (tagbody next-char
                  (let ((c (read-chr-nil)))
                    (cond

                     ((not c)
                      ;; Before returning EOF, return DEDENT for every open INDENT.
                      (lex-todo excl.yacc:eof 'excl.yacc:eof)
                      (loop while (> (pop indentation-stack) 0)
                          do (lex-todo [dedent] '[dedent]))
                      (lex-return [newline] 
                                  (if *include-line-numbers* *curr-src-line* '[newline])))
		   		   
                     ((digit-char-p c 10)
                      (lex-return [number] (read-number c)))

                     ((identifier-char1-p c)
                      (let ((token (read-identifier c)))
                        (assert (symbolp token))
		      
                        ;; u"abc"    : `u' stands for `Unicode string'
                        ;; u + b     : `u' is an identifier
                        ;; r"s/f\af" : `r' stands for `raw string'
                        ;; r + b     : `r' is an identifier
                        ;; ur"asdf"  : `ur' stands for `raw unicode string'
                        ;; ur + a    : `ur' is identifier
                        ;; `u' must appear before `r' if both are string prefix
                        (when (and (<= (length (symbol-name token)) 2)
                                   (member (symbol-name token) '("u" "r" "ur")
                                           :test 'string-equal))
                          (let ((ch (read-chr-nil)))
                            (if (and ch (char-member ch '(#\' #\")))
                                (let* ((sn      (symbol-name token))
                                       (unicode (position #\u sn :test 'char-equal))
                                       (raw     (position #\r sn :test 'char-equal)))
                                  (lex-return [string]
                                              (read-string ch :unicode unicode :raw raw)))
                              (when ch (unread-chr ch)))))
		      
                        (when (reserved-word-p token)
                          (when *lex-debug*
                            (format t "lexer returns: reserved word ~s~%" token))
                          (return-from lexer
                            (values (find-token-code token) token)))
		      
                        (lex-return [identifier] token)))

                     ((char-member c '(#\' #\"))
                      (lex-return [string] (read-string c)))

                     ((or (punct-char1-p c)
                          (punct-char-not-punct-char1-p c))
                      (let ((token (read-punctuation c)))
                        ;; Keep track of whether we are in a bracketed
                        ;; expression (list, tuple or dict), because in
                        ;; that case newlines are ignored. (Note that
                        ;; READ-STRING handles multi-line strings itself.)
                        ;; 
                        ;; There is no check for matching brackets here:
                        ;; left to the grammar.
                        (case token
                          (( [[]  [{] [(] ) (push token open-brackets))
                           (( [\]] [}] [)] ) (pop open-brackets)))
		      
                        (when *lex-debug*
                          (format t "lexer returns: punctuation-token ~s~%" token))
                        (return-from lexer 
                          (values (find-token-code token) token))))

                     ((char-member c +whitespace+)
                      (unread-chr c)
                      (multiple-value-bind (newline new-indent eof-p)
                          (read-whitespace)
                        (declare (ignore eof-p))
                        (when (or (not newline) open-brackets)
                          (go next-char))

                        ;; Return Newline now, but also determine if
                        ;; there are any indents or dedents to be
                        ;; returned in next calls.

                        (cond
                         ((= (car indentation-stack) new-indent)) ; same level

                         ((< (car indentation-stack) new-indent) ; one indent
                          (push new-indent indentation-stack)
                          (lex-todo [indent] '[indent]))

                         ((> (car indentation-stack) new-indent) ; dedent(s)
                          (loop while (> (car indentation-stack) new-indent)
                              do (pop indentation-stack)
                                 (lex-todo [dedent] '[dedent]))
			
                          (unless (= (car indentation-stack) new-indent)
                            (raise-syntax-error 
                             "Dedent did not arrive at a previous indentation level (line ~A)."
                             *curr-src-line*))))
		      
                        (lex-return [newline] 
                                    (if *include-line-numbers* *curr-src-line* '[newline]))))
		   
                     ((char= c #\#)
                      (read-comment-line c)
                      (go next-char))

                     ((char= c #\\) ;; next line is continuation of this one
                      (let ((c2 (read-chr-error)))
                        (cond ((char= c2 #\Newline))
                              ((char= c2 #\Return)
                               (let ((c3 (read-chr-nil)))
                                 (unless (char= c3 #\Newline) ;; \r\n
                                   (unread-char c3))))
                              (t 
                               (raise-syntax-error
                                "Continuation character '\\' must be followed by Newline, ~
                                 but got: '~A' (~S) (line ~A)." c2 c2 *curr-src-line*))))
                      (incf *curr-src-line*)
                      (go next-char))
		   
                     (t (with-simple-restart 
                            (:continue "Discard the character `~A' and continue parsing." c)
                          (raise-syntax-error "Nobody expected this character: `~A' (line ~A)."
                           c *curr-src-line*))
                        (go next-char)))))))))))))


(defun read-chr-nil ()
  "Returns a character, or NIL on eof/error"
  (funcall *lex-read-char*))

(define-compiler-macro read-chr-nil ()
  `(locally (declare (optimize (speed 3) (safety 1) (debug 0)))
     (funcall *lex-read-char*)))


(defun read-chr-error ()
  "Return a character, or raise a SyntaxError"
  (or (read-chr-nil) (raise-unexpected-eof *curr-src-line*)))

;; Used internally to /signal/ EOF of Python syntax.

(define-compiler-macro read-chr-error ()
  `(locally (declare (optimize (speed 3) (safety 1) (debug 0)))
     (or (read-chr-nil) (raise-unexpected-eof *curr-src-line*))))


(defun unread-chr (ch)
  (funcall *lex-unread-char* ch))

(define-compiler-macro unread-chr ()
  `(locally (declare (optimize (speed 3) (safety 1) (debug 0)))
     (funcall *lex-unread-char*)))


(defun char-member (ch list)
  (and ch
       (member ch list :test #'char=)))

(define-compiler-macro char-member (ch list)
  (let ((char '#:char))
    `(let ((,char ,ch))
       (and ,char (member ,char ,list :test #'char=)))))

(defun reserved-word-p (sym)
  (eq (symbol-package sym) (load-time-value (find-package :clpython.ast.reserved))))


;; Identifier

(defun identifier-char1-p (c)
  "Is C a character with which an identifier can start?
C must be either a character or NIL."
  (let ((arr #.(make-array 128
			   :element-type 'bit 
			   :initial-contents
			   (loop for i from 0 below 128
			       if (or (<= (char-code #\a) i (char-code #\z))
				      (<= (char-code #\A) i (char-code #\Z))
				      (= i (char-code #\_))) collect 1
			       else collect 0))))
    (and c
	 (< (char-code c) 128)
	 (when (= (sbit arr (char-code c)) 1)
	   t))))

(defun identifier-char2-p (c)
  "Can C occur in an identifier as second or later character?
C must be either a character or NIL."
  (and c
       (or (alphanumericp c)
	   (char= c #\_))))

(define-compiler-macro identifier-char2-p (c)
  (let ((ch '#:ch)
	(code '#:code)
	(arr #.(loop with arr = (make-array 128 :element-type 'bit :initial-element 0)
		   for ch-code from 0 below 128
		   for ch = (code-char ch-code)
		   do (setf (sbit arr ch-code) (if (or (alphanumericp ch)
						       (char= ch #\_))
						   1 0))
		   finally (return arr))))
    `(let ((,ch ,c))
       (and ,ch
	    (let ((,code (char-code ,ch)))
	      (and (< ,code 128)
		   (= (sbit ,arr ,code) 1)))))))

(defun lookup-external-symbol (sym pkg)
  (check-type sym string)
  (multiple-value-bind (sym kind)
      (find-symbol sym pkg)
    (when sym
      (assert (eq kind :external) ()
	"Package ~A does not support (internal) symbol ~S. ~
         Therefore that symbol should not be in the package at all." pkg sym))
    sym))

(defun read-identifier (first-char)
  "Returns the identifier read as string. ~@
   Identifiers start start with an underscore or alphabetic character; ~@
   second and further characters must be alphanumeric or underscore."
  (assert (identifier-char1-p first-char))
  (let ((res (load-time-value
	      (make-array 6 :element-type 'character
			  :adjustable t
			  :fill-pointer 0))))
    (setf (fill-pointer res) 0)
    (vector-push-extend first-char res)
    
    (loop
	for c = (read-chr-nil)
	while (identifier-char2-p c)
	do (vector-push-extend c res)
	finally (when c (unread-chr c)))
    
    ;; RES is either a reserved word like "def", or an identifier like "foo".
    (let ((s (or (lookup-external-symbol res :clpython.ast.reserved)
		 (find-symbol res :clpython.user))))
      ;; Prevent case mismatches in Allegro ANSI mode
      (when (and s (string= (symbol-name s) res))
	(return-from read-identifier s)))
    
    (intern (simple-string-from-vec res) :clpython.user)
    ;; Maybe make symbol extern?
    ))

(defun simple-string-from-vec (vec)
  (make-array (length vec)
	      :element-type 'character
	      :initial-contents vec))

;; String

(defun read-string (first-char &key unicode raw)
  "Returns string as a string"
  (assert (char-member first-char '( #\' #\" )))
  
  (when raw
    ;; Include escapes literally in the resulting string.
      
    (loop with ch = (read-chr-error)
	with res = (load-time-value
		    (make-array 10 :element-type 'character
				:adjustable t :fill-pointer 0))
	with num-bs = 0
	initially (setf (fill-pointer res) 0)
		    
	do (case ch
	     (#\\  (progn (vector-push-extend ch res)
			  (incf num-bs)))
	       
	     (#\u  (if (and unicode
			    (oddp num-bs))
			 
		       (loop for i below 4
			   with code = 0
			   with ch = (read-chr-error)
			   do (setf code (+ (* code 16)
					    (or (digit-char-p ch 16)
						(raise-syntax-error
						 "Non-hex digit in \"\u...\": ~S (line ~A)."
						 ch *curr-src-line*)))
				    ch (read-chr-error))
			   finally (vector-push-extend (code-char code) res))
		       
		     (vector-push-extend #\u res))
		   (setf num-bs 0))
	       
	     ((#\' #\") (cond ((and (char= ch first-char) (> num-bs 0))         
			       (vector-push-extend ch res)
			       (setf num-bs 0))
			      
			      ((char= ch first-char)
			       (return-from read-string (simple-string-from-vec res)))
			      (t
			       (vector-push-extend ch res)
			       (setf num-bs 0))))
	     
	     (t (vector-push-extend ch res)
		(setf num-bs 0)))
	     
	   (setf ch (read-chr-error))))
    
  (assert (not raw))
    
  (let* ((second (read-chr-error))
	 (third (read-chr-nil)))
    (cond 
     ((and third
	   (char= first-char second third))  ;; """ or ''': a probably long multi-line string
      (loop
	  with res = (load-time-value 
		      (make-array 50 :element-type 'character :adjustable t :fill-pointer 0))
	  with x = (read-chr-error) and y = (read-chr-error) and z = (read-chr-error)
	  initially (setf (fill-pointer res) 0)
		      
	  until (char= first-char z y x)
	  do (vector-push-extend (shiftf x y z (read-chr-error)) res)
	       
	  finally (return-from read-string (simple-string-from-vec res))))
       
     ((char= first-char second)  ;; "" or '', but not """ or ''' --> empty string
      (when third
	(unread-chr third))
      (return-from read-string ""))
       
     (t ;; Non-empty string with one starting quote, possibly containing escapes
      (unless third
	(raise-syntax-error "Quoted string not finished (line ~A)." *curr-src-line*))
      (let ((res (load-time-value
		  (make-array 30 :element-type 'character :adjustable t :fill-pointer 0)))
	    (c third)
	    (prev-backslash (char= second #\\ )))
	(setf (fill-pointer res) 0)
	(unless (char= second #\\)
	  (vector-push-extend second res))
	(loop 
	  (cond
	   (prev-backslash
	    (case c 
	      (#\\ (vector-push-extend #\\ res))
	      (#\' (vector-push-extend #\' res))
	      (#\" (vector-push-extend #\" res))
	      (#\a (vector-push-extend #\Bell res))
	      (#\b (vector-push-extend #\Backspace res))
	      (#\f (vector-push-extend #\Page res))
	      (#\n (vector-push-extend #\Newline res))
	      (#\Newline )  ;; ignore this newline; quoted string continues on next line
	      (#\r (vector-push-extend #\Return res))
	      (#\t (vector-push-extend #\Tab res))
	      (#\v (vector-push-extend #\VT  res))
		
	      (#\N
	       (if unicode  ;; unicode char by name: u"\N{latin capital letter l with stroke}"
		     
		   (let ((ch2 (read-chr-error))) 
		     (unless (char= ch2 #\{)
		       (raise-syntax-error
			"In Unicode string: \N{...} expected, but got ~S after \N (line ~A)."
			ch2 *curr-src-line*))
		     (loop with ch = (read-chr-error)
			 with vec = (make-array 10 :element-type 'character
						:adjustable t :fill-pointer 0)
			 while (char/= ch #\}) do
			   (vector-push-extend (if (char= ch #\space) #\_ ch) vec)
			   (setf ch (read-chr-error))
			 finally
			   #+(or)(break "Charname: ~S" vec)
			   (vector-push-extend (name-char vec) res)))
		   
		 (progn (warn "Unicode escape \"\\N{..}\" found in non-unicode string (line ~A)."
                              *curr-src-line*)
			(vector-push-extend #\\ res)
			(vector-push-extend #\N res))))
		
	      ((#\u #\U) (if unicode
			     
			     (loop for i below (if (char= c #\u) 4 8) ;; \uf7d6 \U12345678
				 with code = 0
				 with ch
				 do (setf ch   (read-chr-error)
					  code (+ (* 16 code) 
						  (or (digit-char-p ch 16)
						      (raise-syntax-error
						       "Non-hex digit in \"\~A...\": ~S (line ~A)."
						       c ch *curr-src-line*))))
				 finally (vector-push-extend (code-char code) res))
			     
			   (progn (warn
				   "Unicode escape \"\\~A...\" found in non-unicode string (line ~A)."
				   c *curr-src-line*)
				  (vector-push-extend #\\ res)
				  (vector-push-extend c res))))
	        
	      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)  ;; char code: up to three octal digits
	       (loop with code = 0
		   with x = c
		   for num from 1
		   while (and (<= num 3) (digit-char-p x 8))
		   do (setf code (+ (* code 8) (digit-char-p x 8))
			    x (read-chr-error))
		   finally (unread-chr x)
			   (vector-push-extend (code-char code) res)))
	      
	      
	      (#\x (let* ((a (read-chr-error)) ;; char code: up to two hex digits
			  (b (read-chr-error)))
		     
		     (cond ((not (digit-char-p a 16))
			    (raise-syntax-error "Non-hex digit found in \x..: ~S (line ~A)."
						a *curr-src-line*))
			   
			   ((digit-char-p b 16)
			    (vector-push-extend (code-char (+ (* 16 (digit-char-p a 16)) 
							      (digit-char-p b 16)))
						res))
			   
			   (t (vector-push-extend (digit-char-p a 16) res)
			      (unread-chr b)))))
	        
	      (t 
	       ;; Backslash is not used for escaping: collect both the backslash
	       ;; itself and the character just read.
	       (vector-push-extend #\\ res)
	       (vector-push-extend c res)))
		
	    (setf prev-backslash nil))
	       
	   ((char= c #\\)
	    (setf prev-backslash t))
	   
	   ((char= c first-char) ;; end quote of literal string
	    (return-from read-string (simple-string-from-vec res)))
	       
	   (t 
	    (vector-push-extend c res)))
	      
	  (setf c (read-chr-error))))))))


;; Number

;; integers:     input  -> base-10 val  system used
;;                 11           11         10 (decimal)
;;                011            9          8 (octal)
;;               0x11           17         16 (hexadecimal)
;;
;; integers with exponent:
;;       11e3 = 011e3 = 11E3 = 11e+3 == 11E+3 = 11000.0 (a float)
;;       11e-3 = 0.011 (approx)
;;       0x11e3 = regular hex 4579
;;
;; floats:       input  -> base-10 val  system used
;;                11.3         11.3        10
;;               011.3         11.3        10    ! (exp: error)
;;              0x11.3           *syntax error*
;;
;; floats with exponent:
;;        011.3e3 = 11.3e3 = 011.3e+3 = 11.3e+3 = 11300
;;        011.3e-3 = 11.3e-3 = 11300
;;
;; imag ints:    input  -> base-10 val  system used
;;                11j          11j         10
;;               011j          11j         10    ! (exp: 9j)
;;              0x11j           *syntax error*   ! (exp: 17j)
;;
;; imag ints with exponent:
;;         1e3j = 1000j etc.
;;
;; imag floats:  input  -> base-10 val  system used
;;               11.3j         11.3j       10
;;              011.3j         11.3j       10   ! (exp: error)
;;             0x11.3j          *syntax error*
;;
;; imag floats with exp:
;;    1.0e-3j -> 0.001j etc.
;;
;; Integers (dec, oct, hex) may have the suffix `l' or `L', meaning
;; `long', though the difference between regular and long integers
;; has mostly ceased to exist (operations on regular integers that
;; return an integer in the range of long integers, implicitly
;; convert the result to a long). We ignore any difference between
;; regular and long integers.

(defun read-number (&optional (first-char (read-chr-error)))
  (assert (digit-char-p first-char 10))
  (let ((base 10)
	(res 0))
	
    (flet ((read-int (&optional (res 0))
	     
	     ;;#+(or)
	     ;; This version calls the Lisp reader on the string with digits
	     (loop with vec = (make-array 5 :adjustable t :fill-pointer 0
					  :element-type 'character)
		 with ch = (read-chr-nil)
		 initially 
		   (when (/= 0 res)
		     (vector-push-extend (code-char (+ (char-code #\0) res)) vec))
		 
		 while (and ch (digit-char-p ch base))
		 do (vector-push-extend ch vec)
		    (setf ch (read-chr-nil))
		    
		 finally (when ch (unread-chr ch))
			 (return (parse-integer vec :radix base)))))
	  
      (if (char= first-char #\0)

	  (let ((second (read-chr-nil)))
	    (setf res (cond ((null second) 0) ;; eof
			     
			    ((char-member second '(#\x #\X))
			     (setf base 16)
			     (read-int))
			    
			    ((digit-char-p second 8)
			     (setf base 8)
			     (read-int (digit-char-p second 8)))
			    
			    ((char= second #\.)
			     (unread-chr second)
			     0)
			    
			    ((digit-char-p second 10) 
			     (read-int (digit-char-p second 10)))
			    
			    ((char-member second '(#\j #\J))  (complex 0 0))
			    ((char-member second '(#\l #\L))  0)
			    
			    (t (unread-chr second)
			       0)))) ;; non-number, like `]' in `x[0]'
	
	(setf res (read-int (digit-char-p first-char 10))))
      
      (let ((has-frac nil) (has-exp nil))
	
	(when (= base 10)
	  (let ((dot? (read-chr-nil)))
	    (if (and dot? (char= dot? #\.))
		
		(progn
		  (setf has-frac t)
		  (incf res
			(loop
			    with ch = (read-chr-nil)
			    with lst = ()
			    while (and ch (digit-char-p ch 10))
			    do (push ch lst)
			       (setf ch (read-chr-nil))
			       
			    finally (push #\L lst) ;; use `long-float' (CPython uses 32-bit C `long')
				    (push #\0 lst)
				    (setf lst (nreverse lst))
				    (push #\. lst)
				    (push #\0 lst)
				    (when ch
				      (unread-chr ch))
				    (return (with-standard-io-syntax
					      (read-from-string 
					       (coerce lst 'string)))))))
		      
	      (when dot? (unread-chr dot?)))))
	
	;; exponent marker
	(when (= base 10)
	  (let ((ch (read-chr-nil)))
	    (if (char-member ch '(#\e #\E))
		
		(progn
		  (setf has-exp t)
		  (let ((ch2 (read-chr-error))
			(exp 0)
			(minus nil)
			(got-num nil))
		  
		    (cond
		     ((char= ch2 #\+))
		     ((char= ch2 #\-)       (setf minus t))
		     ((digit-char-p ch2 10) (setf exp (digit-char-p ch2 10)
						  got-num t))
		     (t (raise-syntax-error
			 "Exponent for literal number invalid: ~A ~A (line ~A)."
			 ch ch2 *curr-src-line*)))
		  
		    (unless got-num
		      (let ((ch3 (read-chr-error)))
			(if (digit-char-p ch3 10)
			    (setf exp (+ (* 10 exp) (digit-char-p ch3 10)))
			  (raise-syntax-error
			   "Exponent for literal number invalid: ~A ~A ~A (line ~A)."
			   ch ch2 ch3 *curr-src-line*))))
		    
		    (loop with ch
			while (and (setf ch (read-chr-nil))
				   (digit-char-p ch 10))
			do (setf exp (+ (* 10 exp) (digit-char-p ch 10)))
			finally (when ch (unread-chr ch)))
		  
		    (when minus
		      (setf exp (* -1 exp)))
		  
		    (setf res (* res (expt 10 exp)))
		    
		    ;; CPython: 1e10 -> float, even though it's an int
		    (setf res (coerce res 'double-float))))
	      
	      (when ch
		(unread-chr ch)))))
	
	;; CPython allows `j', decimal, not for hex (SyntaxError) or
	;; octal (becomes decimal!!?)  and allows 'L' for decimal,
	;; hex, octal
	
	;; suffix `L' for `long integer'
	(unless (or has-frac has-exp)
	  (let ((ch (read-chr-nil)))
	    (if (and ch 
		     (not (char-member ch '(#\l #\L))))
		(unread-chr ch))))
		
	;; suffix `j' means imaginary
	(let ((ch (read-chr-nil)))
	  (if (char-member ch '(#\j #\J))
	      (setf res (complex 0 res))
	    (when ch (unread-chr ch)))))
		
      res)))
		

;;; Punctuation

(defun read-punctuation (c1)
  "Returns puncutation as symbol."
  
  (assert (or (punct-char1-p c1)
	      (punct-char-not-punct-char1-p c1)))
  
  (flet ((lookup-3char (c1) (ecase c1
			      (#\* '[**=])
			      (#\< '[<<=])
			      (#\> '[>>=])
			      (#\/ '[//=])))
	 
	 (lookup-2char (c1 c2) (ecase c2
				 (#\= (ecase c1
					(#\= '[==]) (#\> '[>=])
					(#\< '[<=]) (#\+ '[+=])
					(#\- '[-=]) (#\* '[*=])
					(#\^ '[^=]) (#\! '[!=])
					(#\/ '[/=]) (#\| '[\|=])
					(#\% '[%=]) (#\& '[&=])))
				 (#\> (ecase c1
					(#\< 
					 ;; The comparison operator "<>" is synonym for "!=". There is some
					 ;; freedom in where to conflate the two. Doing it here in the lexer
					 ;; is the earliest possibility; it saves the need for a '<> symbol.
					 '[!=])
					(#\> '[>>])))
				 (#\< (ecase c1
					(#\< '[<<])))
				 (#\/ (ecase c1
					(#\/ '[//])))
				 (#\* (ecase c1
					(#\* '[**])))))
	 (lookup-1char (c)
	   (let* ((vec #.(loop with vec = (make-array 128
						      :element-type t 
						      :initial-element nil)
			     for sym in '([=] [+] [-] [*] [/] [<] [>] [~] [^] [\|] [&] [%]
					  [(] [)] [.] [[] [\]] [{] [}] [`] [,] [:] [@] [\|] [\;] )
			     for char = (char (symbol-name sym) 0)
			     do (setf (svref vec (char-code char)) sym)
			     finally (return vec)))
		  (c.code (char-code c)))
	     (assert (< c.code 128))
	     (svref vec c.code))))
    
    (let ((c2 (read-chr-nil)))
      (if (and c2 (punct-char2-p c1 c2))
	  
	  (let ((c3 (read-chr-nil)))
	    (if (punct-char3-p c1 c2 c3)
		(lookup-3char c1)
	      (progn (when c3 (unread-chr c3))
		     (lookup-2char c1 c2))))
	    
	    ;; 1 char, or two of three dots
	    (if (and c2 (char= #\. c1 c2))
		(if (char= (read-chr-error) #\.)
		    '[...]
		  (raise-syntax-error "Dots `..' may only occur as part of a triple `...' (line ~A)."
				      *curr-src-line*))
	      (if (punct-char1-p c1)
		  (progn (when c2 (unread-chr c2))
			 (lookup-1char c1))
		(progn (assert (char= c1 #\!))
		       (raise-syntax-error
			"Character `!' may only occur as in `!=', not standalone (line ~A)."
			*curr-src-line*))))))))

(defun punct-char1-p (c)
  (let ((arr (load-time-value
	      (loop
		  with arr = (make-array 128 :element-type 'bit :initial-element 0)
		  for ch across "`=[]()<>{}.,:|^&%+-*/~;@"
		  do (setf (sbit arr (char-code ch)) 1)
		  finally (return arr)))))
    (let ((cc (char-code c)))
      (and (< cc 128) 
	   (= (sbit arr cc) 1)))))

(defun punct-char-not-punct-char1-p (c)
  "Punctuation  !  may only occur in the form  !=  "
  (and c 
       (char= c #\! )))

(defun punct-char2-p (c1 c2)
  "Recognizes: // << >>  <> !=  <= >=
               == += -= *= /= %=  ^= |= &= ** **= <<= >>= "
  (and c1 c2
       (or (and (char= c2 #\= )
		(char-member c1 '( #\+ #\- #\* #\/ #\% #\^ 
				   #\| #\! #\= #\< #\> #\& )))
	   (and (char= c1 c2)
		(char-member c1 '( #\* #\< #\> #\/ #\< #\> )))
	   
	   (and (char= c1 #\< )
		(char= c2 #\> )))))

(defun punct-char3-p (c1 c2 c3)
  "Recognizes:  **= <<= >>= //="
  (and c1 c2 c3
       (char= c3 #\= )
       (char= c1 c2)
       (char-member c1 '( #\* #\< #\> #\/ ))))



(defun read-whitespace ()
  "Reads all whitespace and comments, until first non-whitespace character.

If Newline was found inside whitespace, values returned are (t N) where N
is the amount of whitespace after the Newline before the first
non-whitespace character (in other words, the indentation of the first
non-whitespace character) measured in spaces, where each Tab is equivalent
to *tab-width-spaces* spaces - so N >= 0.

If no Newline was encountered before a non-whitespace character then
NIL, N are returned.

If EOF was encountered then NIL, NIL, T are returned."

  (loop
      with found-newline = nil and n = 0
      for c = (read-chr-nil)
      do (case c
	   ((nil)                  (return-from read-whitespace
                                     (values nil nil t)))
	   
	   ((#\Newline #\Return)   (setf found-newline t
					 n 0))
	   
	   ((#\Space #\Page)       (incf n))
	   
	   (#\Tab                  (incf n *tab-width-spaces*))
	   
	   (#\#                    (progn (read-comment-line c)
					  (setf found-newline t
						n 0)))
	   
	   (t                      (unread-chr c)
				   (return-from read-whitespace
				     (if found-newline
                                         (values t n)
                                       (values nil n)))))))

(defun read-comment-line (c)
  "Read until the end of the line, leaving the last #\Newline in the source."
  (assert (char= c #\#))
  (loop
      for c = (read-chr-nil) while (and c (char/= c #\Newline))
      finally (when c (unread-chr c))))
