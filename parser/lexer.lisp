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

;; Internal

(defvar *lex-read-char*)
(defvar *lex-unread-char*)

(deftype char-code-type ()
  "CHAR-CODE return value type"
  '(integer 0 #.char-code-limit))


(defun read-chr-nil ()
  "Returns a character, or NIL on eof/error"
  (funcall *lex-read-char*))

(define-compiler-macro read-chr-nil ()
  `(locally (declare (optimize speed))
     (funcall *lex-read-char*)))


(defun read-chr-error ()
  "Return a character, or raise a SyntaxError"
  (or (read-chr-nil)
      (raise-unexpected-eof *curr-src-line*)))

(define-compiler-macro read-chr-error ()
  `(locally (declare (optimize speed))
     (or (read-chr-nil)
         (raise-unexpected-eof *curr-src-line*))))


(defun unread-chr (ch)
  (funcall *lex-unread-char* ch))

(define-compiler-macro unread-chr ()
  `(locally (declare (optimize speed))
     (funcall *lex-unread-char*)))


(defun char-member (ch list)
  (and ch (member ch list :test #'char=)))

(define-compiler-macro char-member (ch list)
  `(let ((.char ,ch))
     (and .char (member .char ,list :test #'char=))))


(defvar *curr-src-line*)

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
                           ;; Cache token codes in the symbol
                           `(let ((.tok ,token-name))
                              (or (get .tok 'python-grammar-token-code)
                                  (setf (get .tok 'python-grammar-token-code)
                                    (excl.yacc:tcode-1
                                     (load-time-value (find-class 'python-grammar))
                                     .tok))))))

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
                                              (read-string (if raw :raw :non-raw) ch :unicode unicode)))
                              (when ch (unread-chr ch)))))
		      
                        (when (reserved-word-p token)
                          (when *lex-debug*
                            (format t "lexer returns: reserved word ~s~%" token))
                          (return-from lexer
                            (values (find-token-code token) token)))
		      
                        (lex-return [identifier] token)))

                     ((char-member c '(#\' #\"))
                      (lex-return [string] (read-string :non-raw c)))

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




(defun reserved-word-p (sym)
  (eq (symbol-package sym) (load-time-value (find-package :clpython.ast.reserved))))


;; Identifier

(defun identifier-char1-p (c)
  "Is C a character with which an identifier can start?
C must be either a character or NIL."
  (declare (optimize speed))
  (when c
    (let ((code (char-code c)))
      (declare (type char-code-type code))
      ;; Cannot use alpha-char-p, as that is also true for o+" etc
      (or (<= #.(char-code #\a) code #.(char-code #\z))
          (<= #.(char-code #\A) code #.(char-code #\Z))
          (= code #.(char-code #\_))))))

(defun identifier-char2-p (c)
  "Can C occur in an identifier as second or later character?
C must be either a character or NIL."
  (declare (optimize speed))
  (when c
    (let ((code (char-code c)))
      (declare (type char-code-type code))
      ;; Cannot use alpha-char-p, as that is also true for o+" etc
      (or (<= #.(char-code #\a) code #.(char-code #\z))
          (<= #.(char-code #\A) code #.(char-code #\Z))
          (= code #.(char-code #\_))
          (<= #.(char-code #\0) code #.(char-code #\9))))))
        
(defun lookup-external-symbol (sym pkg)
  (check-type sym string)
  (when (eq pkg #.(find-package :clpython.ast.reserved))
  (multiple-value-bind (sym kind)
      (find-symbol sym pkg)
    (when sym
      (assert (eq kind :external) ()
	"Package ~A does not support (internal) symbol ~S. ~
         Therefore that symbol should not be in the package at all." pkg sym))
    sym)))

(defparameter *temp-string-arrays* ())
(defparameter *temp-string-counter* 0)

(defmacro with-temp-adjustable-string ((var) &body body)
  `(progn 
     (let* ((.num-arrays (length *temp-string-arrays*))
            (*temp-string-counter* (1+ *temp-string-counter*)))
       (assert (<= *temp-string-counter* (1+ .num-arrays)))
       (let ((,var (if (< *temp-string-counter* .num-arrays)
                       (nth (1- *temp-string-counter*) *temp-string-arrays*)
                     (let ((new (make-array 10 :element-type 'character
                                                :adjustable t :fill-pointer 0)))
                       (prog1 new
                         (setf *temp-string-arrays* 
                           (nconc *temp-string-arrays* (list new))))))))
         (setf (fill-pointer ,var) 0)
         ,@body))))

(defconstant +read-identifier-cache-size+ 4)

(defun read-identifier (first-char)
  "Returns the identifier read as symbol."
  (declare (optimize speed))
  (assert (identifier-char1-p first-char))
  (flet ((lookup-reserved-word (str str.len)
           ;; Keep the reserved words in a handy lookup table.
           (let ((rw-vec (load-time-value 
                          (loop with vec = (make-array 128)
                              with pkg = (find-package :clpython.ast.reserved)
                              for rw being the external-symbol in pkg
                              for rw.name = (symbol-name rw)
                              do (assert (>= (length rw.name) 2))
                                 (let ((key (char-code (aref rw.name 0)))
                                       (val (list (aref rw.name 1)
                                                  (length rw.name)
                                                  rw.name
                                                  rw)))
                                   (push val (svref vec key)))
                              finally (return vec)))))
             (dolist (rw (svref rw-vec (the char-code-type (char-code (aref str 0)))))
               (when (and (char= (pop rw) (aref str 1))
                          (= (pop rw) str.len))
                 (let ((sym.name (pop rw)))
                   (when (loop for i fixnum from 2 below str.len
                             always (char= (aref str i) (aref sym.name i)))
                     (return-from lookup-reserved-word (car rw)))))))))
    (declare (dynamic-extent #'lookup-reserved-word))
    ;; Keep a cache of the last identifiers seen for every starting char.
    ;; This saves looking for symbol in package.
    (with-temp-adjustable-string (res)
      (let ((last-read-identifiers
             (load-time-value (make-array (* +read-identifier-cache-size+ 128)))))
        (vector-push-extend first-char res)
        (loop for c = (read-chr-nil)
            while (identifier-char2-p c)
            do (vector-push-extend c res)
            finally (when c (unread-chr c)))
        
        (let ((res.len (length res)))
          (when (>= res.len 2)
            (whereas ((s (lookup-reserved-word res res.len)))
              (return-from read-identifier s)))
          
          (let ((ix (char-code (aref res 0))))
            (declare (type char-code-type ix))
            (dotimes (delta +read-identifier-cache-size+)
              (declare (type (integer 0 10000) delta))
              (whereas ((cached (svref last-read-identifiers (+ ix delta))))
                (when (and (= res.len (car cached))
                           (loop for i fixnum from 1 below res.len
                               for cached.name = (symbol-name (cdr cached))
                               always (char= (aref res i) (aref cached.name i))))
                  (return-from read-identifier (cdr cached))))))
          
          (let ((sym (or (find-symbol res #.(find-package :clpython.user))
                         (intern res #.(find-package :clpython.user)))))
            ;; Store this symbol in a random entry.
            (let ((cc0 (char-code (aref res 0))))
              (declare (type char-code-type cc0))
              (setf (svref last-read-identifiers (+ (* cc0 +read-identifier-cache-size+)
                                                    (random +read-identifier-cache-size+)))
                (cons res.len sym)))
            sym))))))

(defun simple-string-from-vec (vec)
  (make-array (length vec)
	      :element-type 'character
	      :initial-contents vec))

;; String
  
(defun read-string (rawp ch1 &key unicode)
  (assert (char-member ch1 '( #\' #\" )))
  (flet ((read-unicode-char-n-hex-digits (n)
           (loop for i below n
               for ch = (read-chr-error) 
               for ch.code = (or (digit-char-p ch 16) 
                                 (raise-syntax-error
                                  "Non-hex digit in \"\u...\": ~S (line ~A)."
                                  ch *curr-src-line*))
               sum (ash ch.code (- n i)) into unichar-code
               finally (return (code-char unichar-code)))))
    (ecase rawp
      (:raw ;; Include escapes literally in the resulting string.
       (with-temp-adjustable-string (res)
         (loop for ch = (read-chr-error)
             do (cond ((char= ch #\\)
                       (if unicode
                           (let ((ch.next (read-chr-error)))
                             (if (char-member ch '(#\u #\U))
                                 (vector-push-extend (read-unicode-char-n-hex-digits 4) res)
                               (progn (vector-push-extend #\\ res)
                                      (vector-push-extend ch.next res))))
                         (vector-push-extend #\\ res)))
                      ((char= ch ch1)
                       (return-from read-string (simple-string-from-vec res)))
                      (t (vector-push-extend ch res))))))
      (:non-raw
       (let* ((ch2 (read-chr-error))
              (ch3 (read-chr-nil)))
         
         (cond ((and ch3 (char= ch1 ch2 ch3)) ;; """ or ''': multi-line string
                (with-temp-adjustable-string (res)
                  (loop with x = (read-chr-error) and y = (read-chr-error) and z = (read-chr-error)
                      until (char= ch1 z y x)
                      do (vector-push-extend (shiftf x y z (read-chr-error)) res)
                      finally (return-from read-string (simple-string-from-vec res)))))
               
               ((char= ch1 ch2) ;; "" or '', but not """ or ''' --> empty string
                (when ch3
                  (unread-chr ch3))
                (return-from read-string ""))
               
               (t ;; Non-empty string with one starting quote
                (unless ch3
                  (raise-syntax-error "Unfinished literal string (line ~A)." *curr-src-line*))
                (with-temp-adjustable-string (res)
                  (unless (char= ch2 #\\)
                    (vector-push-extend ch2 res))
                  (loop for c = ch3 then (read-chr-error)
                      with prev-backslash = (char= ch2 #\\ )
                      do (cond (prev-backslash
                                (multiple-value-bind (ch.a ch.b)
                                    (case c 
                                      ((#\\ #\' #\" #\a #\b) c)
                                      (#\f #\Page)
                                      (#\n #\Newline)
                                      (#\r #\Return)
                                      (#\Newline nil) ;; ignore newline after backslash
                                      (#\t #\Tab)
                                      (#\v #\VT)
                                      (#\N (if unicode  ;; unicode char by name: u"\N{latin capital letter l with stroke}"
                                               (progn (let ((c.next (read-chr-error))) 
                                                        (unless (char= c.next #\{)
                                                          (raise-syntax-error
                                                           "In Unicode string: \N{...} expected, but got ~S after \N (line ~A)."
                                                           c.next *curr-src-line*)))
                                                      (with-temp-adjustable-string (unichar-name)
                                                        (loop for ch = (read-chr-error)
                                                            until (char= ch #\})
                                                            do (vector-push-extend (if (char= ch #\space) #\_ ch) unichar-name)
                                                            finally (return (name-char unichar-name)))))
                                             #1=(progn (warn "Unicode escape \"\\~A{..}\" found in non-unicode string (line ~A)."
                                                             c *curr-src-line*)
                                                       (values #\\ c))))
                                      ((#\u #\U) (if unicode ;; \uf7d6 or \U1a3b5678
                                                     (read-unicode-char-n-hex-digits (if (char= c #\u) 4 8))
                                                   #1#))
                                      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)  ;; char code: up to three octal digits
                                       (loop with code = 0
                                           for x = c then (read-chr-error)
                                           for x.octal = (digit-char-p x 8)
                                           repeat 3 while x.octal
                                           do (setf code (+ (* code 8) x.octal))
                                           finally (unless x.octal (unread-chr x))
                                                   (return (code-char code))))
                                      (#\x (let* ((a (read-chr-error)) ;; char code: up to two hex digits
                                                  (b (read-chr-error))
                                                  (a.hex (digit-char-p a 16))
                                                  (b.hex (digit-char-p b 16)))
                                             (unless a.hex (raise-syntax-error "Non-hex digit found in \x..: ~S (line ~A)."
                                                                               a *curr-src-line*))
                                             (if (digit-char-p b 16)
                                                 (code-char (+ (* 16 a.hex) b.hex))
                                               (prog1 a.hex 
                                                 (unread-chr b)))))
                                      (t (values #\\ c))) ;; Backslash not used for escaping.
                                  
                                  (when ch.a (vector-push-extend ch.a res))
                                  (when ch.b (vector-push-extend ch.b res))
                                  (setf prev-backslash nil)))
                               
                               ((char= c #\\)
                                (setf prev-backslash t))
                               
                               ((char= c ch1) ;; end quote of literal string
                                (return-from read-string (simple-string-from-vec res)))
                               
                               (t (vector-push-extend c res))))))))))))

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
  (let ((base 10) (res 0))
    (flet ((read-int (&optional (res 0))
             ;; Call the Lisp reader on the string with digits.
	     (loop with vec = (make-array 5 :adjustable t :fill-pointer 0
					  :element-type 'character)
		 for ch = (read-chr-nil)
		 initially (unless (zerop res)
                             (vector-push-extend (code-char (+ (char-code #\0) res)) vec))
                 while (and ch (digit-char-p ch base))
		 do (vector-push-extend ch vec)
                 finally (when ch (unread-chr ch))
			 (return (parse-integer vec :radix base)))))
      (declare (dynamic-extent #'read-int))
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
  (declare (optimize speed))
  (when c
    (let ((code (char-code c)))
      (declare (type char-code-type code))
      (let ((arr (load-time-value
                  (loop with arr = (make-array 128 :element-type 'bit :initial-element 0)
                      for ch across "`=[]()<>{}.,:|^&%+-*/~;@"
                      do (setf (sbit arr (char-code ch)) 1)
                      finally (return arr)))))
        (and (< code 128)
             (= (sbit arr code) 1))))))

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
