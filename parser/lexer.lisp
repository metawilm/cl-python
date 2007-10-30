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

(defvar *warn-indent* t "Warn if suspicious indentation")
(defvar *lex-debug* nil "Print the tokens returned by the lexer")
(defvar *include-line-numbers* nil "Include line number tokens in AST?")
(defvar *tab-width-spaces* 8
  "One tab is equivalent to this many spaces, when it comes to indentation levels.")

(defconstant +whitespace+ '(#\Space #\Tab #\Newline #\Return #\Page))

;; Internal

(deftype char-code-type ()
  "CHAR-CODE return value type"
  '(integer 0 #.char-code-limit))

(defun char-member (ch list)
  (and ch (member ch list :test #'char=)))

(define-compiler-macro char-member (ch list)
  `(let ((.char ,ch))
     (and .char (member .char ,list :test #'char=))))

(defun make-lexer (string &rest options &key yacc-version)
  (ecase yacc-version
    (:allegro-yacc (let ((f (apply #'make-lexer-1 string 
                                   :yacc-version yacc-version
                                   :eof-token 'excl.yacc:eof
                                   options))
                         (grammar-class (find-class 'python-grammar)))
                     (lambda (grammar &optional op)
                       (declare (ignore grammar))
                       (flet ((token-code (token)
                                ;; Cache token codes in the symbol
                                (or (get token 'python-grammar-token-code)
                                    (setf (get token 'python-grammar-token-code)
                                      (excl.yacc:tcode-1 grammar-class token)))))
                         (declare (dynamic-extent #'token-code))
                         (multiple-value-bind (token val) (funcall f op)
                           (if (symbolp token)
                               (values (token-code token) val)
                             (values token val)))))))
    (:cl-yacc (apply #'make-lexer-1 string :yacc-version yacc-version options))))

(defvar *lex-state*)

(defclass lexer-state ()
  ((string        :initarg :string        :accessor ls-string                                     :type string)
   (tab-width     :initarg :tab-width     :accessor ls-tab-width     :initform *tab-width-spaces* :type fixnum) 
   (eof-token     :initarg :eof-token     :accessor ls-eof-token     :initform nil                :type symbol)
   (incl-line-nos :initarg :incl-line-nos :accessor ls-incl-line-nos :initform *include-line-numbers*)
   (yacc-version  :initarg :yacc-version  :accessor ls-yacc-version  :initform nil)
   (last-read-char-ix :accessor ls-last-read-char-ix :initform -1  :type fixnum)
   (curr-line-no  :accessor ls-curr-line-no  :initform 1  :type fixnum)
   (tokens-todo   :accessor ls-tokens-todo   :initform () :type list)
   (indent-stack  :accessor ls-indent-stack  :initform (list 0) :type list)
   (bracket-level :accessor ls-bracket-level :initform 0  :type fixnum)
   (debug         :accessor ls-debug         :initform nil :initarg :debug)
   (warn-indent   :accessor ls-warn-indent   :initform *warn-indent*)))

(define-symbol-macro %lex-last-read-char-ix%   (ls-last-read-char-ix *lex-state*))
(define-symbol-macro %lex-next-unread-char-ix% (1+ (ls-last-read-char-ix *lex-state*)))
(define-symbol-macro %lex-curr-line-no%        (ls-curr-line-no *lex-state*))
(define-symbol-macro %lex-string%              (ls-string *lex-state*))
(define-symbol-macro %lex-tab-width%           (ls-tab-width *lex-state*))
(define-symbol-macro %lex-warn-indent%         (ls-warn-indent *lex-state*))

(defun make-lexer-1 (string &rest options)
  ;; A little hack to merge [not] + [in] into [not in], 
  ;; and [is] + [not] into [is not].
  ;; This evades precedence issues in the grammars.
  (let ((lexer (apply #'make-lexer-2 string options))
        (todo  nil))
    (lambda (&optional op)
      (block lexer
        #+(or)(when (eq op :report-location)
          (return-from lexer (funcall lexer op)))
        (when todo
          (let ((res todo))
            (setf todo nil)
            (return-from lexer (apply #'values res))))
        (multiple-value-bind (x y) (funcall lexer op)
          (case x
            (([not] [is])
             (multiple-value-bind (p q)
                 (funcall lexer op)
               (cond ((and (eq x '[not]) (eq p '[in]))
                      (values '[not in] '[not in]))
                     ((and (eq x '[is]) (eq p '[not]))
                      (values '[is not] '[is not]))
                     (t (setf todo (list p q))
                        (values x y)))))
            (t (values x y))))))))
        
(defun make-lexer-2 (string &rest options)
  "Return a lexer for the given string of Python code.
Will return two value each time: TYPE, VALUE.
On EOF returns: eof-token, eof-token (default: NIL, NIL)."
  (check-type string string)
  (let ((lex-state (apply #'make-instance 'lexer-state :string string options)))
    
    (excl:named-function lexer
      (lambda (op)
        (let ((*lex-state* lex-state))
          (block lexer
            (with-slots (string eof-token incl-line-nos last-read-char-ix
                         curr-line-no tokens-todo indent-stack bracket-level debug) lex-state
              (when (eq op :report-location) ;; used when GRAMMAR-PARSE-ERROR occurs (Allegro CL Yacc)
                (return-from lexer `((:line-no ,curr-line-no)
                                     (:eof-seen ,(member eof-token tokens-todo :key #'second)))))
              (when (= last-read-char-ix -1)
                ;; Check leading whitespace. This will go unnoticed by the lexer otherwise.
                (multiple-value-bind (newline-p new-indent eof-p)
                    (read-whitespace)
                  (declare (ignore newline-p))
                  (when (and (not eof-p) (plusp new-indent))
                    (restart-case
                        (raise-syntax-error "Leading whitespace on first non-blank line.")
                      (cl-user::continue () :report "Continue parsing, ignoring the leading whitespace.")))))
              (flet ((lex-return (token value &optional msg)
                       (when debug (format t "Lexer returns: ~S ~S~@[ ~A~]~%" token value msg))
                       (return-from lexer (values token value)))
                     (lex-todo (token value)
                       (when debug (format t "Lexer todo: ~S ~S~%" token value))
                       (push (list token value) tokens-todo)))
                (when tokens-todo
                  (destructuring-bind (token value) (pop tokens-todo)
                    (lex-return token value "(from todo)")))
                (loop 
                  (let ((c (lex-read-char :eof-error nil)))
                    (cond ((not c)
                           (lex-todo eof-token eof-token)
                           (loop while (plusp (pop indent-stack))
                               do (lex-todo '[dedent] '[dedent]))
                           (lex-return '[newline] (if incl-line-nos curr-line-no '[newline])))
                            
                          ((digit-char-p c 10)
                           (lex-return '[number] (read-number c)))
                            
                          ((identifier-char1-p c)
                           (let ((token (read-identifier c)))
                             ;; u"abc"    : `u' stands for `Unicode string'
                             ;; u + b     : `u' is an identifier
                             ;; r"s/f\af" : `r' stands for `raw string'
                             ;; r + b     : `r' is an identifier
                             ;; ur"asdf"  : `ur' stands for `raw unicode string'
                             ;; ur + a    : `ur' is identifier
                             ;; `u' must appear before `r' if both are string prefix
                             (when (and (<= (length (symbol-name token)) 2)
                                        (member (symbol-name token) '("u" "r" "ur") :test 'string-equal))
                               (let ((ch (lex-read-char :eof-error nil)))
                                 (if (and ch (char-member ch '(#\' #\")))
                                     (let* ((sn      (symbol-name token))
                                            (unicode (position #\u sn :test 'char-equal))
                                            (raw     (position #\r sn :test 'char-equal)))
                                       (lex-return '[string] (read-string ch :raw raw :unicode unicode)))
                                   (when ch (lex-unread-char ch)))))
                             (lex-return (if (eq (symbol-package token)
                                                 (load-time-value (find-package :clpython.ast.reserved)))
                                             token
                                           '[identifier]) 
                                         token)))

                          ((char-member c '(#\' #\"))
                           (lex-return '[string] (read-string c)))

                          ((or (punct-char1-p c)
                               (punct-char-not-punct-char1-p c))
                           (let ((token (read-punctuation c)))
                             ;; Keep track of whether we are in a bracketed
                             ;; expression (list, tuple or dict), because in
                             ;; that case newlines are ignored.
                             ;; (Check on matching brackets is in grammar.)
                             (case token
                               (( [[]  [{] [\(] ) (incf bracket-level))
                               (( [\]] [}] [\)] ) (decf bracket-level)))
                             (lex-return token token)))

                          ((char-member c +whitespace+)
                           (lex-unread-char c)
                           (multiple-value-bind (newline-p new-indent eof-p) (read-whitespace)
                             (declare (ignore eof-p))
                             (when (and newline-p (zerop bracket-level))
                               (cond ((< (car indent-stack) new-indent) ; one indent
                                      (push new-indent indent-stack)
                                      (lex-todo '[indent] '[indent]))
                                     ((> (car indent-stack) new-indent) ; dedent(s)
                                      (loop while (> (car indent-stack) new-indent)
                                          do (pop indent-stack)
                                             (lex-todo '[dedent] '[dedent]))
                                      (unless (= (car indent-stack) new-indent)
                                        (raise-syntax-error 
                                         "Dedent did not arrive at a previous indentation level (line ~A)."
                                         curr-line-no))))
                               (lex-return '[newline] (if incl-line-nos curr-line-no '[newline])))))
                            
                          ((char= c #\#)
                           (read-comment-line c))
                            
                          ((char= c #\\) ;; next line is continuation of this one
                           (let ((c2 (lex-read-char t)))
                             (case c2
                               (#\Newline)
                               (#\Return (let ((c3 (lex-read-char t)))
                                           (unless (char= c3 #\Newline) ;; Windows: \r\n
                                             (lex-unread-char c3))))
                               (t (raise-syntax-error
                                   "Continuation character '\\' must be followed by Newline, ~
                                    but got: '~A' (~S) (line ~A)." c2 c2 curr-line-no))))
                           (incf curr-line-no))
                            
                          (t (with-simple-restart 
                                 (:continue "Discard the character `~A' and continue parsing." c)
                               (raise-syntax-error "Nobody expected this character: `~A' (line ~A)."
                                                   c curr-line-no))))))))))))))

;; Identifier

(defun identifier-char1-p (c)
  "Is C a character with which an identifier can start?
C must be either a character or NIL."
  ;; Cannot use alpha-char-p in these functions, as that is also
  ;; true for accented characters etc.
  (declare (optimize speed))
  (when c
    (let ((code (char-code c)))
      (declare (type char-code-type code))
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

(defun lex-read-char (&key (eof-error t))
  "Returns a character, or NIL on eof/error"
  (cond ((< %lex-next-unread-char-ix% (length %lex-string%))
         (let ((ch (aref %lex-string% (incf %lex-last-read-char-ix%))))
           (when (char= ch #\Newline)
             (incf %lex-curr-line-no%))
           ch))
        (eof-error
         (raise-unexpected-eof %lex-curr-line-no%))))

(defun lex-unread-char (&optional (ch nil))
  "Unread last character read. If CH is supplied, it is checked."
  (if (and (<= 0 %lex-last-read-char-ix% (1- (length %lex-string%)))
           (or (null ch)
               (char= ch (aref %lex-string% %lex-last-read-char-ix%))))
      (progn (when (char= (aref %lex-string% %lex-last-read-char-ix%) #\Newline)
               (decf %lex-curr-line-no%))
             (decf %lex-last-read-char-ix%)
             t)
    (error "Lexer cannot unread char~@[: last character returned was not ~S~]."
           ch)))

(defun lex-substring (start end)
  (assert (<= start end) () "Lex-substring: start=~A which is not <= end=~A" start end)
  (make-array (1+ (- end start))
              :element-type 'character
              :displaced-to %lex-string%
              :displaced-index-offset start))

(defvar *reserved-words-vector*
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
        finally (return vec))
  "Handy lookup table: indexed by first char; entries of the form (2nd char, length, string, symbol)")

(defun read-identifier (first-char)
  "Returns the identifier (which might be a reserved word) as symbol."
  (declare (optimize speed))
  (assert (identifier-char1-p first-char))
    (let* ((start %lex-last-read-char-ix%)
           (end   (loop for c = (lex-read-char :eof-error nil)
                      while (identifier-char2-p c)
                      finally (when c (lex-unread-char))
                              (return %lex-last-read-char-ix%)))
           (str   (lex-substring start end)))
      (or (find-symbol str (load-time-value (find-package :clpython.ast.reserved)))
          (intern str (load-time-value (find-package :clpython.user))))))

;; String

(defun read-string (ch1 &key raw unicode)
  (assert (char-member ch1 '( #\' #\" )))
  
  (labels ((read-unicode-char (s s.ix num-hex-digits)
             (check-type num-hex-digits (member 4 8))
             (loop for i below num-hex-digits
                 for ch = (aref s (+ s.ix i))
                 for ch.code = (or (digit-char-p ch 16) 
                                   (raise-syntax-error "Non-hex digit in \"\u...\": ~S (line ~A)."
                                                       ch %lex-curr-line-no%))
                 sum (ash ch.code (- num-hex-digits i)) into unichar-code
                 finally (return (code-char unichar-code))))

           (replace-unicode-hex-escapes (s)
             (unless (loop for i fixnum from 0 below (1- (length s))
                         for c across s
                         thereis (and (char= c #\\)
                                      (char-member (aref s (1+ i)) '(#\u #\U))))
               (return-from replace-unicode-hex-escapes s))
             (loop with res = (make-array (length s) :adjustable t :element-type 'character :fill-pointer 0)
                 for ch = (lex-read-char)
                 until (char= ch ch1)
                 if (and unicode (char= ch #\\))
                 do (let ((ch.next (lex-read-char)))
                      (if (char-member ch.next '(#\u #\U))
                          (vector-push-extend
                           (read-unicode-char s %lex-last-read-char-ix% (if (char= ch.next #\u) 4 8)) res)
                        (progn (vector-push-extend ch res)
                               (vector-push-extend ch.next res))))
                 else do (vector-push-extend ch res)
                 finally (return res)))
  
           (replace-non-unicode-escapes (s)
             (unless (find #\u s) 
               (return-from replace-non-unicode-escapes s))
             (loop with res = (make-array (length s) :adjustable t :element-type 'character :fill-pointer 0)
                 with s.len fixnum = (length s)
                 with s.ix fixnum = 0
                 do (loop while (and (< s.ix s.len) (char\= #\\ (aref s s.ix)))
                        do (vector-push-extend (aref s s.ix) res)
                           (incf s.ix))
                    (unless (= s.ix s.len)
                      (return-from replace-non-unicode-escapes res))
                    ;; Read one escaped char
                    (assert (char= #\\ (aref s s.ix)))
                    (assert (<= s.ix (- (length s) 2))) ;; last char of S cannot be a backslash
                    (let ((c (aref s (incf s.ix))))
                      (multiple-value-bind (ch.a ch.b)
                          (case c 
                            ;; These clauses must leave s.ix at last handled character
                            ((#\\ #\' #\" #\a #\b) c)
                            (#\f   #\Page)
                            (#\n   #\Newline)
                            (#\r   #\Return)
                            (#\Newline nil) ;; ignore newline after backslash
                            (#\t   #\Tab)
                            (#\v   #\VT)
                            (#\N (if unicode ;; unicode char by name: u"\N{latin capital letter l with stroke}"
                                     (progn (let ((c.next (aref s (incf s.ix))))
                                              (unless (char= c.next #\{)
                                                (raise-syntax-error "Unicode escape `\N' must be followed by `{' ~
                                                                       (line ~A)." %lex-curr-line-no%)))
                                            (let* ((start (incf s.ix))
                                                   (end (or (position #\} s :start s.ix)
                                                            (raise-syntax-error
                                                             "Unicode escape \N{...} misses closing `}' (line ~A)."
                                                             %lex-curr-line-no%))))
                                              (let ((name (nsubstitute #\_ #\Space (subseq s start end))))
                                                (setf s.ix end)
                                                (name-char name))))
                                   (progn (warn "Unicode escape `\N' found in non-unicode string (line ~A)."
                                                %lex-curr-line-no%)
                                          (values #\\ c))))
                            ((#\u #\U) (if unicode ;; \uf7d6 \U1a3b5678
                                           (let ((n (if (char= c #\u) 4 8)))
                                             (read-unicode-char s s.ix n)
                                             (incf s.ix n))
                                         (progn (warn "Unicode escape `\~A' found in non-unicode string (line ~A)."
                                                      c %lex-curr-line-no%)
                                                (values #\\ c))))
                            ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7) ;; char code: up to three octal digits
                             (loop with code = 0
                                 for x = c then (aref s (incf s.ix))
                                 for x.octal = (digit-char-p x 8)
                                 repeat 3 while x.octal
                                 do (setf code (+ (* code 8) x.octal))
                                 finally (unless x.octal
                                           (decf s.ix))
                                         (return (code-char code))))
                            
                            (#\x (let* ((a (aref s (incf s.ix))) ;; char code: up to two hex digits
                                        (b (when (< (incf s.ix) s.len)
                                             (aref s s.ix)))
                                        (a.hex (digit-char-p a 16))
                                        (b.hex (and b (digit-char-p b 16))))
                                   (unless a.hex (raise-syntax-error "Non-hex digit `~A' found after `\x' (line ~A)."
                                                                     a %lex-curr-line-no%))
                                   (if b.hex
                                       (code-char (+ (* 16 a.hex) b.hex))
                                     (prog1 a.hex
                                       (decf s.ix)))))
                            
                            (t (values #\\ c))) ;; Backslash not used for escaping.
                        (when ch.a (vector-push-extend ch.a res))
                        (when ch.b (vector-push-extend ch.b res))
                        (incf s.ix))))))
    
    (let* ((ch2 (lex-read-char))
           (ch3 (lex-read-char :eof-error nil))
           (string (cond ((and ch3 (char= ch1 ch2 ch3)) ;; string delimiter is """ or '''
                          (let* ((start %lex-next-unread-char-ix%)
                                 (end (loop with x = (lex-read-char) and y = (lex-read-char) and z = (lex-read-char)
                                          for prev-bs = nil then (and (char= #\\ (shiftf x y z (lex-read-char)))
                                                                      (not prev-bs))
                                          until (and (char= z x y ch1) (not prev-bs))
                                          finally (return (- %lex-last-read-char-ix% 3)))))
                            (lex-substring start end)))
                         ((char= ch1 ch2) ;; "" or '' but not """ or '''
                          (when ch3 (lex-unread-char ch3))
                          (return-from read-string ""))
                         ((char= ch1 ch3) ;; "x"
                          (return-from read-string (lex-substring (1- %lex-last-read-char-ix%) 
                                                                  (1- %lex-last-read-char-ix%))))
                         (t (let* ((start (- %lex-last-read-char-ix% 1))
                                   (end   (loop with x = (lex-read-char)
                                              for prev-bs = nil then (and (char= #\\ (shiftf x (lex-read-char)))
                                                                          (not prev-bs))
                                              until (and (char= x ch1) (not prev-bs))
                                              finally (return (1- %lex-last-read-char-ix%)))))
                              (lex-substring start end))))))
      (setf string (replace-unicode-hex-escapes string))
      (unless raw
        (setf string (replace-non-unicode-escapes string)))
      string)))

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

(defun read-number (&optional (first-char (lex-read-char)))
  (assert (digit-char-p first-char 10))
  (flet ((read-int (base)
           (multiple-value-bind (integer pos)
               (parse-integer %lex-string% :radix base
                              :start %lex-next-unread-char-ix% :junk-allowed t)
             (prog1 integer
               (setf %lex-last-read-char-ix% (1- pos))))))
    (declare (dynamic-extent #'read-int))
    (let* ((can-have-frac-exp t)
           (res (if (char/= first-char #\0)
                    (progn (lex-unread-char first-char)
                           (read-int 10))
                  (let ((second (lex-read-char :eof-error nil)))
                    (cond ((null second)                   0)
                          ((char-member second '(#\x #\X)) (setf can-have-frac-exp nil)
                                                           (read-int 16))
                          ((digit-char-p second 8)         (setf can-have-frac-exp nil)
                                                           (lex-unread-char second)
                                                           (read-int 8))
                          ((char= second #\.)              (lex-unread-char second)
                                                           0)
                          ((digit-char-p second 10)        (lex-unread-char second)
                                                           (read-int 10))
                          (t                               (lex-unread-char second)
                                                           ;; e.g. suffix `j' for `0j'
                                                           ;;  or suffix 'L' for `0L'
                                                           ;;  or syntax like `]' in `x[0]'
                                                           ;;  or space like ` ' in `0 '.
                                                           0))))))
      (let (has-frac has-exp)
        (when can-have-frac-exp
          ;; Fraction
          (case (lex-read-char :eof-error nil)
            ((nil) )
            (#\. (progn (setf has-frac t)
                        (let* ((start %lex-next-unread-char-ix%)
                               (end   (loop for ch = (lex-read-char :eof-error nil)
                                          while (and ch (digit-char-p ch 10))
                                          finally (when (and ch (not (digit-char-p ch 10)))
                                                    (lex-unread-char ch))
                                                  (return %lex-last-read-char-ix%)))
                               (frac-value (if (<= start end)
                                               (with-standard-io-syntax
                                                 (read-from-string
                                                  ;; Read as `long-float'; CPython uses 32-bit C `long'.
                                                  (format nil "0.~A0L0" (lex-substring start end))))
                                             0.0L0)))
                          (incf res frac-value))))
            (t (lex-unread-char)))
          ;; Exponent marker
          (case (lex-read-char :eof-error nil)
            ((nil) )
            ((#\e #\E) (let ((expo-value (read-int)))
                         (setf has-exp t)
                         (setf res (* res (expt 10 expo-value)))
                         ;; CPython: 1e10 -> float, even though it's an int
                         (setf res (coerce res 'double-float))))
            (t (lex-unread-char))))
          
        ;; CPython allows `j' (imaginary) for decimal, not for hex (SyntaxError) or octal
        ;; (becomes decimal!!?)
        ;; and allows 'L' ("long integer") for decimal, hex, octal
        (unless (or has-frac has-exp)
          (let ((ch (lex-read-char :eof-error nil)))
            (when (and ch (not (char-member ch '(#\l #\L))))
              (lex-unread-char ch)))))
            
      ;; suffix `j' means imaginary
      (let ((ch (lex-read-char :eof-error nil)))
        (if (char-member ch '(#\j #\J))
            (setf res (complex 0 res))
          (when ch (lex-unread-char ch))))
      
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
                                        ;; The comparison operator "<>" is synonym for "!=".
                                        ;; Conflating the two in the lexer is earliest and easiest.
					(#\< '[!=])
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
    
    (let ((c2 (lex-read-char :eof-error nil)))
      (if (and c2 (punct-char2-p c1 c2))
	  
	  (let ((c3 (lex-read-char :eof-error nil)))
	    (if (punct-char3-p c1 c2 c3)
		(lookup-3char c1)
	      (progn (when c3 (lex-unread-char c3))
		     (lookup-2char c1 c2))))
	    
	    ;; 1 char, or two of three dots
	    (if (and c2 (char= #\. c1 c2))
		(if (char= (lex-read-char) #\.)
		    '[...]
		  (raise-syntax-error "Dots `..' may only occur as part of a triple `...' (line ~A)."
				      %lex-curr-line-no%))
	      (if (punct-char1-p c1)
		  (progn (when c2 (lex-unread-char c2))
			 (lookup-1char c1))
		(progn (assert (char= c1 #\!))
		       (raise-syntax-error
			"Character `!' may only occur as in `!=', not standalone (line ~A)."
			%lex-curr-line-no%))))))))

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
  (and c (char= c #\! )))

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
Returns NEWLINE-P, NEW-INDENT, EOF-P."
  (loop with newline-p = nil and n-spaces = 0 and n-tabs = 0
      for c = (lex-read-char :eof-error nil)
      do (case c
	   ((nil)                (return-from read-whitespace
                                   (values nil nil t)))
           ((#\Newline #\Return) (setf newline-p t
                                       n-spaces 0
                                       n-tabs 0))
           ((#\Space #\Page)     (incf n-spaces))
           (#\Tab                (incf n-tabs))
           (#\#                  (progn (read-comment-line c)
                                        (setf newline-p t
                                              n-spaces 0
                                              n-tabs 0)))
           (t                    (lex-unread-char c)
                                 (when (and newline-p (plusp n-tabs) (plusp n-spaces)
                                            %lex-warn-indent%)
                                   (warn "Irregular indentation: both spaces and tabs (line ~A)."
                                         %lex-curr-line-no%))
				 (return-from read-whitespace
                                   (values newline-p 
                                           (+ n-spaces (* %lex-tab-width% n-tabs))))))))

(defun read-comment-line (c)
  "Read until the end of the line, leaving the last #\Newline in the source."
  (assert (char= c #\#))
  (loop for c = (lex-read-char :eof-error nil)
      while (and c (char/= c #\Newline))
      finally (when c (lex-unread-char c))))
