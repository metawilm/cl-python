;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.PARSER; Readtable: PY-AST-USER-READTABLE -*-
;; 
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Lexer for Python code

(in-package :clpython.parser)
(in-syntax *ast-user-readtable*)

(defvar *lex-warn-indent* t "Warn if suspicious indentation")
(defvar *lex-debug* nil "Print the tokens returned by the lexer")
(defvar *tab-width-spaces* 8
  "One tab is equivalent to this many spaces, when it comes to indentation levels.")

(defconstant-once +whitespace+ '(#\Space #\Tab #\Newline #\Return #\Page))

;; Internal

(defgeneric make-lexer (yacc-version string &rest options)
  (:documentation "Create lexer function that when called will either return two values
\(TOKEN-CODE, TOKEN-VALUE) or the grammar-specific EOF indication."))

(defmethod make-lexer (yacc-version (string string) &rest options)
  "Default lexer, which returns TOKEN-KIND, TOKEN-VALUE
where TOKEN-KIND is a symbol like '[identifier]"
  (apply #'make-lexer-1 string :yacc-version yacc-version options))

(defgeneric lexer-eof-token (yacc-version)
  (:documentation "Value returned by lexer to signal eof."))

(defvar *lex-fake-eof-after-toplevel-form* nil)

(define-condition toplevel-form-finished-condition ()
  ((char-ix :initarg :char-ix :accessor toplevel-form-finished-condition.char-ix))
  (:report (lambda (c stream) (print-unreadable-object (c stream :type t)
                                (format stream " :char-ix ~A" (toplevel-form-finished-condition.char-ix c))))))

(define-condition next-eof-fake-after-toplevel-form (toplevel-form-finished-condition) ())
(define-condition next-eof-real (toplevel-form-finished-condition) ())

(defun char-member (ch list)
  (and ch (member ch list :test #'char=)))

(define-compiler-macro char-member (ch list)
  `(let ((.char ,ch))
     (and .char (member .char ,list :test #'char=))))

(defun make-lexer-1 (string &rest options)
  "Source form recording lexer"
  (let ((lexer (apply #'make-lexer-2 string options)))
    (lambda (&optional op)
      (multiple-value-bind (token value source-loc)
          (funcall lexer op)
        ;; Assuming eof-token is a symbol, which is true for acl/cl yacc:
        (check-type value (or list symbol))
        (when (and source-loc *python-form->source-location*)
          (setf (gethash value *python-form->source-location*) source-loc))
        (values token value)))))
    
(defun make-lexer-2 (string &rest options)
  ;; A little hack to merge [not] + [in] into [not in], 
  ;; and [is] + [not] into [is not].
  ;; This evades precedence issues in the grammars.
  (let ((lexer (apply #'make-lexer-3 string options))
        (todo  nil))
    (lambda (&optional op)
      (block lexer
        (when todo
          (let ((res todo))
            (setf todo nil)
            (return-from lexer (values-list res))))
        (multiple-value-bind (x y z) (funcall lexer op)
          (case x
            (([not] [is])
             (multiple-value-bind (p q r)
                 (funcall lexer op)
               (cond ((and (eq x '[not]) (eq p '[in]))
                      (values '[not in] '[not in] nil))
                     ((and (eq x '[is]) (eq p '[not]))
                      (values '[is not] '[is not] nil))
                     (t (setf todo (list p q r))
                        (values x y z)))))
            (t (values x y z))))))))

;; Lexer state

(defclass lexer (standard-generic-function)
  ((string        :initarg :string        :accessor ls-string                                     :type string)
   (tab-width     :initarg :tab-width     :accessor ls-tab-width     :initform *tab-width-spaces* :type fixnum) 
   (yacc-version  :initarg :yacc-version  :accessor ls-yacc-version  :initform nil)
   (last-read-char-ix :accessor ls-last-read-char-ix :initform -1  :type fixnum)
   (curr-line-no  :accessor ls-curr-line-no  :initform 1  :type fixnum)
   (tokens-todo   :accessor ls-tokens-todo   :initform () :type list)
   (indent-stack  :accessor ls-indent-stack  :initform (list 0) :type list)
   (bracket-level :accessor ls-bracket-level :initform 0  :type fixnum)
   (open-deco                                :initform nil)
   (return-count                             :initform 0)
   (last-newline-in-source :reader ls-last-newline-in-source :initform :unknown))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod print-object ((lexer lexer) stream)
  (print-unreadable-object (lexer stream :type t :identity t)
    (with-slots (yacc-version string last-read-char-ix) lexer
      (format stream "for \"~A\" at ~A using ~S"
              (abbreviate-string string 25) 
              (let ((next-unread (1+ last-read-char-ix)))
                (cond ((zerop next-unread)
                       "initial state")
                      ((< next-unread (length string))
                       (format nil "character ~A: \"~A\"" next-unread
                               (abbreviate-string (subseq string next-unread) 10)))
                      (t "end state")))
              yacc-version))))

(defvar *lex-state*)

(define-symbol-macro %lex-last-read-char-ix%   (ls-last-read-char-ix *lex-state*))
(define-symbol-macro %lex-next-unread-char-ix% (1+ (ls-last-read-char-ix *lex-state*)))
(define-symbol-macro %lex-curr-line-no%        (ls-curr-line-no *lex-state*))
(define-symbol-macro %lex-string%              (ls-string *lex-state*))
(define-symbol-macro %lex-tab-width%           (ls-tab-width *lex-state*))

(defgeneric call-lexer (yacc-version lexer op)
  (:documentation "Returns either the eof-token, or two values: TOKEN-KIND, TOKEN-VALUE"))

;; Work around CMUCL bug: http://article.gmane.org/gmane.lisp.cmucl.devel/11052
#.(let ((form 
	 '(defun make-lexer-3 (string &rest options &key yacc-version &allow-other-keys)
	    "Return a lexer for the given string of Python code.
Will return two value each time: TYPE, VALUE.
On EOF returns: eof-token, eof-token."
	    (check-type string string)
	    (let ((lexer (apply #'make-instance 'lexer :string string options)))
	      (flet ((call-lexer-forwarder (&optional op)
					   (let ((*lex-state* lexer))
					     (call-lexer yacc-version lexer op))))
		(closer-mop:set-funcallable-instance-function lexer #'call-lexer-forwarder)
		lexer)))))
    #+cmu
    `(eval ',form)
    #-cmu
    form)

(defmethod call-lexer (yacc-version (lexer lexer) (op (eql nil)))
  (declare (ignorable yacc-version op))
  (with-slots (last-read-char-ix curr-line-no yacc-version tokens-todo indent-stack
               bracket-level open-deco return-count last-newline-in-source eof-returned-already) lexer
    (when (= last-read-char-ix -1)
      ;; Check leading whitespace. This will go unnoticed by the lexer otherwise.
      (destructuring-bind (newline-p new-indent eof-p)
          (read-kind :whitespace (lex-read-char :eof-error nil))
        (declare (ignore newline-p))
        (when (and (not eof-p) (plusp new-indent))
          (restart-case
              (raise-syntax-error "Leading whitespace on first non-blank line.")
            (cl-user::continue () :report "Continue parsing, ignoring the leading whitespace.")))))
    (flet ((lex-return (token value source-loc &optional msg)
             (when *lex-debug* (format t "Lexer returns: ~S ~S ~S~@[ ~A~]~%" token value source-loc msg))
             (incf return-count)
             (return-from call-lexer (values token value source-loc)))
           (lex-todo (token value)
             (when *lex-debug* (format t "Lexer todo: ~S ~S~%" token value))
             (push (list token value) tokens-todo)))
      (when tokens-todo
        (destructuring-bind (token value) (pop tokens-todo)
          (lex-return token value nil "(from todo)"))) ;; loc not important
      (loop 
        (let ((c (lex-read-char :eof-error nil)))
          (cond ((not c)
                 (when (eq last-newline-in-source :unknown)
                   (setf last-newline-in-source nil))
                 (when (zerop return-count) ;; grammar does not like empty files, so dummy content
                   (lex-return '[identifier] '{None} nil))
                 (when *lex-fake-eof-after-toplevel-form*
                   (with-simple-restart (muffle "Muffle")
                     (when *lex-debug* (format t "Lexer signals: next-eof-real~%"))
                     (signal 'next-eof-real :char-ix %lex-next-unread-char-ix%)))
                 (lex-todo (lexer-eof-token yacc-version) (lexer-eof-token yacc-version))
                 (loop while (plusp (pop indent-stack))
                     do (lex-todo '[dedent] '[dedent]))
                 (lex-return '[newline] '[newline] nil))
                
                ((digit-char-p c 10)
                 (multiple-value-bind (val source-loc)
                     (read-kind :number c)
                   (lex-return '[literal-expr] (list '[literal-expr] :number val) source-loc)))
                
                ((identifier-char1-p c)
                 (multiple-value-bind (token source-loc)
                     (read-kind :identifier c)
                   ;; u"abc"    : `u' stands for `Unicode string'
                   ;; u + b     : `u' is an identifier
                   ;; r"s/f\af" : `r' stands for `raw string'
                   ;; r + b     : `r' is an identifier
                   ;; ur"asdf"  : `ur' stands for `raw unicode string'
                   ;; ur + a    : `ur' is identifier
                   ;; `u' must appear before `r' if both are string prefix
                   ;; Bytes (introduced in CPython 2.6):
                   ;;  b"sdf"   : bytes
                   ;;  b'sdf'   : bytes
                   ;;  br"sdf"  : raw bytes
                   ;;  br'sdf'  : raw bytes
                   (when (and (<= (length (symbol-name token)) 2)
                              (member (sort (copy-seq (symbol-name token)) #'char-lessp)
                                      '("r" "u" "ru" "b" "br") 
                                      :test 'string-equal))
                     (let ((ch (lex-read-char :eof-error nil)))
                       (if (and ch (char-member ch '(#\' #\")))
                           (let* ((sn      (symbol-name token))
                                  (unicode (position #\u sn :test 'char-equal))
                                  (raw     (position #\r sn :test 'char-equal))
                                  (bytes   (position #\b sn :test 'char-equal)))
                             (multiple-value-bind (val source-loc)
                                 (read-kind :string ch :raw raw :unicode unicode)
                               (lex-return '[literal-expr] 
                                           (list '[literal-expr] (if bytes :bytes :string) val)
                                           source-loc)))
                         (when ch (lex-unread-char ch)))))
                   (when (and open-deco (eq token '[def]))
                     ;; All outstanding decorators are associated with this function
                     (setf open-deco nil))
                   (let ((type (if (eq (symbol-package token) #.(find-package :clpython.ast.reserved))
                                   token
                                 '[identifier])))
                     (lex-return type token source-loc))))

                ((char-member c '(#\' #\"))
                 (multiple-value-bind (val source-loc)
                     (read-kind :string c)
                   (lex-return '[literal-expr] (list '[literal-expr] :string val) source-loc)))

                ((or (punct-char1-p c)
                     (punct-char-not-punct-char1-p c))
                 (multiple-value-bind (token source-loc)
                     (read-kind :punctuation c)
                   ;; Keep track of whether we are in a bracketed expression
                   ;; (list, tuple or dict), because in that case newlines are ignored.
                   ;; (Check on matching brackets is in grammar.)
                   (case token
                     (( [[]  [{] [\(] ) (incf bracket-level))
                     (( [\]] [}] [\)] ) (decf bracket-level))
                     ([@]               (setf open-deco t)))
                   (lex-return token token source-loc)))

                ((char-member c +whitespace+)
                 (lex-unread-char c)
                 (destructuring-bind (newline-p new-indent eof-p)
                     (read-kind :whitespace c)
                   (when eof-p
                     (setf last-newline-in-source newline-p))
                   (when (and (not eof-p) newline-p (zerop bracket-level))
                     ;; Queue eof before dedents as todo.
                     (when (and (zerop new-indent)
                                *lex-fake-eof-after-toplevel-form*
                                (not open-deco)
                                (not (or (lex-looking-at-token "elif")
                                         (lex-looking-at-token "else")
                                         (lex-looking-at-token "except")
                                         (lex-looking-at-token "finally"))))
                       (with-simple-restart (muffle "Muffle")
                         (when *lex-debug* (format t "Lexer signals: next-eof-fake-after-toplevel-form~%"))
                         (signal 'next-eof-fake-after-toplevel-form :char-ix %lex-next-unread-char-ix%))
                       (lex-todo (lexer-eof-token yacc-version) (lexer-eof-token yacc-version)))
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
                     (lex-return '[newline] '[newline] nil))))
                
                ((char= c #\#)
                 (read-kind :comment-line c))
                
                ((char= c #\\) ;; next line is continuation of this one
                 (let ((c2 (lex-read-char)))
                   (case c2
                     (#\Newline)
                     (#\Return (let ((c3 (lex-read-char)))
                                 (unless (char= c3 #\Newline) ;; Windows: \r\n
                                   (lex-unread-char c3))))
                     (t (raise-syntax-error
                         "Continuation character '\\' must be followed by Newline, ~
                                    but got: '~A' (~S) (line ~A)." c2 c2 curr-line-no))))
                 (incf curr-line-no))
                
                (t (raise-syntax-error "Nobody expected this character: `~A' (line ~A)."
                                       c curr-line-no))))))))

(defmethod call-lexer (yacc-version (lexer lexer) (op (eql :report-location)))
  "Called when Allegro CL Yacc is about to signal a GRAMMAR-PARSE-ERROR.
Also called by CLPython in case of CL-YACC.
In Allegro, result ends up being stored in the condition slot EXCL.YACC:GRAMMAR-PARSE-ERROR-POSITION"
  (declare (ignore yacc-version) (ignorable op))
  (with-slots (curr-line-no last-newline-in-source) lexer
    `((:line-no ,curr-line-no)
      (:last-newline-in-source ,last-newline-in-source))))

(defgeneric read-kind (kind c1 &rest args)
  (:method :around (kind c1 &rest args)
           "Return source code location as second value."
           (declare (ignore kind c1 args)
                    (dynamic-extent args))
           (let* ((start %lex-last-read-char-ix%)
                  (result (call-next-method))
                  (end %lex-last-read-char-ix%))
             (values result (list :start start :end end)))))

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

(defparameter *extra-identifier-char2-p* ()
  "Characters allowed in identifiers, besides the standard ones.
Used by compiler to generate 'forbidden' identfiers.")
        
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
(declaim (ftype (function (&key (:eof-error t)) (or character null)) lex-read-char))

(defun lex-unread-char (&optional (ch nil))
  "Unread last character read. If CH is supplied, it is checked."
  (if (and (<= 0 %lex-last-read-char-ix% (1- (length %lex-string%)))
           (or (null ch)
               (char= ch (aref %lex-string% %lex-last-read-char-ix%))))
      (progn (when (char= (aref %lex-string% %lex-last-read-char-ix%) #\Newline)
               (decf %lex-curr-line-no%))
             (decf %lex-last-read-char-ix%)
             t)
    (error "Lexer cannot unread char~@[: last character returned was not ~S, but ~S~]."
           ch (aref %lex-string% %lex-last-read-char-ix%))))

(defun lex-substring (start end)
  (assert (<= start end) () "Lex-substring: start=~A which is not <= end=~A" start end)
  ;;#+ecl ;; workaround compiler bug: https://sourceforge.net/tracker/?func=detail&aid=3164373&group_id=30035&atid=398053
  ;;(subseq %lex-string% start (1+ end))
  (make-array (1+ (- end start))
              :element-type (array-element-type %lex-string%)
              :displaced-to %lex-string%
              :displaced-index-offset start))

(defun lex-looking-at-token (s &key (char-after-test (complement #'identifier-char2-p)))
  "Returns whether next characters are for token, with whitespace or eof after it." 
  (loop with chs-seen
      for i from 0 below (length s)
      for ch = (let ((c (lex-read-char :eof-error nil)))
                 (when c (push c chs-seen))
                 c)
      for ch-correct = (and ch (char= ch (aref s i)))
      while ch-correct
      finally (when (and ch-correct char-after-test)
                (let ((ch-next (lex-read-char :eof-error nil)))
                  (when ch-next (push ch-next chs-seen))
                  (setf ch-correct (or (null ch-next) ;; eof
                                       (funcall char-after-test ch-next)))))
              (mapc #'lex-unread-char chs-seen)
              (return ch-correct)))

(defmethod read-kind ((kind (eql :identifier)) c1 &rest args)
  "Returns the identifier (which might be a reserved word) as symbol."
  (declare (ignorable kind) (optimize speed))
  (assert (identifier-char1-p c1))
  (assert (null args))
  (let* ((start %lex-last-read-char-ix%)
         (end   (loop for c = (lex-read-char :eof-error nil)
                    while (or (identifier-char2-p c)
                              (member c *extra-identifier-char2-p*))
                    finally (when c (lex-unread-char))
                            (return %lex-last-read-char-ix%)))
         (str   (lex-substring start end)))
    (or (find-symbol str (load-time-value (find-package :clpython.ast.reserved)))
        (intern str (load-time-value (find-package :clpython.user))))))

;; String

(defmethod read-kind ((kind (eql :string)) ch1 &key raw unicode)
  (declare (ignorable kind))
  (assert (char-member ch1 '( #\' #\" )))
  
  (labels ((read-unicode-char (uch s s.ix num-hex-digits)
             (check-type num-hex-digits (member 4 8))
             (check-type uch (member #\u #\U))
             (unless (<= (+ s.ix num-hex-digits) (length s))
               (raise-syntax-error "Unfinished unicode escape character in string literal (line ~A)."
                                   %lex-curr-line-no%))
             (loop for i below num-hex-digits
                 for ch = (aref s (+ s.ix i))
                 for shift downfrom (* 4 (1- num-hex-digits)) by 4
                 for ch.code = (or (digit-char-p ch 16) 
                                   (raise-syntax-error "Invalid Unicode escape: `\\~A' should be ~
                                                        followed by ~A hex digits, but got non-hex ~
                                                        character `~A' (line ~A)."
                                                       uch num-hex-digits ch %lex-curr-line-no%))
                 sum (ash ch.code shift) into unichar-code
                 finally (return (careful-code-char unichar-code))))

           (replace-unicode-hex-escapes (s)
             (unless (loop for i fixnum from 0 below (1- (length s))
                         for c across s
                         thereis (and (char= c #\\)
                                      (char-member (aref s (1+ i)) '(#\u #\U))))
               (return-from replace-unicode-hex-escapes s))
             (loop with res = (make-array (length s) :element-type 'character 
                                          :adjustable t :fill-pointer 0)
                 with s.len = (length s)
                 with s.ix = 0
                 do (loop while (and (< s.ix s.len) (char/= #\\ (aref s s.ix)))
                        do (vector-push-extend (aref s s.ix) res)
                           (incf s.ix))
                    (when (= s.ix s.len)
                      (return-from replace-unicode-hex-escapes res))
                    (assert (char= #\\ (aref s s.ix)))
                    (assert (<= s.ix (- (length s) 2)))
                    (let* ((c (aref s (incf s.ix)))
                           (uni-len (case c 
                                      (#\u 4)
                                      (#\U 8)
                                      (t nil))))
                      (if uni-len
                          (progn (vector-push-extend (read-unicode-char c s (incf s.ix) uni-len) res)
                                 (incf s.ix uni-len))
                        (progn (vector-push-extend #\\ res)
                               (vector-push-extend c res)
                               (incf s.ix))))))
  
           (replace-non-unicode-escapes (s)
             (unless (find #\\ s)
               (return-from replace-non-unicode-escapes s))
             (loop with res = (make-array (length s) :element-type 'character
                                          :adjustable t :fill-pointer 0)
                 with s.len fixnum = (length s)
                 with s.ix fixnum = 0
                 do (loop while (and (< s.ix s.len) (char/= #\\ (aref s s.ix)))
                        do (vector-push-extend (aref s s.ix) res)
                           (incf s.ix))
                    (when (= s.ix s.len)
                      (return-from replace-non-unicode-escapes res))
                    ;; Read one escaped char
                    (assert (char= #\\ (aref s s.ix)))
                    (assert (<= s.ix (- s.len 2))
                        () "Error parsing escapes (last char cannot be backslash): s=~S s.ix=~A" s s.ix)
                    (let ((c (aref s (incf s.ix))))
                      (multiple-value-bind (ch.a ch.b)
                          (case c 
                            ;; These clauses must leave s.ix at last handled character
                            ;; Cross-reference: #'(py-pprint-1 (string)) does the inverse.
                            ((#\\ #\' #\" #\a #\b) c)
                            (#\f   #\Page)
                            (#\n   #\Newline)
                            (#\r   #\Return)
                            (#\Newline nil) ;; ignore newline after backslash
                            (#\t   #\Tab)
                            (#\v   #.(code-char 11)) ;; #\VT or #\PageUp
                            (#\N (if unicode ;; unicode char by name: u"\N{latin capital letter l with stroke}"
                                     (progn (incf s.ix)
                                            (let ((c.next (when (< s.ix s.len)
                                                            (aref s s.ix))))
                                              (unless (and c.next (char= c.next #\{))
                                                (raise-syntax-error "Unicode escape `\\N' must be followed by `{' ~
                                                                     (line ~A)." %lex-curr-line-no%)))
                                            (let* ((start (incf s.ix))
                                                   (end (or (position #\} s :start s.ix)
                                                            (raise-syntax-error
                                                             "Unicode escape \\N{...} misses closing `}' (line ~A)."
                                                             %lex-curr-line-no%))))
                                              (setf s.ix end)
                                              (let ((name (subseq s start end)))
                                                (or (lisp-char-by-python-name name)
                                                    (raise-syntax-error "No Unicode character with name ~S defined."
                                                                        name)))))
                                   (progn (warn "Unicode escape `\\N' found in non-unicode string (line ~A)."
                                                %lex-curr-line-no%)
                                          (values #\\ c))))
                            ((#\u #\U) (if unicode ;; \uf7d6 \U1a3b5678
                                           (let ((n (if (char= c #\u) 4 8)))
                                             (read-unicode-char c s s.ix n)
                                             (incf s.ix n))
                                         (progn (warn "Unicode escape `\\~A' found in non-unicode string (line ~A)."
                                                      c %lex-curr-line-no%)
                                                (values #\\ c))))
                            ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7) ;; char code: up to three octal digits
                             (loop with code = 0
                                 for x = c then (when (< (incf s.ix) s.len)
                                                  (aref s s.ix))
                                 for x.octal = (and x (digit-char-p x 8))
                                 repeat 3 while x.octal
                                 do (setf code (+ (* code 8) x.octal))
                                 finally (unless x.octal
                                           (decf s.ix))
                                         (return (careful-code-char code))))
                            
                            (#\x (let* ((a (aref s (incf s.ix))) ;; char code: up to two hex digits
                                        (b (when (< (incf s.ix) s.len)
                                             (aref s s.ix)))
                                        (a.hex (progn (assert (characterp a))
                                                      (digit-char-p a 16)))
                                        (b.hex (and b (digit-char-p b 16))))
                                   (unless a.hex (raise-syntax-error "Non-hex digit `~A' found after `\\x' (line ~A)."
                                                                     a %lex-curr-line-no%))
                                   (if b.hex
                                       (careful-code-char (+ (* 16 a.hex) b.hex))
                                     (prog1 (careful-code-char a.hex)
                                       (decf s.ix)))))
                            
                            (t (values #\\ c))) ;; Backslash not used for escaping.
                        (when ch.a
                          (check-type ch.a character)
                          (vector-push-extend ch.a res))
                        (when ch.b
                          (check-type ch.b character)
                          (vector-push-extend ch.b res))
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
                          (return-from read-kind ""))
                         
                         ((and ch3 ;; "x"
                               (char= ch1 ch3)
                               (char/= ch2 #\\))
                          (return-from read-kind (lex-substring (1- %lex-last-read-char-ix%) 
                                                                (1- %lex-last-read-char-ix%))))
                         
                         (t (let* ((start (- %lex-last-read-char-ix% 1))
                                   (end   (loop with x = (lex-read-char)
                                              for prev-bs = (and ch3
                                                                 (char= ch3 #\\)
                                                                 (not (char= ch2 #\\)))
                                              then (and (char= #\\ (shiftf x (lex-read-char)))
                                                        (not prev-bs))
                                              until (and (char= x ch1) (not prev-bs))
                                              finally (return (1- %lex-last-read-char-ix%)))))
                              (lex-substring start end))))))
      (when unicode
        (setf string (replace-unicode-hex-escapes string)))
      (unless raw
        (setf string (replace-non-unicode-escapes string)))
      string)))

(defun lisp-char-by-python-name (python-name)
  "PYTHON-NAME has spaces as dividers, e.g. 'latin capital letter l with stroke'.
Returns character or NIL."
  (when (plusp (length python-name))
    (let* ((division-char (checking-reader-conditionals 
			   #+(or allegro ccl sbcl) #\_
                           #+(or ecl cmu) nil
			   #+lispworks #\- ))
           (lisp-char-name (if division-char
                               (substitute division-char #\Space python-name)
                             python-name)))
      (name-char lisp-char-name))))

;; This would be a DEFTYPE if SBCL supported that;
;; now it leads to "STRING is too hairy for sequence functions."
(defconstant +unicode-capable-string-type+
    #+lispworks 'lispworks:text-string
    #-lispworks 'string
  "Used in parser test.")

(defun careful-code-char (code)
  (when (typep code '(integer 0 (#.char-code-limit)))
    (whereas ((char (code-char code)))
      (return-from careful-code-char char)))
  (raise-syntax-error "No character with code ~A (0x~X) defined (line ~A)." 
                      code code %lex-curr-line-no%))

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

(defparameter *normal-float-representation-type* 'double-float
  "The Lisp type normally used for representing \"normal\" Python floats. Values outside the
range of this type are represented by *enormous-float-representation-type*.")

(defparameter *enormous-float-representation-type* 'integer
  "The Lisp type used for representing Python float values outside +NORMAL-FLOAT-RANGE+.
Coercion from float to int must be confirmed by the user.")

(defun number-range (number-type)
  (check-type number-type symbol)
  (loop for prefix in (load-time-value (mapcar 'string '(#:most-negative- #:most-positive-)))
      for sym-name = (with-standard-io-syntax
                       (read-from-string (concatenate 'string prefix (symbol-name number-type))))
      collect (symbol-value sym-name)))

(defun float-suffix (type)
  (ecase type
    (float        "e")
    (short-float  "s")
    (single-float "f")
    (double-float "d")
    (long-float   "l")))

(defmethod read-kind ((kind (eql :number)) c1 &rest args)
  (declare (ignorable kind) (dynamic-extent args))
  (assert (digit-char-p c1 10))
  (assert (null args))
  (flet ((read-int (base)
           (multiple-value-bind (integer pos)
               (parse-integer %lex-string% :radix base
                              :start %lex-next-unread-char-ix% :junk-allowed t)
             (prog1 integer
               (setf %lex-last-read-char-ix% (1- pos))))))
    (declare (dynamic-extent #'read-int))
    (let* ((can-have-frac-exp t)
           (res (if (char/= c1 #\0)
                    (progn (lex-unread-char c1)
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
                                                  (format nil "0.~A0~A0"
                                                          (lex-substring start end)
                                                          (float-suffix *normal-float-representation-type*))))
                                             (with-standard-io-syntax
                                               (read-from-string
                                                (format nil "0.0~A0"
                                                        (float-suffix *normal-float-representation-type*)))))))
                          (incf res frac-value))))
            (t (lex-unread-char)))
          ;; Exponent marker
          (let ((e-ch (lex-read-char :eof-error nil)))
            (case e-ch
              ((nil) )
              ((#\e #\E) (let ((expo-value (read-int 10))
                               (primary res))
                           (setf has-exp t)
                           (setf res (* res (expt 10 expo-value)))
                           ;; CPython: 1e10 -> float, even though it's an int
                           (let* ((normal-flt-tp *normal-float-representation-type*)
                                  (range (number-range normal-flt-tp)))
                             (if (<= (first range) res (second range))
                                 (setf res (coerce res normal-flt-tp))
                               (with-simple-restart
                                   (continue "Represent the value by an ~A instead. (Beware obscure bugs!) ~
                                              ~@:_[~S = ~S]"
                                             (string-upcase *enormous-float-representation-type*)
                                             '*enormous-float-representation-type*
                                             *enormous-float-representation-type*)
                                 (raise-syntax-error
                                  "Literal Python float value `~A~A~A' (line ~A) falls outside the range of ~@:_~
                                   the Lisp type ~A, which is [~A, ~A]. ~:@_[~S = ~S]"
                                  primary e-ch expo-value %lex-curr-line-no%
                                  (string-upcase normal-flt-tp)
                                  (first range) (second range) 
                                  '*normal-float-representation-type* *normal-float-representation-type*))))))
              (t (lex-unread-char)))))
          
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


(defmethod read-kind ((kind (eql :punctuation)) c1 &rest args)
  "Returns puncutation as symbol."
  (declare (ignorable kind) (dynamic-extent args))
  (assert (or (punct-char1-p c1)
	      (punct-char-not-punct-char1-p c1)))
  (assert (null args))
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

(defmethod read-kind ((kind (eql :whitespace)) c1 &rest args)
  "Reads all whitespace and comments, until first non-whitespace character.
Returns NEWLINE-P, NEW-INDENT, EOF-P."
  (declare (ignorable kind) (dynamic-extent args))
  (assert (null args))
  (loop with newline-p = nil and n-spaces = 0 and n-tabs = 0
      for c = c1 then (lex-read-char :eof-error nil)
      do (case c
	   ((nil)                (return-from read-kind
                                   (list newline-p nil t)))
           ((#\Newline #\Return) (setf newline-p t
                                       n-spaces 0
                                       n-tabs 0))
           ((#\Space #\Page)     (incf n-spaces))
           (#\Tab                (incf n-tabs))
           (#\#                  (progn (read-kind :comment-line c)
                                        (setf newline-p t
                                              n-spaces 0
                                              n-tabs 0)))
           (t                    (lex-unread-char c)
                                 (when (and newline-p (plusp n-tabs) (plusp n-spaces)
                                            *lex-warn-indent*)
                                   (warn "Irregular indentation: both spaces and tabs (line ~A)."
                                         %lex-curr-line-no%))
				 (return-from read-kind
                                   (list newline-p 
                                         (+ n-spaces (* %lex-tab-width% n-tabs))
                                         nil))))))

(defmethod read-kind ((kind (eql :comment-line)) c &rest args)
  "Read until the end of the line, leaving the last #\Newline in the source."
  (declare (ignorable kind) (dynamic-extent args))
  (assert (null args))
  (assert (char= c #\#))
  (loop for c = (lex-read-char :eof-error nil)
      while (and c (char/= c #\Newline))
      finally (when c (lex-unread-char c))))

(defun raise-unexpected-eof (&optional line-no)
  (declare (special clpython:*exceptions-loaded*))
  (let ((msg (format nil "Unexpected end of file~@[ (line ~A)~]." line-no)))
    (if clpython:*exceptions-loaded*
	(funcall 'clpython:py-raise '{UnexpectedEofError} msg)
      (raise-syntax-error msg)))
  (break "unreachable"))
