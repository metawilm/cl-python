;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.PARSER; Readtable: PY-AST-USER-READTABLE -*- -*-
;; 
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Handling of mixed Python/Lisp code

(in-package :clpython.parser)
(in-syntax *ast-user-readtable*)

(defparameter *lispy-debug* nil)
        

;;; Parsing progress is communicated using conditions

(define-condition %parse.base ()
  ((language :initarg :language :reader %p.language)))

(define-condition %parse.collect-form (%parse.base)
  ((form     :initarg :form     :reader %p.form)
   (start-ix :initarg :start-ix :reader %p.start-ix)
   (end-ix   :initarg :end-ix   :reader %p.end-ix)))

(define-condition %parse.switch-language (%parse.base)
  ((start-ix     :initarg :start-ix     :reader %p.start-ix)
   (new-language :initarg :new-language :reader %p.new-language :initform nil)
   (reason       :initarg :reason       :reader %p.reason)))

(define-condition %parse.unexpected-eof (%parse.base)
  ((start-ix :initarg :start-ix :reader %p.start-ix)
   (reason   :initarg :reason   :reader %p.reason)))

(define-condition %parse.finished (%parse.base)
  ())

(defmethod print-object ((x %parse.base) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream ":language ~A" (%p.language x))))

(defmethod print-object ((x %parse.collect-form) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream "~% :lang ~A :form ~A :ix [~A - ~A]"
            (%p.language x) (%p.form x) (%p.start-ix x) (%p.end-ix x))))


;;; Parsing as Python and Lisp

(defparameter *lisp-standard-readtable* (copy-readtable nil)
  "Completely standard Lisp readtable")

(defparameter *languages* '(:python :lisp)
  "Supported languages, in order of attempt.
:PYTHON must be in front, as basically everything that is parseable as Python is treated as Python.")

(defun python-ast-ambiguous-p (ast)
  "Whether the Python AST also corresponds to source code that is valid Lisp code." 
  ;; TODO: The alternative empty tuple notatation '(,)' is currently treated the same as '()' so parsed
  ;; as the invalid Lisp form (,) that leads to a syntax error (comma not inside backquote).
  ;; The comma in the source should make the input unambiguously Pythonic, but that fact gets lost during parsing.
  (or
   ;; (foo), (foo-bar), (foo-23), (foo-bar-baz%2)
   (match-p ast '([module-stmt] ([suite-stmt] (([bracketed-expr] ?expr)))))
   ;; () or (,)
   (match-p ast '([module-stmt] ([suite-stmt] (([tuple-expr] nil)))))
   ;; 123
   (match-p ast '([module-stmt] ([suite-stmt] (([literal-expr] :number ?number)))))))

(defparameter *debug-recursion-count* 0)

(defgeneric read-language-forms (language string start-ix &key &allow-other-keys)
  (:documentation "Read source forms from STRING for the specific LANGUAGE.
Signals %parse.collect-form on each complete form.
Ends by signalling one of: %PARSE.FINISHED, %PARSE.SWITCH-LANGUAGE, %PARSE.UNEXPECTED-EOF."))

(defmethod read-language-forms :around (language string start-ix &key &allow-other-keys)
  (declare (ignore language string start-ix))
  (let ((*debug-recursion-count* (1+ *debug-recursion-count*)))
    (when (>= *debug-recursion-count* 10)
      (check-type *lisp-standard-readtable* readtable)
      (setf *readtable* *lisp-standard-readtable*)
      (break "READ-LANGUAGE-FORMS: Too much recursion; *readtable* reset"))
    (call-next-method)))

(defvar *lisp-readtable*)

(defmethod read-language-forms ((language (eql :python)) (string string) (start-ix integer)
                                &key &allow-other-keys)
  (declare (ignorable language))
  (flet ((valid-lisp-form-here-p (start-ix)
           (check-type *lisp-readtable* readtable)
           (let ((*readtable* *lisp-readtable*))
             (ignore-errors (read-from-string string t nil :start start-ix)
                            t))))
    (when (and (char= (aref string start-ix) #\#)
               (valid-lisp-form-here-p start-ix))
      (error '%parse.switch-language
             :language :python
             :new-language :lisp
             :start-ix start-ix
             :reason "Ambiguous # is start of a valid Lisp form -> handle as Lisp"))
    (handler-case 
        (handler-bind ((toplevel-form-finished-condition
                        ;; Called once for each top-level form.
                        (lambda (c)
                          (let* ((rel-end-ix (toplevel-form-finished-condition.char-ix c))
                                 (abs-end-ix (+ start-ix rel-end-ix))
                                 ;; Condition does not know resulting AST, so need to re-parse,
                                 ;; but at least we are sure parse can't fail.
                                 (ast (parse (subseq string start-ix abs-end-ix))))
                            (if (python-ast-ambiguous-p ast)
                                ;; Lisp parser should take over now.
                                (error '%parse.switch-language
                                       :language :python
                                       :new-language :lisp
                                       :start-ix start-ix
                                       :reason "Ambiguous Python/Lisp input -> handle as Lisp")
                              (progn (signal '%parse.collect-form
                                             :language :python
                                             :start-ix start-ix :end-ix abs-end-ix
                                             :form ast)
                                     (setf start-ix abs-end-ix)))))))
          (let ((*signal-toplevel-form-finished-conditions* t))
            (parse (subseq string start-ix))))
      ({UnexpectedEofError} (c)
        (cond ((and (position #\# string :start start-ix)
                    (valid-lisp-form-here-p start-ix))
               (error '%parse.switch-language
                      :language :python
                      :new-language :lisp
                      :start-ix start-ix
                      :reason "Ambiguous hash character (#) apparently part of valid Lisp Lisp form -> handle as Lisp"))
              ;; More special cases as they are found...
              (t 
               (error '%parse.unexpected-eof :start-ix start-ix :language :python :reason c))))
      ({SyntaxError} (c)
        (error '%parse.switch-language :language :python :start-ix start-ix :reason c))))
  (error '%parse.finished :language :python))

(defmethod read-language-forms ((language (eql :lisp)) (string string) (start-ix integer)
                                &key lisp-readtable &allow-other-keys)
  ;; XXX introduce Lisp package parameter? bind read-eval?
  (declare (ignorable language lisp-readtable))
  (check-type lisp-readtable readtable)
  (handler-case
      (let ((*readtable* lisp-readtable)) ;; avoid infinite recursion
        (loop
          (multiple-value-bind (form end-ix)
              (read-from-string string nil string :start start-ix)
            (if (eq form string)
                (error '%parse.finished :language :lisp)
              (progn (signal '%parse.collect-form
                             :language :lisp
                             :start-ix start-ix :end-ix end-ix
                             :form form)
                     (setf start-ix end-ix))))))
    (end-of-file (c)
      (when *lispy-debug*
        (format t "~&READ-LANGUAGE-FORMS :LISP says: end-of-file: ~S" c))
      (error '%parse.unexpected-eof :start-ix start-ix :language :lisp :reason c))
    (reader-error (c)
      (when *lispy-debug*
        (format t "~&READ-LANGUAGE-FORMS :LISP says: reader-error: ~S" c))
      (error '%parse.switch-language :language :lisp :start-ix start-ix :reason c)))
  (break "never"))

(defun read-mixed-source-string (string &key lisp-readtable interactive-p)
  "Returns a list of Python and Lisp source elements, like: ((:lang form) (:lang2 form2) ..)
If INTERACTIVE-P then the last item is possibly (:lang :incomplete)"
  (check-type string string)
  (check-type lisp-readtable readtable)
  (let ((start-ix 0)
        (switch-ix 0)
        (switch-requests ()) ;; ((:lang . reason) ..)
        forms)
    (flet ((collect-lang-form (lang form)
             (when *lispy-debug*
               (format t "~&; Collecting ~S:~%" lang)
               (with-line-prefixed-output ("; | ")
                 (with-python-pprinter ()
                   (format t "~A" form))))
             (push (list lang form) forms))
           (finish-parsing ()
             (return-from read-mixed-source-string (nreverse forms))))
      (loop ;; until string is completely read
        (catch 'restart-reading-all-languages
          (dolist (language *languages*) ;; for given char ix, try all languages in this order
            (when *lispy-debug*
              (format t "~&; Starting reading at pos ~S by lang ~S~%" start-ix language))
            (tagbody 
             restart-reading-with-language
              (flet ((switch-language (curr-lang reason &optional new-lang)
                       (assert (eq curr-lang language))
                       (assert (<= switch-ix start-ix))
                       (when (< switch-ix start-ix)
                         (setf switch-ix start-ix
                               switch-requests ()))
                       (when *lispy-debug*
                         (format t "~&; Switch request at pos ~S by ~S (new-lang: ~S) because: ~A~%"
                                 start-ix curr-lang new-lang reason))
                       (pushnew (cons curr-lang reason) switch-requests :key #'cdr)
                       (let ((lang-candidates (set-difference *languages* (mapcar #'car switch-requests))))
                         (setf language (cond ((null lang-candidates)
                                               (error (with-output-to-string (s)
                                                        (format s "No suitable language found for string:~%   ~S~%"
                                                                (subseq string start-ix))
                                                        (loop for (lang . reason) in switch-requests
                                                            do (format s " parser ~S signals: ~A~%" lang reason)))))
                                              (new-lang
                                               (unless (member new-lang lang-candidates)
                                                 (error "Parser ~S requested switch to ~S but latter declined already (start-ix=~A)."
                                                        curr-lang new-lang start-ix))
                                               new-lang)
                                              (t
                                               (car lang-candidates))))
                         (when *lispy-debug*
                           (format t "~&; Restarting reading at pos ~S by ~S~%" start-ix language))
                         (go restart-reading-with-language))))
                (handler-bind ((%parse.collect-form (lambda (c)
                                                      (assert (eq (%p.language c) language))
                                                      (setf start-ix (%p.end-ix c))
                                                      (collect-lang-form (%p.language c) (%p.form c))
                                                      ;; XXX dependent on whether there was newline?
                                                      (cond ((>= start-ix (length string))
                                                             (when *lispy-debug*
                                                               (format t "~&; start-ix=~S >= length(string)=~S: finished.~%"
                                                                       start-ix (length string)))
                                                             (finish-parsing))
                                                            (t
                                                             (when *lispy-debug*
                                                               (format t "~&; Collecting form, so starting with first language, start-ix=~S.~%"
                                                                       start-ix))
                                                             (throw 'restart-reading-all-languages nil)))))
                               (%parse.switch-language (lambda (c) 
                                                         (assert (eq (%p.language c) language))
                                                         (setf start-ix (%p.start-ix c))
                                                         (switch-language (%p.language c) (%p.reason c) (%p.new-language c))))
                               (%parse.unexpected-eof (lambda (c)
                                                        (assert (eq (%p.language c) language))
                                                        (cond (interactive-p (collect-lang-form (%p.language c) :incomplete)
                                                                             (finish-parsing))
                                                              (t (switch-language (%p.language c) (%p.reason c) nil)))))
                               (%parse.finished (lambda (c)
                                                  (declare (ignore c))
                                                  (finish-parsing))))
                  (read-language-forms language string start-ix :lisp-readtable lisp-readtable)
                  (break "never"))))))))))

(defgeneric read-toplevel-forms (thing &key lisp-readtable))

(defmethod read-toplevel-forms :around (thing &key lisp-readtable)
  (declare (ignore thing lisp-readtable))
  (with-sane-debugging ("Error occured in READ-TOPLEVEL-FORMS:~%  ~A")
    (call-next-method)))

(defmethod read-toplevel-forms ((pathname pathname) &rest options)
  (with-open-file (stream pathname)
    (apply #'read-toplevel-forms stream options)))

(defmethod read-toplevel-forms ((stream stream) &key lisp-readtable force-interactive-p)
  "Returns ((:language form) (l2 form2) ..) where last item might be (:langN :INCOMPLETE)
STREAM can be an interactive (REPL) stream"
  ;; It's a bit tricky to make this work fine in the REPLs of the different implementations,
  ;; due to differences in at what moments the REPL passes input to the readtable function
  ;; (after a char, or after a line) and whether newline is included.
  (check-type lisp-readtable readtable)
  (labels ((normalize-input (str)
	     (cond ((and (= 1 (length str))
                         (member (aref str 0) '(#\Newline ;; Allegro, SBCL
                                                #\Return ;; ?
                                                #\Null))) ;; Allegro: empty input
                    "")
                   (t str)))
	   (input-ends-with-newline (string &optional (number 1))
	     (check-type string string)
             (loop for i from (1- (length string)) downto 0
                 while (< num-newlines number)
                 while (member (aref string i) '(#\Newline #\Return))
                 count (char= (aref string i) #\Newline) into num-newlines
                 finally (return (>= num-newlines number))))
	   (input-pending-p (stream)
	     (check-type stream stream)
	     (let ((ch (read-char-no-hang stream nil nil t))) 
	       (prog1 (not (null ch))
                 (when ch (unread-char ch stream)))))
           (should-unread-last-newline ()
             ;; In some implementations, leaving the #\Newline triggers printing new prompt.
             (checking-reader-conditionals
              #+allegro nil
              #+ccl nil
              #+sbcl t
              #-(or allegro ccl sbcl) nil))
           
           (read-complete-input ()
             (let (input-string)
               (let ((got-concatenated-stream (typep stream 'concatenated-stream)))
                 (if got-concatenated-stream ;; as created by omnivore readtable
                     (progn (assert (= 2 (length (concatenated-stream-streams stream))))
                            (let ((string-stream-1 (car (concatenated-stream-streams stream))))
                              (check-type string-stream-1 (and string-stream (satisfies input-stream-p)))
                              (setf input-string (normalize-input (string (read-char string-stream-1))))
                              (assert (null (read-char string-stream-1 nil nil)) () "String input stream should be clear now")
                              (peek-char nil stream) ;; String-stream will be dropped from the concatenated streams
                              (assert (= 1 (length (concatenated-stream-streams stream))) () "Concat stream should have one stream now")
                              (setf stream (car (concatenated-stream-streams stream))))) ;; and forget about the wrapper
                   (setf input-string (normalize-input (string (read-char stream))))))
               
               ;; Rules:
               ;;  - read at least one line, i.e. upto newline (interactive) or eof (file-stream)
               ;;  - if the line is a valid, complete Python form: treat as Python (regardless content of next lines)
               ;;  - if the line is valid Python, but incomplete: keep reading until either:
               ;;     1) result is not valid Python anymore: read it all like a Lisp form instead
               ;;     2) valid Python, and \n\n reached (empty line marks end of input): parse as Python
               ;;  - if line is invalid Python: read a Lisp form
               
               (let ((interactive-p (or force-interactive-p (clpython.util:interactive-stream-p-recursive stream)))
                     (eof-reached nil))
                 (setf input-string (normalize-input (concatenate 'string input-string (slurp-file stream))))
                 (flet ((enough-input-p ()
                          "Read at least one line of interactive input, or an entire file"
                          (cond ((input-pending-p stream) nil)
                                (eof-reached              t)
                                (interactive-p            (input-ends-with-newline input-string))
                                (t                        nil)))
                        (read-one-char ()
                          (let ((next-char (read-char stream nil nil)))
                            (if next-char
                                (setf input-string (concatenate 'string input-string (string next-char)))
                              (progn (when *lispy-debug*
                                       (format t "~& Input EOF reached~%")
                                       (setf eof-reached t))
                                     (when (and interactive-p (not (input-ends-with-newline input-string)))
                                       (break "READ-CHAR on interactive steam failed unexpectedly: last char was ~S but expected ~S."
                                              (when (plusp (length input-string))
                                                (aref input-string (1- (length input-string))))
                                              #\Newline))))
                            next-char)))
                   (loop until (enough-input-p) do (read-one-char))
                   ;; Parse what we have
                   (loop 
                     (let* ((parsed-input (read-mixed-source-string input-string
                                                                    :lisp-readtable lisp-readtable
                                                                    :interactive-p interactive-p))
                            (parsed-incomplete (member :incomplete (mapcar #'second parsed-input))))
                       (when (and interactive-p eof-reached)
                         ;; Should hold only during debugging of interactive parsing rules, e.g. by passing
                         ;; STRING-INPUT-STREAM but forcing :INTERACTIVE-P to T
                         (unless *lispy-debug*
                           (break "Unexpected: parsed-incomplete=~A interactive-p=~A eof-reached=~A"
                                  parsed-incomplete interactive-p eof-reached)))
                       (cond (parsed-incomplete
                              (if (and interactive-p (not eof-reached))
                                  (read-one-char)
                                (error "Reading toplevel form failed: incomplete input:~%  ~S" input-string)))
                             ((and interactive-p 
                                   (not (cond ((= (count #\Newline input-string) 1)
                                               (input-ends-with-newline input-string))
                                              ((> (count #\Newline input-string) 1)
                                               (input-ends-with-newline input-string 2)))))
                              (read-one-char))
                             (t
                              (return-from read-complete-input
                                (values input-string parsed-input)))))))))))
    
    (multiple-value-bind (input-string parsed-input)
        (read-complete-input)
      (assert (plusp (length input-string)))
      (when (and (should-unread-last-newline)
                 (char= (aref input-string (1- (length input-string))) #\Newline)) ;; XXX Handle #\Return ?
        (when *lispy-debug* (format t "~&[unreading last #\Newline]~%"))
        (unread-char #\Newline stream))
      (when *lispy-debug*
        (with-line-prefixed-output ("; ")
          #+(or)(format t "Input-string: ~S~%" (coerce input-string 'list))
          (clpython:with-ast-user-pprinter ()
            (format t "Parsed-input:~% ~{~S~^~% ~}~%" parsed-input))))
      (values parsed-input (format nil "~A" parsed-input)))))

;;; Reader macro for read Python subforms nested in Lisp expressions

(defun read-one-python-subform (stream char)
  "Read one Python subform as part of a Lisp form.
It must be delimited at the right by a space, closing bracket, or EOF."
  (declare (ignore char))
  (flet ((try-parse (source-string &key must)
           ;; AST, nil, or (if must) the SyntaxError
           (handler-case
               (parse source-string)
             ({SyntaxError} (c)
               (when must
                 (error "Parsing Python subform from ~S failed: ~S" source-string c))
               nil)
             (:no-error (ast)
               `(eval-language-form :python ',ast)))))
    (loop
        for char = (read-char-no-hang stream nil nil t) ;; no-hang: so it works in the REPL
        for all-chars = (list char) then (if char (cons char all-chars) all-chars)
        for is-sure-end = (null char)
        for is-perhaps-end = (or is-sure-end (member char '(#\) #\Space)))
        for parse-result = (when is-perhaps-end
                             (let ((source-string (coerce (reverse (cdr all-chars)) 'string)))
                               (try-parse source-string :must is-sure-end)))
        when (and parse-result char) do (unread-char char stream)
        when parse-result return parse-result)))

(defun setup-python-subform-readmacro (&key (char #\~) (readtable *readtable*))
  (check-type char character)
  (check-type readtable readtable)
  (set-macro-character char 'read-one-python-subform nil readtable)
  readtable)

(defun create-lisp-readtable (&key (python-subform-char #\~))
  (let ((lisp-readtable (copy-readtable nil)))
    (when python-subform-char
      (setup-python-subform-readmacro :char #\~ :readtable lisp-readtable))
    lisp-readtable))

(defvar *eval-inside-mixed-mode* nil
  "T during evaluation of (Python/Lisp) forms controlled by the mixed mode readtable")

(define-condition exit-mixed-mode ()
  ((reason :initarg :reason :reader emm.reason)
   (new-readtable :initarg :new-readtable :reader emm.new-readtable)))

(defun mixed-readtable-reader (stream lisp-readtable)
  (check-type stream stream)
  (check-type lisp-readtable readtable)
  `(let ((*eval-inside-mixed-mode* t)
         (exit-reason nil))
     (handler-bind ((exit-mixed-mode (lambda (c)
                                       (setf exit-reason c))))
       (unwind-protect
           ;; Arbitrary choice: the return values of the evaluation are
           ;; the return values of the last evaluated form.
           (progn ,@(loop for (lang form)
                        in (read-toplevel-forms stream :lisp-readtable lisp-readtable)
                        collect `(eval-language-form ,lang ',form)))
         (when exit-reason
           (do-exit-mixed-mode exit-reason))))))

(defun do-exit-mixed-mode (c)
  (multiple-value-bind (message new-rt)
      (ecase (emm.reason c)
        (:readtable-change (values "The *readtable* has been changed by user code.~@
                                    Therefore exiting the mixed Lisp/Python syntax mode."
                                   (emm.new-readtable c)))
        (:exit-mode-requested (values "Exiting mixed Lisp/Python syntax mode.~@
                                       Standard Lisp *readtable* is now set."
                                      *lisp-standard-readtable*)))
    (check-type new-rt readtable)
    (setf *readtable* new-rt)
    (with-line-prefixed-output (";; ")
      (format t message)))
  (values))

(defun create-mixed-readtable (lisp-readtable)
  (check-type lisp-readtable readtable) 
  (setup-omnivore-readmacro
   :function (lambda (stream) (mixed-readtable-reader stream lisp-readtable))
   :readtable (copy-readtable nil)))

(defparameter *lisp-readtable* (create-lisp-readtable)
  "Lisp forms are read in the standard readtable with the addition of ~ as a prefix for a Python subexpression")

(defparameter *mixed-readtable* (create-mixed-readtable *lisp-readtable*)
  "Readtable where Python and Lisp top-level forms can be mixed. Lisp forms are read in *LISP-READTABLE*")

(defmacro enter-mixed-lisp-python-syntax (&rest args)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (enter-mixed-lisp-python-syntax-1 ,@args)))

(defparameter *lispy-package* (find-package :cl-user)
  "Lisp package that acts as the Python module for Lispy Python code.") 

(defun enter-mixed-lisp-python-syntax-1 (&key package)
  (cond ((and *eval-inside-mixed-mode*
              (or *compile-file-truename* *load-truename*))
         (with-line-prefixed-output (";; ")
           (format t "Enabling mixed Lisp/Python mode in file being ~A."
                   (if *compile-file-truename* "compiled" "loaded")))
         (setf *readtable* *mixed-readtable*))
        (*eval-inside-mixed-mode*
         (with-line-prefixed-output (";; ")
           (format t "The mixed Lisp/Python syntax mode is already active.~@
                      Python variables are interned in the ~A. ~@
                      To exit the mixed mode: ~S" *lispy-package* '(exit-mixed-lisp-python-syntax))))
        (t
         (when package
           (setf *lispy-package* (or (find-package package)
                                     (error "Package not found: ~S." package))))
         (check-type *mixed-readtable* readtable)
         (setf *readtable* *mixed-readtable*)
         (with-line-prefixed-output (";; ")
           (format t "The mixed Lisp/Python syntax mode is now active: custom *readtable* is set. ~@
                      Python variables will be interned in the ~A. ~@
                      To exit the mixed mode: ~S" *lispy-package* '(exit-mixed-lisp-python-syntax)))))
  (values))

(defmacro exit-mixed-lisp-python-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (exit-mixed-lisp-python-syntax-1)))

(defun exit-mixed-lisp-python-syntax-1 ()
  (if *eval-inside-mixed-mode*
      (signal 'exit-mixed-mode :reason :exit-mode-requested)
    (with-line-prefixed-output (";; ")
      (format t "The mixed Lisp/Python syntax mode was not active.")))
  (values))

(defmacro with-mixed-lisp-python-syntax (&body body)
  (with-gensyms (old-readtable)
    `(let ((,old-readtable *readtable*))
       (check-type old-readtable readtable)
       (check-type *mixed-readtable* readtable)
       (let ((*readtable* *mixed-readtable*))
         (handler-bind ((serious-condition (lambda (c)
                                             ;; Give outer handlers a chance
                                             (signal c)
                                             ;; No transfer of control: we'll end up in the debugger
                                             (with-standard-io-syntax
                                               
                                               (setf *readtable* ,old-readtable)
                                               (with-line-prefixed-output (";; ")
                                                 (format t "Serious condition occured inside WITH-MIXED-LISP-PYTHON-SYNTAX:~% ~A~%" c)
                                                 (format t "To enable debugging *readtable* has been reset to its previous value."))
                                               (format t "~%~%")
                                               (error c)))))
           ,@body)))))


;;; Evaluation of the mixed source forms

(defgeneric eval-language-form (language form))

(defmethod eval-language-form ((language (eql :lisp)) form)
  ;; #+allegro ((and (keywordp form) (tpl::find-command-or-alias (symbol-name form) :quiet t))
  ;; A toplevel command like :exit 
  ;; XXX args not supported yet; should only be done when source is interactive input.
  ;; `(tpl:do-command ,form))
  (declare (ignorable language))
  (check-type *lisp-standard-readtable* readtable)
  (let ((*readtable* *lisp-standard-readtable*)) ;; so compile-file etc don't go through mixed mode
    (unwind-protect 
        (eval form)
      (unless (eq *readtable* *lisp-standard-readtable*)
        (check-type *readtable* readtable)
        (signal 'exit-mixed-mode :reason :readtable-change :new-readtable *readtable*)))))

(defpackage :clpython.lispy.stuff)

(defparameter *lispy-habitat* nil
  "Habitat in which the Lispy code is executed")

(defparameter *lispy-namespace* nil
  "Namespace corresponding to *LISPY-MODULE-GLOBALS*")

(defmethod eval-language-form ((language (eql :python)) form)
  (declare (ignorable language))
  (when (match-p form '([module-stmt] ([suite-stmt] (([identifier-expr] {None})))))
    (return-from eval-language-form (values)))
  
  ;; Wrap Python names like FOO_BAR in a restart that tries Lisp variable FOO-BAR on NameError.
  (setf form
    (walk-py-ast form (lambda (ast &key value target)
                        (block wrap
                          (when (and (listp ast)
                                     (eq (car ast) '[identifier-expr])
                                     value
                                     (not target))
                            (let ((name (second ast)))
                              (check-type name symbol)
                              (when (position #\_ (symbol-name name))
                                (let ((dash-name (substitute #\- #\_ (symbol-name name))))
                                  ;; Determining whether symbol is bound happens at compile time already
                                  (return-from wrap
                                    (values 
                                     `(let ((clpython::*signal-unbound-variable-errors* t))
                                        (handler-bind ((cl:unbound-variable
                                                        (lambda (c)
                                                          (assert (eq (cell-error-name c) ',name)) 
                                                          (whereas ((sym (find-symbol ,dash-name *package*))
                                                                    (val (clpython::bound-in-some-way sym)))
                                                            (use-value val)))))
                                          ,ast))
                                     t))))))
                          (return-from wrap (values ast nil))))
                 :into-nested-namespaces t))
  
  (with-sane-debugging ("Error occured in Python/Lisp input mode, while handling a Python form.")
    (let ((clpython:*habitat* (or *lispy-habitat*
                                  (setf *lispy-habitat* (funcall 'clpython:make-habitat))))
          (clpython::*module-namespace* (or *lispy-namespace*
                                            (setf *lispy-namespace* (clpython::make-package-ns
                                                                     :package *lispy-package*
                                                                     :scope :module
                                                                     :parent (clpython::make-builtins-namespace)
                                                                     :incl-builtins t)))))
      (declare (special clpython:*habitat* clpython::*module-namespace*))
      (check-type *lisp-standard-readtable* readtable)
      (let ((*readtable* *lisp-standard-readtable*))
        (clpython:run-python-ast form :module-globals *lispy-package*)))))

#||
Bugs:

python code should start at start of line:

(clpython.parser::with-mixed-lisp-python-syntax (eval (read-from-string "(warn \"~A\" '(+ 1 2))math")))
Warning: (+ 1 2)
#<The clpython.module.math package>


enter/exit not right message

(list 1 2 3) ##.(warn "Foo")

for item in x:
  if x > 8:
;; Error occured in READ-TOPLEVEL-FORMS:
;;   No suitable language found for string:
;;    "x:
;;   if x > 8:"
;;  parser :lisp signals: Package "x" not found. [file position = 14]
;;  parser :python signals: SyntaxError: At line 1, parser got unexpected token: clpython.ast.punctuation:|:|

(symbol-value 'h)


def f(x, *y):
  if x > 1
    return 1
  else:
    return 2
# missing colon: not switch in middle of one line from python to lisp

||#