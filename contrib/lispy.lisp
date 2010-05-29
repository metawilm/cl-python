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
(defparameter *standard-readtable* (copy-readtable nil))

(define-condition ambiguous-input-is-lisp-condition ()
  ()
  (:documentation "Condition signalled when ambiguous Lispy source code should be interpreted ~
as Lisp (not Python) code. In particular it's for these cases:
  1. '()' by itself is Lisp nil, not a empty Python tuple;
  2. '(foo)' by itself is a Lisp call, not a bracketed Python variable.
But as part of larger Python expressions, the above are treated as Python:
  a = (), b = (foo)

The alternative empty tuple notatation '(,)' is currently treated the same as '()' so parsed
as the invalid Lisp form (,) that leads to a syntax error (comma not inside backquote).
The comma does makes this input unambiguously Pythonic, but that fact gets lost during parsing."))
  
(defun limited-parse (object &rest args)
  "Try to parse STRING as Python source code, but if it denotes an ambiguous
expression that should be interpreted as Lisp code, signals AMBIGUOUS-INPUT-IS-LISP-CONDITION."
  (let ((result (apply #'parse object args)))
    (clpython:with-perhaps-matching (result
                                     ([module-stmt] ([suite-stmt] (([bracketed-expr] ([identifier-expr] ?name))))))
      (error 'ambiguous-input-is-lisp-condition))
    (clpython:with-perhaps-matching (result
                                     ([module-stmt] ([suite-stmt] (([tuple-expr] nil)))))
      (error 'ambiguous-input-is-lisp-condition))
    result))

(defgeneric read-lispy-toplevel-forms (thing &key lisp-readtable interactive-p))

(defmethod read-lispy-toplevel-forms :around (thing &key lisp-readtable interactive-p)
  (declare (ignore lisp-readtable interactive-p))
  (with-sane-debugging ("Error occured in Python/Lisp input mode, while reading input from ~A:" thing)
    (call-next-method)))

(defmethod read-lispy-toplevel-forms ((pathname pathname) &rest options)
  (with-open-file (stream pathname)
    (apply #'read-lispy-toplevel-forms stream options)))

(defmethod read-lispy-toplevel-forms ((stream stream) &key lisp-readtable (interactive-p (not (typep stream 'file-stream))))
  ;; It's pretty tricky to make this work fine in the REPLs of the different implementations,
  ;; due to differences in at what moments the REPL passes a line to the readtable function
  ;; (after a char, a line) and whether newline is included.
  (labels ((normalize-input (str)
             (if (and (= 1 (length str))
                      (member (aref str 0) '(#\Newline ;; Allegro, SBCL
                                             #\Return ;; ?
                                             #\Null))) ;; Allegro: empty input
                 ""
               str))
           (string-has-enter-p (all)
             (check-type all string)
             (or (position #\Newline all) (position #\Return all)))
           (input-pending-p (stream)
             (check-type stream stream)
             (let ((ch (read-char-no-hang stream nil nil t))) 
               (prog1 ch
                 (when ch (unread-char ch stream)))))
           (parse-string-toplevel-forms (string)
             (check-type string string)
             (let ((res (read-lispy-toplevel-forms string
                                                   :lisp-readtable lisp-readtable
                                                   :interactive-p interactive-p)))
               (when *lispy-debug*
                 (format t "~&[parse-string-toplevel-forms ~S~& -> ~S]~&" string res))
               res))
           (input-semantically-finished-p (string)
             (check-type string string)
             (not (eq :incomplete (parse-string-toplevel-forms string)))))
    
    (loop with all = (normalize-input (slurp-file stream))
        for dummy = (when *lispy-debug*
                      (format t "~&[iter all: ~S]~&" (coerce all 'list)))
        until (and (string-has-enter-p all)
                   (not (input-pending-p stream))
                   (input-semantically-finished-p all))
        do (or dummy)
           (let ((next-char (read-char stream t))) ;; really expect more chars, so error if not
             (check-type next-char character)
             (setf all (normalize-input (concatenate 'string all (string next-char)))))
        finally (let ((unread-newline #+allegro nil
                                      #+ccl nil
                                      #+sbcl t
                                      #-(or allegro ccl sbcl) nil))
                  ;; In some implementations, leaving the #\Newline triggers printing new prompt.
                  (when (and unread-newline
                             (plusp (length all))
                             (eql (aref all (1- (length all))) #\Newline))
                    (unread-char #\Newline stream)))
                (when *lispy-debug*
                  (format t "[finish all: ~S]" (coerce all 'list)))
                (when (zerop (length all))
                  (break "zero all"))
                (return (parse-string-toplevel-forms all)))))

(defmethod read-lispy-toplevel-forms ((string string) &key lisp-readtable interactive-p)
  "Returns a list of Python and Lisp source elements, like:
\((handle-python <python-ast>) (handle-lisp <sexpr>) ..)
or (in case of INTERACTIVE-P) the error status :INCOMPLETE"
  ;; XXX introduce Lisp package parameter?
  (labels ((print-error-with-source (message source)
             (when (> (length source) 100)
               (setf source (concatenate 'string (subseq source 0 100) "...")))
             (with-line-prefixed-output (";; ")
               (format t "~A~%" message)
               (with-line-prefixed-output ("  ")
                 (format t "~A" source))))
           
           (handle-error (c source string &rest formatargs)
             (signal c) ;; For outer handler.
             (print-error-with-source (apply #'format string formatargs) source)
             (error c)))
    
    (macrolet ((with-errors-handled ((source error-prefix-string &rest formatargs) &body body)
                 (check-type error-prefix-string string)
                 `(handler-bind
                      ((error (lambda (c) (handle-error c ,source ,error-prefix-string ,@formatargs))))
                    ,@body)))
      
      (loop with start-ix = 0
          with res
          with last-python-incomplete-p
          while (< start-ix (1- (length string)))
          do (setf last-python-incomplete-p nil)
             (let ((rest-string (subseq string start-ix))
                   (next-start-ix nil))
               (multiple-value-bind (python-ast error)
                   (handler-case
                       (handler-bind ((toplevel-form-finished-condition
                                       ;; Catching this is a hack to get to know the character position
                                       ;; at which the next top-level form ends. Only signalled if
                                       ;; parsing went fine.
                                       (lambda (c)
                                         (setf next-start-ix (toplevel-form-finished-condition.char-ix c))
                                         #+(or)(warn "~A Next top-level form starts at ~A: ~A" c
                                                     (+ start-ix next-start-ix)
                                                     (subseq string (+ start-ix next-start-ix))))))
                         (values (let ((*signal-toplevel-form-finished-conditions* t))
                                   (limited-parse rest-string))))
                     (ambiguous-input-is-lisp-condition (c)
                       ;; Parsing went fine, but REST-STRING should be treated as Lisp code instead.
                       (assert next-start-ix)
                       (setf next-start-ix nil)
                       (values nil c))
                     ({SyntaxError} (c) ;; includes subclass {UnexpectedEofError}
                       (when (typep c (find-class '{UnexpectedEofError}))
                         (setf last-python-incomplete-p t))
                       (setf next-start-ix nil)
                       (values nil c))
                     (:no-error (python-ast)
                       (incf start-ix next-start-ix)
                       (setf next-start-ix nil)
                       (values python-ast nil)))
                 #+(or)(warn "ast=~A error={~A}  start-ix=~A next-start-ix=~A res=~S" ast error start-ix next-start-ix res)
                 (cond ((not error)
                        ;; REST-STRING contains only Python code.
                        (push `(handle-python ,python-ast) res))

                       (next-start-ix
                        ;; At [START-IX] there is a valid Python top-level form. Lisp code starts at [START-IX + NEXT-START-IX-LIST].
                        ;; REST-STRING already starts at [START-IX].
                        (let ((ast (with-errors-handled (rest-string "Error occured while parsing the following source as Python code~%~
                                                                      around input character position ~A:~%" start-ix)
                                     (limited-parse (subseq rest-string 0 next-start-ix) #| :incl-module nil |#))))
                          (push `(handle-python ,ast) res))
                        (incf start-ix next-start-ix))
                       
                       (t 
                        ;; REST-STRING starts with Lisp code.
                        ))
                 
                 ;; Now expecting a Lisp form at [START-IX].
                 (setf rest-string (subseq string start-ix))
                 #+(or)(warn "rest-string with Lisp: ~A => ~S" start-ix rest-string)
                 (multiple-value-bind (form position)
                     (with-errors-handled (rest-string "Error occured while parsing the following source as Lisp (not Python) code~%~
                                                        around input character position ~A:~%" start-ix)
                       (block try-lisp
                         (handler-bind ((end-of-file (lambda (c)
                                                       (declare (ignore c))
                                                       (return-from read-lispy-toplevel-forms :incomplete)))
                                        (reader-error (lambda (c)
                                                        (declare (ignore c))
                                                        (format t "reader-error: interactive-p=~A last-python-incomplete-p=~A"
                                                                interactive-p last-python-incomplete-p)
                                                        (when (and interactive-p last-python-incomplete-p)
                                                          (return-from read-lispy-toplevel-forms :incomplete)))))
                           (let ((*readtable* (or lisp-readtable *standard-readtable*)))
                             (read-from-string rest-string nil :eof)))))
                   (when (eq form :eof)
                     (loop-finish))
                   (push `(handle-lisp ,form) res)
                   (incf start-ix position))))
          finally (return (nreverse res))))))

;;; Read one Python subform as part of a Lisp form

(defun read-one-python-form (stream char)
  "Read one Python form. It must be delimited at the right by a space, closing bracket, or EOF."
  (declare (ignore char))
  (flet ((try-parse (code)
           "Returns Python code as nice Lisp form on success, or nil if unparsable."
           (etypecase code
             (string)
             (list (setf code (coerce code 'string))))
           (handler-case (values (limited-parse (coerce code 'vector) #| :one-expr nil |#))
             ({SyntaxError} () 
               nil)
             (:no-error (ast)
               `(handle-python ,ast)))))
    (loop with parsed
        for char = (read-char-no-hang stream nil nil t) ;; no-hang: so it works in the REPL
        when (and (or (null char)
                      (member char '(#\Space #\))))
                  (setf parsed (try-parse chars)))
        do (when char
             (unread-char char stream))
           (return parsed)
        collect char into chars)))

;;; Reader macros

(defun setup-~python-readmacro (&optional (readtable *readtable*))
  (set-macro-character #\~ #'read-one-python-form nil readtable)
  readtable)

(defun create-lispy-readtable ()
  (let ((lisp-readtable (setup-~python-readmacro (copy-readtable nil))))
    (setup-omnivore-readmacro :function (lambda (stream)
                                          `(progn ,@(read-lispy-toplevel-forms stream :lisp-readtable lisp-readtable)))
                              :readtable (copy-readtable nil))))
    
(defparameter *lispy-readtable* (create-lispy-readtable)
  "Readtable where Python and Lisp top-level forms can be mixed. Lisp forms are read in the standard readtable
with the addition of ~ as a prefix for a Python expression.")

(defmacro enter-mixed-lisp-python-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (if (eq *readtable* *lispy-readtable*)
         (warn "The mixed Lisp/Python syntax mode was already enabled.")
       (progn (terpri)
              (with-line-prefixed-output (";; ")
                (format t "The mixed Lisp/Python syntax mode is now enabled: custom *readtable* is set. ~%")
                (format t "This readtable will also used by functions like READ and COMPILE-FILE. ~%")
                (format t "To reset the mode: (~S)" 'exit-mixed-lisp-python-syntax))
              (setq *readtable* *lispy-readtable*)))
     (values)))

(defmacro exit-mixed-lisp-python-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (if (eq *readtable* *lispy-readtable*)
         (progn (terpri)
                (with-line-prefixed-output (";; ")
                  (format t "The mixed Lisp/Python syntax mode is now disabled:~% *readtable* is reset to the standard readtable.")))
       (warn "The mixed Lisp/Python syntax mode was not enabled."))
     (setq *readtable* (copy-readtable nil))
     (values)))

(defmacro with-mixed-lisp-python-syntax (&body body)
  (with-gensyms (old-readtable)
    `(let* ((,old-readtable *readtable*)
            (*readtable* *lispy-readtable*))
       (handler-bind ((error (lambda (c)
                               (signal c)
                               (setf *readtable* ,old-readtable)
                               (format t ";; Error occured while WITH-MIXED-LISP-PYTHON-SYNTAX readtable was active;~
                                          ;; readtable has been reset for safety.~%")
                               (error c))))
         ,@body))))

;; End of parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Compilation

(defpackage :clpython.lispy.stuff)

(defparameter *lispy-habitat* nil
  "Habitat in which the Lispy code is executed")

(defparameter *lispy-package*  (find-package :cl-user)
  "Lisp package that acts as the Python module for Lispy Python code.") 

(defparameter *lispy-namespace* nil
  "Namespace corresponding to *LISPY-MODULE-GLOBALS*")

(defparameter *lispy-compile-python* t
  "Whether Python forms are compiled before being run.")

(defmacro handle-lisp (form)
  (cond ((equal form '(exit-mixed-lisp-python-syntax))
         form)
        #+allegro ((and (keywordp form) (tpl::find-command-or-alias (symbol-name form) :quiet t))
                   ;; A toplevel command like :exit 
                   ;; XXX args not supported yet; should only be done when source is interactive input.
                   `(tpl:do-command ,form))
        (t `(let ((*readtable* *standard-readtable*))
              ,form))))

(defmacro handle-python (form)
  `(let ((clpython::*habitat* (or *lispy-habitat*
                                  (setf *lispy-habitat* (funcall 'clpython::make-habitat))))
         (clpython::*module-namespace* (or *lispy-namespace*
                                           (setf *lispy-namespace* (clpython::make-package-ns
                                                                    :package (find-package :cl-user)
                                                                    :scope :module
                                                                    :parent (clpython::make-builtins-namespace)
                                                                    :incl-builtins t)))))
     (declare (special clpython::*habitat* clpython::*module-namespace*))
     (with-sane-debugging ("Error occured in Python/Lisp input mode, while handling a Python form:")
       (let ((*readtable* *standard-readtable*))
         (clpython::run-python-ast ',form
                                   :module-globals *lispy-package*
                                   :compile *lispy-compile-python*
                                   :compile-quiet t)))))


