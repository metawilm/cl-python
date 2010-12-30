;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.PARSER; Readtable: PY-AST-USER-READTABLE -*-
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

(defvar *default-yacc-version* #+allegro :allegro-yacc #-allegro :cl-yacc)

(defparameter *catch-yacc-conditions* t
  "Whether to catch YACC conditions, and translate them into Python exceptions.
\(Disable to debug the grammar rules.)")

(defgeneric parse (thing &rest options)
  
  (:documentation "Parse THING (pathname or string); return AST.
Most important options:
  :YACC-VERSION -- :allegro-yacc (default) or :cl-yacc
  :ONE-EXPR     -- only return first form read, not wrapped in module/suite
  :TAB-WIDTH    -- width of one tab character in spaces")
  
  (:method ((x string) &rest options &key (yacc-version *default-yacc-version*) one-expr record-source-location tab-width)
           (declare (ignore one-expr record-source-location tab-width))
           (let ((lexer (apply #'make-lexer yacc-version x (sans options :one-expr :record-source-location))))
             (apply #'parse-module-with-yacc yacc-version lexer (sans options :tab-width :yacc-version))))
  
  (:method ((x pathname) &rest options)
           (apply #'parse (slurp-file x) options))

  (:method ((x stream) &rest options)
             (apply #'parse (slurp-file x) options)))

(defmacro with-source-locations (&body body)
  `(let (#+clpython-source-level-debugging
         (*python-form->source-location* (make-weak-key-hash-table :test 'eq)))
     ,@body))

(defun parse-module-with-yacc (yacc-version lexer &key one-expr (record-source-location *python-form->source-location*))
  "Collect all parsed top-level forms. If RECORD-SOURCE-LOCATION, the (new or existing)
source location hash-table is returned as second value."
  (let ((*python-form->source-location*
         (case record-source-location
           ((nil))
           ((t) (make-weak-key-hash-table :test 'eq))
           (t record-source-location))))
    (let (forms)
      (loop (multiple-value-bind (form eof-p)
                (parse-form-with-yacc yacc-version lexer)
              (push form forms)
              (when eof-p
                (setf forms (nreverse forms))
                (return))))
      (cond (one-expr
             (assert (= (length forms) 1) ()
               "Got ~A forms, while only one expected (due to :ONE-EXPR), in AST for ~S." (length forms))
             (setf forms (car forms)))
            (t 
             (setf forms `([module-stmt] ([suite-stmt] ,forms)))))
      (if *python-form->source-location*
          (values forms *python-form->source-location*)
        forms))))

(defparameter *signal-toplevel-form-finished-conditions* nil
  "Whether to signal toplevel-form-finished-condition between top-level forms.")

(defmacro with-parser-eof-detection ((at-real-eof-var) &body body)
  (check-type at-real-eof-var symbol)
  `(let* ((*lex-fake-eof-after-toplevel-form* t)
          (,at-real-eof-var nil))
     (declare (special *lex-fake-eof-after-toplevel-form*))
     (handler-bind ((next-eof-real (lambda (c)
                                     (declare (ignore c))
                                     (setf ,at-real-eof-var t)
                                     (unless *signal-toplevel-form-finished-conditions*
                                       (invoke-restart (find-restart 'muffle)))))
                    (next-eof-fake-after-toplevel-form
                     (lambda (c)
                       (declare (ignore c))
                       (unless *signal-toplevel-form-finished-conditions*
                         (invoke-restart (find-restart 'muffle))))))
       ,@body)))

(defgeneric handle-parser-condition (yacc-version condition lexer))

(defmacro with-parser-conditions-handled ((yacc-version lexer) &body body)
  `(handler-bind ((condition (lambda (c)
                               (when *catch-yacc-conditions*
                                 (handle-parser-condition ,yacc-version c ,lexer)))))
     ,@body))

(defgeneric parse-form-with-yacc (yacc-version lexer)
  (:documentation "Parse one top-level form with the given yacc. Returns FORM, EOF-P.")
  (:method :around (yacc-version lexer)
           (with-parser-eof-detection (at-real-eof)
             (with-parser-conditions-handled (yacc-version lexer)
               (let ((res (call-next-method)))
                 (values res at-real-eof)))))
  (:method (v lexer)
           (declare (ignore lexer))
           (error "Parser ~S is unavailable." v)))


(defun parse-with-replacements (string replacements &key (warn-unused t) parse-options)
  "Parse STRING, but replace certain tokens in the resulting AST.
Used to parse a template string, then fill in certain \"gaps\".
REPLACEMENTS is list: ((old . new) ...)
E.g. to replace identifier 'foo' with 'bar', use this replacement:
 ( ([identifier-expr] {foo}) . ([identifier-expr] {foo}) )"
  (let ((ast (apply #'parse string parse-options)))
    (if warn-unused
	(loop for (old . new) in replacements
	    do (let ((new-ast (subst new old ast :test 'equalp)))
		 (when (tree-equal ast new-ast)
		   (warn "[parse-with-replacements] This replacement is unused: ~S => ~S."
                         old new))
		 (setf ast new-ast)))
      (loop for (old . new) in replacements
	  do (setf ast (nsubst new old ast :test 'equalp))))
    ast))