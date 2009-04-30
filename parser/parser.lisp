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
  :DEBUG        -- print debug output?
  :INCL-MODULE  -- wrap AST in module statement?
  :ONE-EXPR     -- only return first form read (implies :INCL-MODULE NIL)
  :TAB-WIDTH    -- width of one tab character in spaces")
  
  (:method :around (x &rest options &key (one-expr nil) (incl-module (not one-expr)) &allow-other-keys)
           (assert (not (and incl-module one-expr)) ()
             "PARSE options :ONE-EXPR and :INCL-MODULE are mutually exclusive.")
           (let ((res-list (multiple-value-list (apply #'call-next-method x :incl-module incl-module (sans options :one-expr)))))
             (when one-expr
               (assert (= (length (car res-list)) 1) ()
                 "PARSE got ~A forms, while only one expected (due to :ONE-EXPR), in AST for ~S."
                 (length (car res-list)) x)
               (setf (car res-list) (caar res-list)))
             (values-list res-list)))
  
  (:method ((x string) &rest options &key (yacc-version *default-yacc-version*))
           (let ((lexer (apply #'make-lexer yacc-version x (sans options :incl-module :record-source-location))))
             (apply #'parse-module-with-yacc yacc-version lexer options)))
  
  (:method ((x pathname) &rest options)
           (apply #'parse (slurp-file x) options))

  (:method ((x stream) &rest options)
           (let* ((length (or (file-length x)
                              (error "Parsing failed: cannot determine length of stream ~A." x)))
                  (seq (make-string length))
                  (n (read-sequence seq x)))
             (setf seq (adjust-array seq n))
             (apply #'parse seq options))))

(defmacro with-source-locations (&body body)
  `(let (#+clpython-source-level-debugging
         (*python-form->source-location* (make-weak-key-hash-table :test 'eq)))
     ,@body))

(defun parse-module-with-yacc (yacc-version lexer &key incl-module (record-source-location *python-form->source-location*))
  "Collect all parsed top-level forms. If RECORD-SOURCE-LOCATION, the (new or existing)
source location hash-table is returned as second value."
  (let ((*python-form->source-location*
         (case record-source-location
           ((nil))
           ((t) (clpython.package::make-weak-key-hash-table :test 'eq))
           (t record-source-location))))
    (let (forms)
      (loop (multiple-value-bind (form eof-p)
                (parse-form-with-yacc yacc-version lexer)
              (push form forms)
              (when eof-p
                (setf forms (nreverse forms))
                (return))))
      (when incl-module
        (setf forms `([module-stmt] ([suite-stmt] ,forms))))
      (if *python-form->source-location*
          (values forms *python-form->source-location*)
        forms))))

(defmacro with-parser-eof-detection ((at-real-eof-var) &body body)
  (check-type at-real-eof-var symbol)
  `(let* ((*lex-fake-eof-after-toplevel-form* t)
          (,at-real-eof-var nil))
     (declare (special *lex-fake-eof-after-toplevel-form*))
     (handler-bind ((next-eof-real (lambda (c)
                                     (declare (ignore c))
                                     (setf ,at-real-eof-var t)
                                     (invoke-restart (find-restart 'muffle))))
                    (next-eof-fake-after-toplevel-form
                     (lambda (c)
                       (declare (ignore c))
                       (invoke-restart (find-restart 'muffle)))))
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