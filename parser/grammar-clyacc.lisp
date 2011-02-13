;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.PARSER; Readtable: PY-AST-USER-READTABLE -*-
;; 
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Python grammar for CL-Yacc

(in-package :clpython.parser)
(in-syntax *ast-user-readtable*)


;;; Grammar

(eval-when (:compile-toplevel)
  #+sbcl (terpri)
  (format t ";;; Compiling Python grammar for CL-Yacc...~%"))

#.(let ((parser-form
         `(yacc:define-parser *cl-yacc-python-parser*
              (:terminals ,*terminals*)
            (:precedence ,(nreverse (get-precedence-and-associativity :left :right :nonassoc)))
            (:start-symbol python-grammar)
            
            ,@(loop for name being each hash-key in *python-prods*
                  using (hash-value rules)
                  collect `(,name ,@(loop for (terms outcome) in rules
                                        for args = (loop for i from 1 to (length terms)
                                                       collect (intern (format nil "$~A" i) :clpython.parser))
                                        for func = `(lambda ,args
                                                      (declare (ignorable ,@args))
                                                      ,outcome)
                                        collect `(,@(or terms `(())) ;; splice terms or insert () 
                                                    ,func)))))))
    ;; Prevent out of memory during compilation in ECL
    #+ecl `(eval ',parser-form)
    #-ecl parser-form)

;;; Lexer

(defmethod make-lexer ((yacc-version (eql :cl-yacc)) (string string) &rest options)
  "Pro forma definition: nothing special."
  (declare (ignorable yacc-version string options))
  (call-next-method))

(defmethod lexer-eof-token ((yacc-version (eql :cl-yacc)))
  (declare (ignorable yacc-version))
  nil)


;;; Parser

(defmethod parse-form-with-yacc ((yacc-version (eql :cl-yacc)) lexer)
  (declare (ignorable yacc-version)
           #+ecl (special *cl-yacc-python-parser*))
  (yacc:parse-with-lexer lexer *cl-yacc-python-parser*))

(defmethod handle-parser-condition ((yacc-version (eql :cl-yacc)) c lexer)
  (declare (ignorable yacc-version))
  (cond ((typep c 'yacc:yacc-parse-error)
         (let* ((pos (funcall lexer :report-location))
                (line (second (assoc :line-no pos)))
                (last-newline-in-source (second (assoc :last-newline-in-source pos)))
                (token (yacc:yacc-parse-error-terminal c))
                (value (maybe-unwrap-literal-value (yacc:yacc-parse-error-value c)))
                (expected-tokens (yacc:yacc-parse-error-expected-terminals c)))
           
           (cond ((or (eq token (lexer-eof-token yacc-version))
                      (and (eq token '[newline]) (not last-newline-in-source))
                      (and (eq token '[dedent]) (not (eq last-newline-in-source :unknown))))
                  (raise-unexpected-eof))
                 
                 (t
                  (raise-syntax-error
                   (format nil "At line ~A, parser got unexpected token: ~S. ~
                                ~_Expected one of: ~{`~A'~^ ~}."
                           line value expected-tokens))))))))
