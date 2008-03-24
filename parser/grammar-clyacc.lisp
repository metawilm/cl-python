;; -*- package: clpython.parser -*-
;; 
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;; Python grammar for CL-Yacc

(in-package :clpython.parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :yacc))

(defvar *cl-yacc-python-parser*)

#.`(yacc:define-parser *cl-yacc-python-parser*
       (:terminals ,*terminals*)
     (:precedence ,(nreverse (get-precedence-and-associativity :left :right :nonassoc)))
     (:start-symbol clpython.parser::python-grammar)
     
     ,@'#.(loop with name-rules = (sort (loop for name being the hash-key in *python-prods*
                                            using (hash-value rules)
                                            collect (list name rules))
                                        #'string< 
                                        :key #'car)
              for (name rules) in name-rules
              collect `(,name ,@(loop for (terms outcome options) in rules
                                    for args = (loop for i from 1 to (length terms)
                                                   collect (intern (format nil "$~A" i) :clpython.parser))
                                    for func = `#'(lambda ,args
                                                    (declare (ignorable ,@args))
                                                    ,outcome)
                                    collect `(,@(or terms `(())) ;; splice term or insert () 
                                                (,func ,@options))))))

(defmethod parse-form-with-yacc ((yacc-version (eql :cl-yacc)) lexer)
  (if *catch-yacc-conditions*
      (handler-bind ((condition (lambda (c) (handle-parser-condition :cl-yacc c lexer))))
        (yacc:parse-with-lexer lexer *cl-yacc-python-parser*))
    (yacc:parse-with-lexer lexer *cl-yacc-python-parser*)))

(defmethod handle-parser-condition ((yacc-version (eql :cl-yacc)) c lexer)
  (cond ((typep c 'yacc:yacc-parse-error)
         (let* ((pos (funcall lexer :report-location))
                (line (second (assoc :line-no pos)))
                (eof-seen (second (assoc :eof-seen pos)))
                (token (yacc:yacc-parse-error-terminal c))
                (value (yacc:yacc-parse-error-value c))
                (expected-tokens (yacc:yacc-parse-error-expected-terminals c)))
           (declare (ignore token))
           ;; Hide line number mechanism hack
           (when (and (listp value)
                      (eq (car value) :newline))
             (setf value '[newline]))
           
           (cond (eof-seen (raise-unexpected-eof))
                 (t        (raise-syntax-error
                            (format nil "Parse error at line ~A, at token `~A'. ~
                                         ~_Expected one of: ~{`~A'~^ ~}.~%[Internal error ~S caught due to ~S]"
                                    line value expected-tokens c '*catch-yacc-conditions*))))))))
