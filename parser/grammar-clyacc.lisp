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
     
     ,@'#.(loop for name being the hash-key in *python-prods*
              using (hash-value rules)
              collect `(,name ,@(loop for (terms outcome options) in rules
                                    for args = (loop for i from 1 to (length terms)
                                                   collect (intern (format nil "$~A" i) :clpython.parser))
                                    collect `(,@(or terms `(())) ;; Splice the terms, or insert ().
                                                #'(lambda ,args
                                                    (declare (ignorable ,@args))
                                                    ,outcome))
                                    when options
                                    do (warn "CLPython + CL-Yacc warning: in grammar rule for `~A'~_~
                                              the option ~S is ignored, because this rule-level~_~
                                              precedence handling is not supported by CL-Yacc.~_~
                                              Unfortunately this means CL-Yacc may return wrong parse trees."
                                             name (car options))))))

(defmethod parse-with-yacc ((x (eql :cl-yacc)) lexer)
  (yacc:parse-with-lexer lexer *cl-yacc-python-parser*))
