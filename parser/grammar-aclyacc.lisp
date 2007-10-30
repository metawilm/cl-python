;; -*- package: clpython.parser; readtable: py-user-readtable -*-
;; 
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;; Python grammar for the Allegro Yacc

(in-package :clpython.parser)
(in-syntax *user-readtable*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :yacc)
  (import 'excl.yacc:(defgrammar grammar defproduction build-grammar)))

#.`(excl.yacc:defgrammar python-grammar (excl.yacc:grammar)
     ()
     ,@(get-precedence-and-associativity :left-associative :right-associative :non-associative)
     (:lexemes ,@*terminals*))

#.`(progn ,@(loop for name being the hash-key in *python-prods*
                using (hash-value rules)
                nconc (loop for (terms outcome options) in rules
                          collect `(defproduction (,name python-grammar) ,terms (,outcome)
                                                  ,@(when options (list options))))))

(build-grammar python-grammar nil nil)


;; Handling parse errors

(defparameter *catch-yacc-conditions* t
  "Whether to catch YACC conditions, and translate them into Python exceptions.
\(Disable to debug the grammar rules.)")

(defmethod parse-with-yacc ((x (eql :allegro-yacc)) lexer)
  (let ((grammar (make-instance 'python-grammar :lexer lexer)))
    (if *catch-yacc-conditions*
        (handler-bind ((condition #'handle-parser-condition))
          (excl.yacc:parse grammar))
      (excl.yacc:parse grammar))))

(defun handle-parser-condition (c)
  ;; When a SyntaxError is thrown by us in the lexer, the parser
  ;; first signals the SyntaxError, then it raises a GRAMMAR-PARSE-ERROR.
  (declare (special clpython:*exceptions-loaded* *include-line-numbers*))
  (cond ((and clpython:*exceptions-loaded* (typep c '{SyntaxError}))
	 (error c)) ;; Converting SIGNAL to ERROR
	
	((typep c 'excl.yacc:grammar-parse-error)
	 (let ((pos (excl.yacc:grammar-parse-error-position c)))
           (assert (listp pos) () "Got invalid grammar position (not a list): ~S" pos)
           (let* ((line (second (assoc :line-no pos)))
                  (eof-seen (second (assoc :eof-seen pos)))
                  (token (excl.yacc:grammar-parse-error-token c))
                  (encl-error (excl.yacc::grammar-parse-error-enclosed-error c)))
             
             (when (and (integerp token) *include-line-numbers*)
               (setf token '[newline]))
	   
             (cond (encl-error ;; Error in one of our grammar rules
                    (when clpython:*exceptions-loaded*
                      (assert (not (typep encl-error '{SyntaxError}))
                          () "CLPython: Strange: Parser raises EXCL.YACC:GRAMMAR-PARSE-ERROR ~
                              with a SyntaxError enclosed, without first signalling that ~
                              SyntaxError (~A)." c))

                    (raise-syntax-error
                     (format nil "Parse error at line ~A~@[, at token `~S'~].~%[inner error: ~A]"
                             line token encl-error)))
                   
                   ((or (eq token 'excl.yacc:eof) eof-seen)
                    (raise-unexpected-eof))
                   
                   (t (raise-syntax-error
                       (format nil "At line ~A, parser got unexpected token: `~A'."
                               line token)))))))))
