;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.PARSER; Readtable: PY-AST-USER-READTABLE -*-
;; 
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Python grammar for the Allegro Yacc

(in-package :clpython.parser)
(in-syntax *ast-user-readtable*)


;;; Grammar

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :yacc))

#.`(excl.yacc:defgrammar python-grammar (excl.yacc:grammar)
     ()
     ,@(get-precedence-and-associativity :left-associative
                                         :right-associative
                                         :non-associative)
     (:lexemes ,@*terminals*))

#.`(progn ,@(loop for name being each hash-key in *python-prods*
                using (hash-value rules)
                nconc (loop for (terms outcome) in rules
                          collect `(excl.yacc:defproduction (,name python-grammar) ,terms (,outcome)))))

(excl.yacc:build-grammar python-grammar nil nil)
(mop:finalize-inheritance (find-class 'python-grammar))


;;; Lexer

(defmethod make-lexer ((yacc-version (eql :allegro-yacc)) (string string) &rest options)
  "Allegro grammar expects lexer to return two values: TOKEN-CODE (as integer), TOKEN.
Therefore need to convert TOKEN-KIND to the corresponding TOKEN-CODE before passing it on."
  (declare (ignore options))
  (let ((f (call-next-method))
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
          (etypecase token
            (symbol (values (token-code token) val))
            (list   (progn (assert (eq op :report-location))
                           (values token val)))))))))

(defmethod lexer-eof-token ((yacc-version (eql :allegro-yacc)))
  'excl.yacc:eof)


;;; Parser

(defmethod parse-form-with-yacc ((yacc-version (eql :allegro-yacc)) lexer)
  (let ((grammar (make-instance 'python-grammar :lexer lexer)))
    (excl.yacc:parse grammar)))

(defmethod handle-parser-condition ((yacc-version (eql :allegro-yacc)) c lexer)
  ;; When a SyntaxError is thrown by us in the lexer, the parser
  ;; first signals the SyntaxError, then it raises a GRAMMAR-PARSE-ERROR.
  (declare (special clpython:*exceptions-loaded* *include-line-numbers*)
           (ignore lexer))
  (cond ((and clpython:*exceptions-loaded* (typep c '{SyntaxError}))
	 (error c)) ;; Converting SIGNAL to ERROR
	
	((typep c 'excl.yacc:grammar-parse-error)
	 (let ((pos (excl.yacc:grammar-parse-error-position c)))
           (assert (listp pos) () "Got invalid grammar position (not a list): ~S" pos)
           (let* ((line (second (assoc :line-no pos)))
                  (last-newline-in-source (second (assoc :last-newline-in-source pos)))
                  (token (maybe-unwrap-literal-value (excl.yacc:grammar-parse-error-token c)))
                  (encl-error (excl.yacc::grammar-parse-error-enclosed-error c)))
             (cond (encl-error ;; Error in one of our grammar rules
                    (when clpython:*exceptions-loaded*
                      (assert (not (typep encl-error '{SyntaxError}))
                          () "CLPython: Strange: Parser raises EXCL.YACC:GRAMMAR-PARSE-ERROR ~
                              with a SyntaxError enclosed, without first signalling that ~
                              SyntaxError (~A)." c))
                    (raise-syntax-error
                     (format nil "Parse error at line ~A~@[, at token `~S'~].~%[Internal error: ~A~_(caught due to ~S)]"
                             line token encl-error '*catch-yacc-conditions*)))
                   
                   ((and token
                         (not (eq token (lexer-eof-token yacc-version)))
                         (not (and (eq token '[newline]) (not last-newline-in-source))))
                    (raise-syntax-error (format nil "At line ~A, parser got unexpected token: ~S" line token)))
                   
                   (t
                    (raise-unexpected-eof))))))))
