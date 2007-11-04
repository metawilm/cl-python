;; -*- package: clpython.parser; readtable: py-ast-user-readtable -*-
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

(defgeneric parse (thing &rest options)
  
  (:documentation "Parse THING (pathname or string); return AST.
Most important options:
  :YACC-VERSION -- :allegro-yacc (default) or :cl-yacc
  :DEBUG        -- print debug output?
  :INCL-MODULE  -- wrap AST in module statement?
  :ONE-EXPR     -- only return first form read (implies :INCL-MODULE NIL)
  :TAB-WIDTH    -- width of one tab character in spaces")
  
  (:method :around (x &rest options &key (incl-module t) (one-expr nil))
           (let ((res (apply #'call-next-method x (sans options :incl-module :one-expr))))
             (cond ((or one-expr (not incl-module))
                    (destructuring-bind (module-stmt (suite-stmt suite-items)) res
                      (assert (eq module-stmt '[module-stmt]))
                      (assert (eq suite-stmt '[suite-stmt]))
                      (cond ((and one-expr (/= (length suite-items) 1))
                             (error "Wanted one (due to :one-expr arg) but got ~A values, in AST for ~S."
                                    (length suite-items) x))
                            ((or one-expr (= (length suite-items) 1))
                             (car suite-items))
                            (t `([suite-stmt] ,suite-items)))))
                   (t res))))
  
  (:method ((x string) &rest options &key (yacc-version *default-yacc-version*))
           (let ((lexer (apply #'make-lexer x :yacc-version yacc-version options)))
             (parse-with-yacc yacc-version lexer)))
  
  (:method ((x pathname) &rest options)
           (apply #'parse (clpython.package::slurp-file x) options))

  (:method ((x stream) &rest options)
           (let ((seq (make-string (file-length x))))
             (read-sequence seq x)
             (parse seq))))

(defgeneric parse-with-yacc (yacc-version lexer)
  (:method (v lexer)
           (declare (ignore lexer))
           (error "The parser ~S could not be found." v)))

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