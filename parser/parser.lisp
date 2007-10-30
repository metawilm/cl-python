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
                      (if one-expr
                          (prog1 (car suite-items)
                            (unless (= (length suite-items) 1)
                              (error "Wanted one, but got ~A values in AST for ~S."
                                     (length suite-items) x)))
                        suite-items)))
                   (t res))))
  
  (:method ((x string) &rest options &key (yacc-version *default-yacc-version*))
           (let ((lexer (apply #'make-lexer x :yacc-version yacc-version options)))
             (parse-with-yacc yacc-version lexer)))
  
  (:method ((x pathname) &rest options)
           (apply #'parse (clpython.package::slurp-file x) options)))

(defgeneric parse-with-yacc (yacc-version lexer)
  (:method (v lexer)
           (declare (ignore lexer))
           (error "The parser ~S could not be found." v)))
