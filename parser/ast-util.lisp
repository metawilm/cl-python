;; -*- package: clpython.parser; readtable: py-ast-readtable -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.parser)
(in-syntax *ast-readtable*)

;;;; Abstract Syntax Tree Utilities

(defvar *multi-line-statements* '([classdef-stmt] [for-in-stmt] [funcdef-stmt]
                                  [if-stmt] [try-except-stmt] [try-finally-stmt]
                                  [while-stmt] [with-stmt])
  "Statements that can occupy several lines.")

(defvar *use-ast-return-stmt-heuristic* t
  "Whether MODULE-STMT-FINISHED-P should apply a heuristic that is correct
about 99% of the cases.")

(defun ast-complete-p (ast)
  "Determine if the (interactively entered) AST is complete, in that the next line in the input ~
starts a new top-level statement. Uses an extra heuristic if *use-ast-return-stmt-heuristic*."

  ;; The RETURN-STMT-HEURISTIC recognizes two common function patterns:
  ;;
  ;;  1. def f():
  ;;       return 42  (`return' at the end of function body)
  ;;
  ;;  2. def f():
  ;;       if c:
  ;;          return X  (`return' at the end of every `if' clause (think `fact')
  ;;       else:
  ;;          return Y
  ;;
  ;; The heuristic fails if the user tries to define a generator in this way:
  ;;
  ;;  def f():
  ;;     return
  ;;     yield 1
  ;;
  ;; The `yield' makes it a generator. But the heuristic decides that the
  ;; top-level `return' signals the end of the function, so the user does
  ;; not get the possiblity to add the `yield' at the end.
  ;;
  ;; The user can however input an equivalent definition, by inserting the
  ;; `yield' somewhere before the return, like:
  ;; 
  ;;  def f():
  ;;    if 0: yield 1
  ;;    return

  (flet ((funcdef-complete-p (ast)
           (assert ([funcdef-stmt-p] ast))
           (with-matching (ast ([funcdef-stmt] ?decorators ([identifier-expr] ?fname) ?fargs
                                               ([suite-stmt] ?stmts)))
             (let ((last-stmt (car (last ?stmts))))
               (when (listp last-stmt)
                 (case (car last-stmt)
                   ([return-stmt] (return-from funcdef-complete-p t)) ;; Pattern 1.
                   ([if-stmt]     (with-matching (last-stmt ([if-stmt] ?if-clauses ?else-clause)) ;; Pattern 2.
                                    (and (loop for ic in ?if-clauses
                                             always (with-matching (ic (?cond ([suite-stmt] ?stmts)))
                                                      ([return-stmt-p] (car (last ?stmts)))))
                                         ?else-clause ;; The `else' clause is always allowed.
                                         (with-matching (?else-clause ([suite-stmt] ?stmts))
                                           ([return-stmt-p] (car (last ?stmts)))))))))))))
    (etypecase ast
      ((or string number) t)
      (list (cond ((not (member (car ast) *multi-line-statements*))
                   t)
                  ((and *use-ast-return-stmt-heuristic* ([funcdef-stmt-p] ast))
                   (funcdef-complete-p ast))
                  (t nil))))))