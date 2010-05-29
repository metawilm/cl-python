;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.PARSER; Readtable: PY-AST-READTABLE -*-
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

(defun ast-p (form &optional ast-node)
  "Whether FORM is a list represeting a Python AST. (Includes parsed literal numbers and strings.)"
  (and (listp form)
       (symbolp (car form))
       (if ast-node
           (progn (check-type ast-node symbol)
                  (eq (car form) ast-node))
         (eq (symbol-package (car form)) #.(find-package :clpython.ast.node)))))

(defun abbreviated-python-code (ast)
  (abbreviate-to-one-line (py-pprint ast)))

(defun symbol-ends-with-p (symbol suffix)
  (check-type symbol symbol)
  (let* ((name (symbol-name symbol))
         (name.len (length name))
         (suffix.len (length suffix)))
    (and (>= name.len suffix.len)
         (string= (subseq name (- name.len suffix.len)) suffix))))

(defun stmt-node-p (symbol)
  (symbol-ends-with-p symbol "-stmt"))

(defun expr-node-p (symbol)
  (symbol-ends-with-p symbol "-expr"))

(defparameter *expr-stmt-nodes*
    (sort (loop for x being each external-symbol in :clpython.ast.node
              when (or (stmt-node-p x) (expr-node-p x))
              collect x)
          'string<)
  "List of all ..-STMT and ..-EXPR symbols that can occur in ASTs.")

(defun stmt-or-expr-node-p (symbol)
  (check-type symbol symbol)
  (member symbol *expr-stmt-nodes*))

(defvar *multi-line-statements* '(([classdef-stmt] "class")
                                  ([for-in-stmt]   "for")
                                  ([funcdef-stmt]  "def")
                                  ([if-stmt]       "if")
                                  ([try-except-stmt]  "try")
                                  ([try-finally-stmt] "try")
                                  ([while-stmt]    "while")
                                  ([with-stmt]     "with"))
  "Statements that can occupy several lines, with their start token.")

(defvar *use-ast-return-stmt-heuristic* t
  "Whether MODULE-STMT-FINISHED-P should apply a heuristic that is correct
about 99% of the cases.")

(defun ast-complete-p (ast)
  "Determine if the (interactively entered) AST is complete, in that the next line in the input ~
starts a new top-level statement. Uses an extra heuristic if *use-ast-return-stmt-heuristic*."

  ;; This heuristic recognizes the following common function patterns:
  ;;
  ;;  1. def f():
  ;;       ...
  ;;       return 42  (`return' at the end of function body)
  ;;
  ;;  2. def f():
  ;;       if c:
  ;;          return X  (`return' at the end of every `if' clause (think `fact')
  ;;       else:
  ;;          return Y
  ;;
  ;;  3. def f():
  ;;        ...
  ;;        pass
  ;;
  ;;  4. def f():
  ;;        ...
  ;;        raise
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
           (assert (ast-p ast '[funcdef-stmt]))
           (with-matching (ast ([funcdef-stmt] ?decorators ([identifier-expr] ?fname) ?fargs
                                               ([suite-stmt] ?stmts)))
             (let ((last-stmt (car (last ?stmts))))
               (when (listp last-stmt)
                 (case (car last-stmt)
                   ([return-stmt] t) ;; Pattern 1.
                   ([if-stmt]     (with-matching (last-stmt ([if-stmt] ?if-clauses ?else-clause)) ;; Pattern 2.
                                    (and (loop for ic in ?if-clauses
                                             always (with-matching (ic (?cond ([suite-stmt] ?stmts)))
                                                      (ast-p (car (last ?stmts)) '[return-stmt])))
                                         ?else-clause ;; The `else' clause must be preesnt
                                         (with-matching (?else-clause ([suite-stmt] ?stmts))
                                           (ast-p (car (last ?stmts)) '[return-stmt])))))
                   ([pass-stmt] t) ;; Pattern 3
                   ([raise-stmt] t))))))) ;; Pattern 4
    (etypecase ast
      ((or string number) t)
      (list (cond ((not (member (car ast) (mapcar #'car *multi-line-statements*)))
                   t)
                  ((and *use-ast-return-stmt-heuristic* (ast-p ast '[funcdef-stmt]))
                   (funcdef-complete-p ast))
                  (t nil))))))