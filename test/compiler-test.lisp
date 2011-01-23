;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.TEST; Readtable: PY-AST-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Python compiler test

(in-package :clpython.test)
(in-syntax *ast-user-readtable*)

(defun parse-with-tests (string test-replacements)
  (clpython.parser::parse-with-replacements
   string
   (loop for (id . test) in test-replacements
       collect `(([identifier-expr] ,id)
		 . (macrolet
		       ((do-test (&environment .e)
			  (macrolet ((pydecl (key)
				       `(clpython::get-pydecl ,key .e)))
			    ,test)))
		     (do-test))))))

(defun run-with-tests (string replacements)
  (let ((ast (parse-with-tests string replacements)))
    (let ((clpython.parser::*walk-warn-unknown-form* nil))
      (declare (special clpython.parser::*walk-warn-unknown-form*))
      (compile nil `(lambda () ,ast)))))

(defmacro run-code-test (string test-expr)
  `(progn
     (when *announce-test*
       (terpri)
       (format t "Run-code-test:~% ~S~% ~S~%" ',string ',test-expr))
     (let ((*announce-test* nil))
       (run-with-tests ,string '(({TEST} . ,test-expr))))))

(defun run-compiler-test ()
  (with-subtest (:name "CLPython-Compiler")
    (test-comp-decl :context-type-stack)
    (test-comp-decl :context-name-stack)
    (test-comp-decl :inside-loop-p)
    (test-comp-decl :mod-futures)
    #+(or)(test-comp-decl :mod-globals-names)
    (test-comp-decl :lexically-visible-vars)
    (test-comp-decl :lexically-declared-globals)
    (test-comp-decl :inside-function-p)
    ;; :safe-lex-visible-vars
    ;; Cannot simply test the use of `:inside-setf-py-attr'.
    ))

(defgeneric test-comp-decl (kind))

(defmethod test-comp-decl :around (kind)
  (with-subtest (:name (format nil "CLPython-Compiler-Decl-~S" kind))
    (call-next-method)))

(defmethod test-comp-decl ((kind (eql :context-type-stack)))
  (declare (ignorable kind))
  (run-code-test "TEST"               (test :module #1=(car (pydecl :context-type-stack))))
  (run-code-test "def foo(x,y): TEST" (test :function #1#))
  (run-code-test "class C(D): TEST"   (test :class #1#))
  (run-code-test "
class C:
  def f():
    TEST"                             (test :function #1#))
  (run-code-test "
def f():
  class C:
    TEST"                             (test :class    #1#)))

(defmethod test-comp-decl ((kind (eql :context-name-stack)))
  (declare (ignorable kind))
  (run-code-test "TEST"            (test-equal ()     #1=(pydecl :context-name-stack)))
  (run-code-test "def f(): TEST"   (test-equal '({f}) #1#))
  (run-code-test "class C: TEST"   (test-equal '({C}) #1#))
  (run-code-test "
class C:
  def f(): TEST"                   (test-equal '({f} {C}) #1#))
  (run-code-test "
def f():
  lambda x: TEST"                  (test-equal '(:lambda {f}) #1#))
  (run-code-test "
def f():
  class C:
    lambda x: TEST"                (test-equal '(:lambda {C} {f}) #1#)))

(defmethod test-comp-decl ((kind (eql :inside-loop-p)))
  (declare (ignorable kind))
  (run-code-test "TEST"              (test-false #1=(pydecl :inside-loop-p)))
  (run-code-test "while foo(): TEST" (test-true  #1#))
  (run-code-test "
while foo():
 if x:
  TEST" (test-true #1#))
  (run-code-test "
while foo():
 bar
TEST" (test-false #1#))
    
  (run-code-test "
for x in y:
 TEST" (test-true #1#)))

(defmethod test-comp-decl ((kind (eql :mod-futures)))
  (declare (ignorable kind))
  ":MOD-FUTURES"
  :todo)

(defmethod test-comp-decl ((kind (eql :mod-globals-names)))
  (declare (ignorable kind))
  (run-code-test "TEST" (test-true (typep #1=(pydecl :mod-globals-names) 'vector)))
  (run-code-test "TEST" (test-true (seq-equal #1# +standard-module-globals+)))
  (run-code-test "TEST"        (test-false (seq-member '{a} #1#)))
  (run-code-test "a; TEST"     (test-true  (seq-member '{a} #1#)))
  (run-code-test "a = 3; TEST" (test-true  (seq-member '{a} #1#)))
  (run-code-test "TEST; a"     (test-true  (seq-member '{a} #1#)))
  (run-code-test "if a: b(); TEST" (test-true (seq-equal #1# '({a} {b})
							 :ignore +standard-module-globals+)))
  ;; in a function
  (run-code-test "
TEST
def f():
   g = 3;
   return g"     (test-true (seq-equal #1# '({f}) :ignore +standard-module-globals+)))
  (run-code-test "
TEST
def f():
   g += 3;
   return g"     (test-true (seq-equal #1# '({f}) :ignore +standard-module-globals+)))
  (run-code-test "
TEST
def f():
   global g
   g += 3;
   return g"     (test-true (seq-equal #1# '({f} {g}) :ignore +standard-module-globals+)
			    :fail-info "Globals inside functions not added to module globals."))
  ;; in a class
  (run-code-test "
TEST
class C: pass"    (test-true (seq-equal #1# '({C}) :ignore +standard-module-globals+)))

  (run-code-test "
TEST
class C:
  x = 3"    (test-true (seq-equal #1# '({C}) :ignore +standard-module-globals+)))
  (run-code-test "
TEST
class C:
  x += 3"    (test-true (seq-equal #1# '({C}) :ignore +standard-module-globals+)))

  (run-code-test "
TEST
class C:
  global x
  x = 3"    (test-true (seq-equal #1# '({C} {x}) :ignore +standard-module-globals+)
                       :fail-info #100="Handling `global' declaration in classdef not correct yet."))

  ;; nested function/class
  (run-code-test "
TEST
def f():
  def g():
    global x
    x = 3"    (test-true (seq-equal #1# '({f} {x}) :ignore +standard-module-globals+)
                         :fail-info "Nested `global' not detected."))
  
  (run-code-test "
TEST
class C:
  def f():
    global x
    x = 3"    (test-true (seq-equal #1# '({C} {x}) :ignore +standard-module-globals+)
                         :fail-info #100#))

  (run-code-test "
TEST
def f():
  class C:
    global x
    x = 3"    (test-true (seq-equal #1# '({f} {x}) :ignore +standard-module-globals+)
                         :fail-info #100#)))

(defmethod test-comp-decl ((kind (eql :lexically-visible-vars)))
  (declare (ignorable kind))
  (run-code-test "TEST"        (test-equal () #1=(pydecl :lexically-visible-vars)))
  (run-code-test "a = 3; TEST" (test-equal () #1#))
  (run-code-test "TEST; a = 3" (test-equal () #1#))
  
  (run-code-test "def f(): TEST"             (test-true (seq-equal '() #1#)))
  (run-code-test "def f(x): TEST"            (test-true (seq-equal '({x}) #1#)))
  (run-code-test "def f(x,y,z=3): TEST"      (test-true (seq-equal '({x} {y} {z}) #1#)))
  (run-code-test "def f(x,*y,**z): TEST"     (test-true (seq-equal '({x} {y} {z}) #1#)))
  (run-code-test "def f( (x,y,z), *a): TEST" (test-true (seq-equal '({x} {y} {z} {a}) #1#)))
  
  (run-code-test "
def f( (x,y) ):
  def g(a,**b):
    TEST"        (test-true (seq-equal '({x} {y} {a} {b} {g}) #1#)
                            :fail-info "G is a local variable in F, therefore visible."))

  (run-code-test "
def f( (x,y) ):
  def g( x=3,**y):
    TEST"        (test-true (seq-equal '({x} {y} {g}) #1#)))

  (run-code-test "
def f( (x,y) ):
  def g(z):
    pass
  TEST"        (test-true (seq-equal '({x} {y} {g}) #1#)
                          :fail-info "G is a local variable in F."))
  
  (run-code-test "
class C(D):
  TEST"
		 (test-equal '() #1#))

  (run-code-test "
class C(D):
  pass
TEST"
		 (test-equal '() #1#
                             :fail-info "At module level, lex-vis-vars is by definition empty."))
  (run-code-test "
def g():
  class C(D):
    pass
  TEST"
		 (test-equal '({C}) #1#))
  (run-code-test "
def f():
  class C:
    def g(self):
      TEST"
                 (test-true (seq-equal '({self} {C}) #1#)
                            :fail-info "C is local var in F."))
  (run-code-test "
def f():
  class C:
    def g(self):
      def h():
        TEST"
                 (test-true (seq-equal '({C} {self} {h}) #1#)
                            :fail-info "H local var in G"))
  (run-code-test "
aaa = 4
class C(D):
  def f(x):
    class Q():
      bbb = 23
      def g(z):
        TEST"
		 (test-true (seq-equal '({x} {Q} {z}) #1#)
                            :fail-info "The class C is not lex-vis"))
  )

(defmethod test-comp-decl ((kind (eql :lexically-declared-globals)))
  (declare (ignorable kind))
  (run-code-test "TEST"             (test-equal () #1=(pydecl :lexically-declared-globals)))
  (run-code-test "a = 3; TEST"      (test-equal () #1#))
  (run-code-test "
global a,b  ## module-levels vars should not end up in :lex-decl-glob
TEST" (test-true (seq-equal '() #1#)))
  (run-code-test "
global x  ## bogus decl; should not leak into F
def f():
  global b
  TEST
  b = x"		 (test-true (seq-equal '({b}) #1#))))

(defmethod test-comp-decl ((kind (eql :inside-function-p)))
  (declare (ignorable kind))
  (run-code-test "TEST"             (test-false #1=(let ((ctx-stack  (pydecl :context-type-stack)))
                                                     (format t "[ctx-stack = ~A]" ctx-stack)
                                                     (member :function ctx-stack))))
  (run-code-test "a = 3; TEST"      (test-false #1#))
  (run-code-test "def f(): TEST"    (test-true  #1#))
  (run-code-test "
def f():
  class C:
    TEST"                           (test-true  #1#))
  (run-code-test "
class C: 
  TEST"                             (test-false #1#))
  (run-code-test "
class C: 
  def m(x):
    TEST"                           (test-true #1#)))
