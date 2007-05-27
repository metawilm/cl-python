;; -*- package: clpython.test; readtable: py-ast-user-readtable -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Python parser test

(in-package :clpython.test)

(in-syntax *ast-user-readtable*)
	   
(defun ps (s &optional (incl-module t))
  (if incl-module
      (parse-python-string s :incl-module t)
    (parse-python-one-expr s)))

(defun run-parser-test ()
  (with-subtest (:name "CLPython-Parser")
    
    ;; packages
    (test-true (find-package :clpython.parser))
    (test-true (find-package :clpython.user))
    (test '(clpython.ast.reserved:|def| :external)
	  (find-symbol "def" :clpython.ast) :multiple-values t)
    (test '(clpython.ast.operator:|<=| :external)
	  (find-symbol "<="  :clpython.ast) :multiple-values t)
    (test '(clpython.ast.node:|funcdef-stmt| :external) 
	  (find-symbol (symbol-name :|funcdef-stmt|) :clpython.ast) :multiple-values t)
    
    ;; ast readtable
    (test-true (eq '[>] 'clpython.ast.operator:>))
    (test-true (eq '[assign-stmt] 'clpython.ast.node:|assign-stmt|))
    (test-error (let ((*readtable* *ast-user-readtable*))
		  (read-from-string "'[foo]"))
		:condition-type 'reader-error) ;; not exist
    (test-false (find-symbol "foo" :clpython.ast)) ;; not created by previous test
    (test-true (eq '[>] '[>] ))
    
    ;; user readtable
    (test-true (eq '{__getitem__} 'clpython.user:|__getitem__|))
    (test-true (eq '{abs} 'clpython.user::|abs|))
    (test-true (eq '{foo} 'clpython.user::|foo|)) ;; auto intern
    (test-true (find-symbol "foo" :clpython.user)) ;; created by previous test
    (test-true (eq '{abs} '{abs} ))

    ;; atoms
    (test-equal '([module-stmt] ([suite-stmt] (42))) (ps "42"))
    (test-equal '([module-stmt] ([suite-stmt] ("x"))) (ps "'x'"))
      
    ;; variables
    (test-equal '([assign-stmt] 3 (([identifier-expr] {y} ))) (ps "y = 3" nil))
    (test-equal '([assign-stmt] 3 (([identifier-expr] {len}))) (ps "len = 3" nil))
      
    ;; suffix operations
    (test-equal '([attributeref-expr]
		  ([call-expr]
		   ([subscription-expr] ([identifier-expr] {x}) 1)
		   ((2) nil nil nil))
		  ([identifier-expr] {a3}))
		(ps "x[1](2).a3" nil))
      
    ;; call arguments
    (test-equal '([attributeref-expr]
		  ([call-expr] ([subscription-expr] ([identifier-expr] {x})
				1)
		   (nil ((([identifier-expr] {len}) 2)) nil nil))
		  ([identifier-expr] {a3}))
		(ps "x[1](len=2).a3" nil))
      
    (test-equal '([call-expr] 
		  ([identifier-expr] {f})
		  ((1 2) ((([identifier-expr] {y}) 3))
		   ([identifier-expr] {args})
		   ([identifier-expr] {kw})))
		(ps "f(1,2, y=3, *args, **kw)" nil))

    ;; order of args: pos, key, *, **
    (test-error (ps "f(a=1,b)" nil)
		:condition-type '{SyntaxError})
    
    (test-error (ps "f(*a,1)" nil)
		:condition-type '{SyntaxError})
    
    (test-error (ps "f(**a,*b)" nil)
		:condition-type '{SyntaxError})
    
    (test-no-error (ps "f(x,y,z=3,*a,**b)" nil))
    
    ;; function decorators
    (test-equal '([funcdef-stmt]
		  ;; list of decorators: first foo(bar)
		  (([call-expr] ([identifier-expr] {foo}) ((([identifier-expr] {bar})) nil nil nil))
		   ;; second deco: zut
		   ([identifier-expr] {zut}))
		  ([identifier-expr] {f})
		  (nil nil nil nil)
		  ([suite-stmt] (([pass-stmt]))))
		(ps "
@foo(bar)
@zut
def f(): pass" nil))
    
    ;; Empty string is parsed as module without body
    (test-equal '([module-stmt] ([suite-stmt] () ))
		(parse-python-string ""))

    ;; 'y = yield x' is todo, currently raises SyntaxError
    (test-error (parse-python-string "y = yield x")
		:condition-type '{SyntaxError})
    
    ;; parsing a file
    #+(and allegro unix) ;; no WITH-OPEN-TEMP-FILE on windows
    (let ((fname (excl.osi:with-open-temp-file (s "clpython-ast-test")
		   (format s "print 42"))))
      (test-equal '(([print-stmt] nil (42) nil))
		  (..parser:parse-python-file fname :incl-module nil))
      (test t (excl.osi:unlink fname)))
    
    ;; handling eof
    (test-true (subtypep '{UnexpectedEofError} '{SyntaxError}))
    (flet ((try-parse (s)
	     (handler-case (parse-python-string s)
	       ({UnexpectedEofError} () :unexp-eof-error)
	       ({SyntaxError}        () :syntax-error)
	       (condition            () :condition))))
      (test :unexp-eof-error (try-parse "'") :fail-info "(EOF in Lexer)")
      (test :unexp-eof-error (try-parse "def")
	    :fail-info "Unexpected EOF in grammar")
      (test :unexp-eof-error (try-parse "def f():") :fail-info "(EOF in grammar)")
      (test :syntax-error    (try-parse "def def") :fail-info "(Incorrect grammar)")
      (test :syntax-error    (try-parse " 42") :fail-info "(Leading whitespace)"))
    (test-equal 42 (handler-bind (({SyntaxError} (lambda (c) (declare (ignore c))
                                                         (continue))))
                     (parse-python-one-expr " 42")))
    ))

(defun run-code-walker-test ()
  (with-subtest (:name "CLPython-Codewalker")
    ))

(defun string-strip-= (x y)
  (string= (clpython::py-string.strip x)
	   (clpython::py-string.strip y)))

(defun run-pretty-printer-test ()
  (with-subtest (:name "CLPython-PrettyPrinter")
    ;; Test  string -> ast -> string  and ast -> string -> ast
    (macrolet ((p (str &rest options)
		 `(progn 
		    (test ,str (py-pprint (parse-python-string ,str))
			  :test 'string-strip-= ,@options)
		    (when (string-strip-= ,str (py-pprint (parse-python-string ,str)))
		      (test-equal (parse-python-string ,str)
				  (parse-python-string (py-pprint (parse-python-string ,str)))
				  ,@options))))
	       (pe (str &rest options)
		 `(progn 
		    (test ,str (py-pprint (parse-python-one-expr ,str))
			  :test 'string-strip-= ,@options)
		    (when (string-strip-= ,str (py-pprint (parse-python-one-expr ,str)))
		      (test-equal (parse-python-string ,str)
				  (parse-python-string (py-pprint (parse-python-one-expr ,str)))
				  ,@options)))))
      (parse-python-string "")
      ;; number
      (pe "42")
      (pe "1.")
      (pe "1.2")
      ;; string
      (pe "'x'")
      (p "'\"'")
      ;; assert
      (p "assert (1, 2, 3)")
      (p "assert x > 0, 'error'")
      ;; assign-stmt
      (p "x = 42")			
      (p "x, y = 1, 2")
      (p "x = y = 4")
      (p "x = y, z = 1, 2")
      ;; attributeref-expr
      (p "x.a")
      (p "a.b.c.d.e.f")
      (p "a.b = 3")
      (p "a.b = x.y")
      (p "a.b = x, x.y")
      ;; augassign-stmt
      (p "x += 3, 4")
      (p "x, y += foo")
      (p "x[0, 1] += 3, (4, 5)")
      ;; backticks-expr
      (p "`p`")
      (p "`42`")
      (p "`f.g[x]`")
      (p "`'water'`")
      ;; binary-expr
      (p "a + 3")
      (p "a.g * 23")
      (p "24 << 12")
      (p "3 + 4 * 5")
      (p "(3 + 4) * 5")
      (p "1 * (2 + 3) << 0")
      (p "1 + 2 - 3 * 4 // 5 / 6 % 7 << 8 >> 9 & 10 | 11 ^ 12 ** 13")
      (p "x + 3 in foo")
      (p "y not in foo")
      (p "x is y")
      (p "x is not x")
      (p "not x is not x")
      (p "not x is (not x)")
      (p "(not x) is (not x)")
      (p "x in (not y)") ;; brackets not necessary
      ;; binary-lazy-expr
      (p "a or b")
      (p "a and b or c")
      (p "(a or b) and c")
      ;; break-stmt
      (p "break")
      ;; call-expr
      (p "f(a, bb, c)")
      (p "f(x=3, y=4)")
      (p "f(*x)")
      (p "f(**x)")
      (p "f(*x, **y)")
      (p "f((1, 2), x=(3, (4, 5)))")
      ;; classdef-stmt (more indenting and layout tests -> suite-stmt)
      (p "class C:
    pass")
      (p "class C(D, E):
    pass")
      (p "class C:
    def m():
        pass")
      ;; comparison-expr
      (p "x < y")
      (p "x <= y <= z")
      (p "a < b > c == d != e <= f >= g != h")
      (test "x != y" (clpython::py-string.strip (py-pprint (parse-python-string "x <> y"))) :test 'string=)
      ;; continue-stmt
      (p "continue")
      ;; del-stmt
      (p "del x")
      (p "del x, y")
      (p "del x[0]")
      (p "del x.a")
      ;; dict-expr
      (p "{}")
      (p "{1: 2}")
      (p "{a: b}")
      (p "{(1, 2): (3, 4)}")
      (p "{[1, 2, 3]: f.g[0](1, 2, 3)}")
      ;; exec-stmt
      (p "exec foo")
      (p "exec foo in glob")
      (p "exec foo in glob, loc")
      (p "exec foo in (a, b, c), (1, 2, 3)")
      ;; for-in-stmt
      (p "for x in y:
    pass")
      (p "for x, y in zut:
    pass")
      (p "for [x, (y, z)] in grub:
    pass")
      (p "for x in a, b, c:
    pass")
      ;; funcdef-stmt
      (p "def foo():
    pass")
      (p "def foo(x, y):
    pass")
      (p "def foo(x, y, z=3, *loc, **kw):
    pass")
      ;; generator-expr
      (p "(x for x in y)")
      ;; global-stmt
      (p "global x")
      (p "global x, y")
      ;; identifier-expr
      (p "x")
      (p "x, y")
      (p "FooBar")
      ;; if-stmt
      (p "if a > 3:
    pass")
      (p "if not a or b:
    pass")
      (p
       "if a > 3:
    x
elif a > 4:
    y
elif a > 5:
    z
else:
    qq")
      (p
       "if a:
    if b1:
        if c1:
            a
        elif c2:
            b
else:
    r")
      ;; import-stmt
      (p "import foo")
      (p "import foo, bar")
      (p "import foo as bar")
      (p "import foo, bar as baz, zut")
      ;; import-from-stmt
      (p "from foo import bar")
      (p "from foo import bar, baz")
      (p "from foo import *")
      ;; lambda-expr
      (p "lambda: 42")
      (p "lambda x: 42")
      (p "lambda x, y=3: x + y")
      (p "lambda *args: args[0]")
      (p "lambda x, y=lambda y: 42: 3")
      (p "lambda (x, y): x, y")
      ;; listcompr-expr
      (p "[x for y in z]")
      (p "[x + y for x, y in a, b() if x > 3]")
      (p "[x for x in a, b, c]")
      ;; list-expr
      (p "[]")
      (p "[x]")
      (p "[1, 2, 3]")
      (p "[1, 2, (3, 4), (5, (6, 7, [8]))]")
      (p "[x] = [3]")
      (p "[x, y] = []")
      ;; pass-stmt
      (p "pass")
      ;; print-stmt
      (p "print")
      (p "print x")
      (p "print x,")
      (p "print x, y")
      (p "print x, y,")
      ;; return-stmt
      (p "return")
      (p "return x")
      (p "return x, y")
      (p "return x + y")
      ;; slice-expr
      (p "x[:]")
      (p "x[1:]")
      (p "x[:1]")
      (p "x[1:3]")
      (p "x[1:2:3]")
      (p "x[::3]")
      (p "x[:3]")
      (p "x[::1]")
      ;; subscription-expr
      (p "x[0]")
      (p "x[...]")
      (p "x[a, b, (c, d)]")
      (p "x[a, ..., b]")
      (p "x[y[0]]")
      ;; suite-stmt
      ;; raise-stmt
      (p "raise")
      (p "raise x")
      (p "raise x, y")
      (p "raise x, y, z")
      ;; try-except-stmt
      (p "try:
    x
except:
    y
else:
    z
")
      ;; try-finally-stmt
      (p "try:
    x
finally:
    y
")
      ;; tuple-expr
      (p "1, 2")
      (p "x, y = 1, 2")
      ;; unary-expr
      (p "+x")
      (p "-x")
      (p "~x")
      ;; while-stmt
      (p "while x:
    y")
      ;; yield-stmt
      (p "yield")
      (p "yield x")
      (p "yield x, y")
      )))