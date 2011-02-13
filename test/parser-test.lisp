;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.TEST; Readtable: PY-AST-USER-READTABLE -*-
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
	   
(defun ps (s &optional (one-expr nil))
  (values (parse s :one-expr one-expr)))

(defmacro test-signals-warning (&body body)
  `(test-true (block .test
                (handler-bind ((warning (lambda (c)
                                          (declare (ignore c))
                                          (return-from .test t))))
                  ,@body)
                nil)))

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

    ;; literals
    (test-equal '([module-stmt] ([suite-stmt] (([literal-expr] :number 42)))) (ps "42"))
    (test-equal '([module-stmt] ([suite-stmt] (([literal-expr] :string "x")))) (ps "'x'"))
    (test-equal '([module-stmt] ([suite-stmt] (([literal-expr] :bytes "x")))) (ps "b'x'"))
      
    ;; variables
    (test-equal '([assign-stmt] ([literal-expr] :number 3) (([identifier-expr] {y} ))) (ps "y = 3" t))
    (test-equal '([assign-stmt] ([literal-expr] :number 3) (([identifier-expr] {len}))) (ps "len = 3" t))

    ;; floating point (complex) numbers
    (test-equal '([literal-expr] :number 0.5d0) (ps "0.5" t))
    (test-equal '([literal-expr] :number 0.5d0) (ps ".5" t))
    (test-equal '([literal-expr] :number #C(0.0 0.5d0)) (ps "0.5j" t))
    (test-equal '([literal-expr] :number #C(0.0 0.5d0)) (ps ".5j" t))

    (assert (not (eq clpython.parser:*normal-float-representation-type*
                     clpython.parser:*enormous-float-representation-type*))
        () "Invalid float representation types: outside a FLOAT range an INTEGER should take over.")
    (destructuring-bind (min-f max-f)
        (clpython.parser::number-range clpython.parser:*normal-float-representation-type*)
      (assert (< min-f 0))
      (assert (< (expt 10 3) max-f) () "Really small float range in this Lisp implementation?!")
      (test-equal '([literal-expr] :number 1D3) (ps "1e3" t)) ;; 1e3 is small enough to be a regular ..-FLOAT
      (let* ((n-expt (1+ (floor (log max-f 10))))
             (s (format nil "1E~A" n-expt)))
        (test-error (ps s t) :condition-type '{SyntaxError}) ;; "too large to represent as ..-FLOAT"
        (test-equal (handler-bind (({SyntaxError} (lambda (c)
                                                    (declare (ignore c))
                                                    (test-true (find-restart 'continue))
                                                    (invoke-restart (find-restart 'continue)))))
                      (ps s t))
                    `([literal-expr] :number ,(expt 10 n-expt)))))
    
    ;; suffix operations
    (test-equal '([attributeref-expr]
		  ([call-expr]
		   ([subscription-expr] ([identifier-expr] {x}) ([literal-expr] :number 1))
		   (([literal-expr] :number 2)) nil nil nil)
		  ([identifier-expr] {a3}))
		(ps "x[1](2).a3" t))
      
    ;; call arguments
    (test-equal '([attributeref-expr]
		  ([call-expr] ([subscription-expr] ([identifier-expr] {x})
				([literal-expr] :number 1))
		   nil ((([identifier-expr] {len}) ([literal-expr] :number 2))) nil nil)
		  ([identifier-expr] {a3}))
		(ps "x[1](len=2).a3" t))
      
    (test-equal '([call-expr] 
		  ([identifier-expr] {f})
		  (([literal-expr] :number 1) ([literal-expr] :number 2))
                  ((([identifier-expr] {y}) ([literal-expr] :number 3)))
                  ([identifier-expr] {args})
                  ([identifier-expr] {kw}))
		(ps "f(1, 2, y=3, *args, **kw)" t))

    ;; order of args: pos, key, *, **
    (test-error (ps "f(a=1,b)" t)
		:condition-type '{SyntaxError})
    
    (test-error (ps "f(*a,1)" t)
		:condition-type '{SyntaxError})
    
    (test-error (ps "f(**a,*b)" t)
		:condition-type '{SyntaxError})
    
    (test-no-error (ps "f(x,y,z=3,*a,**b)" t))
    
    ;; function decorators
    (test-equal '([funcdef-stmt]
                  ;; list of decorators: first foo(bar)
		  (([call-expr] ([identifier-expr] {foo}) (([identifier-expr] {bar})) nil nil nil)
                   ;; second deco: zut
		   ([identifier-expr] {zut}))
		  ([identifier-expr] {f})
		  (nil nil nil nil)
		  ([suite-stmt] (([pass-stmt]))))
		(ps "
@foo(bar)
@zut
def f(): pass" t))

    ;; Precedence of unary operators and exponentiation
    (test-equal '([binary-expr] [*] 
                  ([unary-expr] [-] ([literal-expr] :number 1))
                  ([literal-expr] :number 2))
                (ps "-1 * 2" t)
                :fail-info "-1 * 2 == (-1) * 2")
    (test-equal '([binary-expr] [*] 
                  ([unary-expr] [+] ([literal-expr] :number 1))
                  ([literal-expr] :number 2))
                (ps "+1 * 2" t)
                :fail-info "+1 * 2 == (+1) * 2")
    (test-equal '([unary-expr] [-] ([binary-expr] [**]
                                    ([literal-expr] :number 1)
                                    ([literal-expr] :number 2)))
                (ps "-1 ** 2" t)
                :fail-info "-1 ** 2 == - (1 ** 2)")
    (test-equal '([unary-expr] [+] ([binary-expr] [**]
                                    ([literal-expr] :number 1)
                                    ([literal-expr] :number 2)))
                (ps "+1 ** 2" t)
                :fail-info "+1 ** 2 == + (1 ** 2)")
    (test-equal '([binary-expr] [-]
                  ([literal-expr] :number 1)
                  ([binary-expr] [*]
                   ([literal-expr] :number 2)
                   ([literal-expr] :number 3)))
                (ps "1 - 2 * 3" t)
                :fail-info "1 - 2 * 3 == 1 - (2 * 3)")
    (test-equal '([binary-expr] [/]
                  ([unary-expr] [~] ([literal-expr] :number 1))
                  ([literal-expr] :number 2))
                (ps "~1 / 2" t)
                :fail-info "~1 / 2 == (~1) / 2")
    (test-equal '([unary-expr] [~]
                  ([binary-expr] [**]
                   ([literal-expr] :number 1)
                   ([literal-expr] :number 2)))
                (ps "~1 ** 2" t)
                :fail-info "~1 ** 2 == ~ (1 ** 2)")
    (test-equal '([binary-expr] [*]
                  ([unary-expr] [-] ([literal-expr] :number 1))
                  ([binary-expr] [*]
                   ([unary-expr] [+] ([literal-expr] :number 2))
                   ([binary-expr] [*]
                    ([unary-expr] [-] ([literal-expr] :number 3))
                    ([unary-expr] [+] ([literal-expr] :number 4)))))
                (ps "-1 * +2 * -3 * +4" t))
    (test-equal '([binary-expr] [*]
                  ([literal-expr] :number 1)
                  ([unary-expr] [-]
                   ([bracketed-expr]
                    ([binary-expr] [*]
                     ([bracketed-expr]
                      ([binary-expr] [*] 
                                     ([unary-expr] [-] ([literal-expr] :number 2))
                                     ([literal-expr] :number 3)))
                     ([literal-expr] :number 4)))))
                (ps "1 * -((-2 * 3) * 4)" t))
        
    ;; Empty string is parsed as module without body
    #+(or)(test-equal '([module-stmt] ([suite-stmt] () ))
                      (values (parse "")))

    ;; yield expressions are implemented
    (test-no-error (values (parse "y = yield x")))
    
    ;; parsing a file
    #+(and allegro unix) ;; no WITH-OPEN-TEMP-FILE on windows
    (let ((fname (excl.osi:with-open-temp-file (s "_clpython-ast-test-XXXXXX")
		   (format s "print 42"))))
      (test-equal '([print-stmt] nil (([literal-expr] :number 42)) nil)
		  (values (clpython.parser:parse (pathname fname) :one-expr t)))
      (test t (excl.osi:unlink fname)))
    
    ;; handling eof
    (test-true (subtypep '{UnexpectedEofError} '{SyntaxError}))
    (flet ((try-parse (s)
	     (handler-case (values (parse s))
	       ({UnexpectedEofError} () :unexp-eof-error)
	       ({SyntaxError}        () :syntax-error)
	       (condition            () :condition))))
      (test :unexp-eof-error (try-parse "'") :fail-info "(EOF in Lexer)")
      (test :unexp-eof-error (try-parse "def")
	    :fail-info "Unexpected EOF in grammar")
      (test :syntax-error (try-parse "def f(x):
  if x > 1
    return 3
  else:
    return 1")) ;; missing colon after "x > 1"
      (test :unexp-eof-error (try-parse "
def f():
  def __init__(self):
")) ;; lexer returns dedent before newline

      (test :unexp-eof-error (try-parse "def f():") :fail-info "(EOF in grammar)")
      (test :syntax-error    (try-parse "def def") :fail-info "(Incorrect grammar)")
      (test :syntax-error    (try-parse " 42") :fail-info "(Leading whitespace)"))

    ;; Don't complain about eof if the newline in front is the actual problem
    (multiple-value-bind (value error-p)
        (ignore-errors (clpython:parse (format nil "def ~A" #\Newline)))
      (declare (ignore value))
      (let ((error-string (prin1-to-string error-p)))
        (test-true (and (search "unexpected token" error-string)
                        (search "newline" error-string)))))
    
    (test-equal '([literal-expr] :number 42)
                (handler-bind (({SyntaxError} (lambda (c) (declare (ignore c))
                                                      (continue))))
                  (values (parse " 42" :one-expr t))))
    ;; strings with quotes
    (let ((s (apply #'concatenate 'string (mapcar 'string '(#\' #\" #\\ #\' #\')))))
      (test-equal (values (parse s :one-expr t)) '([literal-expr] :string "\"'")))
    (let ((s (apply #'concatenate 'string (mapcar 'string '(#\' #\\ #\\ #\')))))
      (test-equal (ignore-errors (values (parse s :one-expr t))) '([literal-expr] :string "\\")))
    (let ((s (apply #'concatenate 'string (mapcar 'string '(#\' #\\ #\\ #\' #\Space)))))
      (test-equal (ignore-errors (values (parse s :one-expr t))) '([literal-expr] :string "\\")))
    ;; trailing comma
    (test-no-error (parse "def f(a=3,): pass"))
    ;; backslash at end of whitespace line
    (test-equal (ps "
if a:
  foo
\
else:
  b" t)
                '([if-stmt] ((([identifier-expr] {a})
                              ([suite-stmt] (([identifier-expr] {foo})))))
                  ([suite-stmt] (([identifier-expr] {b})))))
    ;; dedent did not arrive at previous indent level
    (test-error (ps "
if 1:
  if 2:
    pass
 else:
  pass" t) :condition-type '{SyntaxError})
    ;; unexpected character
    (test-error (ps "
a
$" t) :condition-type '{SyntaxError})
    ;; empty file = None
    (test-equal (ps "" t) '([identifier-expr] {None}))
    ;; comments
    (test-equal (ps "
None
##" t) '([identifier-expr] {None}))
    (test-equal (ps "
##
a" t) '([identifier-expr] {a}))
    ;; end-of-line continuation
    (test-equal (ps "
if 1 > \\
 2:
  pass
" t) '([if-stmt] ((([comparison-expr] [>] ([literal-expr] :number 1) ([literal-expr] :number 2))
                     ([suite-stmt] (([pass-stmt])))))
         nil))
    (test-error (ps "\\1" t) :condition-type '{SyntaxError})
    ;; unicode
    
    #-ecl
    (test-equal (ps (concatenate 'string "u'\\N{" #1="Latin Small Letter Y With Acute" "}'") t)
                `([literal-expr] :string
                                 ,(coerce (list (or (clpython.parser::lisp-char-by-python-name #1#)
                                                    (error "Unicode char ~A not available in this Lisp?" #1#)))
                                          'string)))
    #-(or ecl lispworks) ;; Lispworks has no names for chars > 255
    (test-equal (ps "u'\\N{latin capital letter l with stroke}'" t)
                `([literal-expr] :string
                                 ,(coerce (list (name-char "latin_capital_letter_l_with_stroke")) 'string)))
    #-ecl
    (test-equal (ps "u'\\u0141 \\U00000141'" t)
                `([literal-expr] :string
                                 ,(coerce (list (code-char #x0141)
                                                #\Space
                                                (code-char #x0141))
                                          clpython.parser::+unicode-capable-string-type+)))
    (test-error (ps "u'\\N'" t)  :condition-type '{SyntaxError})
    (test-error (ps "u'\\N{foo'" t)  :condition-type '{SyntaxError})
    (test-error (ps "u'\\N{foo}'" t)  :condition-type '{SyntaxError})
    (test-equal (ps "u'\\r'" t) `([literal-expr] :string ,(coerce (list #\Return) 'string)))
    ;; warning then using unicode escape
    (test-signals-warning (ps "'\\N{foo}'"))
    (test-signals-warning (ps "'\\u0123'"))
    (test-signals-warning (ps "'\\U0123'"))
    (test-error (ps "u'\\xG'" t) :condition-type '{SyntaxError})
    (test-error (ps "u'\\uG000'" t) :condition-type '{SyntaxError})
    (test-error (ps "u'\\u'" t) :condition-type '{SyntaxError})
    (test-error (ps "u'\\U0000000G'" t) :condition-type '{SyntaxError})
    (test-error (ps "u'\\UFFFFFFFF'" t) :condition-type '{SyntaxError} :fail-info "No such char")
        
    ;; valid, invalid hex code
    (test-equal (ps "'\\x12'" t) `([literal-expr] :string ,(coerce (list (code-char #x12)) 'string)))
    (test-equal (ps "'\\xF'" t) `([literal-expr] :string ,(coerce (list (code-char #xF)) 'string)))
    (test-error (ps "'\\xG'" t) :condition-type '{SyntaxError})
    
    ;; octal code, non-escaping backslash
    #-ecl
    (test-equal (ps "'\\5019\\z'" t)
                `([literal-expr] :string ,(coerce (list (code-char #x0141) #\9 #\\ #\z)
                                                  clpython.parser::+unicode-capable-string-type+)))
    ;; ..
    (test-error (ps "[1, .., 3]") :condition-type '{SyntaxError})
    ;; !
    (test-error (ps "[1, !1, 3]") :condition-type '{SyntaxError})
    ;; irregular indentation
    (test-signals-warning (ps (format nil "
if 1:
~Afoo
~A~Abar"
                                      (coerce (loop repeat (1+ clpython.parser:*tab-width-spaces*)
                                                  collect #\Space)
                                              'string)
                                      #\Tab #\Space)))
    (test-error (parse "if 1:
  1
	2" :tab-width 21) :condition-type '{SyntaxError})
    (test-equal (parse "if 1:
  1
	2" :tab-width 2)
                (parse "if 1:
  1
  2"))))

(defun run-code-walker-test ()
  (with-subtest (:name "CLPython-Codewalker")
    ))

(defun string-strip-= (x y)
  (string= (clpython:py-string.strip x)
	   (clpython:py-string.strip y)))
;;  #+allegro-cl-express
;;  (warn "Skipped running RUN-PRETTY-PRINTER-TEST in Allegro Express edition, to prevent out-of-memory,")
;;  #-allegro-cl-express

(defun run-pretty-printer-test ()
  #.(let ((test-form
           #1='(with-subtest (:name "CLPython-PrettyPrinter")
                ;; Test  string -> ast -> string  and ast -> string -> ast
                (macrolet ((p (str &rest options)
                             `(progn 
                                (test ,str (py-pprint (parse ,str))
                                      :test 'string-strip-= ,@options)
                                (when (string-strip-= ,str (py-pprint (parse ,str)))
                                  (test-equal (values (parse ,str))
                                              (values (parse (py-pprint (parse ,str))))
                                              ,@options))))
                           (pe (str &rest options)
                             `(progn 
                                (test ,str (py-pprint (parse ,str :one-expr t))
                                      :test 'string-strip-= ,@options)
                                (when (string-strip-= ,str (py-pprint (parse ,str :one-expr t)))
                                  (test-equal (values (parse ,str))
                                              (values (parse (py-pprint (parse ,str :one-expr t))))
                                              ,@options)))))
                  #+(or)(parse "")
                  ;; number
                  (pe "42")
                  (pe "1.")
                  (pe "1.2")
                  (pe "3j" :known-failure t :fail-info "spaces between number and j")
                  ;; string
                  (pe "'x'")
                  (p "'\"'")
                  ;; bytes
                  (p "b'\\x41\\x42\\x03'")
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
                  (p "-1 * -2 * -(3 * 4)")
                  (p "1 < 2 < (3 < 4) < 5 < (6 < (7 < 8))")
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
                  (test "x != y" (clpython:py-string.strip (py-pprint (parse "x <> y"))) :test 'string=)
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
                  ;; if-expr
                  (p "(3 if 1 > 0 else 2)")
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
                  (p "(yield)")
                  (p "(yield x)")
                  (p "(yield x, y)")
                  (p "()")
                  ;; some strings
                  (p "'asdf'")
                  (p "'asdf\"'")
                  (p "\"'\"")
                  (p "abc
defg")
                  ))))
      (declare (ignore test-form))
      #+allegro-cl-express ;; Work around out-of-memory due to heap limitation
      `(eval '#1#)
      #-allegro-cl-express
      #1#))

#+(or) ;; TODO: update Lispy test
(defun run-lispy-test ()
  (let ((tests '(("()" (clpython.parser::handle-lisp nil))
                 ("(,)" :error)
                 ("(f)" (clpython.parser::handle-lisp (f)))
                 ("(1,2)" (clpython.parser::handle-python
                           ([module-stmt] ([suite-stmt]
                                           (([tuple-expr] (([literal-expr] :number 1)
                                                           ([literal-expr] :number 2)))))))))))
    (loop for (test expected) in tests
        do (multiple-value-bind (val error)
               (ignore-errors (car (let ((*package* #.*package*))
                                     (clpython.parser::read-lispy-toplevel-forms test))))
             (cond (val (test-equal val expected))
                   (error (test-equal :error expected))
                   (t (break "unexpected: ~A ~A" val error)))))))
