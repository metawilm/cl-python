(defpackage :clpython.test
  (:use :common-lisp :clpython :util.test)
  (:export #:run))

(in-package :clpython.test)
(in-syntax *ast-user-readtable*)
	   
(defun ps (s &optional (incl-module t))
  (if incl-module
      (parse-python-string s :incl-module t)
    (car (parse-python-string s :incl-module nil))))

(defun bool (x) 
  (if x t nil))

(defun run (&optional test-compile-load)
  (with-tests (:name :clpython.ast)
    ;; package
    (test t (and (find-package :clpython.parser) t))
    (test '(clpython.ast.reserved:|def| :inherited)
	  (find-symbol "def" :clpython.parser) :multiple-values t)
    (test '(clpython.ast.operator:|<=|  :inherited)
	  (find-symbol "<="  :clpython.parser) :multiple-values t)
    (test '(clpython.ast.node:funcdef-stmt :inherited) 
	  (find-symbol (symbol-name :funcdef-stmt) :clpython.parser) :multiple-values t)
    
    ;; ast readtable
    (test t (bool (eq '[>] 'clpython.ast.operator:>)))
    (test t (bool (eq '[assign-stmt] 'clpython.ast.node:assign-stmt)))
    (test-error (read-from-string "'[foo]") :condition-type 'reader-error) ;; not exist
    (test nil (find-symbol "foo" :clpython.ast)) ;; not created by previous test
    (test t (bool (eq '[>] '[>] )))
    
    ;; user readtable
    (test t (bool (eq '{__getitem__} 'clpython.user:|__getitem__|)))
    (test t (bool (eq '{abs} 'clpython.user:|abs|)))
    (test t (bool (eq '{foo} 'clpython.user::|foo|))) ;; auto intern
    (test t (bool (find-symbol "foo" :clpython.ast))) ;; created by previous test
    (test t (bool (eq '{abs} '{abs} )))
    
    ;; grammar.lisp
    (when test-compile-load
      (test-no-warning (compile-file "../ast/grammar.lisp"))
      (test-no-warning (load "../ast/grammar.lisp")))
    
    ;; lexer.lisp
    ;; parser.lisp
    (when test-compile-load
      (test-no-warning (compile-file "../ast/parser.lisp"))
      (test-no-warning (load "../ast/parser.lisp")))
    
    ;; atoms
    (test '([module-stmt] ([suite-stmt] (42))) (ps "42") :test 'equal)
    (test '([module-stmt] ([suite-stmt] ("x"))) (ps "'x'") :test 'equal)
      
    ;; symbols
    (test '([assign-stmt] 3 (([identifier-expr] {|y|} )))
	  (ps "y = 3" nil) :test 'equal)
    (test '([assign-stmt] 3 (([identifier-expr] {len}))) (ps "len = 3" nil) :test 'equal)
      
    ;; suffix operations
    (test '([attributeref-expr]
	    ([call-expr]
	     ([subscription-expr] ([identifier-expr] {x}) 1)
	     ((2) nil nil nil))
	    ([identifier-expr] {a3}))
	  (ps "x[1](2).a3" nil) :test 'equal)
      
    ;; call arguments
    (test '([attributeref-expr]
	    ([call-expr] ([subscription-expr] ([identifier-expr] {x})
			1)
	     (nil ((([identifier-expr] {len}) 2)) nil nil))
	    ([identifier-expr] {a3}))
	  (ps "x[1](len=2).a3" nil) :test 'equal)
      
    (test '(call-expr 
	    ([identifier-expr] {f})
	    ((1 2) ((([identifier-expr] {y}) 3))
	     ([identifier-expr] {args})
	     ([identifier-expr] {kw})))
	  (ps "f(1,2,y=3,*args,**kw)" nil) :test 'equal)

    ;; function decorators
    (test '([funcdef-stmt]
	    ;; list of decorators: first foo(bar)
	    (([call-expr] {foo}) ((([identifier-expr] {bar})) nil nil nil))
	     ;; second deco: zut
	     ({zut}))
	      
	    ([identifier-expr] {f})
	    (nil nil nil nil)
	    ([suite-stmt] (([pass-stmt]))))
	  (ps "
@foo(bar)
@zut
def f(): pass" nil) :test 'equal)
    
    ;; Empty string is parsed as module without body.
    (test nil (ps "" nil))

    ;; 'y = yield x' is todo, currently raises SyntaxError.
    (test-error (parse-python-string "y = yield x")
		:condition-type 'clpython::|SyntaxError|)
    
    ;; parsing a file
    #+(and allegro unix) ;; no WITH-OPEN-TEMP-FILE on windows
    (let ((fname (excl.osi:with-open-temp-file (s "clpython-ast-test")
		   (format s "print 42"))))
      (test '((clpython.ast.node:print-stmt nil (42) nil))
	    (parse-python-file fname :incl-module nil) :test 'equal)
      (test t (excl.osi:unlink fname)))
    
    ;; handling eof
    (flet ((test-eof (want-eof-signal)
	     (block :body
	       (let ((*signal-unexpected-eof* want-eof-signal))
		 (handler-case
		     (parse-python-string "if 3:")
		   (unexpected-eof () (return-from :body :eof-signal))
		   (|SyntaxError|  () (return-from :body :syntax-error)))))))
      (test :eof-signal   (test-eof t))
      (test :syntax-error (test-eof nil)))
    
    ;; walk.lisp
    ;; astpp.lisp
    ;; lispy.lisp
    ))