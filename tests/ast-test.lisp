(asdf:operate 'asdf:load-op :clpython)

(defpackage :clpython.test
  (:use :common-lisp :clpython :util.test)
  (:export #:run))

(in-package :clpython.test)

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
    
    ;; grammar.lisp
    (when test-compile-load
      (test-no-warning (compile-file "../ast/grammar.lisp"))
      (test-no-warning (load "../ast/grammar.lisp")))
    
    ;; lexer.lisp
    ;; parser.lisp
    (when test-compile-load
      (test-no-warning (compile-file "../ast/parser.lisp"))
      (test-no-warning (load "../ast/parser.lisp")))
    
    (flet ((ps (s &optional (incl-module t)) 
	     (if incl-module
		 (parse-python-string s :incl-module t)
	       (car (parse-python-string s :incl-module nil)))))
      
      ;; atoms
      (test '(module-stmt (suite-stmt (42))) (ps "42") :test 'equal)
      (test '(module-stmt (suite-stmt ("x"))) (ps "'x'") :test 'equal)
      
      ;; symbols
      (test '(assign-stmt 3 ((identifier-expr clpython.ast.user::|y|)))
	    (ps "y = 3" nil) :test 'equal)
      (test '(assign-stmt 3 ((identifier-expr clpython.builtin:|len|))) (ps "len = 3" nil) :test 'equal)
      
      ;; suffix operations
      (test '(attributeref-expr
	      (call-expr
	       (subscription-expr (identifier-expr clpython.ast.user::|x|) 1)
	       ((2) nil nil nil))
	      (identifier-expr clpython.ast.user::|a3|))
	    (ps "x[1](2).a3" nil) :test 'equal)
      
      ;; call arguments
      (test '(attributeref-expr
	      (call-expr (subscription-expr (identifier-expr clpython.ast.user::|x|)
			  1)
	       (nil (((identifier-expr clpython.builtin:|len|) 2)) nil nil))
	      (identifier-expr clpython.ast.user::|a3|))
	    (ps "x[1](len=2).a3" nil) :test 'equal)
      
      (test '(call-expr 
	      (identifier-expr clpython.ast.user::|f|)
	      ((1 2) (((identifier-expr clpython.ast.user::|y|) 3))
	       (identifier-expr clpython.ast.user::|args|)
	       (identifier-expr clpython.ast.user::|kw|)))
	    (ps "f(1,2,y=3,*args,**kw)" nil) :test 'equal)

      ;; function decorators
      (test '(funcdef-stmt
	      ;; list of decorators: first foo(bar)
	      ((call-expr (clpython.ast.user::|foo|) (((identifier-expr clpython.ast.user::|bar|))
						      nil nil nil))
	       ;; second deco: zut
	       (clpython.ast.user::|zut|))
	      
	      (identifier-expr clpython.ast.user::|f|)
	      (nil nil nil nil)
	      (suite-stmt ((pass-stmt))))
	    (ps "
@foo(bar)
@zut
def f(): pass" nil) :test 'equal)
      
      ) ;; flet
    
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
    
    ;; walk.lisp
    
    ;; astpp.lisp
    ;; lispy.lisp
    ))