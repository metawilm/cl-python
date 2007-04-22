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
    (test '(clpython.ast.node:funcdef-stmt :external) 
	  (find-symbol (symbol-name :funcdef-stmt) :clpython.ast) :multiple-values t)
    
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
	    :known-failure t
	    :fail-info "Unexpected EOF in grammar not detected as such, because lexer adds newline.")
      (test :unexp-eof-error (try-parse "def f():") :fail-info "(EOF in grammar)")
      (test :syntax-error    (try-parse "def def") :fail-info "(Incorrect grammar)"))
    ))

(defun run-code-walker-test ()
  (with-subtest (:name "CLPython-Codewalker")
    ))

(defun run-pretty-printer-test ()
  (with-subtest (:name "CLPython-PrettyPrinter")
    ))
