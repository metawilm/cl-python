;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;; Python grammar

(in-package :clpython.parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :yacc)
  (use-package :yacc))

(defgrammar python-grammar (grammar)
  ()
  (:left-associative |or| )
  (:left-associative |and| )
  (:left-associative |not| )
  (:left-associative |in| ) ;; and "not in"
  (:left-associative |is| ) ;; and "is not"
  (:left-associative < <= > >= <> != == )
  (:left-associative |\|| )
  (:left-associative ^ )
  (:left-associative &)
  (:left-associative << >> )
  (:left-associative + -)
  (:left-associative * / % //)
  (:left-associative unary-plusmin )
  (:left-associative ~)
  (:right-associative **)
  (:non-associative high-prec)
  (:lexemes identifier number string 
	    newline indent dedent
	    ;; punctuation:
	    = [ ] |(| |)| < > { } |.| |,| |:| |\|| ^ % + - * / ~ & |`|
            &= // << >>  <> != += -= *= /= //= %= ** <= >= ^= |\|=| ==
	    **= <<= >>= |...|
	    |;|   ;; multiple statements same line
	    @     ;; decorator
	    ;; reserved words:
	    |def| |class| |lambda| |return| |yield|
	    |and| |or| |not| |for| |in| |is|
	    |print| |from| |import| |as| |assert| |break| |continue|
	    |global| |del| |exec| |pass|
	    |if| |elif| |else| |while| |try| |except| |finally| |raise|
	    |lispy-lisp-form|
	    ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-python-rules (name)
    (check-type name symbol)
    (let* ((name (intern name #.*package*))
	   (str  (symbol-name name))
	   (len  (length str))
	   (item (intern (subseq str 0 (- len 1)) #.*package*))
	   (dp   'defproduction))
      (ecase (aref str (1- len))

	(#\+ `((,dp (,name python-grammar) (,item) ($1))
	       (,dp (,name python-grammar) (,name |,| ,item) ((append $1 (list $3))))))

	(#\* `((,dp (,name python-grammar) () ())
	       (,dp (,name python-grammar) (,name ,item) ((append $1 (list $2))))))

	(#\? `((,dp (,name python-grammar) ())
	       (,dp (,name python-grammar) (,item) ($1))))))))


(defmacro python-prods (&rest prodrules)
  (let ((res ()))
    (loop for (name . rest) in prodrules
	do (cond ((keywordp name) (assert (null rest))
				  (nconc res (generate-python-rules name)))
		 ((eq (car rest) ':or)
		  (dolist (x (cdr rest))
		    (push `(defproduction (,name python-grammar)
			       ,@(if (symbolp x)
				     ` ((,x) ($1))
				   `(,(car x) ,(cdr x))))
			  res)))
		 (t (push `(defproduction (,name python-grammar) ,@rest)
			  res))))
    `(progn ,@(nreverse res))))


;; These rules, including most names, are taken from the CPython
;; grammar file from CPython CVS, file Python/Grammar/Grammar,
;; 20040827.

(python-prods

 (python-grammar (file-input) (`(module-stmt (suite-stmt ,(nreverse $1)))))

 (file-input () ())
 (file-input (file-input newline) ($1))
 (file-input (file-input stmt) ((cons $2 $1)))

 (:decorator*)
 (decorator (|@| dotted-name                 newline) ($2))
 (decorator (|@| dotted-name |(| arglist |)| newline) ((list 'call-expr $2 $4)))

 (funcdef (decorator* |def| identifier |(| parameters |)| |:| suite)
	  (`(funcdef-stmt ,$1 (identifier-expr ,$3) ,$5 ,$8)))

 (parameters ()               ((list nil nil nil nil)))
 (parameters (parameter-list) ($1))
	     
 (parameter-list (parameter-list5)
		 ((destructuring-bind (poskey *-a **-a) $1
		    `(,@(loop with seen-kwarg and pos and kw
			    for p in poskey
			    do (cond ((and (listp p) (eq (car p) :key))
				      (push (cdr p) kw)
				      (setf seen-kwarg (cdr p)))
				     (seen-kwarg
				      (raise-syntax-error
				       "Positional arguments should precede keyword arguments ~
                                        (found pos arg `~A' after kw arg `~A')" seen-kwarg p))
				     (t (push p pos)))
			    finally (return `(,(nreverse pos)
					      ,(nreverse kw)
					      ,(when *-a `(identifier-expr ,*-a))
					      ,(when **-a `(identifier-expr ,**-a)))))))))
 
 (parameter-list5 (defparameter+                       ) ((list  $1 nil nil)))
 (parameter-list5 (defparameter+ ni-*-ident ni-**-ident) ((list  $1  $2  $3)))
 (parameter-list5 (defparameter+ ni-*-ident            ) ((list  $1  $2 nil)))
 (parameter-list5 (defparameter+            ni-**-ident) ((list  $1 nil  $2)))
 (parameter-list5 (              *-ident    ni-**-ident) ((list nil  $1  $2)))
 (parameter-list5 (              *-ident               ) ((list nil  $1 nil)))
 (parameter-list5 (                         **-ident   ) ((list nil nil  $1)))

 (defparameter+ (defparameter) ((list $1)))
 (defparameter+ (defparameter+ |,| defparameter) ((append $1 (list $3))))

 (ni-*-ident  ( |,| *-ident    ) ($2))
 (*-ident     ( |*| identifier ) ($2))

 (ni-**-ident ( |,| **-ident    ) ($2))
 (**-ident    ( |**| identifier ) ($2))

 (defparameter (fpdef         ) ($1))
 (defparameter (fpdef |=| test) (`(:key ,$1 ,$3)))
 ;; Can't use symbol vs. cons for distinguishing positional and
 ;; keyword arguments, as as positional args may be a structure:
 ;;   def f((x,y), z, q=4): ...
 
 (fpdef :or
	(( identifier     ) . (`(identifier-expr ,$1)))
	(( |(| fplist |)| ) . (`(tuple-expr ,$2))))

 (fplist (fpdef comma--fpdef* comma?) ((cons $1 $2)))
 (:comma--fpdef*)
 (comma--fpdef (|,| fpdef) ($2))

 (:comma?)
 (comma (|,|) ((list $1)))

 (stmt :or simple-stmt compound-stmt)
 (simple-stmt (small-stmt semi--small-stmt* semi? newline)
	      ((if $2 (list 'suite-stmt (cons $1 $2)) $1)))

 (semi--small-stmt (|;| small-stmt) ($2))
 (:semi--small-stmt*)
 (:semi?)
 (semi (|;|))

 (small-stmt :or expr-stmt print-stmt del-stmt pass-stmt flow-stmt
	     import-stmt global-stmt exec-stmt assert-stmt)

 (expr-stmt (testlist expr-stmt2)
	    ((cond ((null $2) ;; not an assignment expression
		   $1)
		  
		  ((and $2 (eq (car $2) '=))
		   (let ((items (nreverse `(,$1 ,@(second $2)))))
		     `(assign-stmt ,(car items) ,(cdr items))))
		  
		  ($2
		   (list 'augassign-stmt (car $2) $1 (cdr $2)))
		  
		  (t
		   $1))))

 (expr-stmt2 (augassign testlist) ((cons $1 $2)))
 (expr-stmt2 (=--testlist*) ((when $1 (list '= $1))))
 (:=--testlist*)
 (=--testlist (|=| testlist) ($2))

 (augassign :or |+=| |-=| |*=| |/=| |%=| |&=| |\|=| |^=| |<<=| |>>=| |**=| |//=| )

 (print-stmt :or
	     ((|print|)                             . (`(print-stmt nil nil nil)))
	     ((|print| test |,--test*| comma?)      . (`(print-stmt nil (,$2 . ,$3) ,(and $4 t))))
	     ((|print| |>>| test |,--test*| comma?) . (`(print-stmt ,$3 ,$4 ,(and $5 t)))))
 (:|,--test*|)
 (|,--test| (|,| test) ($2))

 (del-stmt  (|del| exprlist) ((list 'del-stmt $2)))
 (pass-stmt (|pass|)         ((list 'pass-stmt)))
 (flow-stmt :or break-stmt continue-stmt return-stmt raise-stmt yield-stmt)

 (break-stmt    (|break|)            ((list 'break-stmt)))
 (continue-stmt (|continue|)         ((list 'continue-stmt)))
 (return-stmt   (|return| testlist?) ((list 'return-stmt $2)))
 (yield-stmt    (|yield|  testlist?) ((list 'yield-stmt $2)))
 (:testlist?)

 (raise-stmt (|raise|                       ) ((list 'raise-stmt nil nil nil)))
 (raise-stmt (|raise| test                  ) ((list 'raise-stmt  $2 nil nil)))
 (raise-stmt (|raise| test |,| test         ) ((list 'raise-stmt  $2  $4 nil)))
 (raise-stmt (|raise| test |,| test |,| test) ((list 'raise-stmt  $2  $4  $6)))
 
 ;; import a, b        (import-stmt (((a)   nil) ((b) nil)))
 ;; import a, b as b2  (import-stmt (((a)   nil) ((b) b2)))
 ;; import a.b         (import-stmt (((a b) nil)))
 ;; import a.b as c    (import-stmt (((a b) c)))
 ;; from a   import b, c        (import-from-stmt (a)   ((b nil) (c nil)))
 ;; from a.b import b, c as c2  (import-from-stmt (a b) ((b nil) (c c2)))
 ;; from a   import *           (import-from-stmt (a)   *)
 ;; from a.b import *           (import-from-stmt (a b) *)
 (import-stmt :or import-normal import-from)
 
 (import-normal (|import| dotted-as-name comma--dotted-as-name*) (`(import-stmt (,$2 ,@$3))))
 (:comma--dotted-as-name*)
 (comma--dotted-as-name ( |,| dotted-as-name) ($2))
 
 (import-from (|from| dotted-name |import| import-from-2) (`(import-from-stmt ,$2 ,$4)))
 (import-from-2 :or |*| 
		    ((import-as-name comma--import-as-name*) . ((cons $1 $2))))
 (:comma--import-as-name*)
 (comma--import-as-name (|,| import-as-name) ($2))
 
 (import-as-name (identifier)                  (`(,$1 nil)))
 (import-as-name (identifier |as| identifier)  (`(,$1 ,$3)))
 (dotted-as-name (dotted-name)                 (`(,$1 nil)))
 (dotted-as-name (dotted-name |as| identifier) (`(,$1 ,$3)))
 
 (dotted-name (identifier dot--name*) (`(,$1 ,@$2)))
 (:dot--name*)
 (dot--name (|.| identifier) ($2))

 (global-stmt (|global| identifier comma--identifier*)
	      (`(global-stmt ,(if $3 (cons $2 $3) (list $2)))))
 
 (:comma--identifier*)
 (comma--identifier (|,| identifier) (`(identifier-expr ,$2)))
 
 (exec-stmt (|exec| expr                   ) ((list 'exec-stmt $2 nil nil)))
 (exec-stmt (|exec| expr |in| test         ) ((list 'exec-stmt $2  $4 nil)))
 (exec-stmt (|exec| expr |in| test |,| test) ((list 'exec-stmt $2  $4  $6)))
 
 (assert-stmt (|assert| test comma--test?)   ((list 'assert-stmt $2 $3)))
 (:comma--test?)
 (comma--test (|,| test) ($2))

 (compound-stmt :or if-stmt while-stmt for-stmt try-stmt funcdef classdef)
 (if-stmt (|if| test |:| suite elif--test--suite* else--suite?) (`(if-stmt ((,$2 ,$4) ,@$5) ,$6)))
 (:elif--test--suite*)
 (elif--test--suite (|elif| test |:| suite) ((list $2 $4)))
 (else--suite (|else| |:| suite) ($3))
 (while-stmt (|while| test |:| suite else--suite?) ((list 'while-stmt $2 $4 $5)))
 (:else--suite?)
 (for-stmt (|for| exprlist |in| testlist |:| suite else--suite?)
	   ((list 'for-in-stmt $2 $4 $6 $7))
	   (:precedence high-prec))
 (try-stmt :or
	   ((|try| |:| suite except--suite+ else--suite?) . (`(try-except-stmt ,$3 ,$4 ,$5)))
	   ((|try| |:| suite |finally| |:| suite)  	  . (`(try-finally-stmt ,$3 ,$6)))
	   
	   ;; PEP 341 - "Unifying try-except and try-finally"
	   ((|try| |:| suite except--suite+ else--suite? |finally| |:| suite)
	    . (`(try-finally-stmt (suite-stmt ((try-except-stmt ,$3 ,$4 ,$5)) ,$8)))))

 (except--suite (|except|               |:| suite) (`(nil nil ,$3)))
 (except--suite (|except| test          |:| suite) (`(,$2 nil ,$4)))
 (except--suite (|except| test |,| test |:| suite) (`(,$2 ,$4 ,$6)))

 (except--suite+ (except--suite) ((list $1)))
 (except--suite+ (except--suite+ except--suite) ((append $1 (list $2))))

 (suite :or
	((simple-stmt)                 . ((list 'suite-stmt (list $1))))
	((newline indent stmt+ dedent) . ((list 'suite-stmt $3))))

 (stmt+ (stmt)       ((list $1)))
 (stmt+ (stmt+ stmt) ((append $1 (list $2))))

 (expr (binop2-expr) ($1))

 (test :or
       lambdef
       binop-expr)

 (binop-expr :or
	     ((binop-expr |and| binop-expr) . ((list 'binary-lazy-expr $2 $1 $3)))
	     ((binop-expr |or|  binop-expr) . ((list 'binary-lazy-expr $2 $1 $3)))
	     ((|not| binop-expr)            . ((list 'unary-expr          $1 $2)))
	     ((binop-expr |<|  binop-expr)  . ((list 'comparison-expr $2 $1 $3)))
	     ((binop-expr |<=| binop-expr)  . ((list 'comparison-expr $2 $1 $3)))
	     ((binop-expr |>|  binop-expr)  . ((list 'comparison-expr $2 $1 $3)))
	     ((binop-expr |>=| binop-expr)  . ((list 'comparison-expr $2 $1 $3)))
	     ((binop-expr |!=| binop-expr)  . ((list 'comparison-expr $2 $1 $3)))
	     ((binop-expr |<>| binop-expr)  . ((list 'comparison-exr '!= $1 $3))) ;; <> is !=
	     ((binop-expr |==| binop-expr)  . ((list 'comparison-expr $2 $1 $3)))
	     ((binop-expr |in| binop-expr)  . ((list 'binary-expr     $2 $1 $3)))
	     ((binop-expr |is| binop-expr)  . ((list 'binary-expr     $2 $1 $3))))
 (binop-expr (binop2-expr) ($1) (:precedence |or|))

 (binop2-expr :or
	      atom
	      ((atom trailer+)                . ((parse-trailers (list 'trailers $1 $2))))
	      ((binop2-expr |+|  binop2-expr) . ((list 'binary-expr $2 $1 $3)))
	      ((binop2-expr |-|  binop2-expr) . ((list 'binary-expr $2 $1 $3)))
	      ((binop2-expr |*|  binop2-expr) . ((list 'binary-expr $2 $1 $3)))
	      ((binop2-expr |/|  binop2-expr) . ((list 'binary-expr $2 $1 $3)))
	      ((binop2-expr |**| binop2-expr) . ((list 'binary-expr $2 $1 $3)))
	      ((binop2-expr |//| binop2-expr) . ((list 'binary-expr $2 $1 $3)))
	      ((binop2-expr |<<| binop2-expr) . ((list 'binary-expr $2 $1 $3)))
	      ((binop2-expr |>>| binop2-expr) . ((list 'binary-expr $2 $1 $3)))
	      ((binop2-expr |&|  binop2-expr) . ((list 'binary-expr $2 $1 $3)))
	      ((binop2-expr |^|  binop2-expr) . ((list 'binary-expr $2 $1 $3)))
	      ((binop2-expr |\|| binop2-expr) . ((list 'binary-expr $2 $1 $3)))
	      ((            |~|  binop2-expr) . ((list 'unary-expr     $1 $2)))
	      ((binop2-expr |%|  binop2-expr) . ((list 'binary-expr $2 $1 $3))))
 
 ;; some with explicit precedences
 
 (binop-expr (binop-expr |not| |in| binop-expr)
	     ((list 'binary-expr '|not in| $1 $4)) (:precedence |in|))
 (binop-expr (binop-expr |is| |not| binop-expr)
	     ((list 'binary-expr '|is not| $1 $4)) (:precedence |is|))
 
 (binop2-expr (|+| binop2-expr) ((list 'unary-expr $1 $2))
	      (:precedence unary-plusmin))
 (binop2-expr (|-| binop2-expr) ((list 'unary-expr $1 $2))
	      (:precedence unary-plusmin))

 (atom :or
       ((|(| comma? |)|)        . ((list 'tuple-expr nil)))
       ((|(| testlist-gexp |)|) . ($2))
       ((|[|           |]|)     . ((list 'list-expr nil)))
       ((|[| listmaker |]|)     . ($2))
       ((|{|           |}|)     . ((list 'dict-expr nil)))
       ((|{| dictmaker |}|)     . ((list 'dict-expr $2)))
       ((|`| testlist1 |`|)     . ((list 'backticks-expr $2)))
       ((identifier)            . ((list 'identifier-expr $1)))
       ((number)                . ($1))
       ((string+)               . ($1))
       ((|lispy-lisp-form|)     . ($1)))

 ;; consecutive string literals are joined: "s" "b" => "sb"
 (string+ (string) ($1)) 
 (string+ (string+ string) ((concatenate 'string $1 $2))) 

 (listmaker (test list-for) ((list 'listcompr-expr $1 $2)))
 (listmaker (test comma--test* comma?) ((list 'list-expr (cons $1 $2))))
 (:comma--test*)

 (testlist-gexp (test gen-for)             (`(generator-expr ,$1 ,$2)))
 (testlist-gexp (test comma--test* comma?) ((if (or $2 $3) `(tuple-expr (,$1 . ,$2)) $1)))
 
 (lambdef (|lambda| parameters |:| test) (`(lambda-expr ,$2 ,$4)))

 (trailer+ :or
	   ((|(| arglist      |)|)           . ((list (list 'call-expr $2))))
	   ((|[| subscriptlist |]|)          . ((list (list 'subscription-expr $2))))
	   ((|.| identifier)                 . (`((attributeref-expr (identifier-expr ,$2)))))
	   ((trailer+ |(| arglist |)|)       . ((append $1 (list (list 'call-expr $3)))))
	   ((trailer+ |[| subscriptlist |]|) . ((append $1 (list (list 'subscription-expr $3)))))
	   ((trailer+ |.| identifier)        . ((append $1 `((attributeref-expr
							      (identifier-expr ,$3)))))))

 (subscriptlist (subscript comma--subscript* comma?)
		((if (or $2 $3) `(tuple-expr (,$1 . ,$2)) $1)))
 (:comma--subscript*)
 (comma--subscript (|,| subscript) ($2))
 (subscript :or
	    ((|...|)                    . (`(identifier-expr Ellipsis)))
	    test
	    ((test? |:| test? sliceop?) . (`(slice-expr ,$1 ,$3 ,$4))))
 (:sliceop?)
 (sliceop (|:| test?) ($2))
 (:test?)

 (exprlist (expr exprlist2)
	   ((if $2 (list 'tuple-expr (cons $1 (butlast $2))) $1)))
 
 (exprlist2 :or
	    (()                   . (nil))
	    ((|,|)                . ((list t)))
	    ((|,| expr exprlist2) . ((list $2 $3))))

 (testlist (test testlist2)
	   ((if $2 
		`(tuple-expr ,(cons $1 (if (eq (car (last $2)) :dummy)
					   (butlast $2)
					 $2)))
	      $1)))
 
 (testlist2 :or
	    (()                   . (nil))
	    ((|,|)                . ((list :dummy)))
	    ((|,| test testlist2) . ((cons $2 $3))))

 (testlist-safe :or
		((test)                     . ($1))
		((test comma--test+ comma?) . ((list $1 $2))))
 (:comma--test+)

 (dictmaker (test |:| test comma--test--\:--test* comma?) ((cons (cons $1 $3) $4)))
 (:comma--test--\:--test*)
 (comma--test--\:--test (|,| test |:| test) ((cons $2 $4)))
 
 (classdef (|class| identifier inheritance |:| suite)
	   (`(classdef-stmt (identifier-expr ,$2) ,$3 ,$5)))

 (inheritance (                ) ('(tuple-expr nil)))
 (inheritance (|(|          |)|) ('(tuple-expr nil)))
 (inheritance (|(| testlist |)|) ((if (eq (car $2) 'tuple-expr) $2 `(tuple-expr (,$2)))))
 
 (arglist () (`(nil nil nil nil)))
 (arglist (argument--comma* arglist-2)
	  ((loop with key-args
	       for sublist on $1
	       for item = (car sublist)
	       while (eq (car item) :pos)
	       collect (second item) into pos-args
	       finally (setf key-args (mapcar #'cdr sublist))
		       (destructuring-bind (a *-a **-a) $2
			 (when a
			   (ecase (car a)
			     (:pos (setf pos-args
				     (nconc pos-args
					    (list (second a)))))
			     (:key (setf key-args
				     (nconc key-args
					    (list (cdr a)))))))
			 (return (list pos-args key-args
				       *-a **-a))))))
 (arglist-2 :or
	    ((argument comma?)            . ((list  $1 nil nil)))
	    ((|*|  test comma--**--test?) . ((list nil  $2  $3)))
	    ((|**| test)                  . ((list nil nil  $2))))
 
 (:argument--comma*)
 (argument--comma (argument |,|) ($1))
 (:comma--**--test?)
 (comma--**--test (|,| |**| test) ($3))

 (argument (test)                (`(:pos ,$1)))
 (argument (identifier |=| test) (`(:key (identifier-expr ,$1) ,$3)))
 (argument (test gen-for)        (`(:pos (generator-expr ,$1 ,$2))))

 (list-iter :or list-for list-if)
 (list-for (|for| exprlist |in| testlist-safe list-iter?) 
	   (`((:for-in ,$2 ,$4) . ,$5)))
 (:list-iter?)
 (list-if (|if| test list-iter?) (`((:if ,$2) . ,$3)))
 
 (gen-iter :or gen-for gen-if)
 (gen-for (|for| exprlist |in| test gen-iter?) (`((:for-in ,$2 ,$4) . ,$5)))
 (gen-if  (|if|  test               gen-iter?) (`((:if ,$2) . ,$3)))
 (:gen-iter?)
 
 (testlist1 (test |,--test*|) ((if $2 `(tuple-expr (,$1 . ,$2)) $1))))


(build-grammar python-grammar t t)


(defun parse-trailers (ast)
  ;; foo[x].a => (trailers (id foo) ((subscription (id x)) (attributeref (id a))))
  ;;          => (attributeref (subscription (id foo) (id x)) (id a))
  (assert (eq (car ast) 'trailers))
  (destructuring-bind (primary trailers)
      (cdr ast)
    (loop for tr in trailers
	with res = primary
	do (setf res `(,(car tr) ,res ,@(cdr tr)))
	finally (return res))))
