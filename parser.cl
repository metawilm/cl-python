; Parsing Python

(in-package :python)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :yacc)
  (use-package :yacc))

(defvar *reserved-words*
    ;; A few of these are not actually reserved words in CPython yet,
    ;; because of backward compatilibity reasons, but they will be in
    ;; the future (`as' is an example).
    '(def class return yield lambda
      and or not for in is
      print import from as assert break continue global del exec pass
      try except finally raise 
      if elif else while
      ))

(defgrammar python-grammar (grammar)
  ()
  (:left-associative or)
  (:left-associative and)
  (:left-associative not)
  (:left-associative in) ;; and "not in"
  (:left-associative is) ;; and "is not"
  (:left-associative < <= > >= <> != ==)
  (:left-associative |\|| )
  (:left-associative ^)
  (:left-associative &)
  (:left-associative << >> )
  (:left-associative + -)
  (:left-associative * / % //)
  (:left-associative unary-plusmin)
  (:left-associative ~)
  (:right-associative **)
  (:non-associative high-prec)
  (:lexemes identifier number string newline indent dedent
	    ;; punctuation:
	    = [ ] |(| |)| < > { } |.| |,| |:| |\|| ^ % + - * / ~ & |`|
            &= // << >>  <> != += -= *= /= //= %= ** <= >= ^= |\|=| ==
	    **= <<= >>= |...|
	    |;|   ;; multiple statements same line
	    @     ;; decorator
	    ;; reserved words:
	    def class lambda return yield
	    and or not for in is
	    print from import as assert break continue global del exec pass
	    if elif else while try except finally raise))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-python-rule (name)
    (check-type name symbol)
    (let* ((name (intern name #.*package*))
	   (str  (symbol-name name))
	   (len  (length str))
	   (item (intern (subseq str 0 (- len 1)) #.*package*))
	   (dp   'defproduction))
      (ecase (aref str (1- len))

	(#\+ `((,dp (,name python-grammar) (,item) ($1))
	       (,dp (,name python-grammar) (,name |,| ,item) ((append $1 (list $3))))))

	(#\* `((,dp (,name python-grammar) ())
	       (,dp (,name python-grammar) (,name ,item) ((append $1 (list $2))))))

	(#\? `((,dp (,name python-grammar) ())
	       (,dp (,name python-grammar) (,item) ($1))))))))

(defmacro python-prods (&rest prodrules)
  (let ((res ()))
    (loop for (name . rest) in prodrules
	if (keywordp name)
	do (assert (null rest))
	   (dolist (rule (generate-python-rule name))
	     (push rule res))
	else if (eq (car rest) ':or)
	do (dolist (x (cdr rest))
	     (push `(defproduction (,name python-grammar)
			,(if (symbolp x) (list x) (car x))
		      ,(if (symbolp x) '($1) (cdr x)))
		   res))
	else
	do  (push `(defproduction (,name python-grammar) ,@rest)
		  res))
    `(progn ,@(nreverse res))))

(python-prods

 ;; Rules (including their names) are taken from the CPython grammar
 ;; file from CPython CVS, file Python/Grammar/Grammar, 20040827.

 ;; The start symbol: regular python files
 (python-grammar (file-input) ((list 'module-stmt (nreverse $1))))

 (file-input () ())
 (file-input (file-input newline) ($1))
 (file-input (file-input stmt) ((cons $2 $1)))

 (decorator+ (decorator) ($1))
 (decorator+ (decorator+ decorator) ((list $1 $2)))
 (decorator (|@| dotted-name                 newline) ((list 'decorator $2 nil)))
 (decorator (|@| dotted-name |(|         |)| newline) ((list 'decorator $2 nil)))
 (decorator (|@| dotted-name |(| arglist |)| newline) ((list 'decorator $2 $4)))
 ;;(:decorator+)
 (:decorator+?)

 (funcdef (decorator+? |def| identifier parameters |:| suite)
	  (`(funcdef-stmt ,$1 (identifier ,$3) ,$4 ,$6)))

 (parameters ( |(| |)| ) ((list nil nil nil nil)))
 
 (parameters ( |(| parameter-list |)| ) ($2))
	     
 #+(or)(parameters ( |(| parameter-list5 |)| )
	     (`(,@(loop for p in (car $2)
		      if (and (listp p) (eq (car p) :key))
			 collect (cdr p) into kw
		      else collect p into pos
		      finally (return (list pos kw)))
		 ,@(cdr $2))))

 (parameter-list (parameter-list5) ((destructuring-bind (poskey *-a **-a) $1
				      `(,@(loop for p in poskey
					     if (and (listp p) (eq (car p) :key))
					     collect (cdr p) into kw
					     else collect p into pos
					     finally (return (list pos kw)))
					  ,*-a
					  ,**-a))))
 (parameter-list5 (defparameter+                       ) ((list  $1 nil nil)))
 (parameter-list5 (defparameter+ ni-*-ident ni-**-ident) ((list  $1  $2  $3)))
 (parameter-list5 (defparameter+ ni-*-ident            ) ((list  $1  $2 nil)))
 (parameter-list5 (defparameter+            ni-**-ident) ((list  $1 nil  $2)))
 (parameter-list5 (              *-ident    ni-**-ident) ((list nil  $1  $2)))
 (parameter-list5 (              *-ident               ) ((list nil  $1 nil)))
 (parameter-list5 (                         **-ident   ) ((list nil nil  $1)))

 (defparameter+ (defparameter) ((list $1)))
 (defparameter+ (defparameter+ |,| defparameter) ((append $1 (list $3))))

 (ni-*-ident  ( |,| *-ident ) ($2))
 (*-ident ( |*| identifier ) ($2))

 (ni-**-ident ( |,| **-ident) ($2))
 (**-ident ( |**| identifier ) ($2))

 (defparameter (fpdef         ) ($1))
 (defparameter (fpdef |=| test) (`(:key ,$1 . ,$3)))
 ;; Can't use symbol vs. cons for distinguishing positional and
 ;; keyword arguments, as as positional args may be a structure:
 ;;   def f((x,y), z, q=4): ...
 
 (fpdef :or
	((identifier) . (`(identifier-expr ,$1)))
	((|(| fplist |)|) . (`(tuple ,$2))))

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
		     `(assign-expr ,(car items) ,(cdr items))))
		  
		  ($2
		   (list 'augassign-expr (car $2) $1 (cdr $2)))
		  
		  (t
		   $1))))

 (expr-stmt2 (augassign testlist) ((cons $1 $2)))
 (expr-stmt2 (=--testlist*) ((when $1 (list '= $1))))
 (:=--testlist*)
 (=--testlist (|=| testlist) ($2))

 (augassign :or |+=| |-=| |*=| |/=| |%=| |&=| |\|=| |^=| |<<=| |>>=| |**=| |//=| )

 (print-stmt :or
	     ((print) . (`(print nil nil)))
	     ((print test |,--test*| comma?)      . ((list 'print-stmt nil (cons $2 $3) (if $4 t nil))))
	     ((print |>>| test |,--test*| comma?) . ((list 'print-stmt $3  $4           (if $5 t nil)))))
 (:|,--test*|)
 (|,--test| (|,| test) ($2))

 (del-stmt  (|del| exprlist) ((list 'del-stmt $2)))
 (pass-stmt (|pass|)         ((list 'pass-stmt)))
 (flow-stmt :or break-stmt continue-stmt return-stmt raise-stmt yield-stmt)

 (break-stmt    (|break|)            ((list 'break-stmt)))
 (continue-stmt (|continue|)         ((list 'continue-stmt)))
 (return-stmt   (|return| testlist?) ((list 'return-stmt $2)))
 (:testlist?)
 (yield-stmt    (|yield| testlist?)  ((list 'yield-stmt $2)))

 (raise-stmt (|raise|                       ) ((list 'raise-stmt nil nil nil)))
 (raise-stmt (|raise| test                  ) ((list 'raise-stmt  $2 nil nil)))
 (raise-stmt (|raise| test |,| test         ) ((list 'raise-stmt  $2  $4 nil)))
 (raise-stmt (|raise| test |,| test |,| test) ((list 'raise-stmt  $2  $4  $6)))
 
 ;; "import" module ["as" name] ( "," module ["as" name] )*
 (import-stmt :or import-normal import-from)
 
 (import-normal (import dotted-as-name comma--dotted-as-name*) (`(import-stmt (,$2 ,@$3))))
 (:comma--dotted-as-name*)
 (comma--dotted-as-name ( |,| dotted-as-name) ($2))
 
 (import-from (|from| dotted-name |import| import-from-2) (`(import-from-stmt ,$2 ,$4)))
 (import-from-2 :or
		*
		((import-as-name comma--import-as-name*) . ((cons $1 $2))))
 (:comma--import-as-name*)
 (comma--import-as-name (|,| import-as-name) ($2))
 (import-as-name (identifier) (`(|as| ,$1 (identifier ,$1))))
 (import-as-name (identifier |as| identifier) (`(as ,$1 (identifier ,$3))))
 
 (dotted-as-name (dotted-name) (`(as ,$1 (identifier ,$1))))  ;; must have (attributeref ..) as target
 (dotted-as-name (dotted-name |as| identifier) (`(as ,$1 (identifier ,$3))))
 
 (dotted-name (identifier dot--name*) ((if $2
					   `(dotted ,$1 ,@(if (eq (first $2) 'dotted)
							      (second $2)
							    $2))
					 $1)))
 (:dot--name*)
 (dot--name (|.| identifier) ($2))

 (global-stmt (|global| identifier comma--identifier*)
	      (`(global-stmt ,(if $3 (cons $2 $3) (list $2)))))
 (:comma--identifier*)
 (comma--identifier (|,| identifier) ($2))
 (exec-stmt (|exec| expr                   ) ((list 'exec-stmt $1 $2)))
 (exec-stmt (|exec| expr |in| test         ) ((list 'exec-stmt $1 $2 $4)))
 (exec-stmt (|exec| expr |in| test |,| test) ((list 'exec-stmt $1 $2 $4 $6)))
 (assert-stmt (|assert| test comma--test?)   ((list 'assert-stmt $2 $3)))
 (:comma--test?)
 (comma--test (|,| test) ($2))

 (compound-stmt :or if-stmt while-stmt for-stmt try-stmt funcdef classdef)
 (if-stmt (|if| test |:| suite elif--test--suite* else--suite) (`(if-stmt ((,$2 ,$4) ,@$5) ,$6)))
 (if-stmt (|if| test |:| suite elif--test--suite*)             (`(if-stmt ((,$2 ,$4) ,@$5) nil)))
 (:elif--test--suite*)
 (elif--test--suite (|elif| test |:| suite) ((list $2 $4)))
 (else--suite (|else| |:| suite) ($3))
 (while-stmt (|while| test |:| suite else--suite?) ((list 'while-stmt $2 $4 $5)))
 (:else--suite?)
 (for-stmt (|for| exprlist |in| testlist |:| suite else--suite?)
	   ((list 'for-in-stmt $2 $4 $6 $7))
	   (:precedence high-prec))
 (try-stmt :or
	   ((|try| |:| suite except--suite+ else--suite?) . ((list 'try-except-stmt $3 $4 $5)))
	   ((|try| |:| suite |finally| |:| suite)  	  . ((list 'try-finally-stmt $3 $6))))

 (except--suite (|except| |:| suite) (`(nil nil ,$3)))
 (except--suite (|except| test |:| suite) (`(,$2 nil ,$4)))
 (except--suite (|except| test |,| test |:| suite) (`(,$2 ,$4 ,$6)))

 (except--suite+ (except--suite) ((list $1)))
 (except--suite+ (except--suite+ except--suite) ((append $1 (list $2))))

 (suite :or
	((simple-stmt) . ((list 'suite-stmt (list $1))))
	((|newline| |indent| stmt+ |dedent|) . ((list 'suite-stmt $3))))

 (stmt+ (stmt)       ((list $1)))
 (stmt+ (stmt+ stmt) ((append $1 (list $2))))

 (expr (binop2-expr) ($1))

 (test :or
       lambdef
       binop-expr)

 (binop-expr :or
	     ((binop-expr |and| binop-expr) . ((list 'binary-lazy-expr $2 $1 $3)))
	     ((binop-expr |or|  binop-expr) . ((list 'binary-lazy-expr $2 $1 $3)))
	     ((|not| binop-expr)            . ((list 'unary-expr $1 $2)))
	     ((binop-expr |<|  binop-expr)  . ((list 'comparison-expr $2 $1 $3)))
	     ((binop-expr |<=| binop-expr)  . ((list 'comparison-expr $2 $1 $3)))
	     ((binop-expr |>|  binop-expr)  . ((list 'comparison-expr $2 $1 $3)))
	     ((binop-expr |>=| binop-expr)  . ((list 'comparison-expr $2 $1 $3)))
	     ((binop-expr |!=| binop-expr)  . ((list 'comparison-expr $2 $1 $3)))
	     ((binop-expr |<>| binop-expr)  . ((list 'comparison-exr '!= $1 $3))) ;; <> is same as !=
	     ((binop-expr |==| binop-expr)  . ((list 'comparison-expr $2 $1 $3)))
	     ((binop-expr |in| binop-expr)  . ((list 'binary-expr     $2 $1 $3)))
	     ((binop-expr |is| binop-expr)  . ((list 'binary-expr     $2 $1 $3))))
 (binop-expr (binop2-expr) ($1) (:precedence or))

 (binop2-expr :or
	      atom
	      ((atom trailer+)              . ((parse-trailers (list 'trailers $1 $2))))
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
	     ((list 'binary-expr '|not in| $1 $4)) (:precedence |in|)) ;; was binop2
 (binop-expr (binop-expr |is| |not| binop-expr)
	     ((list 'binary-expr '|is not| $1 $4)) (:precedence |is|)) ;; was binop2
 
 (binop2-expr (|+| binop2-expr) ((list 'unary-expr $1 $2))
	      (:precedence unary-plusmin))
 (binop2-expr (|-| binop2-expr) ((list 'unary-expr $1 $2))
	      (:precedence unary-plusmin))

 (atom :or
       ((|(| comma? |)|)        . ((list 'tuple-expr nil)))
       ((|(| testlist-gexp |)|) . ($2))

       ((|[|           |]|)    . ((list 'list-expr nil)))
       ((|[| listmaker |]|)    . ($2))
       ((|{|           |}|)    . ((list 'dict-expr nil)))
       ((|{| dictmaker |}|)    . ((list 'dict-expr $2)))
       ((|`| testlist1 |`|)    . ((list 'backticks-expr $2)))
       ((identifier)           . ((list 'identifier-expr $1)))
       ((number)               . ($1))
       ((string+)              . ($1)))

 (string+ (string) ($1))  ;; consecutive string literals are joined: "s" "b" => "sb"
 (string+ (string+ string) ((concatenate 'string $1 $2))) 

 (listmaker (test list-for) ((list 'list-compr-expr $1 $2)))
 (listmaker (test comma--test* comma?) ((list 'list-expr (cons $1 $2))))
 (:comma--test*)

 (testlist-gexp (test gen-for)             (`(generator-expr ,$1 ,$2)))
 (testlist-gexp (test comma--test* comma?) ((if (or $2 $3) `(tuple-expr (,$1 . ,$2)) $1)))
 
 (lambdef (|lambda|                 |:| test) ((list 'lambda-expr '(nil nil nil nil) $3)))
 (lambdef (|lambda| parameter-list |:| test)
	  (`(lambda-expr ,$2 ,$4)))

 (trailer+ :or
	   ((|(| arglist?      |)|)          . ((list (cons 'call-expr $2))))
	   ((|[| subscriptlist |]|)          . ((list (list 'subscription-expr $2))))
	   ((|.| identifier)                 . (`((attributeref-expr (identifier ,$2)))))
	   ((trailer+ |(| arglist? |)|)      . ((append $1 (list (list 'call-expr $3)))))
	   ((trailer+ |[| subscriptlist |]|) . ((append $1 (list (list 'subscription-expr $3)))))
	   ((trailer+ |.| identifier)        . ((append $1 `((attributeref-expr
							      (identifier-expr ,$3)))))))

 (:arglist?)
 (subscriptlist (subscript comma--subscript* comma?)
		((if (or $2 $3) `(tuple (,$1 . ,$2)) $1)))
 (:comma--subscript*)
 (comma--subscript (|,| subscript) ($2))
 (subscript :or
	    |...|
	    test
	    ((test? |:| test? sliceop?) . (`(slice-expr ,$1 ,$3 ,$4))))
 (:sliceop?)
 (sliceop (|:| test?) ($2))
 (:test?)

 (exprlist (expr exprlist2) ((if $2 (list 'tuple (cons $1 (butlast $2))) $1)))
 (exprlist2 :or
	    (()                   . (nil))
	    ((|,|)                . ((list t)))
	    ((|,| expr exprlist2) . ((list $2 $3))))

 (testlist (test testlist2) ((if $2 `(tuple ,(cons $1 $2)) $1)))
 (testlist2 :or
	    (()                   . (nil))
	    ((|,|)                . ((list t)))
	    ((|,| test testlist2) . ((cons $2 $3))))

 (testlist-safe :or
		((test)                     . ($1))
		((test comma--test+ comma?) . ((list $1 $2))))
 (:comma--test+)

 (dictmaker (test |:| test comma--test--\:--test* comma?) ((cons (cons $1 $3) $4)))
 (:comma--test--\:--test*)
 (comma--test--\:--test (|,| test |:| test) ((cons $2 $4)))
 
 (classdef (|class| identifier inheritance |:| suite)
	   (`(classdef-stmt (identifier ,$2) ,$3 ,$5)))

 (inheritance (                ) ('()))
 (inheritance (|(| testlist |)|) ((if (eq (car $2) 'tuple) $2 `(tuple (,$2)))))
 
 (arglist (argument--comma* arglist-2) (#+(or)(break "$1: ~A $2: ~A" $1 $2)
					(loop with pos and key
					    for a in $1
					    do (ecase (car a)
						 (:pos (push (cdr a) pos))
						 (:key (push (cdr a) key)))
					    finally (destructuring-bind (a *-a **-a) $2
						      (when a
							(ecase (car a)
							  (:pos (push (cdr a) pos))
							  (:key (push (cdr a) key))))
						      (return (list pos key *-a **-a))))))
 (arglist-2 :or
	    ((argument comma?)           . ((list  $1 nil nil)))
	    ((|*| test comma--**--test?) . ((list nil  $2  $3)))
	    ((|**| test)                 . ((list nil nil  $2))))
 (:argument--comma*)
 (argument--comma (argument |,|) ($1))
 (:comma--**--test?)
 (comma--**--test (|,| ** test) ($2))

 (argument (test)                ((list :pos $1)))
 (argument (identifier |=| test) (`(:key (identifier ,$1) . ,$3)))
 (argument (test gen-for) (`(:pos (generator-expr ,$1 ,$2))))

 (list-iter :or list-for list-if)
 (list-for (|for| exprlist |in| testlist-safe list-iter?) 
	   (`((list-for-in ,$2 ,$4) . ,$5)))
 (:list-iter?)
 (list-if (|if| test list-iter?) (`((list-if ,$2) . ,$3)))
 
 (gen-iter :or gen-for gen-if)
 (gen-for (|for| exprlist |in| test gen-iter?) (`((gen-for-in ,$2 ,$4) . ,$5)))
 (gen-if  (|if| test                gen-iter?) (`((gen-if ,$2) . ,$3)))
 (:gen-iter?)
 
 (testlist1 (test |,--test*|) ((if $2 `(tuple (,$1 . ,$2)) $1))))

(build-grammar python-grammar t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST manipulating

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

#+(or) ;; test
(parse-trailers '(trailers (id foo) ((subscription (id x)) (attributeref (id a)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
