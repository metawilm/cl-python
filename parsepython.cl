;;; Parsing Python

(in-package :python)

(eval-when (:compile-toplevel)
  ;; update when yacc integrated
  (require :yacc (merge-pathnames #p"yacc.fasl" *compile-file-pathname*))
  (use-package :yacc))

(eval-when (:load-toplevel :execute)
  ;; update when yacc integrated
  (require :yacc (merge-pathnames #p"yacc.fasl" *load-pathname*))
  (use-package :yacc))

(defvar *reserved-words*
    ;; Well, a few of these are not actually reserved words in Python
    ;; yet, because of backward compatilibity reasons, but they will
    ;; be in the future (`as' is an example).
    '(def class return yield lambda
      and or not for in is
      print import from as assert break continue global del exec pass
      try except finally raise
      if elif else while))

;; NEW!!
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
  (:lexemes identifier number string #|punctuation|# newline indent dedent
	    ;; punctuation:
	    = [ ] |(| |)| < > { } |.| |,| |:| |\|| ^ % + - * / ~ & |`|
            &= // << >>  <> != += -= *= /= //= %= ** <= >= ^= |\|=| ==
	    **= <<= >>= |...|
	    |;| ;; multiple statements same line
	    @ ;; decorator
	    ;; keywords:
	    def class return yield lambda
	    and or not for in is
	    print import from as assert break continue global del exec pass
	    try except finally raise
	    if elif else while )
  )


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-python-rule (name)
    (check-type name symbol)
    (let* ((name (intern name #.*package*))
	   (str (symbol-name name))
	   (len (length str))
	   (item (intern (subseq str 0 (- len 1)) #.*package*))
	   ;;(name-2 (intern (concatenate 'string str "2") #.*package*))
	   (dp 'defproduction))
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

 ;; Rules (including their names) are taken from the Python
 ;; grammar in file in CPython CVS, file Python/Grammar/Grammar, 20040827.

 ;; The start symbol: regular python files
 (python-grammar ( file-input ) ((cons 'file-input (nreverse $1))))

 (file-input () ())
 (file-input (file-input newline) ($1))
 (file-input (file-input stmt) ((cons $2 $1)))

 (decorator (@ dotted-name newline) ((list 'decorator $1 $2)))
 (decorator (@ dotted-name |(| arglist |)| newline) ((list 'decorator $2 $4)))
 (:decorator+)
 (:decorator+?)

 ;; XXX for now: ignore decorators above function definition
 (funcdef (decorator+? def identifier parameters |:| suite) ((list 'funcdef $3 $4 $6)))
 ;;(parameters (|(| varargslist? |)|) ($2))

 (parameters ( |(| |)| ) ((list nil nil nil)))
 (parameters ( |(| parameter-list5 |)| ) ($2))

 (parameter-list5 (defparameter+) ((list $1 nil nil)))
 (parameter-list5 (defparameter+ ni-*-ident ni-**-ident) ((list $1 $2 $3)))
 (parameter-list5 (defparameter+ ni-*-ident            ) ((list $1 $2 nil)))
 (parameter-list5 (defparameter+            ni-**-ident) ((list $1 nil $2)))
 (parameter-list5 (              *-ident    ni-**-ident) ((list nil $1 $2)))
 (parameter-list5 (              *-ident               ) ((list nil $1 nil)))
 (parameter-list5 (                         **-ident   ) ((list nil nil $1)))

 (defparameter+ (defparameter) ((list $1)))
 (defparameter+ (defparameter+ |,| defparameter) ((append $1 (list $3))))

 (ni-*-ident  ( |,| *-ident ) ($2))
 (*-ident ( * identifier ) ($2))

 (ni-**-ident ( |,| **-ident) ($2))
 (**-ident ( ** identifier ) ($2))

 (defparameter (fpdef) ($1))
 (defparameter (fpdef = test) ((cons $1 $3)))


 #+(or)
 (progn (varargslist4 :or
		      ((|(| |)|) . (nil))
		      ((|(| va4-item |)|))
		      (va4va4-item+ |)|) ($2))

	(:va4-item+)
	(va4-item :or
		  fpdef comma?
		  ((fpdef = test comma?))
		  ((* identifier comma?))
		  ((** identifier comma?)))


	(varargslist4 (|(| stuff-c |)|) ($2))
	(varargslist4 (|(| * identifier |)|) ((list $2 $4)))
	(varargslist4 (|(| stuff-c * identifier |)|) ((list $2 $4)))
	(varargslist4 (|(| ** identifier |)|) ((list $2 $4)))
	(varargslist4 (|(| stuff-c ** identifier |)|) ((list $2 $4)))
	(varargslist4 (|(| * identifier |,| ** identifier |)|) ((list $2 $4 $7)))
	(varargslist4 (|(| stuff-c * identifier |,| ** identifier |)|) ((list $2 $4 $7)))

	(varargslist3 (|(| stuff |)|) ($2))
	(varargslist3 (|(| stuff-c |)|) ($2))
	(varargslist3 (|(| * identifier |)|) ((list $2 $4)))
	(varargslist3 (|(| stuff-c * identifier |)|) ((list $2 $4)))
	(varargslist3 (|(| ** identifier |)|) ((list $2 $4)))
	(varargslist3 (|(| stuff-c ** identifier |)|) ((list $2 $4)))
	(varargslist3 (|(| * identifier |,| ** identifier |)|) ((list $2 $4 $7)))
	(varargslist3 (|(| stuff-c * identifier |,| ** identifier |)|) ((list $2 $4 $7)))

	(stuff ())
	(stuff (stuff+) ($1))
	(stuff+ (fpdef) ($1))
	(stuff+ (fpdef = test) ((cons $1 $3)))
	(stuff+ (stuff+ |,| fpdef) ((list $1 $3)))
	(stuff+ (stuff+ |,| fpdef = test) ((list $1 (cons $3 $5))))

	(stuff-c (fpdef |,|) ($1))
	(stuff-c (fpdef = test |,|) ((list $1 $3)))
	(stuff-c (stuff-c fpdef |,|) ((list $1 $2)))
	(stuff-c (stuff-c fpdef = test |,|) ((list $1 $2 $4)))


	(varargslist2 (|(| |fpdef--=--test--,*| fpdef comma? |)|) ((list $1 $2 nil nil)))
	(varargslist2 (|(| |fpdef--=--test--,*| fpdef = test comma? |)|) ((list $1 $2 $4 nil nil)))

	(varargslist2 (|(| |fpdef--=--test--,+| * identifier |,| ** identifier |)|)
		      ((list $1 $3  $6)))
	(varargslist2 (|(| |fpdef--=--test--,+| * identifier                   |)|)
		      ((list $1 $3 nil)))
	(varargslist2 (|(| |fpdef--=--test--,+|                  ** identifier |)|)
		      ((list $1 nil $3)))
	(varargslist2 (|(|                     * identifier |,|  ** identifier |)|)
		      ((list nil $2 $5)))
	(varargslist2 (|(|                     * identifier                    |)|)
		      ((list nil $2 nil)))
	(varargslist2 (|(|                                       ** identifier |)|)
		      ((list nil nil $2)))

	;;(:comma?)
	;;(comma (|,|))

	(:varargslist?)
	(varargslist (|fpdef--=--test--,*| ) ((list $1 nil nil)))

	(varargslist (|fpdef--=--test--,+| * identifier |,| ** identifier ) ((list $1 $3  $6)))
	(varargslist (|fpdef--=--test--,+| * identifier                   ) ((list $1 $3 nil)))
	(varargslist (|fpdef--=--test--,+|                  ** identifier ) ((list $1 nil $3)))
	(varargslist (                     * identifier |,| ** identifier ) ((list nil $2 $5)))
	(varargslist (                     * identifier                   ) ((list nil $2 nil)))
	(varargslist (                                      ** identifier ) ((list nil nil $2)))

	(|fpdef--=--test--,| :or
			     ((fpdef |,|) . ($1))
			     ((fpdef = test |,|) . ((list $1 $2 $3))))
	(:|fpdef--=--test--,+|)
	(:|fpdef--=--test--,*|))


 (fpdef :or
	((identifier) . ($1))
	((|(| fplist |)|) . (`(list ,$2)))) ;; WWW or just $2

 (fplist (fpdef comma--fpdef* comma?) ((cons $1 $2)))
 (:comma--fpdef*)
 (comma--fpdef (|,| fpdef) ($2))

 (:comma?)
 (comma (|,|) ((list $1)))

 (stmt :or simple-stmt compound-stmt)
 (simple-stmt (small-stmt semi--small-stmt* semi? newline) ((if $2 (list $1 $2) $1)))

 (semi--small-stmt (|;| small-stmt) ($2))
 (:semi--small-stmt*)
 (:semi?)
 (semi (|;|))

 (small-stmt :or expr-stmt print-stmt del-stmt pass-stmt flow-stmt
	     import-stmt global-stmt exec-stmt assert-stmt)

 ;; XXX assign-expr only is correct for 1 level assignents (a = b, not for a=b=c)
 (expr-stmt (testlist expr-stmt2) ((cond ((and $2 (eq (car $2) '=))
					  (list 'assign-expr $1 (caadr $2))) ;; XXX caadr ok?
					 ($2
					  (list 'augassign-expr (car $2) (car $1) (cdr $2)))
					 (t
					  $1))))

 (expr-stmt2 (augassign testlist) ((cons $1 $2)))
 (expr-stmt2 (=--testlist*) ((when $1 (list '= $1))))
 (:=--testlist*)
 (=--testlist (= testlist) ($2))

 (augassign :or += -= *= /= %= &= |\|=| ^= <<= >>= **= //= )

 (print-stmt :or
	     ((print test |,--test*| comma?) . ((list 'print (cons $2 $3) (if $4 t nil))))
	     ((print >> test |,--test*| comma?) . ((list 'print->> $3 $4 (if $5 t nil)))))
 (:|,--test*|)
 ;;(:|,--test+|)
 (|,--test| (|,| test) ($2))

 (del-stmt (del exprlist) ((list $1 $2)))
 (pass-stmt (pass) ((list $1)))
 (flow-stmt :or break-stmt continue-stmt return-stmt raise-stmt yield-stmt)

 (break-stmt (break) ((list $1)))
 (continue-stmt (continue) ((list $1)))
 (return-stmt (return testlist?) ((list $1 $2)))
 (:testlist?)
 (yield-stmt (yield testlist?) ((list $1 $2)))

 (raise-stmt (raise) ((list $1)))
 (raise-stmt (raise test) ((list $1 $2)))
 (raise-stmt (raise test |,| test) ((list $1 $2 $4)))
 (raise-stmt (raise test |,| test |,| test) ((list $1 $2 $4 $6)))


 ;; "import" module ["as" name] ( "," module ["as" name] )*
 (import-stmt :or import-normal import-from)
 (import-normal (import dotted-as-name comma--dotted-as-name*) ((list $1 $2 $3)))
 (:comma--dotted-as-name*)
 (comma--dotted-as-name ( |,| dotted-as-name) ($2))
 (import-from (from dotted-name import import-from-2) ((list $1 $2 $3)))
 (import-from-2 :or
		*
		((import-as-name comma--import-as-name*) . ((list $1 $2))))
 (:comma--import-as-name*)
 (comma--import-as-name (|,| import-as-name) ($2))
 (import-as-name (identifier) ((list 'normal $1)))
 (import-as-name (identifier as identifier) ((list 'as $2 $3)))
 (dotted-as-name (dotted-name) ((list 'normal $1)))
 (dotted-as-name (dotted-name as identifier) ((list 'as $1 $2 $3)))
 (dotted-name (identifier dot--name*) ((list $1 $2)))
 (:dot--name*)
 (dot--name (|.| identifier))

 (global-stmt (global identifier comma--identifier*) ((list $1 $2 $3)))
 (:comma--identifier*)
 (comma--identifier (|,| identifier) ($2))
 (exec-stmt (exec expr) ((list $1 $2)))
 (exec-stmt (exec expr in test) ((list $1 $2 $4)))
 (exec-stmt (exec expr in test |,| test) ((list $1 $2 $4 $6)))
 (assert-stmt (assert test comma--test?) ((list $1 $2)))
 (:comma--test?)
 (comma--test (|,| test) ($2))

 (compound-stmt :or if-stmt while-stmt for-stmt try-stmt funcdef classdef)
 (if-stmt (if test |:| suite elif--test--suite* else--suite)
	  (`(if ((,$2 ,$4) ,@$5) ,$6)))
 (if-stmt (if test |:| suite elif--test--suite*)
	  (`(if ((,$2 ,$4) ,@$5) nil)))
 (:elif--test--suite*)
 (elif--test--suite (elif test |:| suite) ((list $2 $4)))
 (else--suite (else |:| suite) ($3))
 (while-stmt (while test |:| suite else--suite?) ((list $1 $2 $4 $5)))
 (:else--suite?)
 (for-stmt (for exprlist in testlist |:| suite else--suite?)
	   ((list 'for-in $2 $4 $6 $7))
	   (:precedence high-prec))
 (try-stmt :or
	   ((try |:| suite except--suite+ else--suite?)
	    . ((list 'try-except $3 $4 $5)))
	   ((try |:| suite finally |:| suite)
	    . ((list 'try-finally $3 $6))))

 ;;(:except--suite+)
 ;;(except--suite (except |:| suite) ((list $1 $3)))

 (except--suite (except |:| suite) (`(except (nil nil) ,$3)))
 (except--suite (except test |:| suite) (`(except (,$2 nil) ,$4)))
 (except--suite (except test |,| test |:| suite) (`(except (,$2 ,$4) ,$6)))

 (except--suite+ (except--suite) ((list $1)))
 (except--suite+ (except--suite+ except--suite) ((append $1 (list $2))))

;;; (except--suite+ (except |:| suite) (`(except (nil nil) ,$3)))
;;; (except--suite+ (except test |:| suite) (`(except (,$2 nil) ,$4)))
;;; (except--suite+ (except test |,| test |:| suite) (`(except (,$2 ,$4) $6)))
;;;
;;; (except--suite+ (except--suite+ except |:| suite) ((append $1 (list $2 $4))))

 (suite :or
	((simple-stmt) . ((list 'suite (list $1))))
	((newline indent stmt+ dedent) . ((list 'suite $3))))

 ;; old (:stmt+)
 (stmt+ (stmt) ((list $1)))
 (stmt+ (stmt+ stmt) ((append $1 (list $2))))

 #|
 (test :or
       ((and-test or--and-test*) . ((if $2 (list $1 $2) $1)))
       lambdef)

 (:or--and-test*)
 (or--and-test (or and-test) ((list $1 $2)))
 (and-test (not-test and--not-test*) ((if $2 (list $1 $2) $1)))
 (:and--not-test*)
 (and--not-test (and not-test) ((list $1 $2)))
 (not-test :or
	   ((not not-test) . ((list $1 $2)))
	   comparison)
 (comparison (expr comp-op--expr*) ((if $2 (list $1 $2) $1)))
 (:comp-op--expr*)
 (comp-op--expr (comp-op expr) ((list $1 $2)))
 (comp-op :or < > == >= <= <> != in is
	  ((not in) . ((list 'not-in)))
	  ((is not) . ((list 'is-not))))

 (expr (xor-expr XOR--xor-expr*) ((if $2 (list $1 $2) $1)))
 (:XOR--xor-expr*)
 (XOR--xor-expr (|\|| xor-expr) ((list 'xor $2)))

 (xor-expr (and-expr AND--and-expr*) ((if $2 (list $1 $2) $1)))
 (:AND--and-expr*)
 (AND--and-expr (^ and-expr) ((list $1 $2)))

 (and-expr (shift-expr AMP--shift-expr*) ((if $2 (list $1 $2) $1)))
 (:AMP--shift-expr*)
 (AMP--shift-expr (& shift-expr) ((list $1 $2)))

 (shift-expr (arith-expr <<\|>>--arith-expr*) ((if $2 (list $1 $2) $1)))
 (:<<\|>>--arith-expr*)
 (<<\|>>--arith-expr :or
		     ((<< arith-expr) . ((list $1 $2)))
		     ((>> arith-expr) . ((list $1 $2))))

 (arith-expr (term +---term*) ((format t "~&Here ~s ~s~%" $1 $2)
			       (if $2
				   (if (and (consp $1) (eq (car $1) (car $2)))
				       (append $1 (cdr $2))
				     `(,(car $2) ,$1 ,@(cdr $2)))
				 $1)))
 (:+---term*)
 (+---term :or
	   ((+ term) . ((list $1 $2)))
	   ((- term) . ((list $1 $2))))

 (term (factor termop--factor*) ((if $2 `(,(car $2) ,$1 ,@(cdr $2)) $1)))
 (:termop--factor*)
 (termop--factor (termop factor) ((list $1 $2)))
 (termop :or * / % //)
 (factor :or
	 ((factorop factor) . ((list $1 $2)))
	 power)
 (factorop :or |+| |-| |~|) ;; XXX set precedence
 (power (atom trailer*) ((if $2 (list $1 $2) $1)))
 (:trailer*)
 (power (atom trailer* **--factor) ((cond ($3 (list $1 $2 $3))
                                    ($2 (list $1 $2))
                                    ($1 $1))))
 (**--factor (** factor) ((list $1 $2)))
 |#

 (expr (binop2-expr) ($1)) ;; YYY: binop-2 2 2 2 2
 ;; WWW new: using precedences
 (test :or
       lambdef
       binop-expr)

 (binop-expr :or
	     ((binop-expr and binop-expr) . ((list 'binary $2 $1 $3)))
	     ((binop-expr or binop-expr) . ((list 'binary $2 $1 $3)))
	     ((not binop-expr) . ((list 'unary $1 $2)))

	     ((binop-expr < binop-expr) .  ((list 'comparison $2 $1 $3)))
	     ((binop-expr <= binop-expr) . ((list 'comparison $2 $1 $3)))
	     ((binop-expr > binop-expr) .  ((list 'comparison $2 $1 $3)))
	     ((binop-expr >= binop-expr) . ((list 'comparison $2 $1 $3)))
	     ((binop-expr != binop-expr) . ((list 'comparison $2 $1 $3)))
	     ((binop-expr <> binop-expr) . ((list 'comparison '!= $1 $3)))
	     ((binop-expr == binop-expr) . ((list 'comparison $2 $1 $3)))
	     ((binop-expr in binop-expr) . ((list 'comparison $2 $1 $3)))
	     ((binop-expr is binop-expr) . ((list 'comparison $2 $2 $1 $3))))
 (binop-expr (binop2-expr) ($1) (:precedence or))

 (binop2-expr :or
	      atom
	      ((atom trailer+) . ((parse-trailers (list 'trailers $1 $2))))
	      ((binop2-expr + binop2-expr) . ((list 'binary $2 $1 $3)))
	      ((binop2-expr - binop2-expr) . ((list 'binary $2 $1 $3)))
	      ((binop2-expr * binop2-expr) . ((list 'binary $2 $1 $3)))
	      ((binop2-expr / binop2-expr) . ((list 'binary $2 $1 $3)))
	      ((binop2-expr ** binop2-expr) . ((list 'binary $2 $1 $3)))
	      ((binop2-expr // binop2-expr) . ((list 'binary $2 $1 $3)))

	      ((binop2-expr << binop2-expr) . ((list 'binary $2 $1 $3)))
	      ((binop2-expr >> not binop2-expr) . ((list 'binary $2 $1 $3)))
	      ((binop2-expr & binop2-expr) . ((list 'binary $2 $1 $3)))
	      ((binop2-expr ^ binop2-expr) . ((list 'binary $2 $1 $3)))
	      ((binop2-expr \| binop2-expr) . ((list 'binary $2 $1 $3)))
	      ((binop2-expr ~ binop2-expr) . ((list 'binary $2 $1 $3)))
	      ((binop2-expr % binop2-expr) . ((list 'binary $2 $1 $3)))
	      )
 ;; some with explicit precedences
 (binop2-expr (binop2-expr not in binop2-expr) ((list 'comparison 'not-in $1 $4)) (:precedence in))
 (binop2-expr (binop2-expr is not binop2-expr) ((list 'comparison 'not-in $1 $4)) (:precedence is))
 (binop2-expr (+ binop2-expr) ((list 'unary $1 $2)) (:precedence unary-plusmin))
 (binop2-expr (- binop2-expr) ((list 'unary $1 $2)) (:precedence unary-plusmin))


 #+(or)(bin-op :or
	       and or not
	       < <= > >= == != <>
	       in is
	       ((not in) . ('not-in))
	       ((is not) . ('is-not))
	       + -
	       * / // **
	       ^ & << >>)

 (atom :or
       ((|(| |)|) . ((list 'testlist nil t)))
       ((|(| testlist-gexp |)|) . ((cons 'testlist $2)))

       ((|[| |]|) . ((list 'list nil)))
       ((|[| listmaker |]|) . ((list 'list $2)))
       ((|{| |}|) . ((list 'dict nil)))
       ((|{| dictmaker |}|) . ((list 'dict $2)))
       ((|`| testlist1 |`|) . ((list 'repr-list $2)))
       ((identifier) . ((list 'identifier $1)))
       ((number) . ($1))
       ((string+) . ($1)))

 (string+ (string) ($1))
 (string+ (string+ string) ((concatenate 'string $1 $2)))

 (listmaker (test list-for) ((list $1 $2)))
 (listmaker (test comma--test* comma?) ((cons $1 $2)))
 (:comma--test*)


 (testlist-gexp (test gen-for) ((list $1 $2)))
 (testlist-gexp (test comma--test* comma?) ((list (cons $1 $2) (if $3 t nil))))

 ;; new:
 ;;(testlist-gexp (test) ($1))
 ;;(testlist-gexp (testlist-gexp |,| test) ((cons $3 $1)))

 (lambdef (lambda #+(or)varargslist? |:| test))

 #+(or)(progn (:trailer+)
	      (trailer :or
		       ((|(| arglist? |)|) . ((list 'call $2)))
		       ((|[| subscriptlist |]|) . (`(subscription . ,$2)))
		       ((|.| identifier) . (`(attributeref (identifier ,$2))))))

 (trailer+ :or
	   ((|(| arglist? |)|) . ((list (list 'call $2))))
	   ((|[| subscriptlist |]|) . ((list (list 'subscription $2))))
	   ((|.| identifier) . (`((attributeref (identifier ,$2)))))
	   ((trailer+ |(| arglist? |)|) . ((append $1 (list (list 'call $3)))))
	   ((trailer+ |[| subscriptlist |]|) . ((append $1 (list (list 'subscription $3)))))
	   ((trailer+ |.| identifier) . ((append $1 `((attributeref (identifier ,$3)))))))


 (:arglist?)
 ;;(subscriptlist (subscript comma--subscript*) ((append (list $1) $2)))
 (subscriptlist (subscript comma--subscript* comma?) ((list (append (list $1) $2)
							    (if $3 t nil))))
 (:comma--subscript*)
 (comma--subscript (|,| subscript) ($2))
 (subscript :or
	    |...|
	    test
	    ((test? |:| test? sliceop?) . (`(slice ,$1 ,$3 . ,$4))))
 (:sliceop?)
 (sliceop (|:| test?))
 (:test?)

 ;; old:
 ;; (exprlist (expr comma--expr* comma?) ((cons $1 $2)))
 ;; new:
 (exprlist (expr exprlist2) ((list 'exprlist
				   (cons $1 (butlast $2))
				   (car (last $2)))))
 (exprlist2 :or
	    (() . (nil))
	    ((|,|) . ((list t)))
	    ((|,| expr exprlist2) . ((list $2 $3))))

 #|
 (testlist (test comma--test* comma?) ((if $2 (cons $1 $2) $1)))
 |#
 #|(testlist (test) ($1))
 (testlist (test |,|) ($1 :comma)) ; n
 (testlist (testlist |,| test) ((list $1 $3)))
 |#

 (testlist (test testlist2) ((list 'testlist
				   (cons $1 (butlast $2))
				   (car (last $2)))))
 (testlist2 :or
	    (() . ((list nil)))
	    ((|,|) . ((list t)))
	    ((|,| test testlist2) . ((cons $2 $3))))


 (testlist-safe :or
		((test) . ($1))
		((test comma--test+ comma?) . ((list $1 $2))))
 (:comma--test+)
 ;;(:comma--expr*)
 ;;(comma--expr (|,| expr) ($2))
 (dictmaker :or
	    ;;((test |:| test comma?) . ((cons $1 $3)))
	    ((test |:| test comma--test--\:--test* comma?) . ((cons (cons $1 $3) $4))))
 (:comma--test--\:--test*)
 (comma--test--\:--test (|,| test |:| test) ((cons $2 $4)))
 (classdef (class identifier OB--testlist--CB? |:| suite)
	   ((list $1 $2 $3 $5)))
 (:OB--testlist--CB?)
 (OB--testlist--CB (|(| testlist |)|) ($2))

 (arglist (argument--comma* arglist-2) ((append $1 $2)))

 (arglist-2 :or
	    ;; optional comma is semantically meaningless
	    ((argument comma?) . ((list $1)))
	    ((|*| test comma--**--test?) . ((append (list (list $1 $2))
						    $3
						    nil)))
	    ((|**| test) . ((list (list $1 $2)))))
 (:argument--comma*)
 (argument--comma (argument |,|) ($1))
 (:comma--**--test?)
 (comma--**--test (|,| ** test) ((list (list $2 $3))))

 ;; old:
 ;; (argument (test--=? test gen-for?) ((list $1 $2 $3)))
 ;; new:
 (argument (test) ($1))
 (argument (test = test) ((list $2 $1 $3)))
 (argument (test gen-for) ((list 'gen-for $1 $2)))
 ;;test--=? test gen-for?) ((list $1 $2 $3)))

 #+(or) ;; unused now
 (progn (:test--=?)
	(test--= (test =) ((list $1 $2)))
	(:gen-for?))

 (list-iter :or list-for list-if)
 (list-for (for exprlist in testlist-safe list-iter?) (`((list-for-in ,$2 ,$4) . ,$5)))
 (:list-iter?)
 (list-if (if test list-iter?) (`((list-if ,$2) . ,$3)))
 (gen-iter :or gen-for gen-if)
 (gen-for (for exprlist in test gen-iter?))
 (gen-if (if test gen-iter?) ((list $1 $2 $3)))
 (:gen-iter?)
 (testlist1 (test |,--test*|) ((list $1 $2))))



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

#|
  1 char:  =[]()<>{}.,:|^%+-*/~
  2 chars: != // << >>  <> != ==
               += -= *= /= //= %=  ^= |=  **= <<= >>=
  3 chars: **= <<= >>=  "
|#


(defvar *tab-width-spaces* 8)

(defmacro read-char-nil ()
  "Returns a character, or NIL on eof"
  `(read-char *standard-input* nil nil t))


(defmacro identifier-char1-p (c)
  "Says whether C is a valid character to start an identifier with. ~@
   C must be either a character or nil."
  `(and ,c
       (or (alpha-char-p ,c)
	   (eql ,c #\_))))

(defmacro identifier-char2-p (c)
  "Says whether C is a valid character to occur in an identifier as ~@
   second or later character. C must be either a character or nil."
  `(and ,c
       (or (alphanumericp ,c)
	   (eql ,c #\_))))

(defun read-identifier (first-char)
  "Returns the identifier read as string. ~@
   Identifiers start start with an underscore or alphabetic character; ~@
   second and further characters must be alphanumeric or underscore."
  (assert (identifier-char1-p first-char))
  (let ((res (make-array 6
			 :element-type 'character
			 :adjustable t
			 :fill-pointer 0)))
    (vector-push-extend first-char res)
    (let ((c (read-char-nil)))
      (excl:while (identifier-char2-p c)
	(vector-push-extend c res)
	(setf c (read-char-nil)))
      (unless (null c)
	(unread-char c)))
    (intern res #.*package*)))


;;; STRINGS

(defun read-string (first-char)
  "Returns string as a string"
  (assert (member first-char '(#\' #\") :test 'eql))
  (macrolet ((read-char-error () `(or (read-char-nil)
				      (error "Unexpected end of file"))))
    (let ((second (read-char-error))
	  (third (read-char-nil)))

      ;; "" ('') but not """ (''') --> empty string
      (when (and (char= first-char second)
		 (or (null third)
		     (char/= first-char third)))
	(when third
	  (unread-char third))
	(return-from read-string ""))

      ;;; """ or ''': a long? multi-line? string
      (when (char= first-char second third)
	(let ((res (make-array 50
			       :element-type 'character
			       :adjustable t
			       :fill-pointer 0)))
	  (let ((x (read-char-error))
		(y (read-char-error))
		(z (read-char-error)))
	    (loop
	      (when (char= first-char x y z)
		(return))
	      (vector-push-extend
	       (shiftf x y z (read-char-error))
	       res))
	    (return-from read-string res))))

      ;; non-empty string with one starting quote
      (let ((res (make-array 30
			     :element-type 'character
			     :adjustable t
			     :fill-pointer 0)))
	(vector-push-extend second res)
	(let ((c third))
	  (excl:while (char/= c first-char)
	    (vector-push-extend c res)
	    (setf c (read-char-error)))
	  (return-from read-string res))))))

#+(or)
(defun read-number (first-char)
  ;; Actually, parsing numbers is a weird (dare I say, wrong) in
  ;; Python. These examples make that clear (note the !'s) (using
  ;; Python 2.3.4):
  ;;
  ;; integers:     input  -> base-10 val  system used
  ;;                 11           11         10 (decimal)
  ;;                011            9          8 (octal)
  ;;               0x11           17         16 (hexadecimal)
  ;;
  ;; integers with exponent:
  ;;       11e3 = 011e3 = 11E3 = 11e+3 == 11E+3 = 11000.0 (a float)
  ;;       11e-3 = 0.011 (approx)
  ;;       0x11e3 = regular hex 4579
  ;;
  ;; floats:       input  -> base-10 val  system used
  ;;                11.3         11.3        10
  ;;               011.3         11.3        10    ! (exp: error)
  ;;              0x11.3           *syntax error*
  ;;
  ;; floats with exponent:
  ;;        011.3e3 = 11.3e3 = 011.3e+3 = 11.3e+3 = 11300
  ;;        011.3e-3 = 11.3e-3 = 11300
  ;;
  ;; imag ints:    input  -> base-10 val  system used
  ;;                11j          11j         10
  ;;               011j          11j         10    ! (exp: 9j)
  ;;              0x11j           *syntax error*   ! (exp: 17j)
  ;;
  ;; imag ints with exponent:
  ;;         1e3j = 1000j etc.
  ;;
  ;; imag floats:  input  -> base-10 val  system used
  ;;               11.3j         11.3j       10
  ;;              011.3j         11.3j       10   ! (exp: error)
  ;;             0x11.3j          *syntax error*
  ;;
  ;; imag floats with exp:
  ;;    1.0e-3j -> 0.001j etc.
  ;;
  ;; Integers (dec, oct, hex) may have the suffix `l' or `L', meaning
  ;; `long', though the difference between regular and long integers
  ;; has mostly ceased to exist (operations on regular integers that
  ;; return an integer in the range of long integers, implicitly
  ;; convert the result to a long). We ignore any difference between
  ;; regular and long integers.

  ;; The algorithm:
  ;;  keep reading digits, or one dot
  ;;      until next char is neither `j' nor hexdigit
  ;;  if there's a dot -> ...
  )

(defun read-number (first-char)
  (assert (digit-char-p first-char 10))
  (let* ((res (digit-char-p first-char 10))
	 (base 10))

    (when (char= first-char #\0)
      (let ((second (read-char-nil)))
	(cond
	 ((not second)
	  (return-from read-number 0))

	 ((member second '(#\x #\X) :test #'char=)
	  (setf base 16))

	 ((digit-char-p second 8)
	  (setf base 8)
	  (setf res (digit-char-p second 8)))

	 ((char= second #\.) ;; <digit>.<something> like 1.2 or 1.23 or 1.234 etc
	  (unread-char second)) ;; and continue

	 (t ;; non-number stuff after a zero, like `]' in `x[0]'
	  (unread-char second)
	  (return-from read-number 0)))))

    (let* ((c (read-char-nil))
	   (d (and c (digit-char-p c base))))
      (excl:while d
	(setf res (+ (* res base) d)
	      c (read-char-nil)
	      d (and c (digit-char-p c base))))

      ;; only decimal numbers may have a fraction
      (if (and c
	       (= base 10)
	       (char= c #\.))

	  ;; let's collect all digits and call the Lisp reader on it
	  (let ((fraction-v (make-array 4
					:element-type 'character
					:adjustable t
					:fill-pointer 0)))
	    (vector-push #\0 fraction-v)
	    (vector-push #\. fraction-v)
	    (let ((c (read-char-nil)))

	      (excl:while (and c (digit-char-p c 10))
		(vector-push-extend c fraction-v)
		(setf c (read-char-nil)))

	      (incf res (read-from-string fraction-v))

	      (when (and c
			 (not (digit-char-p c 10)))
		(unread-char c))))

	(when c
	  (unread-char c))))

    res))


;;; punctuation

(defmacro punct-char1-p (c)
  `(and ,c
	(find ,c "`=[]()<>{}.,:|^&%+-*/~;"))) ;; XXX `;' included

(defmacro punct-char-not-punct-char1-p (c)
  "Punctuation  !  may only occur in the form  !=  "
  `(and ,c
	(char= ,c #\!)))

(defmacro punct-char2-p (c1 c2)
  "Recognizes: // << >>  <> !=  <= >=
               == += -= *= /= %=  ^= |= &= ** **= <<= >>= "
  `(and ,c1 ,c2
	(or (and (char= ,c2 #\= )
		 (member ,c1 '( #\+ #\- #\* #\/ #\%  #\^ #\&
			       #\| #\! #\= #\< #\> )))
	    (and (char= ,c1 ,c2)
		 (member ,c1 '( #\* #\< #\> #\/ #\< #\> )))
	    (and (char= ,c1 #\< )
		 (char= ,c2 #\> )))))

(defmacro punct-char3-p (c1 c2 c3)
  "Recognizes:  **= <<= >>= //="
  `(and ,c1 ,c2 ,c3
	(char= ,c3 #\= )
	(char= ,c1 ,c2)
	(member ,c1 '( #\* #\< #\> #\/ ))))

(defun read-punctuation (c1)
  "Returns puncutation as a string."
  (assert (or (punct-char1-p c1)
	      (punct-char-not-punct-char1-p c1)))
  (let ((c2 (read-char-nil)))
    (if (punct-char2-p c1 c2)
	(let ((c3 (read-char-nil)))
	  (if (punct-char3-p c1 c2 c3)
	      ;; 3 chars
	      (make-array 3 :element-type 'character
			  :initial-contents (list c1 c2 c3))
	    ;; 2 chars
	    (progn (when c3 (unread-char c3))
		   (make-array 2 :element-type 'character
			       :initial-contents (list c1 c2)))))
      ;; 1 char, or two of three dots
      (if (and c2 (char= #\. c1 c2))
	  (let ((c3 (read-char-nil)))
	    (if (and c3 (char= c3 #\.))
		"..."
	      (error "Characters `..' may only occur as in `...'")))

	(if (punct-char1-p c1)
	    (progn (when c2 (unread-char c2))
		   (string c1))
	  (error
	   "Character `!' may only occur as in `!=', not standalone"))))))

(defun punctuation->token (punc-str)
  " Punctuation
      1 char:  =[]()<>{}.,:|^%+-*/~  and also: ;
      2 chars: != // << >>  <> !=
               += -= *= /= %=  ^= |=  **= <<= >>=
      3 chars: **= <<= >>=  "
  (intern punc-str)) ;; easiest!

  #+(or)
  (cdr (assoc punc-str '(("=" . =) ("[" . [) ("]" . ]) ("(" . |(|) (")" . |)|)
			 ("<" . <) (">" . >) ("{" . {) ("}" . }) ("." . |.|)
			 ("," . |,|) (":" . |:|) ("|" . |\||) ("^" . ^)
			 ("%" . %) ("+" . +) ("-" . -) ("*" . *) ("/" . /) ("~" . ~))
	      :test #'string=))




(defun read-whitespace ()
  "Reads all whitespace, until first non-whitespace character.

   If Newline was found inside whitespace, values returned are (t N)
   where N is the amount of whitespace after the Newline (N >= 0)
   before the first non-whitespace character (in other words, the
   indentation of the first non-whitespace character) measured in
   spaces, where each Tab is equivalent to *tab-width-spaces* spaces.

   If no Newline was encountered, returns NIL.
   If EOF encountered, NIL is returned."

  (let ((found-newline nil)
	(c (read-char-nil))
	(n 1))
    (loop
      (cond ((not c) (return-from read-whitespace
		       (if found-newline (values t n) nil)))
	    ((char= c #\Newline) (setf found-newline t
				       n 0))
	    ((char= c #\Space) (incf n))
	    ((char= c #\Tab) (incf n *tab-width-spaces*))
	    (t (unread-char c)
	       (return-from read-whitespace
		 (if found-newline (values t n) nil))))
      (setf c (read-char-nil)))))

(defun read-comment-line (c)
  "Read until the end of the line, excluding the Newline."
  (assert (char= c #\#))
  (let ((c (read-char-nil)))
    (excl:while (and c (char/= c #\Newline))
      (setf c (read-char-nil)))
    (unread-char c)))

#+(or)
(defun test-prog ()
"if a == 2:
   if b > 3:
      pass
   else:
      pass
 else:
   pass")

#+(or)
(defun test-prog ()
"if a == 2:
   b = 24
   if b > 3:
      pass
   else:
      pass
 else:
   pass")

#+(or)
(defun test-prog ()
"def foo(a, b):
   if a == 1:
      a = 2
      if b > 3:
         3 + 23
         b
      else:
         6 + 23
         a = 5
         return a
   elif a == 6:
      pass71
      pass72
   elif a == 8:
      b = 9
   else:
      pass10
      pass11")

#+(or)
(defun test-prog ()
"def foo(a, b):
   if a == 2:
      a = 4
      if b > 3:
         return a + b
      else:
         return 'bazzz'
   else:
      return 42")


;;; tests


(defparameter *lex-debug* nil)

;; XXX take a function that gives characters
(defun make-py-lexer (&optional (*standard-input* *standard-input*))
  (let ((tokens-todo (list))
	(indentation-stack (list 0)))

    (lambda (grammar &optional op)
      (declare (ignore grammar op))
      (block lexer

	(when tokens-todo
	  (when *lex-debug*
	    (format t "lexer returns ~s  (from TODO)~%" (car tokens-todo)))
	  (return-from lexer (apply #'values (pop tokens-todo))))

	(with-terminal-codes (python-grammar)
	  (macrolet ((lex-todo (val1 val2)
		       `(progn (when *lex-debug*
				 (format t "lexer-todo: ~s~%" ',val1 ',val2))
			       (push (list (tcode ,val1) ,val2) tokens-todo)))
		     (lex-return (val1 val2)
		       `(let ((v2 ,val2))
			  (when *lex-debug*
			    (format t "lexer returning: ~s ~s~%" ',val1 v2))
			  (return-from lexer (values (tcode ,val1) v2))))
		     (char-member (c list)
		       `(member ,c ',list :test #'char=)))

	    (tagbody next-char
	      (let ((c (read-char-nil)))

		(cond

		 ((not c)

		  ;; Before returning EOF, return NEWLINE followed by
		  ;; a DEDENT for every currently active indent.
		  ;;
		  ;; Returning NEWLINE is not needed when last line
		  ;; in file was already NEWLINE, but for simplicity
		  ;; just return it always here.
		  (if (> (car indentation-stack) 0)

		      (progn
			(lex-todo eof 'eof)
			(lex-todo newline 'newline) ;; needed?

			(excl:while (> (car indentation-stack) 0)
			  (pop indentation-stack)
			  (lex-todo dedent 'dedent))

			(lex-return newline 'newline))

		    (progn (lex-todo eof 'eof)
			   (lex-return newline 'newline))))

		 ((digit-char-p c 10) (lex-return number (read-number c)))

		 ((identifier-char1-p c)
		  (let* ((read-id (read-identifier c))
			 (token (intern read-id)))
		    (if (member token *reserved-words*
				:test #'eq)
			(progn
			  (when *lex-debug*
			    (format t "lexer returning reserved word: ~s~%" token))
			  (return-from lexer
			    (values (tcode-1 (find-class 'python-grammar) token)
				    token)))
		      (progn
			(when *lex-debug*
			  (format t "lexer returning identifier: ~s~%" read-id))
			(return-from lexer
			  (values (tcode identifier)
				  read-id))))))

		 ((or (char= c #\')
		      (char= c #\")) (lex-return string (read-string c)))

		 ((or (punct-char1-p c)
		      (punct-char-not-punct-char1-p c))
		  (let* ((punct (read-punctuation c))
			 (token (punctuation->token punct)))
		    (when *lex-debug*
		      (format t "lexer returning punctuation token: ~s  ~s~%" token punct))
		    (return-from lexer (values (tcode-1 (find-class 'python-grammar) token)
					       token))))

		 ((char-member c (#\Space #\Tab #\Newline))
		  (unread-char c)
		  (multiple-value-bind (newline new-indent)
		      (read-whitespace)

		    (when newline
		      ;; Always return Newline now, but also determine if
		      ;; there are any indents or dedents to be
		      ;; returned in next calls.
		      (cond
		       ((= (car indentation-stack) new-indent)) ;; same indentation

		       ((< (car indentation-stack) new-indent) ; one indent
			(push new-indent indentation-stack)
			(lex-todo indent 'indent))

		       ((> (car indentation-stack) new-indent) ; dedent(s)
			(excl:while (> (car indentation-stack) new-indent)
			  (pop indentation-stack)
			  (lex-todo dedent 'dedent))
			(assert (= (car indentation-stack) new-indent) ()
			  "SyntaxError: Dedent didn't arrive at a previous indentation ~
                           level (indent level after dedent: ~A spaces)"
			  new-indent)))

		      ;;(format t "lexer returning: newline~%")
		      (lex-return newline 'newline))

		    ;; ordinary whitespace is ignored
		    (go next-char)))

		 ;;((char= c #\;) ;; equivalent to Newline (used for >1 stms on same line)
		 ;; (lex-return Newline))

		 ((char= c #\#) ;; comment to end-of-line
		  (read-comment-line c)
		  (go next-char))

		 ((char= c #\\) ;; next line is continuation of this one
		  (let ((c2 (read-char-nil)))
		    (if (and c2 (char= c2 #\Newline))
			(go next-char)
		      (error "Syntax error: continuation character \\ ~
                              must be followed by Newline"))))

		 (t (error "Unexpected character: ~A" c)))))))))))

(defun ptl ()
  (let ((r (find-restart 'return-python-toplevel)))
    (when r
      (invoke-restart r))
    (warn "No return-python-toplevel restart available")))


(defun parse-python (&optional (*standard-input* *standard-input*))
  (let* ((lexer (make-py-lexer))
	 (grammar (make-instance 'python-grammar :lexer lexer))
	 (*print-level* nil)
	 (*print-pretty* t))
    (parse grammar)))

(defun parse-python-string (string)
  (with-input-from-string (stream string)
    (parse-python stream)))

(defun print-time (g)
  (multiple-value-bind (s m h)
      (decode-universal-time g)
    (format t "time: ~A:~A:~A~%" h m s)))

(defparameter *show-ast* nil)

(defun repl ()
  (format t "[CLPython -- type `:q' to quit]~%")
  (loop
    (let ((*scope* (make-namespace :name "repl ns"
				   :builtins t)))
      (declare (special *scope*))
      (loop
	(with-simple-restart (return-python-toplevel "Return to Python top level")
	  (let ((acc ()))
	    (loop
	      (format t ">>> ")
	      (let ((x (read-line)))
		(cond
		 ((member x '(":exit" ":quit" ":q") :test 'string=)
		  (return-from repl 'Bye))

		 ((string= x ":cl")
		  (compile-file "parsepython")
		  (load "parsepython")
		  (setf acc ()))

		 ((string= x ":ns")
		  (format t "~&REPL namespace:~%~A~&" *scope*))

		 ((and (>= (length x) 5)
		       (string= (subseq x 0 5) ":lisp"))
		  (format t "~A~%"
			  (eval (read-from-string (subseq x 6)))))

		 ((string= x "")
		  (let ((total (apply #'concatenate 'string (nreverse acc))))
		    (setf acc ())
		    (loop
		      (restart-case
			  (progn
			    (let ((ast (parse-python-string total)))
			      (when *show-ast*
				(format t "~S~%" ast))
			      (loop
				(restart-case
				    (let ((ev (py-eval ast)))
				      (assert (eq (car ev) :file-input))
				      (when (> (length ev) 1)
					(setf ev (car (last ev)))
					(locally (declare (special *None*))
					  (unless (member ev (list *None* nil)
							  :test 'eq)
					    (handler-case
						(eval-print (list ev) nil)
					      (error ()
						(format t "~A (PE)~%" ev)))
					    (namespace-bind *scope* '_ ev))))
				      (return))
				  (retry-py-eval ()
				      :report "Retry (py-eval AST)"
				    ())))
			      (return)))
			(try-parse-again ()
			    :report "Parse string again into AST")
			(recompile-grammar ()
			    :report "Recompile grammar"
			  (compile-file "parsepython")
			  (load "parsepython"))))))
		 (t
		  (push (concatenate 'string x (string #\Newline))
			acc)))))))))))

(build-grammar python-grammar t t)
