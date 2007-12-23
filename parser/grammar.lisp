;; -*- package: clpython.parser; readtable: py-ast-readtable -*-
;; 
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;; Python grammar
;;; Defines grammar rules, and conversion from parse tree into abstract syntax tree.

(in-package :clpython.parser)
(in-syntax *ast-readtable*)

(defparameter *precedence-and-associativity*
    '((left [or])
      (left [and] )
      (left [not] )
      (left [in] [not in])
      (left [is] [is not])
      (left [<] [<=] [>] [>=] [!=] [==] )
      (left [\|]  )
      (left [^]   )
      (left [&]   )
      (left [<<] [>>] )
      (left [+] [-] )
      (left [*] [/] [%] [//] )
      (left unary-plusmin )
      (left [~]   )
      (right [**] )
      #+(or)(nonassoc high-prec))
  "Precedence and associativity rules, ordered by increasing precedence.")

(defun get-precedence-and-associativity (left-token right-token no-assoc-token)
  (let ((list (copy-tree *precedence-and-associativity*)))
    (loop for (old new) in `((left ,left-token) 
                                 (right ,right-token)
                                 (nonassoc ,no-assoc-token))
        do (setf list (nsubst new old list)))
    list))

(defparameter *terminals*
    (sort (loop for pkg in '(:clpython.ast.reserved :clpython.ast.operator
                             :clpython.ast.punctuation :clpython.ast.token)
              nconc (loop for s being the external-symbol in pkg
                        collect s))
          #'string<)
  "Terminal tokens")

(defparameter *python-prods* (make-hash-table :test 'eq)
  "Hashtable containing all grammar rules")

(defmacro p (name &rest rules)
  (if (eq (car rules) :or)
      `(progn ,@(loop for rule in (cdr rules)
                    collect `(p ,name ,@rule)))
    (destructuring-bind (terms outcome &optional options) rules
      `(add-rule ',name ',terms ',outcome ,@options))))

(defun add-rule (name terms outcome &rest options)
  (pushnew (list terms outcome options) (gethash name *python-prods*) :test 'equal))
  
(defmacro gp (name)
  "Generate a production rule:
 Y+ => ITEM [, ITEM [, ITEM [...]]
 Y* => Y [Y [Y p...]]
 Y? => Y or nothing"
  (check-type name symbol)
  (assert (eq (symbol-package name) #.*package*))
  (let* ((str  (symbol-name name))
         (len  (length str))
         (item (intern (subseq str 0 (- len 1)) #.*package*)))
    (ecase (aref str (1- len))
      (#\+ `(progn (add-rule ',name '(,item) '(list $1))
                   (add-rule ',name '(,name [,] ,item) '(nconc $1 (list $3)))))
      (#\* `(progn (add-rule ',name () ())
                   (add-rule ',name '(,name ,item) '(nconc $1 (list $2)))))
      (#\? `(progn (add-rule ',name ()      ())
                   (add-rule ',name '(,item) '$1))))))
  
;; These rules, including most names, are taken from the CPython
;; grammar file from CPython CVS, file Python/Grammar/Grammar,
;; 20040827.

(p python-grammar (file-input) `([module-stmt] ([suite-stmt] ,(nreverse $1))))

(p file-input ()                     ())
(p file-input (file-input [newline]) $1)
(p file-input (file-input stmt)      (cons $2 $1))

(p decorator ([@] dotted-name [newline]) (dotted-name-to-attribute-ref $2))
(p decorator ([@] dotted-name [(] arglist [)] [newline]) `([call-expr] ,(dotted-name-to-attribute-ref $2) ,$4))

(gp decorator*)

(p funcdef (decorator* [def] [identifier] [(] parameters [)] [:] suite) 
   `([funcdef-stmt] ,$1 ([identifier-expr] ,$3) ,$5 ,$8))

(p parameters () (list nil nil nil nil))
(p parameters (parameter-list) $1)

(p parameter-list (parameter-list5)
   (destructuring-bind (poskey *-a **-a) $1
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
                               ,(when *-a `([identifier-expr] ,*-a))
                               ,(when **-a `([identifier-expr] ,**-a))))))))

(p defparameter (fpdef) $1)
(p defparameter (fpdef [=] test) `(:key ,$1 ,$3))

(gp defparameter+)

(p ni-*-ident ([,] *-ident     ) $2)
(p    *-ident ([*] [identifier]) $2)

(p ni-**-ident ([,] **-ident     ) $2)
(p    **-ident ([**] [identifier]) $2)

(p parameter-list5 (defparameter+                       ) (list  $1 nil nil))
(p parameter-list5 (defparameter+ ni-*-ident ni-**-ident) (list  $1  $2  $3))
(p parameter-list5 (defparameter+ ni-*-ident            ) (list  $1  $2 nil))
(p parameter-list5 (defparameter+            ni-**-ident) (list  $1 nil  $2))
(p parameter-list5 (              *-ident    ni-**-ident) (list nil  $1  $2))
(p parameter-list5 (              *-ident               ) (list nil  $1 nil))
(p parameter-list5 (                         **-ident   ) (list nil nil  $1))

;; Can't use symbol vs. cons for distinguishing positional and
;; keyword arguments, as as positional args may be a structure:
;;   def f((x,y), z, q=4): ...

(p fpdef :or
   (([identifier])    `([identifier-expr] ,$1))
   (([(] fplist [)] ) `([tuple-expr] ,$2)))

(p comma--fpdef ([,] fpdef) $2)
(gp comma--fpdef*)
(p fplist (fpdef comma--fpdef* comma?) (cons $1 $2))

(gp comma?)
(p comma ([,]) (list $1))

(p stmt :or
   ((simple-stmt) $1)
   ((compound-stmt) $1))

(p simple-stmt (small-stmt semi--small-stmt* semi? [newline])
   (let ((ss (if $2
                 `([suite-stmt] ,(cons $1 $2))
               $1)))
     (declare (special *include-line-numbers*))
     (if *include-line-numbers*
         `([suite-stmt] (([clpython-stmt] :line-no ,(1- $4))
                         ,ss))
       ss)))

(p semi--small-stmt ([\;] small-stmt) $2)
(gp semi--small-stmt*)
(gp semi?)
(p semi ([\;]) $1)

(p small-stmt :or
   ((expr-stmt)   $1)
   ((print-stmt)  $1)
   ((del-stmt)    $1)
   ((pass-stmt)   $1)
   ((flow-stmt)   $1)
   ((import-stmt) $1)
   ((global-stmt) $1)
   ((exec-stmt)   $1)
   ((assert-stmt) $1))

(p expr-stmt (testlist expr-stmt2)
   (cond ((null $2) $1) ;; not an assignment expression
         ((and $2 (eq (car $2) '[=]))
          (setf $2 (second $2))
          (let* ((val (car (last $2)))
                 (targets (cons $1 (nbutlast $2))))
            `([assign-stmt] ,val ,targets)))
         ($2 `([augassign-stmt] ,(car $2) ,$1 ,(cdr $2)))
         (t $1)))

(gp =--testlist*)
(p =--testlist ([=] testlist) $2)

(p expr-stmt2 (=--testlist*)       (when $1 `([=] ,$1)))
(p expr-stmt2 (augassign testlist) (cons $1 $2))

(p augassign :or
   (([+=])  $1)
   (([-=])  $1)
   (([*=])  $1)
   (([/=])  $1)
   (([%=])  $1)
   (([&=])  $1)
   (([\|=]) $1)
   (([^=])  $1)
   (([<<=]) $1)
   (([>>=]) $1)
   (([**=]) $1)
   (([//=]) $1))

(p print-stmt :or
   (([print])                             `([print-stmt] nil nil nil))
   (([print] test |,--test*| comma?)      `([print-stmt] nil (,$2 . ,$3) ,(and $4 t)))
   (([print] [>>] test |,--test*| comma?) `([print-stmt] ,$3 ,$4 ,(and $5 t))))

(gp |,--test*|)
(p |,--test| ([,] test) $2)

(p del-stmt      ([del] exprlist)     `([del-stmt] ,$2))
(p pass-stmt     ([pass])             `([pass-stmt] ))
(p break-stmt    ([break])            `([break-stmt]))
(p continue-stmt ([continue])         `([continue-stmt]))
(p return-stmt   ([return] testlist?) `([return-stmt] ,$2))
(p yield-stmt    ([yield]  testlist?) `([yield-stmt] ,$2))

(p flow-stmt :or
   ((break-stmt)    $1)
   ((continue-stmt) $1)
   ((return-stmt)   $1)
   ((raise-stmt)    $1)
   ((yield-stmt)    $1))

(gp testlist?)

(p raise-stmt ([raise]                       ) `([raise-stmt] nil nil nil))
(p raise-stmt ([raise] test                  ) `([raise-stmt] ,$2 nil nil))
(p raise-stmt ([raise] test [,] test         ) `([raise-stmt] ,$2 ,$4 nil))
(p raise-stmt ([raise] test [,] test [,] test) `([raise-stmt] ,$2 ,$4 ,$6))

;; import a, b        (import-stmt (((a)   nil) ((b) nil)))
;; import a, b as b2  (import-stmt (((a)   nil) ((b) b2)))
;; import a.b         (import-stmt (((a b) nil)))
;; import a.b as c    (import-stmt (((a b) c)))
;; from a   import b, c        (import-from-stmt (a)   ((b nil) (c nil)))
;; from a.b import b, c as c2  (import-from-stmt (a b) ((b nil) (c c2)))
;; from a   import *           (import-from-stmt (a)   *)
;; from a.b import *           (import-from-stmt (a b) *)
(p import-stmt :or
   ((import-normal) $1)
   ((import-from)   $1))

(p import-normal ([import] dotted-as-name comma--dotted-as-name*) `([import-stmt] (,$2 ,@$3)))
(gp comma--dotted-as-name*)
(p comma--dotted-as-name ([,] dotted-as-name) $2)

(p import-from ([from] dotted-name [import] import-from-2) `([import-from-stmt] ,$2 ,$4))
(p import-from-2 :or
   (([*]) $1)
   ((import-as-name comma--import-as-name*) (cons $1 $2)))

(gp comma--import-as-name*)
(p comma--import-as-name ([,] import-as-name) $2)

(p import-as-name ([identifier])                   `(,$1 nil))
(p import-as-name ([identifier] [as] [identifier]) `(,$1 ,$3))
(p dotted-as-name (dotted-name)                    `(,$1 nil))
(p dotted-as-name (dotted-name [as] [identifier] ) `(,$1 ,$3))

(p dot--name ([.] [identifier]) $2)
(gp dot--name*)
(p dotted-name ([identifier] dot--name*) `(,$1 ,@$2))

(p global-stmt ([global] [identifier] comma--identifier*)
   `([global-stmt] ([tuple-expr] ,(let ((this `([identifier-expr] ,$2)))
                                     (if $3 (cons this $3) (list this))))))

(gp comma--identifier*)
(p comma--identifier ([,] [identifier]) `([identifier-expr] ,$2))

(p exec-stmt ([exec] expr                   ) `([exec-stmt] ,$2 nil nil))
(p exec-stmt ([exec] expr [in] test         ) `([exec-stmt] ,$2 ,$4 nil))
(p exec-stmt ([exec] expr [in] test [,] test) `([exec-stmt] ,$2 ,$4 ,$6))

(p assert-stmt ([assert] test comma--test?) `([assert-stmt] ,$2 ,$3))
(gp comma--test?)
(p comma--test ([,] test) $2)

(p compound-stmt :or
   ((if-stmt) $1)
   ((while-stmt) $1)
   ((for-stmt) $1)
   ((try-stmt) $1)
   ((with-stmt) $1)
   ((funcdef) $1)
   ((classdef) $1))

(p if-stmt ([if] test [:] suite elif--test--suite* else--suite?) `([if-stmt] ((,$2 ,$4) ,@$5) ,$6))
(gp elif--test--suite*)
(p elif--test--suite ([elif] test [:] suite) (list $2 $4))
(p else--suite ([else] [:] suite) $3)
(p while-stmt ([while] test [:] suite else--suite?) `([while-stmt] ,$2 ,$4 ,$5))
(gp else--suite?)
(p for-stmt ([for] exprlist [in] testlist [:] suite else--suite?)
   `([for-in-stmt] ,$2 ,$4 ,$6 ,$7) #+(or)(:precedence high-prec))

(p try-stmt :or
   (([try] [:] suite except--suite+ else--suite?) `([try-except-stmt] ,$3 ,$4 ,$5))
   (([try] [:] suite [finally] [:] suite)         `([try-finally-stmt] ,$3 ,$6))
   
   ;; PEP 341 - "Unifying try-except and try-finally"
   (([try] [:] suite except--suite+ else--suite? [finally] [:] suite)
    `([try-finally-stmt] ([suite-stmt] (([try-except-stmt] ,$3 ,$4 ,$5))) ,$8)))

(p except--suite ([except]               [:] suite) `(nil nil ,$3))
(p except--suite ([except] test          [:] suite) `(,$2 nil ,$4))
(p except--suite ([except] test [,] test [:] suite) `(,$2 ,$4 ,$6))

(p except--suite+ (except--suite) (list $1))
(p except--suite+ (except--suite+ except--suite) (nconc $1 (list $2)))

(p with-stmt ([with] test           [:] suite) `([with-stmt] ,$2 nil ,$4))
(p with-stmt ([with] test [as] expr [:] suite) `([with-stmt] ,$2 ,$4 ,$6))

(p suite :or
   ((simple-stmt)                       `([suite-stmt] (,$1)))
   (([newline] [indent] stmt+ [dedent]) `([suite-stmt] ,$3)))

(p stmt+ (stmt)       (list $1))
(p stmt+ (stmt+ stmt) (nconc $1 (list $2)))

(p expr (binop2-expr) $1)

(p test :or
   ((lambdef) $1)
   ((binop-expr) $1))

(p binop-expr :or
   ((binop-expr [and] binop-expr) `([binary-lazy-expr] ,$2 ,$1 ,$3))
   ((binop-expr [or]  binop-expr) `([binary-lazy-expr] ,$2 ,$1 ,$3))
   (([not] binop-expr)            `([unary-expr]           ,$1 ,$2))
   ((binop-expr [<]  binop-expr)  `([comparison-expr]  ,$2 ,$1 ,$3))
   ((binop-expr [<=] binop-expr)  `([comparison-expr]  ,$2 ,$1 ,$3))
   ((binop-expr [>]  binop-expr)  `([comparison-expr]  ,$2 ,$1 ,$3))
   ((binop-expr [>=] binop-expr)  `([comparison-expr]  ,$2 ,$1 ,$3))
   ((binop-expr [!=] binop-expr)  `([comparison-expr]  ,$2 ,$1 ,$3))
   ((binop-expr [==] binop-expr)  `([comparison-expr]  ,$2 ,$1 ,$3))
   ((binop-expr [in] binop-expr)  `([binary-expr]      ,$2 ,$1 ,$3))
   ((binop-expr [is] binop-expr)  `([binary-expr]      ,$2 ,$1 ,$3))
   ((binop-expr [not in] binop-expr) `([binary-expr] [not in] ,$1 ,$3))
   ((binop-expr [is not] binop-expr) `([binary-expr] [is not] ,$1 ,$3)))

(p binop-expr (binop2-expr) $1)

(p binop2-expr :or
   ((atom) $1)
   ((atom trailer+)                (parse-trailers `(trailers ,$1 ,$2)))
   ((binop2-expr [+]  binop2-expr) `([binary-expr] ,$2 ,$1 ,$3))
   ((binop2-expr [-]  binop2-expr) `([binary-expr] ,$2 ,$1 ,$3))
   ((binop2-expr [*]  binop2-expr) `([binary-expr] ,$2 ,$1 ,$3))
   ((binop2-expr [/]  binop2-expr) `([binary-expr] ,$2 ,$1 ,$3))
   ((binop2-expr [**] binop2-expr) `([binary-expr] ,$2 ,$1 ,$3))
   ((binop2-expr [//] binop2-expr) `([binary-expr] ,$2 ,$1 ,$3))
   ((binop2-expr [<<] binop2-expr) `([binary-expr] ,$2 ,$1 ,$3))
   ((binop2-expr [>>] binop2-expr) `([binary-expr] ,$2 ,$1 ,$3))
   ((binop2-expr [&]  binop2-expr) `([binary-expr] ,$2 ,$1 ,$3))
   ((binop2-expr [^]  binop2-expr) `([binary-expr] ,$2 ,$1 ,$3))
   ((binop2-expr [\|] binop2-expr) `([binary-expr] ,$2 ,$1 ,$3))
   ((            [~]  binop2-expr) `([unary-expr]      ,$1 ,$2))
   ((binop2-expr [%]  binop2-expr) `([binary-expr] ,$2 ,$1 ,$3)))

(p binop2-expr ([+] binop2-expr) `([unary-expr] ,$1 ,$2) (:precedence 'unary-plusmin))
(p binop2-expr ([-] binop2-expr) `([unary-expr] ,$1 ,$2) (:precedence 'unary-plusmin))

(p atom :or
   (( [(] comma?        [)]  )  `([tuple-expr] nil)      )
   (( [(] testlist-gexp [)]  )  $2                       )
   (( [[]               [\]] )  `([list-expr] nil)       )
   (( [[] listmaker     [\]] )  $2                       )
   (( [{]               [}]  )  `([dict-expr] nil)       )
   (( [{] dictmaker     [}]  )  `([dict-expr] ,$2)       )
   (( [`] testlist1     [`]  )  `([backticks-expr] ,$2)  )
   (( [identifier]           )  `([identifier-expr] ,$1) )
   (( [number]               )  $1                       )
   (( [.] [number]           ) (cond ;; little hack for floats starting with dot, like ".5"
                                ((integerp $2)
                                 (let ((str (format nil "0.~Ad0" $2)))
                                   (with-standard-io-syntax (read-from-string str))))
                                ((and (complexp $2)
                                      (zerop (realpart $2))
                                      (integerp (imagpart $2)))
                                 (let ((str (format nil "0.~Ad0" (imagpart $2))))
                                   (complex 0 (with-standard-io-syntax (read-from-string str)))))
                                (t (raise-syntax-error
                                    "Invalid format for number starting with dot: .~G" $2))))
   (( string+                )  $1                       ))

;; consecutive string literals are joined: "s" "b" => "sb"
(p string+ ([string]) $1)
(p string+ (string+ [string]) (concatenate 'string $1 $2))

(p listmaker (test list-for) `([listcompr-expr] ,$1 ,$2))
(p listmaker (test comma--test* comma?) `([list-expr] ,(cons $1 $2)))
(gp comma--test*)

(p testlist-gexp (test gen-for) `([generator-expr] ,$1 ,$2))
(p testlist-gexp (test comma--test* comma?) (if (or $2 $3) `([tuple-expr] (,$1 . ,$2)) $1))
 
(p lambdef ([lambda] parameters [:] test) `([lambda-expr] ,$2 ,$4))

(p trailer+ :or
   (( [(] arglist      [)]            )  `(([call-expr] ,$2))                                     )
   (( [[] subscriptlist [\]]          )  `(([subscription-expr] ,$2))                             )
   (( [.] [identifier]                )  `(([attributeref-expr] ([identifier-expr] ,$2)))         )
   (( trailer+ [(] arglist       [)]  )  `(,@$1 ([call-expr] ,$3))                                )
   (( trailer+ [[] subscriptlist [\]] )  `(,@$1 ([subscription-expr] ,$3))                        )
   (( trailer+ [.] [identifier]       )  `(,@$1 ([attributeref-expr] ([identifier-expr] ,$3))))   )

(p subscriptlist (subscript comma--subscript* comma?) (if (or $2 $3) `([tuple-expr] (,$1 . ,$2)) $1))
(gp comma--subscript*)
(p comma--subscript ([,] subscript) $2)
(p subscript :or
   (([...]) `([identifier-expr] clpython.user.builtin.value:|Ellipsis|)) ;; Ugly rule
   ((test)   $1)
   ((test? [:] test? sliceop?) `([slice-expr] ,$1 ,$3 ,$4)))
(gp sliceop?)
(p sliceop ([:] test?) $2)
(gp test?)

(p exprlist (expr exprlist2) (if (not (equal $2 '(nil)))
                                 `([tuple-expr] (,$1 ,@(butlast $2)))
                               $1))

(p exprlist2 :or
           (()                    `(nil)     )
           (([,])                 `(t)       )
           (([,] expr exprlist2)  `(,$2 ,@$3)))

(p testlist (test testlist2) (if $2
                                 `([tuple-expr] ,(cons $1 (if (eq (car (last $2)) :dummy)
                                                              (butlast $2)
                                                            $2)))
                               $1))
 
(p testlist2 :or
           (()                   nil)
           (([,])                (list :dummy))
           (([,] test testlist2) (if (eq (car $3) :dummy)
                                     $2
                                   (cons $2 $3))))

(p testlist-safe (test testlist-safe2) (if $2
                                           `([tuple-expr] ,(cons $1 (if (eq (car (last $2)) :dummy)
                                                                        (butlast $2)
                                                                      $2)))
                                         $1))
(p testlist-safe2 :or
   (()                         nil)
   (([,])                      (list :dummy))
   (([,] test testlist-safe2)  (if (eq (car $3) :dummy)
                                   $2
                                 (cons $2 $3))))

(p dictmaker (test [:] test comma--test--\:--test* comma?) (cons (cons $1 $3) $4))
(gp comma--test--\:--test*)
(p comma--test--\:--test ([,] test [:] test) (cons $2 $4))

(p classdef ([class] [identifier] inheritance [:] suite)
   `([classdef-stmt] ([identifier-expr] ,$2) ,$3 ,$5))

(p inheritance (                ) '([tuple-expr] nil))
(p inheritance ([(]          [)]) '([tuple-expr] nil))
(p inheritance ([(] testlist [)]) (if (eq (car $2) '[tuple-expr]) $2 `([tuple-expr] (,$2))))

(p arglist () `(nil nil nil nil))
(p arglist (argument--comma* arglist-2) 
   (destructuring-bind (a *-a **-a) $2
     (when a (if $1
                 (nconc $1 (list a))
               (setf $1 (list a))))
     (let ((key-start (position :key $1 :key 'car))
           (pos-end   (position :pos $1 :key 'car :from-end t)))
       (multiple-value-bind (pos key)
           (cond ((and key-start pos-end)
                  (when (< key-start pos-end)
                    (raise-syntax-error
                     "Postional argument was found after keyword argument `~A = ...'."
                     (cadadr (nth key-start $1))))
                  (let ((pos-args $1)
                        (key-args (nthcdr key-start $1)))
                    (setf (cdr (nthcdr pos-end $1)) nil)
                    (values pos-args key-args)))
                 (key-start (values () $1))
                 (pos-end   (values $1 ()))
                 (t         (values () ())))
         (map-into pos 'second pos)
         (map-into key 'cdr key)
         (list pos key *-a **-a)))))
(p arglist-2 :or
   ((argument comma?)            (list  $1 nil nil))
   (([*]  test comma--**--test?) (list nil  $2  $3))
   (([**] test)                  (list nil nil  $2)))
 
(gp argument--comma*)
(p argument--comma (argument [,]) $1)
(gp comma--**--test?)
(p comma--**--test ([,] [**] test) $3)

(p argument (test) `(:pos ,$1))
(p argument ([identifier] [=] test) `(:key ([identifier-expr] ,$1) ,$3))
(p argument (test gen-for) `(:pos ([generator-expr] ,$1 ,$2)))

(p list-iter :or 
   ((list-for) $1) 
   ((list-if) $1))
(p list-for ([for] exprlist [in] testlist-safe list-iter?) `(([for-in-clause] ,$2 ,$4) . ,$5))
(gp list-iter?)
(p list-if ([if] test list-iter?) `(([if-clause] ,$2) . ,$3))
 
(p gen-iter :or
   ((gen-for) $1)
   ((gen-if) $1))
(p gen-for ([for] exprlist [in] test gen-iter?) `(([for-in-clause] ,$2 ,$4) . ,$5))
(p gen-if  ([if]  test               gen-iter?) `(([if-clause] ,$2) . ,$3))
(gp gen-iter?)

(p testlist1 (test |,--test*|) (if $2 `([tuple-expr] (,$1 . ,$2)) $1))


(defun parse-trailers (ast)
  ;; foo[x].a => (trailers (id foo) ((subscription (id x)) (attributeref (id a))))
  ;;          => (attributeref (subscription (id foo) (id x)) (id a))
  (declare (optimize speed))
  (assert (eq (car ast) 'trailers))
  (destructuring-bind (x trailers)
      (cdr ast)
    (dolist (tr trailers)
      (setf (cdr tr) (cons x (cdr tr))
            x        tr))
    x))

(defun dotted-name-to-attribute-ref (dotted-name)
  (assert dotted-name)
  (let ((res `([identifier-expr] ,(pop dotted-name))))
    (dolist (x dotted-name res)
      (setf res `([attributeref-expr] ,res ([identifier-expr] ,x))))))


;; Register AST node patterns. The pattern matcher in the compiler checks
;; structural consistency with these entries.

(defparameter *ast-patterns* (make-hash-table :test 'eq)
  "The patterns of all AST nodes.")

(defun register-ast-pattern (ast-node args)
  (assert (symbolp ast-node))
  (assert (every #'symbolp args) ()
    "Pattern members must be symbols (not e.g. sublists)")
  (let ((tail (loop for x in args
                  do (assert (char/= (aref (string x) 0) #\?))
                  collect (intern (concatenate 'string "?" (string x)) #.*package*))))
    (setf (gethash ast-node *ast-patterns*) (cons ast-node tail))))

(defun get-ast-pattern (ast-node)
  (gethash ast-node *ast-patterns*))

(defun reg-patterns ()
  (macrolet ((rp (ast-node &rest args)
               `(progn (register-ast-pattern ',ast-node ',args)
                       (defstruct (,ast-node
                                   :named
                                   (:type list)
                                   (:constructor ,(intern (format nil "make-~A" ast-node) :clpython.ast))
                                   (:predicate ,(intern (format nil "~A-p" ast-node) :clpython.ast)))
                         ,@args))))
    
    (rp [assert-stmt] test raise-arg)
    (rp [assign-stmt] value targets)
    (rp [attributeref-expr] item attr)
    (rp [augassign-stmt] op place val)
    (rp [backticks-expr] item)
    (rp [binary-expr] op left right)
    (rp [binary-lazy-expr] op left right)
    (rp [break-stmt])
    (rp [call-expr] primary all-args)
    (rp [classdef-stmt] name inheritance suite)
    (rp [comparison-expr] cmp left right)
    (rp [continue-stmt])
    (rp [del-stmt] item)
    (rp [dict-expr] alist)
    (rp [exec-stmt] code globals locals)
    (rp [for-in-stmt] target source suite else-suite)
    (rp [funcdef-stmt] decorators fname args suite)
    (rp [generator-expr] item for-in/if-clauses)
    (rp [global-stmt] names)
    (rp [identifier-expr] name)
    (rp [if-stmt] if-clauses else-clause)
    (rp [import-stmt] items)
    (rp [import-from-stmt] mod-name-as-list items)
    (rp [lambda-expr] args expr)
    (rp [listcompr-expr] item for-in/if-clauses)
    (rp [list-expr] items)
    (rp [module-stmt] suite)
    (rp [pass-stmt] )
    (rp [print-stmt] dest items comma?)
    (rp [raise-stmt] exc var tb)
    (rp [return-stmt] val)
    (rp [slice-expr] start stop step)
    (rp [subscription-expr] item subs)
    (rp [suite-stmt] stmts)
    (rp [try-except-stmt] suite except-clauses else-suite)
    (rp [try-finally-stmt] try-suite finally-suite)
    (rp [tuple-expr] items)
    (rp [unary-expr] op item)
    (rp [while-stmt] test suite else-suite)
    (rp [with-stmt] test var suite)
    (rp [yield-stmt] val))
    
  ;; Some shortcuts
  (defun [make-identifier-expr*] (name)
    (check-type name symbol)
    ([make-identifier-expr] :name name))
  
  (defun [make-suite-stmt*] (&rest args)
    ([make-suite-stmt] :stmts args))
  
  (defun [make-assign-stmt*] (&key value target)
    ([make-assign-stmt] :value value :targets (list target))))

(reg-patterns)


