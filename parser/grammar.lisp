;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.PARSER; Readtable: PY-AST-READTABLE -*-
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'raise-syntax-error :clpython))

;; Register AST node patterns. The pattern matcher in the compiler checks
;; structural consistency with these entries.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ast-forms* ;; TODO combine with DEF-AST-NODE in walk.lisp
    '(([assert-stmt] test raise-arg)
      ([assign-stmt] value targets)
      ([attributeref-expr] item attr)
      ([augassign-stmt] op place val)
      ([backticks-expr] item)
      ([binary-expr] op left right)
      ([binary-lazy-expr] op left right)
      ([bracketed-expr] expr)
      ([break-stmt])
      ([call-expr] primary pos key *a **a)
      ([classdef-stmt] name inheritance suite)
      ([comparison-expr] cmp left right)
      ([continue-stmt])
      ([del-stmt] item)
      ([dict-expr] alist)
      ([exec-stmt] code globals locals)
      ([for-in-stmt] target source suite else-suite)
      ([funcdef-stmt] decorators fname args suite)
      ([generator-expr] item for-in/if-clauses)
      ([global-stmt] names)
      ([identifier-expr] name)
      ([if-expr] condition then else)
      ([if-stmt] if-clauses else-clause)
      ([import-stmt] items)
      ([import-from-stmt] mod-name-as-list items)
      ([lambda-expr] args expr)
      ([listcompr-expr] item for-in/if-clauses)
      ([list-expr] items)
      ([module-stmt] suite)
      ([pass-stmt] )
      ([print-stmt] dest items comma?)
      ([raise-stmt] exc var tb)
      ([return-stmt] val)
      ([slice-expr] start stop step)
      ([subscription-expr] item subs)
      ([suite-stmt] stmts)
      ([try-except-stmt] suite except-clauses else-suite)
      ([try-finally-stmt] try-suite finally-suite)
      ([tuple-expr] items)
      ([unary-expr] op item)
      ([while-stmt] test suite else-suite)
      ([with-stmt] test var suite)
      ([yield-expr] val)
      ([yield-stmt] val))))

(defun get-ast-pattern (ast-node)
  (check-type ast-node symbol)
  (let ((rec (or (find ast-node *ast-forms* :key #'car)
                 (error "Unknown ast node: ~A." ast-node))))
    (cons ast-node  (loop for x in (cdr rec) collect (make-symbol (format nil "?~A" x))))))

#.`(progn ,@(loop for x in (mapcar #'car *ast-forms*)
                for fname = (intern (format nil "~A-p" (string-downcase x)) :clpython.ast.node-p)
                collect `(defun ,fname (x)
                           (and (listp x) (eq (car x) ',x)))))


;;; Grammar definition

(defparameter *precedence-and-associativity*
    '((left [or])
      (left [and])
      (left [not])
      (left [in] [not in])
      (left [is] [is not])
      (left [<] [<=] [>] [>=] [!=] [==])
      (left [\|])
      (left [^])
      (left [&])
      (left [<<] [>>])
      (left [+] [-]) ;; binary [+] [-]
      (left [*] [/] [//] [%])
      ;; unary [+] [-] goes here
      (left [~])
      (right [**]))
  "Precedence and associativity rules, ordered by increasing precedence.")

(defparameter *operators-with-precedence-between-binary-and-unary-plusmin* '([*] [/] [//] [%])
  "Operators that introduce AST errors due to unary +/- differing in precedence from binary +/-.")

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
              nconc (loop for s being each external-symbol in pkg
                        collect s))
          #'string<)
  "Terminal tokens")

(defparameter *python-prods* (make-hash-table :test 'eq)
  "Hashtable containing all grammar rules")

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun make-number-token (n)
  (check-type n (integer 1 #.most-positive-fixnum))
  (intern (format nil "$~D" n) #.*package*))
) ;; eval-when

(defmacro p (name &rest rules)
  (when (eq (car rules) :or)
    (return-from p `(progn ,@(loop for rule in (cdr rules)
                                 do (check-type rule list)
                                 collect `(p ,name ,@rule)))))
  (destructuring-bind (terms outcome #| &optional options |#) rules
    (check-type terms list)
    (flet ((token-suffix (x)
             (whereas ((sn (and (symbolp x)
                                (not (eq (symbol-package x) #.(find-package :clpython.ast.operator))) ;; skip "**" etc
                                (symbol-name x))))
               (aref sn (1- (length sn))))))
      (unless (some (lambda (x) (member (token-suffix x) '(#\? #\*))) terms)
        (return-from p `(add-rule ',name ',terms ',outcome #| ,@options |# )))
      (labels ((remove-token-suffix (x)
                 (check-type x symbol)
                 (let ((sn (symbol-name x)))
                   (assert (member (aref sn (- (length sn) 1)) '(#\? #\*)))
                   (intern (subseq sn 0 (- (length sn) 1)) #.*package*)))
               (change-token-suffix (x suffix)
                 (check-type suffix character)
                 (let ((s (remove-token-suffix x)))
                   (intern (format nil "~A~C" s suffix) #.*package*))))
        (flet ((shift-outcome (removed-$)
                 `(let ((,(make-number-token removed-$) nil)
                        ,@(loop for i from (1+ removed-$) to (length terms)
                              collect (list (make-number-token i) (make-number-token (1- i)))))
                    (declare (ignorable ,@(loop for i from removed-$ to (length terms) collect (make-number-token i))))
                    ,outcome)))
          (loop for term in terms
              for term-ix from 0
              when (member (token-suffix term) '(#\? #\*))
              do (let ((with (copy-list terms))
                       (without (copy-list terms)))
                   (if (zerop term-ix)
                       (pop without)
                     (setf (cdr (nthcdr (1- term-ix) without))
                       (nthcdr (1+ term-ix) without)))
                   (setf (nth term-ix with)
                     (ecase (token-suffix term)
                       (#\? (remove-token-suffix term))
                       (#\* (change-token-suffix term #\+))))
                   (return-from p
                     `(progn (p ,name ,with ,outcome #| ,@options |# )
                             (p ,name ,without ,(shift-outcome (1+ term-ix)) #| ,@options |# ))))))))))

;;; Source location recording

(defparameter *python-form->source-location* nil
  "EQ hashtable, mapping AST subforms to source position. Used during file compilation.
Value should be a (weak) EQ hash table: (make-weak-key-hash-table :test 'eq).")

(defparameter *module->source-positions* (make-hash-table :test 'equal)
  "EQ hashtable, mapping from pathname to source data. Filled during loading.")

(defstruct (literal (:constructor make-literal (value)))
  value)

(defun maybe-unwrap-literal-value (x)
  (if (literal-p x)
      (literal-value x)
    x))
    
(defun record-source-location (outcome start end)
  "Records location in *python-form->source-location*"
  (check-type outcome (or list literal number string symbol)) ;; XXX reduce number of possibilities
  (when *python-form->source-location*
    (let ((existing (gethash outcome *python-form->source-location*))
          (loc (list :start start :end end)))
      (cond ((not existing)
             (setf (gethash outcome *python-form->source-location*) loc))
            (existing
             (unless (<= start (getf existing :start) (getf existing :end) end)
               "Duplicate location for ~A, and new [~A, ~A] does not overlap old [~A, ~A]"
               outcome start end (getf existing :start) (getf existing :end)))))))

(defun contour-source-location (terms)
  (when *python-form->source-location*
    (loop for term in terms
        for pos = (gethash term *python-form->source-location*)
        for some-pos = (or some-pos pos)
        when (progn #+(or)(warn "pos of ~A is ~A" term pos) pos)
        minimize (getf pos :start) into min and maximize (getf pos :end) into max
        finally (return (when some-pos (values min max))))))

(defun instrument-outcome (terms outcome)
  (if (and (= 1 (length terms))
           (equal outcome '$1))
      (return-from instrument-outcome outcome)
    (let* (($vars (loop for i from 1 to (length terms)
                      collect (make-number-token i))))
      `(progn (with-stack-list (.terms ,@$vars)
                (multiple-value-bind (.min .max)
                    (contour-source-location .terms)
                  (let ((.outcome ,outcome))
                    (when .min (record-source-location .outcome .min .max))
                    .outcome)))))))

(defun add-rule (name terms outcome &rest options)        
  ;; Enable reduction trackin, for source form location recording purposes
  (let ((out (instrument-outcome terms outcome)))
    (pushnew (list terms out options) (gethash name *python-prods*) :test 'equal)))

(defmacro gp (name)
  "Generate a production rule:
 Y+ => ITEM [, ITEM [, ITEM [...]]
 Y* => Y [Y [Y p...]]
 Y? => Y or nothing"
  (check-type name symbol)
  (assert (eq (symbol-package name) #.*package*))
  (let* ((str  (symbol-name name))
         (len  (length str))
         (item-name (subseq str 0 (- len 1)))
         (item (or (find-symbol (string-downcase item-name) :clpython.ast)
                   (intern item-name #.*package*))))
    (ecase (aref str (1- len))
      (#\+ `(progn (add-rule ',name '(,item) '(list $1))
                   ;; using APPEND instead of NCONC, as latter screws up source form positions
                   (add-rule ',name '(,name ,item) '(append $1 (list $2))))))))
  
;; These rules, including most names, are taken from the CPython
;; grammar file from CPython CVS, file Python/Grammar/Grammar,
;; 20040827.
;; Try/except/finally-stmt and with-stmt added later.

(p python-grammar (one-stmt-input) (finish-ast $1))

(p one-stmt-input (stmt)      $1)
(p one-stmt-input (stmt [newline])      $1)

(p decorator ([@] dotted-name                 [newline]) (dotted-name-to-attribute-ref $2))
(p decorator ([@] dotted-name [(]         [)] [newline]) `([call-expr] ,(dotted-name-to-attribute-ref $2) nil nil nil nil))
(p decorator ([@] dotted-name [(] arglist [)] [newline]) `([call-expr] ,(dotted-name-to-attribute-ref $2) ,@$4))

(gp decorator+)

(p funcdef (decorator* [def] [identifier] [(] [)] [:] suite)
   `([funcdef-stmt] ,$1 ([identifier-expr] ,$3) (nil nil nil nil) ,$7))
(p funcdef (decorator* [def] [identifier] [(] parameters [)] [:] suite) 
   `([funcdef-stmt] ,$1 ([identifier-expr] ,$3) ,$5 ,$8))

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

(p defparameter+ (defparameter) (list $1))
(p defparameter+ (defparameter+ comma defparameter) (nconc $1 (list $3)))

(p ni-*-ident ([,] *-ident     ) $2)
(p    *-ident ([*] [identifier]) $2)

(p ni-**-ident ([,] **-ident     ) $2)
(p    **-ident ([**] [identifier]) $2)

(p parameter-list5 (defparameter+                        comma?) (list  $1 nil nil))
(p parameter-list5 (defparameter+ ni-*-ident ni-**-ident comma?) (list  $1  $2  $3))
(p parameter-list5 (defparameter+ ni-*-ident             comma?) (list  $1  $2 nil))
(p parameter-list5 (defparameter+            ni-**-ident comma?) (list  $1 nil  $2))
(p parameter-list5 (              *-ident    ni-**-ident comma?) (list nil  $1  $2))
(p parameter-list5 (              *-ident                comma?) (list nil  $1 nil))
(p parameter-list5 (                         **-ident    comma?) (list nil nil  $1))

;; Can't use symbol vs. cons for distinguishing positional and
;; keyword arguments, as as positional args may be a structure:
;;   def f((x,y), z, q=4): ...

(p fpdef :or
   (([identifier])    `([identifier-expr] ,$1))
   (([(] fplist [)] ) `([tuple-expr] ,$2)))

(p comma--fpdef ([,] fpdef) $2)
(gp comma--fpdef+)
(p fplist (fpdef comma--fpdef* comma?) (cons $1 $2))

(p comma ([,]) (list $1))

(p stmt :or
   ((simple-stmt) $1)
   ((compound-stmt) $1))

(p simple-stmt (small-stmt semi--small-stmt* semi? [newline])
   (if $2 `([suite-stmt] ,(cons $1 $2)) $1))

(p semi--small-stmt ([\;] small-stmt) $2)

(gp semi--small-stmt+)
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

(p expr-stmt :or
   ((testlist expr-stmt2)
    (cond ((null $2) $1) ;; not an assignment expression
          ((and $2 (eq (car $2) '[=]))
           (setf $2 (second $2))
           (let* ((val (car (last $2)))
                  (targets (cons $1 (nbutlast $2))))
             `([assign-stmt] ,val ,targets)))
          ($2
           `([augassign-stmt] ,(car $2) ,$1 ,(cdr $2)))
          (t $1)))
   ((testlist) $1)) 

(gp =--testlist+)
(p =--testlist ([=] testlist) $2)
(p =--testlist ([=] yield-expr) $2)

(p expr-stmt2 (=--testlist+)       (when $1 `([=] ,$1)))
(p expr-stmt2 (augassign testlist) (cons $1 $2))
(p expr-stmt2 (augassign yield-expr) (cons $1 $2))

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
   (([print] test comma--test* comma?)      `([print-stmt] nil (,$2 . ,$3) ,$4))
   (([print] [>>] test comma--test* comma?)  `([print-stmt] ,$3 ,$4 ,$5)))

(gp comma--test+)
(p comma--test ([,] test) $2)

(p del-stmt      ([del] exprlist)     `([del-stmt] ,$2))
(p pass-stmt     ([pass])             `([pass-stmt] ))
(p break-stmt    ([break])            `([break-stmt]))
(p continue-stmt ([continue])         `([continue-stmt]))

(p return-stmt :or
   (([return])           `([return-stmt] nil))
   (([return] testlist)  `([return-stmt] ,$2)))

(p yield-stmt    (yield-expr)         `([yield-stmt] ,@(cdr $1)))

(p yield-expr ([yield] testlist?) `([yield-expr] ,$2))

(p flow-stmt :or
   ((break-stmt)    $1)
   ((continue-stmt) $1)
   ((return-stmt)   $1)
   ((raise-stmt)    $1)
   ((yield-stmt)    $1))

(p raise-stmt ([raise]                       ) `([raise-stmt] nil nil nil))
(p raise-stmt ([raise] test                  ) `([raise-stmt] ,$2 nil nil))
(p raise-stmt ([raise] test [,] test         ) `([raise-stmt] ,$2 ,$4 nil))
(p raise-stmt ([raise] test [,] test [,] test) `([raise-stmt] ,$2 ,$4 ,$6))

(p import-stmt :or
   ((import-normal) $1)
   ((import-from)   $1))

(p import-normal ([import] dotted-as-name comma--dotted-as-name*) `([import-stmt] (,$2 ,@$3)))

(gp comma--dotted-as-name+)
(p comma--dotted-as-name ([,] dotted-as-name) $2)

(p import-from ([from] dotted-name [import] import-from-2) `([import-from-stmt] ,$2 ,$4))
(p import-from-2 :or
   (([*]) $1)
   ((import-as-name comma--import-as-name*) (cons $1 $2))
   (([(] import-as-name comma--import-as-name* [)]) (cons $2 $3)))

(gp comma--import-as-name+)
(p comma--import-as-name ([,] import-as-name) $2)

(p import-as-name ([identifier])                   `(,$1 nil))
(p import-as-name ([identifier] [as] [identifier]) `(,$1 ,$3))
(p dotted-as-name (dotted-name)                    `(,$1 nil))
(p dotted-as-name (dotted-name [as] [identifier] ) `(,$1 ,$3))

(p dot--name ([.] [identifier]) $2)
(gp dot--name+)
(p dotted-name ([identifier] dot--name*) `(,$1 ,@$2))

(p global-stmt ([global] [identifier] comma--identifier*)
   `([global-stmt] ([tuple-expr] ,(let ((this `([identifier-expr] ,$2)))
                                    (cons this $3)))))

(gp comma--identifier+)
(p comma--identifier ([,] [identifier]) `([identifier-expr] ,$2))

(p exec-stmt ([exec] expr                   ) `([exec-stmt] ,$2 nil nil))
(p exec-stmt ([exec] expr [in] test         ) `([exec-stmt] ,$2 ,$4 nil))
(p exec-stmt ([exec] expr [in] test [,] test) `([exec-stmt] ,$2 ,$4 ,$6))

(p assert-stmt ([assert] test comma--test?) `([assert-stmt] ,$2 ,$3))

(p compound-stmt :or
   ((if-stmt) $1)
   ((while-stmt) $1)
   ((for-stmt) $1)
   ((try-stmt) $1)
   ((with-stmt) $1)
   ((funcdef) $1)
   ((classdef) $1))

(p if-stmt ([if] test [:] suite elif--test--suite* else--suite?) `([if-stmt] ((,$2 ,$4) ,@$5) ,$6))
(gp elif--test--suite+)
(p elif--test--suite ([elif] test [:] suite) (list $2 $4))
(p else--suite ([else] [:] suite) $3)
(p while-stmt ([while] test [:] suite else--suite?) `([while-stmt] ,$2 ,$4 ,$5))

(p for-stmt ([for] exprlist [in] testlist [:] suite else--suite?)
   `([for-in-stmt] ,$2 ,$4 ,$6 ,$7))

(p try-stmt :or
   (([try] [:] suite except--suite+ else--suite?) `([try-except-stmt] ,$3 ,$4 ,$5))
   (([try] [:] suite [finally] [:] suite)         `([try-finally-stmt] ,$3 ,$6))
   
   ;; PEP 341 - "Unifying try-except and try-finally"
   (([try] [:] suite except--suite+ else--suite? [finally] [:] suite)
    `([try-finally-stmt] ([suite-stmt] (([try-except-stmt] ,$3 ,$4 ,$5))) ,$8)))

(p except--suite ([except]               [:] suite) `(nil nil ,$3))
(p except--suite ([except] test          [:] suite) `(,$2 nil ,$4))
(p except--suite ([except] test [,] test [:] suite) `(,$2 ,$4 ,$6))

(gp except--suite+)

(p with-stmt ([with] test           [:] suite) `([with-stmt] ,$2 nil ,$4))
(p with-stmt ([with] test [as] expr [:] suite) `([with-stmt] ,$2 ,$4 ,$6))

(p suite :or
   ((simple-stmt)                       `([suite-stmt] (,$1)))
   (([newline] [indent] stmt+ [dedent]) `([suite-stmt] ,$3)))

(gp stmt+)

(p expr (binop2-expr) $1)

(p test :or
   ((lambdef) $1)
   ((binop-expr) $1)
   ((binop-expr [if] binop-expr [else] test) `([if-expr] ,$3 ,$1 ,$5)))

;; These `old-' rules are named after corresponding CPython grammar rule (20071230).
(p old-test :or 
   ((binop-expr) $1)
   ((old-lambdef) $1))
   
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
   ((atom)                          $1)
   ((atom trailer+)                 (parse-trailers $1 $2))
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

(p binop2-expr ([+] binop2-expr) (maybe-fix-unary-expr `([unary-expr] ,$1 ,$2)))
(p binop2-expr ([-] binop2-expr) (maybe-fix-unary-expr `([unary-expr] ,$1 ,$2)))

(p atom :or
   (( [(] yield-expr    [)]  )  $2                       )
   (( [(] comma?        [)]  )  `([tuple-expr] nil)      )
   (( [(] testlist-gexp [)]  )  $2                       )
   (( [[]               [\]] )  `([list-expr] nil)       )
   (( [[] listmaker     [\]] )  $2                       )
   (( [{]               [}]  )  `([dict-expr] nil)       )
   (( [{] dictmaker     [}]  )  `([dict-expr] ,$2)       )
   (( [`] testlist1     [`]  )  `([backticks-expr] ,$2)  )
   (( [identifier]           )  `([identifier-expr] ,$1) )
   (( [number]               )  $1                       )
   (( [.] [number]           ) (let (($2val (literal-value $2))
                                     (suffix (float-suffix *normal-float-representation-type*)))
                                 ;; A float value starting with a dot, like ".5"
                                 ;; As NUMBER is a fraction between 0 and 1, it always falls in the range
                                 ;; of the float representation type.
                                 (make-literal
                                  (typecase $2val
                                    (integer (let ((str (format nil "0.~A~A0" $2val suffix)))
                                               (with-standard-io-syntax (read-from-string str))))
                                    (complex (assert (zerop (realpart $2val)))
                                             (assert (typep (imagpart $2val) 'integer))
                                             (let ((str (format nil "0.~A~A0" (imagpart $2val) suffix)))
                                               (complex 0 (with-standard-io-syntax (read-from-string str)))))
                                    (t (raise-syntax-error
                                        "Invalid format for number starting with dot: .~G" $2val))))))
   (( string+ )
    ;; consecutive string literals are joined: "ab" "c" => "abc"
    (make-literal (apply #'concatenate 'string (mapcar #'literal-value $1)))))

(gp string+)

(p listmaker (test list-for) `([listcompr-expr] ,$1 ,$2))
(p listmaker (test comma--test* comma?) `([list-expr] ,(cons $1 $2)))

(p testlist-gexp (test gen-for) `([generator-expr] ,$1 ,$2))
(p testlist-gexp (test comma--test* comma?) (if (or $2 $3)
                                                `([tuple-expr] (,$1 . ,$2))
                                              ;; Brackets are needed to fix unary expressions.
                                              `([bracketed-expr] ,$1)))

(p lambdef ([lambda] [:] test)                    `([lambda-expr] (nil nil nil nil) ,$3))
(p lambdef ([lambda] parameters [:] test)         `([lambda-expr] ,$2 ,$4))
(p old-lambdef ([lambda] [:] old-test)            `([lambda-expr] (nil nil nil nil) ,$3))
(p old-lambdef ([lambda] parameters [:] old-test) `([lambda-expr] ,$2 ,$4))

(p trailer :or
   (( [(] [)]                ) `(call-trailer nil nil nil nil))
   (( [(] arglist [)]        ) `(call-trailer ,@$2))
   (( [[] subscriptlist [\]] ) `(subs-trailer ,$2))
   (( [.] [identifier]       ) `(attr-trailer ([identifier-expr] ,$2))))

(gp trailer+)

(p subscriptlist (subscript comma--subscript* comma?) (if (or $2 $3) `([tuple-expr] (,$1 . ,$2)) $1))

(gp comma--subscript+)
(p comma--subscript ([,] subscript) $2)

(p subscript :or
   (([...]) `([identifier-expr] clpython.user.builtin.value:|Ellipsis|)) ;; Ugly rule
   ((test)   $1)
   ((test? [:] test? sliceop?) `([slice-expr] ,$1 ,$3 ,$4)))
(p sliceop ([:] test?) $2)

(p exprlist (expr)           $1)
(p exprlist (expr exprlist2) `([tuple-expr] (,$1 ,@(if (eq (car (last $2)) t)
                                                       (butlast $2)
                                                     $2))))
(p exprlist2 :or
           (([,])                 `(t))
           (([,] expr)            `(,$2))
           (([,] expr exprlist2)  `(,$2 ,@$3)))

(p testlist :or
   ((test)           $1)
   ((test testlist2) `([tuple-expr] (,$1 ,@(if (eq (car (last $2)) t)
                                               (butlast $2)
                                             $2)))))

(p testlist2 :or
           (([,])                `(t))
           (([,] test)           `(,$2))
           (([,] test testlist2) `(,$2 ,@$3)))

(p testlist-safe :or
   ((old-test testlist-safe2) (if $2
                                  `([tuple-expr] ,(cons $1 (if (eq (car (last $2))
                                                                   :dummy)
                                                               (butlast $2)
                                                             $2)))
                                $1))
   ((old-test)  $1))
   
(p testlist-safe2 :or
   (([,])                         (list :dummy))
   (([,] old-test)                (list $2))
   (([,] old-test testlist-safe2) (if (eq (car $3) :dummy)
                                      $2
                                    (cons $2 $3))))

(p dictmaker (test [:] test comma--test--\:--test* comma?)
   ;; Store items in order of eval: v1, k1, v2, k2, ..
   (loop for (k . v) in (acons $1 $3 $4) collect v collect k))

(gp comma--test--\:--test+)
(p comma--test--\:--test ([,] test [:] test) (cons $2 $4))

(p classdef ([class] [identifier] [:] suite)
   `([classdef-stmt] ([identifier-expr] ,$2) ([tuple-expr] nil) ,$4))
(p classdef ([class] [identifier] inheritance [:] suite)
   `([classdef-stmt] ([identifier-expr] ,$2) ,$3 ,$5))

(p inheritance ([(]          [)]) `([tuple-expr] nil))
(p inheritance ([(] testlist [)]) (if (eq (car $2) '[tuple-expr]) $2 `([tuple-expr] (,$2))))

(p arglist (arglist-2) (handle-arglist () $1))
(p arglist (argument--comma+ arglist-2) (handle-arglist $1 $2))

(defun handle-arglist ($1 $2)
  (destructuring-bind (a *-a **-a) $2
    (let* ((all-pos/key (nconc $1 (when a (list a))))
           (key-start (position :key all-pos/key :key 'car))
           (pos-end   (position :pos all-pos/key :key 'car :from-end t)))
      (when (and key-start pos-end (< key-start pos-end))
        (raise-syntax-error
         "Postional argument was found after keyword argument `~A = ...'."
         (cadadr (nth key-start all-pos/key))))
      (multiple-value-bind (pos key)
          (cond ((and key-start pos-end)
                 (let ((pos-args all-pos/key)
                       (key-args (nthcdr key-start all-pos/key)))
                   (setf (cdr (nthcdr pos-end all-pos/key)) nil)
                   (values pos-args key-args)))
                (key-start (values () all-pos/key))
                (pos-end   (values all-pos/key ()))
                (t         (values () ())))
        (map-into pos 'second pos)
        (map-into key 'cdr key)
        (list pos key *-a **-a)))))

(p arglist-2 :or
   ((argument comma?)            (list  $1 nil nil))
   (([*]  test comma--**--test?) (list nil  $2  $3))
   (([**] test)                  (list nil nil  $2)))
 
(gp argument--comma+)
(p argument--comma (argument [,]) $1)
(p comma--**--test ([,] [**] test) $3)

(p argument (test) `(:pos ,$1))
(p argument ([identifier] [=] test) `(:key ([identifier-expr] ,$1) ,$3))
(p argument (test gen-for) `(:pos ([generator-expr] ,$1 ,$2)))

(p list-iter :or 
   ((list-for) $1) 
   ((list-if) $1))
(p list-for ([for] exprlist [in] testlist-safe list-iter?) `(([for-in-clause] ,$2 ,$4) . ,$5))
(p list-if ([if] old-test list-iter?) `(([if-clause] ,$2) . ,$3))
 
(p gen-iter :or
   ((gen-for) $1)
   ((gen-if) $1))
(p gen-for ([for] exprlist [in] binop-expr gen-iter?) `(([for-in-clause] ,$2 ,$4) . ,$5))
(p gen-if  ([if]  old-test                 gen-iter?) `(([if-clause] ,$2) . ,$3))

(p testlist1 (test comma--test*) (if $2 `([tuple-expr] (,$1 . ,$2)) $1))


(defun parse-trailers (item trailers)
  ;; foo[x].a => (id foo) + ((subscription (id x)) (attributeref (id a)))
  ;;          => (attributeref (subscription (id foo) (id x)) (id a))
  (dolist (tr trailers item)
    (let ((head (or (second (assoc (car tr) '((call-trailer [call-expr])
                                              (subs-trailer [subscription-expr])
                                              (attr-trailer [attributeref-expr]))))
                    (error "Unknown trailer: ~S" tr))))
      ;; Don't modify first cons: might be a constant.
      (setf item `(,head ,item ,@(cdr tr))))))

(defun maybe-fix-unary-expr (unary-expr)
  "Fix AST so that unary +/- has higher precedence than *,/,//,%.
For example: - (1 * 2) => (-1) * 2"
  ;; Should be fixable in the grammar too.
  (assert ([unary-expr-p] unary-expr))
  (destructuring-bind (unary-op unary-val) (cdr unary-expr)
    (when ([binary-expr-p] unary-val)
      (destructuring-bind (binary-op bin-left bin-right) (cdr unary-val)
        (when (member binary-op *operators-with-precedence-between-binary-and-unary-plusmin*)
          ;; - (1 * 2 * 3) => (-1) * 2 * 3
          ;; ([unary-expr] [-] ([binary-expr] [*] 1 2)) => ([binary-expr] ([unary-expr] [-] 1) 2)
          (setf unary-expr
            `([binary-expr] ,binary-op ,(maybe-fix-unary-expr `([unary-expr] ,unary-op ,bin-left)) ,bin-right))))))
  unary-expr)
  
(defun dotted-name-to-attribute-ref (dotted-name)
  (assert dotted-name)
  (let ((res `([identifier-expr] ,(pop dotted-name))))
    (dolist (x dotted-name res)
      (setf res `([attributeref-expr] ,res ([identifier-expr] ,x))))))

(defun finish-ast (ast)
  (prog1 (unwrap-literals ast)
    (clean-source-locations)))

(defun unwrap-literals (x)
  (etypecase x
    ((or symbol number) x)
    (literal (literal-value x))
    (list (let ((res (mapcar #'unwrap-literals x)))
            (when *python-form->source-location*
              (let ((loc (gethash x *python-form->source-location*)))
                (when loc
                  (setf (gethash res *python-form->source-location*) loc)
                  (remhash x *python-form->source-location*))))
            res))))

(defun clean-source-locations ()
  (declare (special *expr-stmt-nodes*))
  (when *python-form->source-location*
    (maphash (lambda (k v)
               (declare (ignore v))
               (unless (and (listp k)
                            (member (car k) *expr-stmt-nodes*))
                 (remhash k *python-form->source-location*)))
             *python-form->source-location*)))

(defun debug-print-locs (&optional (ht *python-form->source-location*))
  (when ht
    (maphash (lambda (k v) (format t "~30A: ~2@A .. ~2@A ~A~%"
                                   (clpython.parser::py-pprint k) (getf v :start) (getf v :end) (if (listp k) (car k) k)))
             ht)))

