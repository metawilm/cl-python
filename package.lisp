;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :user)

;; CLPython package structure:
;;
;;  :clpython                      -- aggrgation of other packages
;;
;;    :clpython.builtin            -- built-ins
;;      :clpython.builtin.function   -- functions like `len', `repr'
;;      :clpython.builtin.type       -- types like `int', `function'
;;        :clpython.builtin.type.exception  -- exceptions, like `IndexError'
;;      :clpython.builtin.value      -- values like `True', `None'
;;      :clpython.builtin.module     -- modules like `sys', `time'
;;
;;    :clpython.ast                -- symbols for representing source code
;;      :clpython.ast.node           -- AST nodes like `funcdef-stmt', `call-expr'
;;      :clpython.ast.reserved       -- reserved words like `def', `if'
;;      :clpython.ast.user           -- variables like `foo', `fact'
;;
;;    :clpython.parser             -- parsing Python source code into AST
;;
;;    :clpython.app                -- applications build on CLPython
;;    :clpython.app.repl            -- read-eval-print loop
;;
;; Below exported symbols are #:symbols if case is irrelevant, and "strings" if case
;; is important.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cascade-external-symbols (pkg &optional used-pkg-list)
    (dolist (p (or used-pkg-list (package-use-list pkg)))
      (do-external-symbols (s p)
	(export s pkg)))))


;;; Abstract syntax tree

(defpackage  :clpython.ast.reserved
  (:documentation "Reserved words in the grammar")
  ;; A few of these (e.g. `as') are not actually reserved words in CPython yet
  ;; (because of backward compatilibity reasons), but they will be in the future.
  (:use )
  (:export "and" "as" "assert" "break" "class" "continue" "def" "del" "elif" "else"
	   "except" "exec" "finally" "for" "from" "global" "if" "import" "in" "is"
	   "lambda" "not" "or" "pass" "print" "raise" "return" "try" "while" "yield"))

(defpackage :clpython.ast.operator
  (:documentation "Unary and binary operators")
  (:use )
  (:import-from :common-lisp "<" "<=" ">" ">=" "/=" "/" "//" "+" "-" "*" "**")
  (:export "<" "<=" ">" ">=" "!=" "=="
	   "|" "^" "&" "<<" ">>" "+" "-" "*" "/" "%" "//" "~" "**"
	   "|=" "^=" "&=" "<<=" ">>=" "+=" "-=" "*=" "/=" "*=" "/=" "%=" "//=" "**="
	   ;; not really operators in the grammar, but used internally...
	   #:/t/ #:<divmod>))

(defpackage :clpython.ast.punctuation
  (:use )
  (:import-from :common-lisp "=" #:string #:number)
  (:export "@" ":" "[" "]" "{" "}" "(" ")" "=" "." "," "`" ";" "..." "<>"
	   #:newline #:indent #:dedent #:idenfifier #:number #:string))

(defpackage :clpython.ast.user
  (:documentation "Identifiers")
  (:use )
  (:export "__getitem__" "__delitem__" "__setitem__" "__new__" "__init__"
	   "__dict__" "__get__" "__del__" "__set__" "__name__" "__all__"
	   "__getattribute__" "__getattr__" "__class__" "__delattr__"
	   "__setattr__" "__call__" "__nonzero__" "__hash__" "__iter__"
	   ;; binary
	   "__add__" "__radd__" "__iadd__"  "__sub__" "__rsub__" "__isub__"
	   "__mul__" "__rmul__" "__imul__"  "__truediv__" "__rtruediv__" "__itruediv__"
	   "__mod__" "__rmod__" "__imod__"  "__floordiv__" "__rfloordiv__" "__ifloordiv__"
	   "__div__" "__rdiv__" "__idiv__"  "__lshift__" "__rlshift__" "__irshift__"
	   "__and__" "__rand__" "__iand__"  "__rshift__" "__rrshift__" "__irshift__"
	   "__or__"  "__ror__"  "__ior__"   "__divmod__" "__rdivmod__"
	   "__xor__" "__rxor__" "__ixor__"  "__pow__" "__ipow__"
	   ;; unary
	   "__invert__" "__pos__" "__neg__" "__contains__" "__cmp__" "__abs__" "__len__"
	   ;; comparison
	   "__eq__" "__lt__" "__gt__" "__cmp__" 
	   ;; representation
	   "__repr__" "__str__" "__hex__" "__oct__" ))

(defpackage :clpython.ast.node
  (:documentation "Statement and expression nodes")
  (:use )
  (:export #:assign-stmt #:assert-stmt #:augassign-stmt #:break-stmt #:classdef-stmt
	   #:continue-stmt #:del-stmt #:exec-stmt #:for-in-stmt #:funcdef-stmt
	   #:global-stmt #:if-stmt #:import-stmt #:import-from-stmt #:module-stmt
	   #:pass-stmt #:print-stmt #:return-stmt #:suite-stmt #:raise-stmt
	   #:try-except-stmt #:try-finally-stmt #:while-stmt #:yield-stmt
	   
	   #:attributeref-expr #:backticks-expr #:binary-expr #:binary-lazy-expr
	   #:call-expr #:comparison-expr #:dict-expr #:generator-expr
	   #:identifier-expr #:lambda-expr #:listcompr-expr #:list-expr #:slice-expr
	   #:subscription-expr #:tuple-expr #:unary-expr ))

(defpackage :clpython.ast
  (:documentation "Python abstract syntax tree representation")
  (:use :clpython.ast.reserved :clpython.ast.user :clpython.ast.node :clpython.ast.operator))

;; Don't export operators, as 21 of them conflict with symbols in the Common Lisp package.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cascade-external-symbols :clpython.ast
			    (remove (find-package :clpython.ast.operator)
				    (remove (find-package :clpython.ast.punctuation)
					    (package-use-list :clpython.ast)))))

(defpackage :clpython.ast.all
  (:nicknames :py.ast)
  (:documentation "Python abstract syntax tree representation (All symbols)")
  (:use :clpython.ast.reserved :clpython.ast.user :clpython.ast.node
	:clpython.ast.operator :clpython.ast.punctuation))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cascade-external-symbols :clpython.ast.all))

;;; Builtins

(defpackage :clpython.builtin.function
  (:nicknames :pybf)
  (:use )
  (:export "__import__" "abs" "apply" "callable" "chr" "cmp" "coerce" "compile"
	   "delattr" "dir" "divmod" "eval" "execfile" "filter" "getattr" "globals"
	   "hasattr" "hash" "hex" "id" "input" "intern" "isinstance" "issubclass"
	   "iter" "len" "locals" "map" "max" "min" "oct" "ord" "pow" "range"
	   "raw_input" "reduce" "reload" "repr" "round" "setattr" "sorted" "sum"
	   "unichr" "vars" "zip"))

(defpackage :clpython.builtin.type.exception
  (:nicknames :pybte)
  (:use )
  (:export "ArithmeticError" "AssertionError" "AttributeError" "DeprecationWarning"
	   "EOFError" "EnvironmentError" "Exception" "FloatingPointError"
	   "FutureWarning" "IOError" "ImportError" "IndentationError" "IndexError"
	   "KeyError" "KeyboardInterrupt" "LookupError" "MemoryError" "NameError"
	   "NotImplementedError" "OSError" "OverflowError" "OverflowWarning"
	   "PendingDeprecationWarning" "ReferenceError" "RuntimeError"
	   "RuntimeWarning" "StandardError" "StopIteration" "SyntaxError"
	   "SyntaxWarning" "SystemError" "SystemExit" "TabError" "TypeError"
	   "UnboundLocalError" "UnicodeDecodeError" "UnicodeEncodeError"
	   "UnicodeError" "UnicodeTranslateError" "UserWarning" "VMSError"
	   "ValueError" "Warning" "WindowsError" "ZeroDivisionError"))

(defpackage :clpython.builtin.type
  (:nicknames :pybt)
  (:use :clpython.builtin.type.exception)
  (:export "basestring" "bool" "classmethod" "complex" "dict" "enumerate" "file"
	   "float" "int" "list" "long" "number" "object" "property" "slice"
	   "staticmethod" "str" "super" "tuple" "type" "unicode" "xrange"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cascade-external-symbols :clpython.builtin.type))

(defpackage :clpython.builtin.value
  (:nicknames :pybv)
  (:use )
  (:export "None" "Ellipsis" "True" "False" "NotImplemented"))

(defpackage :clpython.builtin
  (:use :clpython.builtin.function :clpython.builtin.type :clpython.builtin.value ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cascade-external-symbols :clpython.builtin))

;;; Parser

(defpackage :clpython.parser
  (:documentation "")
  (:use :common-lisp :clpython.ast.all)
  (:import-from :clpython.builtin.type.exception "SyntaxError")
  
  #+(or)
  (:shadowing-import-from :clpython.ast.operator
			  "<" "<=" ">" ">=" "!=" "<>" "=="
			  "|" "^" "&" "<<" ">>" "+" "-" "*" "/" "%" "//" "~" "**"
			  "|=" "^=" "&=" "<<=" ">>=" "+=" "-=" "*=" "/=" "*=" "/=" "%=" "//=" "**="
			  ;; not really operators in the grammar, but used internally...
			  #:/t/ #:<divmod>)
  
  (:export #:parse-python-file #:parse-python-string ))


;;; Main package

(defpackage :clpython
  (:documentation "CLPython: An implementation of Python in Common Lisp.")
  (:use :common-lisp :clpython.ast :clpython.parser :clpython.builtin)
  (:export #:py-val->string #:py-str-string #:py-repr #:py-bool #:initial-py-modules #:make-module
	   #:*the-none* #:*the-true* #:*the-false* #:*the-ellipsis* #:*the-notimplemented*
	   #:*py-modules* #:dyn-globals
	   ;; more to come...
	   )
  (:shadow ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cascade-external-symbols :clpython))

;;; Applications that depend on CLPython, but not the other way around

(defpackage :clpython.app.repl
  (:documentation "Python read-eval-print loop")
  (:use :common-lisp :clpython :clpython.parser )
  (:export #:repl))
