(in-package :python)

;;; Pretty printer for parsed Python code
;;; 
;;; Of course this is fully dependent on the form of the AST that the
;;; parser yields.

(defvar *py-pprint-dispatch* (copy-pprint-dispatch nil))

(defmacro def-pprint-funcs (data)
  `(let ((*print-pprint-dispatch* *py-pprint-dispatch*))
     ,@(loop for (ast-car func) in data
	   collect `(set-pprint-dispatch '(cons (member ,ast-car))
					 (lambda (stream x)
					   ,func)))))


(def-pprint-funcs
    ((identifier (format stream "~S" (cdr x)))
     (number  (format stream "~A" (cdr x)))
     (string  (format stream "~S" (cdr x)))
     (or      (format stream "~A or ~A" (second x) (third x)))
     (list    (format stream "[~{~A~^, ~}]" (cdr x)))
     (tuple   (format stream "(~{~A~^, ~},)" (cdr x)))
     (dict    (progn (format stream "{")
		     (loop for (k . v) in (second x)
			 do (format stream "~S: ~S, ~_" k v))
		     (format stream "}")))
     (attributeref (format stream "~S.~S" (second x) (third x)))
     (subscription (format stream "~S[~S]" (second x) (third x)))
     
     ;; math
     (power (format stream "~S ** ~S" (second x) (third x)))
     (unary (format stream "~S~S" (second x) (third x)))
     (binary (format stream "~S ~S ~S" (third x) (second x) (fourth x)))
     
     (comparison (format stream "~S ~S ~S" (third x) (second x) (fourth x)))
     (or  (format stream "~S or ~S" (second x) (third x)))
     (and (format stream "~S and ~S" (second x) (third x)))
     
     (suite (format stream "~&~<   ~@;~@{~S~&~}~:>" (cdr x)))
     (if-stmt (format stream "if ~S: ~&~S ~&else: ~& ~S" (second x) (third x) (fourth x)))
     ))

(pyprint '(suite
	   (if-stmt (comparison = (identifier . x) (identifier . y))
	    (suite
	     (identifier . foo)
	     (number . 3)
	     (string . "asdf"))
	    (suite
	     (number . 42)))
	   (dict (("a" . (number . 3))
		  ("b" . (number . 4))))
	   (list 1 2 3)
	   (attributeref (identifier . x) (identifier . a))
	   (subscription (identifier . x) (number . 3))
	   (power (number . 3) (number . 4))
	   (unary + (number . 3))
	   (binary + (number . 3) (number . 4))
	   (comparison < (number . 3) (number . 4))
	   (or (identifier . x) (identifier . y))
	   (and (identifier . x) (identifier . y))
	   (tuple 3)
	   (tuple 1 2)
	   ))

(defun pyprint (ast)
  (let ((*print-pprint-dispatch* *py-pprint-dispatch*))
    (pprint ast)))