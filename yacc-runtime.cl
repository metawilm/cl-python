;; -*- mode: common-lisp; package: yacc -*-
;;
;; yacc-runtime.cl -[Thu Aug 12 16:01:34 2004 by smh]-
;;
;; copyright (c) 2003-2004 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.

;;; See the accompanying file yacc.cl

(provide :yacc-runtime)

(eval-when (compile load eval)
  (require :yacc-defs (merge-pathnames "yacc-defs.fasl" *load-pathname*)))
(eval-when (compile)
  (require :yacc-defs (merge-pathnames "yacc-defs.fasl" *compile-file-pathname*)))

(in-package :yacc)

(defvar *yacc-trace* nil
  "When non-NIL the parser state machine reports its actions for debugging.")

;;; Interface to main LALR parser stack machine.

;;; Easy user-callable interface to invoke a grammar by name or by class.
(defmethod parse ((grammar symbol) &key (state-machine 'state-machine))
  (funcall state-machine (make-instance grammar)))

(defmethod parse ((grammar grammar-class) &key (state-machine 'state-machine))
  (funcall state-machine (make-instance grammar)))

(defmethod parse ((grammar grammar) &key (state-machine 'state-machine))
  (funcall state-machine grammar))

;;; This is called to clear the error state inside the machine.
(defun error-resynchronized (grammar)
  (setf (grammar-error grammar) nil))

;; This is used as the default action code when there is none specified in the production.
(defun $$ (grammar &optional $1 &rest rest)
  (declare (ignore grammar rest))
  $1)

;;;
;;; These are necessary if we turn on *yacc-trace*.
;;;

(defmethod print-object ((terminal terminal) stream)
  (let ((x (terminal-terminal terminal)))
    (format stream #-never "{~s}" #+never "~s"
	    (if (integerp x) (code-char x) x))))

(defun print-term (s term)
  (cond ((numberp term)
	 (let* ((char (code-char term))
		(char-name (char-name char)))
	   (if char-name
	       (princ char-name s)
	     (prin1 char-name s))))
	;; ((consp term) (prin1 (car term) s)) ; nonterminal -- entry on nonterminal-alist
	(t (prin1 term s)))
  #+nomore (write-char #\space s))

(defun print-term-1 (s code grammar)
  (let* ((terminal (find code (grammar-terminals grammar) :key #'terminal-code))
	 (term (terminal-terminal terminal)))
    (if (numberp term)
	(let* ((char (code-char term))
	       (char-name (char-name char)))
	  (if char-name
	      (format s "~a" char-name)
	    (format s "~c" char)))
      (format s "~a" term))))

(defmethod print-object ((nonterminal nonterminal) stream)
  (format stream "[~s]" (nonterminal-symbol nonterminal)))

(defmethod print-object ((prod production) stream)
  (print-production-1 (production-lhs prod) (production-rhs prod) stream))

(defun print-production-1 (lhs rhs stream)
  (pprint-logical-block (stream nil :prefix "[" :suffix "]")
    (princ lhs stream)
    (write-char #\space stream)
    (pprint-newline :miser stream)
    (pprint-indent :block 2 stream)
    (write-string "->" stream)
    (do ((rhs rhs (cdr rhs)))
	((not (consp rhs)))
      (write-char #\space stream)
      (pprint-newline :fill stream)
      ;; The rhs symbol is changed to a cons (symbol . lookaheadv) during grammar build.
      (print-term stream (if (consp (car rhs)) (caar rhs) (car rhs))))))

(defun print-action (stream action)
  (let ((type (car action))
	(arg (cdr action)))
    (case type
      (shift (format stream "Shift ~D" arg))
      (reduce (format stream "Reduce ~S" arg))
      (t (format stream "~a~@[ ~s~]" type arg)))))

#+unused
(defun print-prod (p s d)
  (declare (ignore d))
  (format s "#s(~s :lhs ~s :number ~s"
	  'production (production-lhs p) (production-number p))
  (when (production-plist p)
    (format s " :plist ~s" (production-plist p)))
  (when (consp (production-rhs p))
    (format s " :rhs (")
    (do ((rhs (production-rhs p) (cdr rhs)))
	((not (consp rhs)) (format s ")"))
      (format s "~s " (car rhs))))
  (when (production-action-code p)
    (format s " :action-code ~s" (production-action-code p)))
  (format s ")"))

;;;
;;;
;;;

(defmacro array-dispatch (dispatch symbol index)
  `(let ((v (svref ,dispatch ,symbol)))
     (or (and v (svref v ,index))
	 '(error))))

(defun state-machine (grammar)
  (declare (optimize (speed 3) (safety 0)))
  (or (grammar-goto-table grammar) (construct-grammar-tables grammar))
  (or (grammar-state-stack grammar)
      (setf (grammar-state-stack grammar) (make-array (grammar-initial-stack-size grammar)
						      :fill-pointer 0
						      :adjustable t)
	    (grammar-value-stack grammar) (make-array (grammar-initial-stack-size grammar)
						      :fill-pointer 0
						      :adjustable t)))
  (setf (grammar-error grammar) nil)
  (let ((lexer (or (grammar-lexer grammar) (error "No lexer for ~S" grammar)))
	(state 0)
	(state-stack (grammar-state-stack grammar))
	(value-stack (grammar-value-stack grammar))
	(action-table (grammar-action-table grammar))
	(goto-table (grammar-goto-table grammar))
	(defaults (grammar-defaults grammar))
	(orig-state nil)		; Used in error recovery
	(reduction-args
	 (do ((target (make-sequence 'list (grammar-longest-rhs grammar))
		      (cdr target))
	      (result))
	     ((null target) (cons nil result))
	   (push target result)))
	token token-type type value action where)
    (setf (fill-pointer state-stack) 0)
    (setf (fill-pointer value-stack) 0)
    (vector-push 0 state-stack)
    (vector-push nil value-stack)
    (handler-bind
	;; The theory of error handling inside the parser state machine: If there is a
	;; grammar-parse-error, just resignal it and let any outer handler deal with it,
	;; or else enter the debugger.  If some action or IO code signals any other kind
	;; of error, first resignal it silently and give any outer handler a chance to
	;; deal with it, or else resignal it noisily using error, allowing entry to the
	;; debugger.  This annotation makes it easiler for programmers to diagnose
	;; exceptions inside the state-machine, while not interfering with regular silent
	;; condition handling.
	((grammar-parse-error (lambda (c) (error c)))
	 (error (lambda (c)
		  (signal c)
		  (error "parser ~s signalled an unhandled error: ~a" grammar c))))
      (loop
	(unless token
	  (or (setq token-type (and (svref defaults state)
				    0))
	      (multiple-value-setq (token-type token where)
		(funcall lexer grammar))))
	(setq action
	  (array-dispatch action-table token-type state))
	(setq type (car action))
	(setq value (cdr action))
	(when *yacc-trace*
	  (let* ((*print-length* 2) (*print-level* 2)
		 (term (find token-type (grammar-terminals grammar) :key #'terminal-code))
		 (type (and term (terminal-terminal term))))
	    (format t "~&~3d~@[ ~s~]~@[ ~s~]: "
		    state
		    (unless (numberp type) type)
		    token))
	  (print-action t action)
	  (format t "~%"))

	(case type
	  (accept (vector-pop state-stack)
		  (return (vector-pop value-stack)))
	  (shift (vector-push-extend token value-stack)
		 (vector-push-extend value state-stack)
		 (setq state value token nil))
	  (reduce
	   (do ((tokens (production-rhs value) (cdr tokens))
		(args reduction-args)
		(function (production-action-code value)))
	       ((not (consp tokens))
		(setq state
		  (array-dispatch goto-table
				  (production-lhs-goto-index value)
				  (aref state-stack (1- (fill-pointer state-stack)))))
		(vector-push-extend (if function
					(apply function grammar (car args))
				      (caar args))
				    value-stack)
		(vector-push-extend state state-stack))
	     (vector-pop state-stack)
	     (setq args (cdr args))
	     (rplaca (car args) (vector-pop value-stack))))
	  (ireduce
	   (do ((n (production-rhs value) (1- n))
		(args reduction-args)
		(fill-pointer (1- (fill-pointer value-stack)) (1- fill-pointer))
		(function (production-action-code value)))
	       ((= n 0)
		(setq state
		  (array-dispatch goto-table
				  (production-lhs-goto-index value)
				  (aref state-stack (1- (fill-pointer state-stack)))))
		(vector-push-extend (if function
					(apply function grammar (car args))
				      (caar args))
				    value-stack)
		(vector-push-extend state state-stack))
	     (setq args (cdr args))
	     (rplaca (car args) (aref value-stack fill-pointer))))
	  (error
	   (cond
	    ((null (grammar-error grammar)) ;Initial entry to error.
	     (when *yacc-trace*
	       (format t "~^syntax error with token ~s state ~s~%" token state))
	     (setf orig-state state)	; Save for error reporting.
	     ;;Search the stack for a state where it is legal to shift an error.
	     (loop with disp = (svref action-table (grammar-error-code grammar))
		 until (and disp (eq (car (svref disp state)) 'shift))
		 do (when (eql (fill-pointer state-stack) 1)
		      (signal-grammar-error grammar :token token :state orig-state :where where))
		    (vector-pop state-stack) (vector-pop value-stack)
		    (when *yacc-trace*
		      (format
		       t "~^error recovery discards state ~s uncovering state ~s~%"
		       state (aref state-stack (1- (fill-pointer state-stack)))))
		    (setq state (aref state-stack (1- (fill-pointer state-stack)))))
	     (setq token token-type	;Just to remember the bad token.
		   token-type (grammar-error-code grammar)) ;ERROR pseudo-token becomes input.
	     (setf (grammar-error grammar) 'discarding))
	    (t
	     (when *yacc-trace*
	       (format t "~&error recovery discards token ~s ~s~%" token-type token))
	     (when (eq token 'eof)	; Give up if eof doesn't lead to accept!
	       (signal-grammar-error grammar :token token :state orig-state :where where))
	     (setq token nil))))
	  ((nil)
	   (signal-grammar-error grammar :token token :state state :where where)
	   (error "the lexer for grammar ~s in state ~s returned an illegal token: ~s ~s~%"
		  grammar state token token-type)
	   ))))))

(defun state-machine-for-debugging (grammar)
  (or (grammar-goto-table grammar) (construct-grammar-tables grammar))
  (or (grammar-state-stack grammar)
      (setf (grammar-state-stack grammar) (make-array (grammar-initial-stack-size grammar)
						      :fill-pointer 0
						      :adjustable t)
	    (grammar-value-stack grammar) (make-array (grammar-initial-stack-size grammar)
						      :fill-pointer 0
						      :adjustable t)))
  (setf (grammar-error grammar) nil)
  (let* ((lexer (or (grammar-lexer grammar) (error "No lexer for ~S" grammar)))
	 (known-lexemes (grammar-known-lexemes grammar))
	 (lexeme-limit (length known-lexemes))
	 (state 0)
	 (state-stack (grammar-state-stack grammar))
	 (value-stack (grammar-value-stack grammar))
	 (action-table (grammar-action-table grammar))
	 (goto-table (grammar-goto-table grammar))
	 (defaults (grammar-defaults grammar))
	 (orig-state nil)		; Used in error recovery
	 (reduction-args
	  (do ((target (make-sequence 'list (grammar-longest-rhs grammar))
		       (cdr target))
	       (result))
	      ((null target) (cons nil result))
	    (push target result)))
	 token token-type type value action where)
    (setf (fill-pointer state-stack) 0)
    (setf (fill-pointer value-stack) 0)
    (vector-push 0 state-stack)
    (vector-push nil value-stack)
    (handler-bind
	;; The theory of error handling inside the parser state machine: If there is a
	;; grammar-parse-error, just resignal it and let any outer handler deal with it,
	;; or else enter the debugger.  If some action or IO code signals any other kind
	;; of error, first resignal it silently and give any outer handler a chance to
	;; deal with it, or else resignal it noisily using error, allowing entry to the
	;; debugger.  This annotation makes it easiler for programmers to diagnose
	;; exceptions inside the state-machine, while not interfering with regular silent
	;; condition handling.
	((grammar-parse-error (lambda (c) (error c)))
	 (error (lambda (c)
		  (signal c)
		  (error "parser ~s signalled an unhandled error: ~a" grammar c))))
      (loop
	(unless token
	  (unless (setq token-type (and (svref defaults state)
					0))
	    (multiple-value-setq (token-type token where)
	      (funcall lexer grammar))
	    (unless (and (integerp token-type)
			 (>= token-type 0)
			 (< token-type lexeme-limit)
			 (sbit known-lexemes token-type))
	      (error "lexer for ~s returned an unknown token: ~s" token-type token))))

	(setq action
	  (array-dispatch action-table token-type state))
	(setq type (car action))
	(setq value (cdr action))
	(when *yacc-trace*
	  (let* ((*print-length* 2) (*print-level* 2)
		 (term (find token-type (grammar-terminals grammar) :key #'terminal-code))
		 (type (and term (terminal-terminal term))))
	    (format t "~&~3d~@[ ~s~]~@[ ~s~]: "
		    state
		    (unless (numberp type) type)
		    token))
	  (print-action t action)
	  (format t "~%"))

	(case type
	  (accept (vector-pop state-stack)
		  (return (vector-pop value-stack)))
	  (shift (vector-push-extend token value-stack)
		 (vector-push-extend value state-stack)
		 (setq state value token nil))
	  (reduce
	   (do ((tokens (production-rhs value) (cdr tokens))
		(args reduction-args)
		(function (production-action-code value)))
	       ((not (consp tokens))
		(setq state
		  (array-dispatch goto-table
				  (production-lhs-goto-index value)
				  (aref state-stack (1- (fill-pointer state-stack)))))
		(vector-push-extend (if function
					(apply function grammar (car args))
				      (caar args))
				    value-stack)
		(vector-push-extend state state-stack))
	     (vector-pop state-stack)
	     (setq args (cdr args))
	     (rplaca (car args) (vector-pop value-stack))))
	  (ireduce
	   (do ((n (production-rhs value) (1- n))
		(args reduction-args)
		(fill-pointer (1- (fill-pointer value-stack)) (1- fill-pointer))
		(function (production-action-code value)))
	       ((= n 0)
		(setq state
		  (array-dispatch goto-table
				  (production-lhs-goto-index value)
				  (aref state-stack (1- (fill-pointer state-stack)))))
		(vector-push-extend (if function
					(apply function grammar (car args))
				      (caar args))
				    value-stack)
		(vector-push-extend state state-stack))
	     (setq args (cdr args))
	     (rplaca (car args) (aref value-stack fill-pointer))))
	  (error
	   (cond
	    ((null (grammar-error grammar)) ;Initial entry to error.
	     (when *yacc-trace*
	       (format t "~^syntax error with token ~s state ~s~%" token state))
	     (setf orig-state state)	; Save for error reporting.
	     ;;Search the stack for a state where it is legal to shift an error.
	     (loop with disp = (svref action-table (grammar-error-code grammar))
		 until (and disp (eq (car (svref disp state)) 'shift))
		 do (when (eql (fill-pointer state-stack) 1)
		      (signal-grammar-error grammar :token token :state orig-state :where where))
		    (vector-pop state-stack) (vector-pop value-stack)
		    (when *yacc-trace*
		      (format
		       t "~^error recovery discards state ~s uncovering state ~s~%"
		       state (aref state-stack (1- (fill-pointer state-stack)))))
		    (setq state (aref state-stack (1- (fill-pointer state-stack)))))
	     (setq token token-type	;Just to remember the bad token.
		   token-type (grammar-error-code grammar)) ;ERROR pseudo-token becomes input.
	     (setf (grammar-error grammar) 'discarding))
	    (t
	     (when *yacc-trace*
	       (format t "~&error recovery discards token ~s ~s~%" token-type token))
	     (when (eq token 'eof)	; Give up if eof doesn't lead to accept!
	       (signal-grammar-error grammar :token token :state orig-state :where where))
	     (setq token nil))))
	  ((nil)
	   (signal-grammar-error grammar :token token :state state :where where)
	   (error "the lexer for grammar ~s in state ~s returned an illegal token: ~s ~s~%"
		  grammar state token token-type)
	   ))))))

(define-condition grammar-parse-error (parse-error)
  ((grammar  :accessor grammar-parse-error-grammar  :initarg :grammar)
   (source   :accessor grammar-parse-error-source   :initarg :source)
   (state    :accessor grammar-parse-error-state    :initarg :state)
   (token    :accessor grammar-parse-error-token    :initarg :token)
   (position :accessor grammar-parse-error-position :initarg :position))
  (:report grammar-parse-error-printer))

(defun grammar-parse-error-printer (x stream)
  (format stream
	  "grammar ~a in state ~s detected error~@[ on token ~s~]~@[ at source position ~d~]"
	  (grammar-parse-error-grammar x)
	  (grammar-parse-error-state x)
	  (grammar-parse-error-token x)
	  (grammar-parse-error-position x)))

(defmethod signal-grammar-error ((grammar grammar) &key token where state)
  (error 'grammar-parse-error
	 :grammar grammar
	 :state state
	 :token token
	 :position (or where
		       (funcall (grammar-lexer grammar) grammar
				:report-location))))
