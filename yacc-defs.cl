;; -*- mode: common-lisp; package: yacc -*-
;; yacc.cl
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

;;; For an introduction how to write yacc parsers, see the YACC documentation in almost
;;; any *nix distribution.  Bison is also similar, but differs in some details.

;;; For an explanation of the LALR1 parser generation algorithm used here, see:
;;;   "LR Parsing", V.Aho and S.C.Johnson, Computing Surveys 6/2 June 1974.

;;; To build, compile and load three files: yacc, yacc-compile, yacc-runtime
;;; The partitioning into three fuiles yacc, yacc-compile, yacc-ruintmie is intended to
;;; allow the compiler to be omitted when only runtime is needed.

;;; Recent to do list:

;;; = There was a recent change how the lexer and fsm communicate.  Tokens used to be
;;;   symbols but now are small integers.  The new with-terminal-codes macro can be
;;;   wrapped around the body of the lexer, and the tcode lexical macro used inside.  This
;;;   machinery needs more checking and integration into the compile/load file cycle.

;;; - There remain some O^2 issues at grammar generation time in certain difficult cases.
;;;   These need to receive some more optimization

;;; - There is something like O^4 scaling (perhaps in number of productions for a single
;;;   nonterminal) sometimes in compute-lookahead and same-cores-p.  Find and fix.

;;; - It used to be possible, and should be possible, to construct the grammar tables
;;;   either at grammar compile time or grammar load time.  It depends on when the
;;;   build-grammar-tables macro is executed.  Compile-time generation has not been
;;;   checked in a long time and might be broken, but this is the preferred mechanism for
;;;   delivered applications.  Start with checking the make-load-form stuff.  The grammar
;;;   tables are in slots in the finalized grammar class, so the whole table-building mess
;;;   is now tied into the MOP.

(provide :yacc)
(provide :yacc-defs)

(defpackage :yacc
  (:export :build-grammar :build-grammar-1 :defgrammar #+notyet :ensure-grammar
	   :defproduction #+nomore :defproduction-1 :*yacc-trace*
	   :grammar :grammar-class
	   :with-terminal-codes :tcode :tcode-1
	   :*generate-listing* :string-grammar-mixin :eof :grammar-lexer
	   :string-grammar-string :string-grammar-index :string-grammar-length
	   :grammar-parse-error :signal-grammar-error
	   :grammar-parse-error-grammar :grammar-parse-error-source
	   :grammar-parse-error-state :grammar-parse-error-token
	   :grammar-parse-error-position :error-resynchronized
	   :parse :state-machine :state-machine-for-debugging))

(in-package :yacc)

(defvar *yacc-version* "Release 0.98")

(when (boundp 'excl::*version-info*)
  (locally (declare (special excl::*version-info*))
    (push (cons '*yacc-version* *yacc-version*)
	  excl::*version-info*)))

#-never
(progn

(defstruct (erminal (:copier nil))
  nonterminalp
  )

(defstruct (terminal (:include erminal (nonterminalp nil)) (:copier nil))
  terminal
  code					; lexer code
  pcode					; codes packed into 0..nterms for lookahead index
  precedence)

(defstruct (nonterminal (:include erminal (nonterminalp t)) (:copier nil))
  symbol
  (lookaheadv (make-lookaheadv))	; A lookahead bitvector containing all the initial
					; terminals that can be produced immediately by
					; this nonterminal.
  ;;(new-lookaheadp t)			; True if new any terminals are discovered in
					; lookahead scan.
  (lookahead-complete-p nil)		; True iff all paths through all productions
					; eventually lead to a terminal, implying the
					; lookahead vector is complete and does not depend
					; on following context.
  leading-nonterminals			; The set of nonterminals that occur in the
					; initial position of any rhs.
  productions)

(defstruct (production (:copier nil))
  lhs					;Left Hand Side nonterminal.
  lhs-goto-index			;Index of nonterminal in goto table.
  rhs					;Right Hand Side list of terminals and
					;nonterminals.  After the grammar has been
					;compiled the last cdr points back at the
					;production.
  action-code				;User action-code function which is applied to the
					;list of RHS values when the rule is reduced.  May
					;be nil, in which case the value of the rule is
					;the first RHS value.
  plist					;Storage for precedence and other flags about a
					;production,
  number				;Cardinal number, purely for reporting.
  precedence				;Cached production precedence if given, or nil.
  )

;; These only accept a terminal or nonterminal.
(defmacro nonterminalp (erminal) `(erminal-nonterminalp ,erminal))
(defmacro terminalp (erminal) `(not (erminal-nonterminalp ,erminal)))

)

#+never
(progn

(defclass terminal ()
  ((terminal :accessor terminal-terminal :initarg :terminal)
   (code :accessor terminal-code :initarg :code)
   (precedence :accessor terminal-precedence :initarg :precedence :initform nil)))

(defclass nonterminal ()
  ((symbol :accessor nonterminal-symbol :initarg :symbol)
   (productions :accessor nonterminal-productions :initarg :productions :initform nil)))

(defclass production ()
  ((lhs :accessor production-lhs :initarg :lhs) ;Left Hand Side nonterminal.
   (lhs-goto-index			;Index of nonterminal in goto table.
    :accessor production-lhs-goto-index :initarg :lhs-goto-index)
   (rhs :accessor production-rhs	;Right Hand Side list of terminals and
	:initarg :rhs)			;nonterminals.  After the grammar has been
					;compiled the last cdr points back at the
					;production.
   (action-code :accessor production-action-code :initarg :action-code)
					;User action-code function which is applied to the
					;list of RHS values when the rule is reduced.  May
					;be nil, in which case the value of the rule is
					;the first RHS value.
   (plist :accessor production-plist	;Storage for precedence and other flags about a
	  :initarg :plist :initform nil) ;production,
   (number :accessor production-number :initarg :number) ;Cardinal number, purely for reporting.
   (precedence				;Cached production precedence if given, or nil.
    :accessor production-precedence :initarg :precedence :initform nil)
   (in-lookahead :accessor production-in-lookahead :initform nil)
   ))

(defmacro make-terminal (&rest args)    `(make-instance 'terminal ,@args))
(defmacro make-nonterminal (&rest args) `(make-instance 'nonterminal ,@args))
(defmacro make-production (&rest args)  `(make-instance 'production ,@args))
(defmacro nonterminalp (arg) `(typep ,arg 'nonterminal))

)

(defclass grammar-class (standard-class)
  ((composedp :accessor composedp :initform nil :initarg :composedp)
   ;;List of lexemes plus ERROR.  At the start of building the grammar, all terminals are
   ;; interned as terminal objects to avoid O^2 behavior discriminating terminals from
   ;; nonterminals in lookahead.
   (terminals :accessor grammar-terminals :initform nil :initarg :terminals)
   ;; Set once the terminal codes have been assigned by assign-terminal-codes.
   (terminals-assigned :accessor grammar-terminals-assigned :initform nil
		       :initarg :terminals-assigned)
   ;;Lexemes plus nonterminals plus EOF.
   (lexemes :accessor grammar-lexemes :initarg :lexemes :initform nil)
   (known-lexemes :accessor grammar-known-lexemes :initarg :known-lexemes :initform nil)
   (symbols :accessor grammar-symbols :initform nil :initarg :symbols)
   ;;List of all productions.
   (productions :accessor grammar-productions :initform nil :initarg :productions)
   ;; At load time compiled action fucntions are stored here until they can be reunited
   ;; with their productions.
   (action-code-functions :accessor grammar-action-code-functions :initform nil
			  :initarg :action-code-functions)
   ;;An alist of sublists of productions keyed on LHS.
   (nonterminal-alist :accessor grammar-nonterminal-alist :initform nil
		      :initarg :nonterminal-alist)
   ;;Augmented top-level production.
   (top-level-production :accessor grammar-top-level-production :initarg :top-level-production)
   ;;A dispatch table from state,symbol.
   (action-table :accessor grammar-action-table :initarg :action-table :initform nil)
   ;;States which need not know next token.
   (defaults :accessor grammar-defaults :initarg :defaults :initform nil)
   ;;A dispatch table from state,symbol
   (goto-table :accessor grammar-goto-table :initarg :goto-table :initform nil)
   ;;Prefer reduce over shift.
   (left-associative-terminals :accessor grammar-left-associative
			       :initarg :left-associative :initform nil)
   ;;Prefer shift over reduce.
   (right-associative-terminals :accessor grammar-right-associative
				:initarg :right-associative :initform nil)
   ;;Give error if get to ambiguous state.
   (non-associative-terminals :accessor grammar-non-associative
			      :initarg :non-associative :initform nil)
   ;;List of lists of lexemes and dummies.
   (precedence-list :accessor grammar-precedence-list :initarg :precedence-list)
   ;;Length of the longest RHS.
   (longest-rhs :accessor grammar-longest-rhs :initarg :longest-rhs :initform nil)
   ;; The numeric code for the error nonterminal.
   (error-code :accessor grammar-error-code :initarg :error-code :initform nil)
   ))

(defclass grammar ()
  ((lexer :accessor grammar-lexer	;A function returning terminal tokens.  A lexer
	  :initarg :lexer)		;function must take a single argument which, if
					;non-NULL, asks the lexer to return the location
					;of the previous token for error reporting.
   (state-stack :accessor grammar-state-stack :initform nil) ;Vectors for the state machine
   (value-stack :accessor grammar-value-stack :initform nil) ;value and state stacks.
   ;;A guess how big the stack should be.
   (initial-stack-size :accessor grammar-initial-stack-size :initform 30)
   ;;Used by state-machine error recovery.
   (error :accessor grammar-error :initform nil))
  (:metaclass grammar-class))

(macrolet ((deligate (reader)
	     `(progn (defmethod ,reader ((x grammar))
		       (,reader (class-of x)))
		     (defmethod (setf ,reader) (new (x grammar))
		       (setf (,reader (class-of x)) new)))))
  (deligate grammar-goto-table)
  (deligate grammar-defaults)
  (deligate grammar-action-table)
  (deligate grammar-terminals)
  (deligate grammar-longest-rhs)
  (deligate grammar-error-code)
  (deligate grammar-known-lexemes)
  )

(defclass string-grammar-mixin (grammar)
  ((string :accessor string-grammar-string :initarg :string)
   (index  :accessor string-grammar-index :initform 0)
   (len    :accessor string-grammar-length)))

(defmethod shared-initialize :after ((grammar grammar-class) slots &rest initargs)
  (declare (ignore initargs slots))
  (unless (composedp grammar)
    (let ((terminals
	   (union (grammar-lexemes grammar)
		  (union (grammar-left-associative grammar)
			 (union (grammar-right-associative grammar)
				(grammar-non-associative grammar))))))
      (pushnew 'error terminals)
      (when (member 'eof terminals)
	(error "~s may not be used as a terminal in ~s." 'eof grammar))
      (push 'eof terminals)
      (setf (grammar-symbols grammar) (reverse terminals))
      (setf (grammar-terminals grammar) terminals))
    (let ((top-level-production (make-production :lhs (gensym)
						 :rhs (list (class-name grammar))
						 :number 0)))
      (setf (grammar-top-level-production grammar) top-level-production)
      (setf (grammar-productions grammar) (list top-level-production)))))

(defmethod shared-initialize :after ((x string-grammar-mixin) slots &rest initargs)
  (declare (ignore slots))
  (setf (string-grammar-index x) 0
	(string-grammar-length x) (length (slot-value x 'string))))
