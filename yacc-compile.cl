;; -*- mode: common-lisp; package: yacc -*-
;;
;; yacc-compile.cl -[Fri Aug 27 08:28:55 2004 by smh]-
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

(provide :yacc-compile)

(eval-when (load eval)
  (require :yacc-defs (merge-pathnames "yacc-defs.fasl" *load-pathname*)))
(eval-when (compile)
  (require :yacc-defs (merge-pathnames "yacc-defs.fasl" *compile-file-pathname*)))

(in-package :yacc)

(defvar *generate-listing* nil
  "When true a formatted listing will be produced during generatation of
the parser state machine.")

(defvar *verbose* nil
  "When true build-grammar reports progress to stdout.")

(defmethod make-load-form ((x terminal) &optional e)
  (make-load-form-saving-slots x :environment e))

(defmethod make-load-form ((x nonterminal) &optional e)
  (make-load-form-saving-slots x :environment e))

(defmethod make-load-form ((x production) &optional e)
  (make-load-form-saving-slots x :environment e))

(defmacro dump-grammar-class-slots-macro (class)
  `',(dump-grammar-class-slots class))

(defun dump-grammar-class-slots (class)
  (loop for slot in (clos:class-direct-slots (class-of class))
      as slot-name = (clos:slot-definition-name slot)
      as initarg = (first (clos:slot-definition-initargs slot))
      unless initarg			; consistency check
      do (error "class ~s slot ~s has no initargs." class slot)
      unless (eq initarg :action-code-functions)
      collect initarg and
      collect (slot-value class slot-name)))

(defmacro build-grammar (&environment e grammar &optional (listingp '*generate-listing*)
							  (verbose '*verbose*))
  `(progn (eval-when (compile)
	    (build-grammar-1 ',(find-class grammar t e) ,listingp ,verbose))
	  (eval-when (load)
	    (let ((grammar (find-class ',grammar)))
	      (apply #'reinitialize-instance
		     grammar
		     (dump-grammar-class-slots-macro ,(find-class grammar t e)))
	      (reunite-action-code grammar)))
	  (eval-when (eval)
	    (unless (composedp (find-class ',grammar))
	      (build-grammar-1 ',grammar ,listingp ,verbose)
	      (reunite-action-code (find-class ',grammar))))
	  ))

(defun reunite-action-code (grammar)
  ;; Reunite the action-code functions with their productions.
  (loop for prod in (grammar-productions grammar)
      do (setf (production-action-code prod)
	   (pop (grammar-action-code-functions grammar)))))

;; May need a :before method to return grammar to initial unbuilt state.
(defmethod finalize-inheriance :after ((class grammar-class))
  (unless (composedp class)
    (build-grammar-1 class)))

(defmethod build-grammar-1 ((grammar symbol) &optional (listingp *generate-listing*)
						       (verbose *verbose*))
  (build-grammar-1 (find-class grammar) listingp verbose))

(defmethod build-grammar-1 ((grammar grammar-class) &optional (listingp *generate-listing*)
							      (*verbose* *verbose*))
  (construct-grammar-tables grammar listingp))

;;; Build the ACTION-TABLE and GOTO-TABLE tables for GRAMMAR.
(defun construct-grammar-tables (grammar &optional (listingp *generate-listing*))
  (when (eq listingp t) (setq listingp grammar))
  (cond ((or (null listingp) (streamp listingp))
	 (construct-grammar-tables-1 grammar listingp))
	(t (with-open-file
	       (listing-stream
		(make-pathname :type "text"
			       :defaults (pathname
					  (string-downcase (class-name listingp))))
		:direction :output
		:if-exists :supersede)
	     (construct-grammar-tables-1 grammar listing-stream)))))

(defvar *reduce-reduce-conflicts*)
(defvar *shift-reduce-conflicts*)
(defvar *unknown-conflicts*)
(defvar *max-terminal-code*)
(defvar *max-terminal-pcode*)

(defmacro with-terminal-codes ((grammar-name) &body body &environment e)
  (let ((gram (find-class grammar-name t e)))
    `(macrolet ((tcode (term)
		  (assign-terminal-codes ',gram)
		  (let ((terminal (find (if (characterp term) (char-code term) term)
					(grammar-terminals ',gram)
					:key #'terminal-terminal)))
		    (cond (terminal (terminal-code terminal))
			  (t (warn "terminal ~s is not known in ~s" term ',gram))))))
       ,@body)))

(defun tcode-1 (grammar term &optional (warnp t))
  (assign-terminal-codes grammar)
  (let ((terminal (find (if (characterp term) (char-code term) term)
			(grammar-terminals grammar)
			:key #'terminal-terminal)))
    (cond (terminal (terminal-code terminal))
	  (warnp (warn "terminal ~s is not known in ~s" term grammar)))))

(defun assign-terminal-codes (grammar)
  (unless (grammar-terminals-assigned grammar)
    (let ((terminals (grammar-terminals grammar)))
      (multiple-value-bind (set unset)
	  (loop for terminal in terminals
	      if (integerp terminal)
	      collect (make-terminal :terminal terminal :code terminal) into set
	      else
	      collect (make-terminal :terminal terminal) into unset
	      finally (return (values (sort set #'< :key #'terminal-code)
				      unset)))
	(loop with s = set
	    for i from 0
	    while unset
	    if (and s
		    (eql i (terminal-code (car s))))
	    do (pop s)
	    else
	    do (setf (terminal-code (car unset)) i)
	    (push (pop unset) set)
	    end
	    finally (setf terminals (sort set #'< :key #'terminal-code))))
      (loop for terminal in terminals
	  for i from 0
	  do (setf (terminal-pcode terminal) i))
      (setf (grammar-terminals grammar) terminals
	    (grammar-terminals-assigned grammar) t))))

;; construct-grammar-tables-1 derives a parser machine description from a grammar
;; specification.  This is somewhat arbitrarily divided into five phases:

;; (1) Perform consistency checks on the grammar specification, compute several useful
;; lists, and do minor transformations on the spec.

;; (2) Compute the set of parser states. (More below).

;; (3) Derive the two tables which describe the parser machine.

;; (4) Derive the default table, and fill in remaining actions with error.  Check that all
;;     states can be reached and all rules reduced.

;; (5) Compile the action functions. (Now happens only at compile time.)
;;

;; Phase 2 is the most opaque.  It's task is to construct a list of parser states which is
;; represented by this hairy data structure:
;;
;; state-list
;;   o---------------------------------------------------------------- ...
;;   |                                                         |
;; state0                                                    state1
;;   o------------------------------------------------ ...     o------ ...
;;   |                                   |                     |
;;  item   lookahead-set                item                  item
;;   o-----o-------------------nil       o------- ...          o------ ...
;;   |     |     |         |             |     |               |
;;   |   term1 term2 ... termN           |   term1             |
;;  core                                core                  core
;;   o----------------------production   o------- ...          o------ ...
;;   |    |    |        |                |    |                |
;;  sym1 sym2 sym3 ... symN             sym1 sym2 ...         sym1     ...
;;
;; But this has been reimplemented to use bit vectors:
;;
;; state-list
;;   o---------------------------------------------------------------- ...
;;   |                                                         |
;; state0                                                    state1
;;   o------------------------------------------------ ...     o------ ...
;;   |                                   |                     |
;;  item                                item                  item
;;   o---#*lookahead-set                 o---#*lookahead-set   o---- ...
;;   |                                   |                     |
;;  core                                core                  core
;;   o----------------------production   o------- ...          o---- ...
;;   |    |    |        |                |    |                |
;;  sym1 sym2 sym3 ... symN             sym1 sym2 ...         sym1   ...
;;
;; Each sym is actually a terninal or nonterminal structure after pass1 starts.
;;
;; A lookahead set is a simple bit-vector the same length as the highest terminal pcode.
;; All nodes in this picture are cons cells except the productions and syms, which are
;; defstructs.  The names on the internal nodes of the structure identified with `o'
;; (state, item, core) are those usually employed as variable names in the code and derive
;; from the Aho & Johnson article.
;;
;; For speed reasons (e.g., using eq instead of equal) the cores share structure; a
;; particular position in a particular production rhs will always be shared between the
;; several states that contain it.  The list of symbols which is a core is terminated by a
;; last cdr which is the production containing the rhs (and thus the core).  Lookahead
;; sets must be merged during state generation, so they never share cons cells.
;;

(defun construct-grammar-tables-1 (grammar stream)
  ;; This is a place to put profiling or timing wrappers.
  (#+never prof:with-profiling #+never () #-never progn
  (construct-grammar-tables-2 grammar stream)))

(defun construct-grammar-tables-2
       (grammar stream
	&aux terminals #+nomore nonterminals state-list nstates top-level-production
	     productions nonterminal-alist (longest-rhs 0)
	     *max-terminal-code* *max-terminal-pcode*
	     action-table defaults goto-table state-nums known-lexemes
	     (*reduce-reduce-conflicts* 0)
	     (*shift-reduce-conflicts* 0)
	     (*unknown-conflicts* 0))

  (when *verbose* (format t "; yacc constructing ~S ...~%" grammar))
  (assign-terminal-codes grammar)
  (setq terminals (grammar-terminals grammar))
  (setq *max-terminal-code*  (1+ (loop for term in terminals maximize (terminal-code term))))
  (setq *max-terminal-pcode* (1+ (loop for term in terminals maximize (terminal-pcode term))))

  (setq productions (grammar-productions grammar))
  (setq top-level-production (grammar-top-level-production grammar))
  ;;;
  ;;; Phase 1
  ;;;

  (when *verbose* (format t ";;; Starting Phase 1  at ~d~%" (get-universal-time)))

  ;; Rewrite each production rhs to use the interned terminal objects.
  (loop for prod in productions
      do (loop for tokens on (production-rhs prod)
	     as terminal = (find (car tokens) terminals :key #'terminal-terminal)
	     when terminal do (setf (car tokens) terminal)))

  ;; Do some consistency checking on the grammar.

  ;; Check that each terminal appears on the RHS of some rule.
  (loop for terminal in terminals
      as term = (terminal-terminal terminal)
      do (unless (or (member term '(eof error) :test #'eq)
		     (dolist (prod productions)
		       (when (or (member terminal (production-rhs prod) :test #'eq)
				 (eq term ; Might be used just for precedence.
				     (getf (production-plist prod) :precedence)))
			 (return t))))
	   (format t "Warning: In grammar ~S the terminal " (class-name grammar))
	   (print-term t terminal)
	   (format t "does not appear in any rule.~%")
	   (setf terminals (delete terminal terminals))))
  (setf (grammar-terminals grammar) terminals)

  #+nomore
  (dolist (symbol (grammar-symbols grammar))
    (unless (member symbol terminals :key #'terminal-terminal :test #'eq)
      (push symbol nonterminals)))

  (assign-terminal-precedences grammar)

  ;; Check that each nonterminal occurs on a RHS for some other nonterminal.
  ;; Determine the longest RHS.
  ;; Check that each RHS element is a known symbol.
  ;; Build the alist of lists of productions by LHS symbol.
  ;; Setup the production COREs by making the last CDR of each RHS point
  ;; back to the PRODUCTION itself.

  (dolist (prod productions)
    (let ((symbol (production-lhs prod))
	  (rhs (production-rhs prod)))
      (if (consp rhs)			; In case grammar being rebuilt, maybe.
	  (rplacd (last rhs) nil)
	(setq rhs nil))
      (setq longest-rhs (max (length rhs) longest-rhs))
      (unless (eq prod top-level-production)
	(unless (dolist (oprod productions)
		  (unless (eq symbol (production-lhs oprod))
		    (when (do ((rhs (production-rhs oprod) (cdr rhs)))
			      ((not (consp rhs)) nil)
			    (when (eq symbol (car rhs)) (return t)))
		      (return t))))
	  (format t
		  "Warning: In grammar ~S nonterminal ~S cannot be reached.~%"
		  (class-name grammar) symbol))
	(dolist (token rhs)
	  (unless (member (if (typep token 'terminal) (terminal-terminal token) token)
			  (grammar-symbols grammar) :test #'eq)
	    (format t "Warning: In grammar ~S symbol ~S is unknown in a production for ~S.~%"
		    (class-name grammar) token symbol))))
      (let ((alist (assoc symbol nonterminal-alist :test #'eq)))
	(unless alist
	  (setq alist (cons symbol (make-nonterminal :symbol symbol)))
	  (push alist nonterminal-alist))
	(push prod (nonterminal-productions (cdr alist)))
	(if rhs
	    (rplacd (last rhs) prod)
	  (setf (production-rhs prod) prod)))))
  (setf (grammar-nonterminal-alist grammar) nonterminal-alist)

  ;; Now replace each nonterminal in every rhs with the alist entry for the nonterminal.
  ;; This makes lookahead much faster.

  (loop for prod in productions
      do (loop for p on (production-rhs prod)
	     while (consp p)
	     as term = (car p)
	     if (symbolp term)
	     do (setf (car p)
		  (or (cdr (assoc term nonterminal-alist :test #'eq))
		      (error "internal yacc consistency error for ~s" prod)))))

  (assign-rule-precedences grammar)

  (when *verbose* (format t ";;; Starting Phase 1a at ~d~%" (get-universal-time)))

  (precompute-nonterminal-complete-ps nonterminal-alist)
  ;; (break "precompute-nonterminal-complete-ps" nonterminal-alist)

  (when *verbose* (format t ";;; Starting Phase 1b at ~d~%" (get-universal-time)))

  (precompute-nonterminal-lookaheads nonterminal-alist)

  ;; (break "precompute-nonterminal-lookaheads" nonterminal-alist)

  (when *verbose* (format t ";;; Starting Phase 1c at ~d~%" (get-universal-time)))

  (setq state-list
    (list				;list of states
     (compute-closure
      (list				;list of items
       (cons				;item
	(production-rhs (grammar-top-level-production grammar)) ;core
	(let ((lookaheadv (make-lookaheadv))) ;lookahead set
	  (setf (sbit lookaheadv
		      (terminal-pcode (find 'eof terminals :key #'terminal-terminal)))
	    1)
	  lookaheadv))))))

  ;; (format t "state-list:~% ~s~%" state-list)

  ;;;
  ;;; Phase 2
  ;;;

  (when *verbose* (format t ";;; Starting Phase 2  at ~d~%" (get-universal-time)))

  ;; For each state V(g) and each symbol X, compute V(gX)=GOTO(V(g),X).
  (do ((states state-list (cdr states)))
      ((null states))
    (let ((state (car states))
	  goto)
      ;;(when *verbose*
      ;;  (format t ";;; ~d states at ~d~%" (length state-list) (get-universal-time)))
      (dolist (x (grammar-symbols grammar))
	(when (setq goto (goto x state))
	  (compute-closure goto)
	  (or (dolist (state state-list)
		;;If the new state is already present, merge the lookahead
		;;sets of its items with those of the existing set's items.
		(when (same-cores-p state goto)
		  (dolist (new-item goto)
		    (dolist (item state)
		      (when (eq (car new-item) (car item))
			(nbit-ior (cdr item) (cdr new-item))
			(return))))
		  (return t)))
	      ;;If the new state is not already present, push it on the
	      ;;cdr of the current position in the list of states.
	      (push goto (cdr states)))))))

  (setq nstates (length state-list))
  ;; (when *verbose* (format t ";;; finished: ~d states at ~d~%" nstates (get-universal-time)))

  (setq action-table (make-array *max-terminal-code*))
  (setq known-lexemes
    (make-array (1+ (loop for terminal in terminals maximize (terminal-code terminal)))
		:element-type 'bit :initial-element 0))
  (loop for term in terminals
      do (setf (sbit known-lexemes (terminal-code term)) 1))
  (setf goto-table (make-table (mapcar #'cdr nonterminal-alist) nstates))

  (do ((states state-list (cdr states))
       (n 0 (1+ n)))
      ((null states))
    (push (cons (car states) n) state-nums))

  (when stream
    (format stream "~&Terminals:~%")
    (loop for terminal in terminals
	as term = (if (integerp terminal) (code-char terminal) terminal)
	do (format stream " ~3d ~12a  ~@[~4d~]"
		   (terminal-code terminal)
		   term
		   (terminal-precedence terminal))
	   (format stream "~%"))
    (format stream "~%")

    (format stream "~&Productions:~%")
    (loop for production in (reverse productions)
	do (format stream "~3d: ~:[   ~;{~:*~d}~] ~20a  -> "
		   (production-number production)
		   (production-precedence production)
		   (production-lhs production))
	   (do ((rhs (production-rhs production) (cdr rhs)))
	       ((not (consp rhs)))
	     (format stream " ")
	     (print-term stream (car rhs)))
	   (format stream "~%"))
    (format stream "~%"))

  ;;;
  ;;; Phase 3
  ;;;

  (when *verbose* (format t ";;; Starting Phase 3  at ~d~%" (get-universal-time)))

  (loop for state-num from 0
      for state in state-list
      do
	(and stream (format stream "~&State ~D:~%" state-num))
	;; (format stream "state: ~S~%" state) (force-output stream)
	(dolist (item state)
	  ;; (format stream "item: ~s~%" item) (force-output stream)
	  (let* ((core (car item))
		 (production (do ((rhs core (cdr rhs)))
				 ((not (consp rhs)) rhs))))
	    ;; (format stream "core: ~s~%" core) (force-output stream)
	    ;; (format stream "prod: ~s~%" production) (force-output stream)
	    (when stream
	      (format stream "~&  ~a -> " (production-lhs production))
	      (do ((where (production-rhs production) (cdr where)))
		  ((not (consp where))
		   (when (not (consp core))
		     (format stream "_    (~d)" (production-number production)))
		   (format stream "~%"))
		(when (equal where core) (format stream "_ "))
		(print-term stream (car where))))
	    (cond ((not (consp core))	;If item is completed
		   (loop with lookaheadv = (cdr item)
		       for i below *max-terminal-pcode*
		       unless (eql 0 (sbit lookaheadv i))
		       do (set-action (find i terminals :key #'terminal-pcode)
				      (if (eq production top-level-production)
					  '(accept)
					(cons 'reduce production))
				      action-table
				      state-num
				      nstates
				      grammar
				      stream)))
		  ((member (car core) terminals :test #'eq) ;Shift on terminals.
		   (let ((goto (gotox (car core) state)))
		     ;; (format stream "(car core) ~s goto ~s~%" (car core) goto)
		     (when goto
		       (setq goto (compute-closure goto))
		       (dolist (state state-list)
			 (when (same-cores-p state goto)
			   (set-action (car core)
				       (cons 'shift (cdr (assoc state state-nums :test #'eq)))
				       action-table
				       state-num
				       nstates
				       grammar
				       stream)
			   (return)))))))))
	(dolist (nonterminal (mapcar #'cdr nonterminal-alist))
	  (let ((goto (gotox nonterminal state)))
	    (when goto
	      (setq goto (compute-closure goto))
	      (setf (svref (cdr (assoc nonterminal goto-table :test #'eq)) state-num)
		(dolist (state state-list)
		  (when (same-cores-p state goto)
		    (return (cdr (assoc state state-nums :test #'eq)))))))))
	(when stream
	  (format stream "~%")
	  (loop with alist = nil
	      for code from 0
	      for row across action-table
	      do (when row
		   (let* ((what (svref row state-num))
			  (ent (assoc what alist :test #'equal)))
		     (if ent
			 (push code (cdr ent))
		       (push (list what code) alist))))
	      finally (loop for ent in alist
			  do (block nil
			       (case (caar ent)
				 (reduce (format stream "   Reduce ~D "
						 (production-number (cdar ent))))
				 (accept (format stream "   Accept"))
				 (shift  (format stream "   Shift ~D " (cdar ent)))
				 (t      (return nil)))
			       (pprint-logical-block (stream (cdr ent))
				 (loop for token in (cdr ent)
				     do (write-char #\space stream)
					(pprint-newline :fill stream)
					(print-term-1 stream token grammar)))
			       (format stream "~%"))))
	  (format stream "~%")
	  (dolist (row goto-table)
	    (when (svref (cdr row) state-num)
	      (format stream "  ~20AGoto ~3D~%" (car row) (svref (cdr row) state-num))))
	  (format stream "~%")))

  ;;;
  ;;; Phase 4
  ;;;

  (when *verbose* (format t ";;; Starting Phase 4  at ~d~%" (get-universal-time)))

  ;; Now determine which states have (REDUCE K) or ERROR for all token which therefore
  ;; don't need to look at the token before doing the reduction.  Otherwise, if a state
  ;; has any REDUCE actions, replace all ERROR actions except for the ERROR token with an
  ;; arbitrary one of them.  [This step seems necessary to make the generated parser
  ;; accept everything it should, but is totally indefensible if there is a choice between
  ;; two different reductions.  Some more analysis of this problem seems warranted!]
  (setq defaults (make-array nstates))
  (let ((dispatch (loop
		      with error-token = (find 'error (grammar-terminals grammar)
					       :key #'terminal-terminal)
		      with error-token-num = (and error-token
						  (setf (grammar-error-code grammar)
						    (terminal-code error-token)))
		      for row across action-table
		      for n from 0
		      when row
		      if (eq n error-token-num)
		      do (dotimes (state nstates)
			   (unless (svref row state)
			     (setf (svref row state) '(error))))
		      else collect row))
	(has-reduce (make-array nstates)))
    (dotimes (state nstates)
      (setf (svref defaults state)
	(do ((rows dispatch (cdr rows))
	     (reduction)
	     (action)
	     (default-reduce-p t))
	    ((null rows)
	     (when (and default-reduce-p reduction)
	       (dolist (row dispatch)
		 (unless (svref row state)
		   (setf (svref row state) (cons 'reduce reduction)))))
	     default-reduce-p)
	  (setq action (svref (car rows) state))
	  (unless (or (null action)
		      (and (eq (car action) 'reduce)
			   (or (and (null reduction)
				    (prog1 (setq reduction (cdr action))
				      (setf (svref has-reduce state) action)))
			       (eq reduction (cdr action)))))
	    (setq default-reduce-p nil)))))

    ;; Now fill in everything else with an arbitrary reduce action from that state, or
    ;; error if none exists.
    (dolist (array dispatch)
      (dotimes (state nstates)
	(unless (svref array state)
	  (setf (svref array state) (or (svref has-reduce state)
					'(error)))))))

  ;; Check that all rules can be reduced and all states reached.
  ;; Change the action slots for internal actions from REDUCE to IREDUCE.

  (let* ((nrules (length (grammar-productions grammar)))
	 (rules (make-array nrules :initial-element nil))
	 (states (make-array nstates :initial-element nil))
	 x)
    (setf (aref states 0) t)
    (setf (aref rules 0) t)
    (loop for row across action-table
	when row
	do (dotimes (state nstates)
	     (case (car (setq x (aref row state)))
	       (reduce (setf (aref rules (production-number (cdr x))) t)
		       (when (getf (production-plist (cdr x)) :internal)
			 (rplaca x 'ireduce)))
	       (shift (setf (aref states (cdr x)) t)))))
    (dolist (row goto-table)
      (dotimes (state nstates)
	(and (setq x (aref (cdr row) state))
	     (setf (aref states x) t))))
    (dolist (rule (grammar-productions grammar))
      (when (null (aref rules (production-number rule)))
	(format t "rule ~d ~s is never reduced.~%"
		(production-number rule) rule)
	(when stream
	  (format stream "rule ~d ~s is never reduced.~%"
		  (production-number rule) rule))))
    (dotimes (state nstates)
      (when (null (aref states state))
	(format t "state ~d cannot be reached.~%" state)
	(when stream
	  (format stream "state ~d cannot be reached.~%" state)))))

  ;; Store the number of args for ireduce action functions in the rhs slot of the
  ;; production, which is as good as anywhere else.

  (dolist (prod productions)
    (setf (production-rhs prod)
      (or (getf (production-plist prod) :internal)
	  (production-rhs prod))))

  ;; Store the index of each lhs in the goto table.
  (dolist (prod productions)
    (setf (production-lhs-goto-index prod)
      (or (position (production-lhs prod) goto-table
		    :key (lambda (x) (nonterminal-symbol (car x)))
		    :test #'eq)
	  (error "internal consistency failure lhs-index: ~s" prod))))
  ;; Convert the goto-table to use indexes instead of nonterminal symbols.
  (let ((new (make-array (length nonterminal-alist))))
    (loop for (nil . vec) in goto-table
	for i from 0
	do (setf (svref new i) vec))
    (setf goto-table new))

  ;; Save away the state machine in the grammar structure.
  (setf (grammar-nonterminal-alist grammar) nil)	;No longer needed.
  (setf (grammar-action-table grammar) action-table)
  (setf (grammar-defaults grammar) defaults)
  (setf (grammar-goto-table grammar) goto-table)
  (setf (grammar-longest-rhs grammar) longest-rhs)
  (setf (grammar-known-lexemes grammar) known-lexemes)

  ;;;
  ;;; Phase 5
  ;;;

  (when *verbose* (format t ";;; Starting Phase 5  at ~d~%" (get-universal-time)))

  (when *verbose* (format t "~&; Finished~
 	~[~:; ~:*~d reduce-reduce conflicts~]~
 	~[~:; ~:*~d shift-reduce conflicts~]~
 	~[~:; ~:*~d unknown conflicts~]~
	" *reduce-reduce-conflicts* *shift-reduce-conflicts* *unknown-conflicts*))
  (setf (composedp grammar) t)
  grammar)

;;; Compute the closure of a state: For each item that starts with a nonterminal, add as
;;; new items in the state each production for that nonterminal.  The difficult part is
;;; computing the new lookahead sets.
(defun compute-closure (state)
  ;;(declare (optimize speed (safety 0)))
  ;; Since we also must close over any new items added to the set, we push any new items
  ;; onto the cdr of the current location in the scan through the state set.  This way,
  ;; only one pass through the set is necessary.
  (do ((items state (cdr items)))
      ((null items) state)
    (let ((item (car items))
	  nonterminal new-item core)
      (and (consp (setq core (car item))) ;Ignore ITEM if completed.
	   (nonterminalp (setq nonterminal (car core)))
	   (dolist (prod (nonterminal-productions nonterminal))
	     ;; (format t "~&In dolist prod ~s rhs ~s~%" prod (production-rhs prod))
	     (let ((new-core (production-rhs prod)))
	       (setq new-item		;The resulting item ...
		 (or (dolist (item state)
		       (when (eq (car item) new-core)
			 (return item))) ; ... may already exist
		     (progn (push (cons new-core (make-lookaheadv)) (cdr items))
			    (cadr items)))) ; ... or else create it.
	       ;;Now compute the lookahead set.
	       ;;If any path reaches the end of SYMBOL-LIST without
	       ;;generating a terminal, include the generating item's
	       ;;lookahead list as well.
	       (unless (lookahead new-item (cdr core))
		 (nbit-ior (cdr new-item) (cdr item)))))))))

;;; Compute the lookahead set of a new item by doing a depth-first scan expanding
;;; nonterminals in RHS, stopping on and collecting the first terminal on each path.  This
;;; recursive function adds new items by side effect and returns NIL iff a scan path was
;;; found producing no terminals.
(defun lookahead (new-item core)
  ;;(declare (optimize speed (safety 0)))
  (loop for symbols on core		; Really terminals and nonterminals.
      while (consp symbols)		; Until end of RHS.
      as next = (car symbols)		; Next term in RHS.
      do (cond
	  ((nonterminalp next)
	   (nbit-ior (cdr new-item) (nonterminal-lookaheadv next))
	   (when (nonterminal-lookahead-complete-p next) ; Nonterm always yields a terminal.
	     (return t)))
	  (t				; (typep next 'terminal)
	   (setf (sbit (cdr new-item) (terminal-pcode next)) 1)
	   (return t))			;Path produced a terminal.
	  )))

;; This computes for each nonterminal whether every of possible expansion includes a
;; terminal.  If so, the set of lookahead symbols can be computed without reference to the
;; following context.  This information is stored in the nonterminal-lookahead-complete-p
;; slot of the nonterminal.

(defun precompute-nonterminal-complete-ps (nonterminal-alist)
  (loop as progress = nil
      do (loop for (nil . nonterminal) in nonterminal-alist
	     unless (nonterminal-lookahead-complete-p nonterminal)
		    ;; This nonterminal not yet decided.
	     when (loop for production in (nonterminal-productions nonterminal)
		      always		; Every production always contains a terminal?
			(loop as core = (production-rhs production) then (cdr core)
			    while (consp core)
			    thereis (or (terminalp (car core))
					(nonterminal-lookahead-complete-p (car core)))))
	     do (setf (nonterminal-lookahead-complete-p nonterminal) t
		      progress t))
	 ;; Continue until no new completions in an entire pass.
      while progress))

;; This computes for each nonterminal the value of nonterminal-lookaheadv.  This is a
;; bitvector of initial terminals that can occur in the expansion of this nonterminal.

(defun precompute-nonterminal-lookaheads (nonterminal-alist)
  ;;(declare (optimize speed (safety 0)))

  ;; First, for each nonterminal determine which nonterminals occur in the initial
  ;; positions of any rhs.  Also set any immediate leading terminals in the lookaheadv.
  (loop for (nil . nonterminal) in nonterminal-alist
      do (loop for production in (nonterminal-productions nonterminal)
	     do (loop for core on (production-rhs production)
		    while (consp core)
		    as leading-symbol = (car core)
		    if (terminalp leading-symbol)
		    do (setf (sbit (nonterminal-lookaheadv nonterminal)
				   (terminal-pcode leading-symbol))
			 1)
		       (loop-finish)
		    else do (pushnew leading-symbol
				     (nonterminal-leading-nonterminals nonterminal)
				     :test #'eq)
			 (when (nonterminal-lookahead-complete-p leading-symbol)
			   (loop-finish)))))

  ;; Now iteratively propagate the lookaheads for each nonterminal until the system
  ;; becomes stable.
  (loop as progress = nil
      do (loop for (nil . nonterminal) in nonterminal-alist
	     do (loop for leading-nonterminal in (nonterminal-leading-nonterminals nonterminal)
		    when (nbit-ior (nonterminal-lookaheadv nonterminal)
				   (nonterminal-lookaheadv leading-nonterminal))
					; True if any bits were added.
		    do (setf progress t)))
      while progress)

  )

;;; Compute the next states from the current state.
(defun goto (symbol state)
  (let ((new-state nil))
    (dolist (item state)
      (when (consp (car item))		; rhs not exhausted
	(let ((next-symbol (caar item)))
	  (when (etypecase next-symbol
		  (terminal (eq (terminal-terminal next-symbol) symbol))
		  (nonterminal (eq (nonterminal-symbol next-symbol) symbol)))
	    (push (cons (cdar item) (copy-lookaheadv (cdr item)))
		  new-state)))))
    new-state))

(defun gotox (term/nonterm state)
  (let ((new-state nil))
    (dolist (item state)
      (when (consp (car item))		; rhs not exhausted
	(let ((next-symbol (caar item)))
	  (when (eq next-symbol term/nonterm)
	    (push (cons (cdar item) (copy-lookaheadv (cdr item)))
		  new-state)))))
    new-state))

(defun same-cores-p (state1 state2)
  (declare (optimize speed (safety 0)))
  (and (= (length state1) (length state2))
       (dolist (item1 state1 t)
	 (let ((core1 (car item1)))
	   (unless (dolist (item2 state2)
		     (when (eq core1 (car item2))
		       (return t)))
	     (return nil))))))

(defun make-table (nonterminals nstates)
  (loop for nonterminal in nonterminals
      collect (cons nonterminal (make-array nstates))))

(defun make-lookaheadv ()
  (make-array *max-terminal-pcode* :element-type 'bit :initial-element 0))

(defun copy-lookaheadv (original)	; Consider a compiler macro.
  (copy-seq original))

(defun nbit-ior (target source)
  ;; Destructively iors the second bitvec into the first, returning true iff any new bits
  ;; were set.
  (declare (optimize (speed 3) (safety 0))
	   (simple-bit-vector target source))
  (loop with any-new = nil
      for i fixnum below *max-terminal-pcode*
      when (and (= 1 (sbit source i))
		(= 0 (sbit target i)))
      do (setf any-new t)
	 (setf (sbit target i) 1)
      finally (return any-new)))

;; A TERMINAL may have associated with it a precedence which is an integer based on its
;; position in the succession of :LEFT-ASSOCIATIVE, :RIGHT-ASSOCIATIVE, and
;; :NON-ASSOCIATIVE options to DEFGRAMMAR.  Each production may have a precedence derived
;; from some terminal.  This terminal is either given explicitly in the DEFPRODUCTION
;; form, or else defaults to the last TERMINAL in the RHS of the rule.  If no terminal is
;; associated with a rule in this way, or if the terminal has no precedence associated
;; with it, the rule's precedence defaults to 0.

(defun assign-terminal-precedences (grammar)
  (loop with terminals = (grammar-terminals grammar)
      for pred from 1
      for level in (grammar-precedence-list grammar)
      do (loop for term in level
	     as terminal = (find term terminals :key #'terminal-terminal)
	     when terminal
	     do (setf (terminal-precedence terminal) pred))))

(defun assign-rule-precedences (grammar)
  (loop with terminals = (grammar-terminals grammar)
      for prod in (grammar-productions grammar)
      as explicit = (getf (production-plist prod) :precedence)
      if explicit
      do (setf (production-precedence prod)
	   (terminal-precedence (or (find explicit terminals :key #'terminal-terminal)
				    (error "Precedence for rule ~s is unknown for terminal ~s"
					   prod explicit))))
      else
      do (do ((rhs-symbols (production-rhs prod) (cdr rhs-symbols))
	      (terminals (grammar-terminals grammar))
	      (last-terminal))
	     ((not (consp rhs-symbols))
	      (when last-terminal
		(setf (production-precedence prod) (terminal-precedence last-terminal))))
	   (when (member (car rhs-symbols) terminals :test #'eq)
	     (setq last-terminal (car rhs-symbols))))))

;; This function is called during construction of the parser to fill in the
;; action table.  It's major task is to resolve and/or report conflicts.

(defun set-action (terminal val table index nstates grammar stream
		   &aux report rule-prec term-prec)
  (let* ((symbol (terminal-terminal terminal))
	 (code (terminal-code terminal))
	 (row (or (svref table code)
		  (setf (svref table code) (make-array nstates))))
	 (old (svref row index)))
    (when
	(cond
	 ((null old) t)			;Nothing yet there.
	 ((equal val old) nil)		;Same as previous?
	 ((eq (car old) 'error) nil)	;Don't change ERROR. [???]
	 ;; Reduce-reduce conflict: Prefer the earlier production, and always report the
	 ;; conflict.  But why not allow explicit precedences to disambiguate here?
	 ;; Explicit control added 22Nov03 smh.
	 ((and (eq (car old) 'reduce) (eq (car val) 'reduce))
	  (setq rule-prec (production-precedence (cdr old))
		term-prec (production-precedence (cdr val))) ; actually, new reduce
	  (cond ((or (null rule-prec) (null term-prec))
		 (cond ((< (production-number (cdr old)) (production-number (cdr val)))
			(setq report :old)
			nil)
		       (t (setq report :new) t)))
		(t (cond ((> term-prec rule-prec) #+never (setq report :new) t)
			 ((< term-prec rule-prec) #+never (setq report :old) nil)
			 ((< (production-number (cdr old)) (production-number (cdr val)))
			  (setq report :old)
			  nil)
			 (t
			  (setq report :new)
			  t)))))
	 ((eq (car old) 'reduce)	; Shift-reduce conflict
	  (setq rule-prec (production-precedence (cdr old))
		term-prec (terminal-precedence terminal))
	  (cond ((or (null rule-prec)
		     (null term-prec))
		 (setq report :new)
		 t)
		((< rule-prec term-prec) #+never (setq report :new) t)
		((> rule-prec term-prec) #+never (setq report :old) nil)
		((member symbol (grammar-non-associative grammar) :test #'eq)
		 (setq val '(error)))
		((member symbol (grammar-left-associative grammar) :test #'eq)
		 nil)
		(t t)))
	 (t				; Reduce-shift conflict
	  (setq rule-prec (production-precedence (cdr val))
		term-prec (terminal-precedence terminal))
	  (cond ((or (null rule-prec)
		     (null term-prec))
		 (setq report :old)
		 nil)
		((< rule-prec term-prec) #+never (setq report :old) nil)
		((> rule-prec term-prec) #+never (setq report :new) t)
		((member symbol (grammar-non-associative grammar) :test #'eq)
		 (setq val '(error)))
		((member symbol (grammar-left-associative grammar) :test #'eq)
		 t)
		(t nil))))
      (setf (svref row index) val))
    (when report
      (case (car old)
	(reduce (incf *reduce-reduce-conflicts*))
	(shift (incf *shift-reduce-conflicts*))
	(t *unknown-conflicts*))
      (dolist (s (if stream (list stream t) '(t)))
	(pprint-logical-block (s nil)
	  (fresh-line s)
	  (format s "~1iConflict at state ~D for ~:_" index)
	  (print-term s symbol) (format s "between ~:_")
	  (print-action s old)
	  (format s " ~:_and ~:_") (print-action s val)
	  (format s ".~2i~% Preferring the ~:[former~;latter~] [~a:~a].~%"
		  (eq report :new) term-prec rule-prec))))))

;; There are two user-visible forms for defining a grammar, each available in both macro
;; and function form.

(defmacro defgrammar (name (&rest supers) (&rest slots) &rest options)
  (multiple-value-bind (options left-associative right-associative non-associative
			lexemes precedences)
      (flet ((dechar (list)
	       (loop for c in list
		   if (characterp c)
		   collect (char-code c)
		   else collect c)))
	(loop for option in options
	    as key = (car option)
	    as val = (dechar (cdr option))
	    if (eq key :left-associative)
	    append val into left and
	    collect val into prec
	    else if (eq key :right-associative)
	    append val into right and
	    collect val into prec
	    else if (eq key :non-associative)
	    append val into non and
	    collect val into prec
	    else if (eq key :lexemes)
	    append val into lexemes
	    else collect option into others
	    finally (return (values others left right non lexemes prec))))
    `(progn #+never (eval-when (compile load eval)
		      (setf (find-class ',name) nil)) ; ouch -- compile-time?
	    (defclass ,name
		,(if (member 'grammar supers) supers (append supers '(grammar)))
	      (,@slots)
	      ,@(if (assoc :metaclass options) nil '((:metaclass grammar-class)))
	      (:left-associative ,@left-associative)
	      (:right-associative ,@right-associative)
	      (:non-associative ,@non-associative)
	      (:precedence-list ,@precedences) ; not a public initarg
	      (:lexemes ,@lexemes)
	      ,@options))))

(defmacro defproduction (&environment environment (lhs grammar-name) rhs
			 &optional (action-code nil action-code-p)
			 &rest options)
  (let ((grammar (if (symbolp grammar-name)
		     (find-class grammar-name t environment)
		   grammar-name))
	(internal-productions nil)
	precedence internal)
    (loop for (key val) in options
	do (ecase key
	     (:precedence (setf precedence val))
	     (:internal   (setf internal   val))))
    (or (and lhs (symbolp lhs))
	(error "In grammar ~A the LHS of a rule is not a symbol: ~S"
	       grammar-name lhs))
    (and (member lhs (grammar-terminals grammar) :test #'eq)
	 (error "In grammar ~A a terminal appears on the LHS of a rule: ~S" grammar-name lhs))
    (or (listp rhs)
	(error "In grammar ~A the RHS of rule for ~A is not a list: ~S" grammar-name lhs rhs))
    (let ((rhs1 (loop for token in rhs
		    for n from 0
		    if (characterp token) ;Interpret characters as lexemes.
		    collect (char-code token) ;Workaround CL/SCL lossage.
		    else if (stringp token) ;Interpret string as sequence of chars.
		    nconc (loop for char across token collect (char-code char))
		    else if (numberp token) ; deprecated -- char-code for character
		    collect token
		    else if (consp token) ; internal action code
		    collect (let ((sym (gensym)))
			      (push `(defproduction (,sym ,grammar-name)
					 nil
				       ,token ; action code
				       (:internal ,n)) ; number of terms to the left for args
				    internal-productions)
			      sym)
		    else collect token))
	  (args (loop for i from 1 to (or internal (length rhs))
		    collect (intern (format nil "$~d" i))))
	  (production (gensym)))
      `(progn
	 (eval-when (compile eval)
	   (add-production
	    ',grammar
	    (let ((,production
		   (make-production
		    :number (1+ (production-number (car (grammar-productions ',grammar))))
		    :lhs ',lhs
		    :rhs ',rhs1
		    )))
	      ,@(when precedence
		  `((setf (getf (production-plist ,production) :precedence) ',precedence)))
	      ,@(when internal
		  `((setf (getf (production-plist ,production) :internal) ',internal)))
	      ,production)
	    ',lhs
	    ',rhs1))
	 (eval-when (load eval)
	   (push ,(if action-code-p
		      `(named-function ,(make-symbol (with-output-to-string (s)
						       (print-production-1 lhs rhs1 s)))
			 (lambda (grammar ,@args)
			   (declare (ignorable grammar ,@args))
			   ,@action-code))
		    `#'$$)
		 (grammar-action-code-functions
		  (if (symbolp ',grammar-name)
		      (find-class ',grammar-name)
		    ',grammar-name))))
	 ,@internal-productions))))

(defun add-production (grammar production lhs rhs)
  (pushnew lhs (grammar-symbols grammar))
  (loop for token in rhs
      when (numberp token)
      do (pushnew token (grammar-symbols grammar))
	 (pushnew token (grammar-terminals grammar)))
  (push production (grammar-productions grammar)))

;;; Formatted print functions for a compiled grammar, useful for debugging.
#+needs-rewrite
(defun describe-grammar (name &optional (s *standard-output*))
  (let* ((gram (or (and (typep name 'grammar-class) name)
		   (find-class name nil)))
	 (productions (reverse (grammar-productions gram)))
	 (*package* (symbol-package (class-name gram)))
	 nstates)
    (format s "~&Grammar:~20T~S~%" (class-name gram))
    (format s "Terminals:~20T")
    (pprint-logical-block (s (grammar-terminals gram))
      (loop
	(pprint-exit-if-list-exhausted)
	(print-term s (pprint-pop))
	(pprint-newline :fill s)))
    (format s "~&Nonterminals:~20t")
    (pprint-logical-block (s (set-difference (grammar-symbols gram)
					     (mapcar #'terminal-code (grammar-terminals gram))
					     :test #'eql))
      (loop
	(pprint-exit-if-list-exhausted)
	(print-term s (pprint-pop))
	(pprint-newline :fill s)))
    (when (grammar-left-associative gram)
      (format s "~&Left associative:~20t")
      (dolist (term (grammar-left-associative gram))
	(print-term s term)))
    (when (grammar-right-associative gram)
      (format s "~&Right associative:~20t")
      (dolist (term (grammar-right-associative gram))
	(print-term s term)))
    (when (grammar-non-associative gram)
      (format s "~&Non associative:")
      (dolist (term (grammar-right-associative gram))
	(print-term s term)))
    (let ((precs (grammar-precedence-list gram)))
      (when precs
	(format s "~&Precedence:")
	(dolist (prec precs)
	  (format s "~20t")
	  (dolist (term (if (atom prec) (list prec) prec)) (print-term s term))
	  (format s "~%"))))
    (dolist (prod productions)
      (format s "~&~2d: ~s -> " (production-number prod) (production-lhs prod))
      (do ((terms (production-rhs prod) (cdr terms)))
	  ((not (consp terms)))
	(print-term s (car terms)))
      #+nomore
      (let ((*print-length* 4) (*print-level* 3))
	(format s "~56t~@[~s~]~@[ ~s~]~%"
		(cond ((null (production-action-code prod)) nil)
		      ((consp (production-action-code prod))
		       (cdddr (production-action-code prod)))
		      (t (production-action-code prod)))
		(production-plist prod)))
      (format s "~56t~@[~s~]~%" (production-plist prod)))
    (if (null (grammar-goto-table gram))
	(format s "The grammar has not yet been compiled.~%")
      (progn
	(setq nstates (array-dimension (grammar-defaults gram) 0))
	(format s "Gotos:    ")
	(dotimes (n nstates) (format s "~4,'-d" n))
	(loop for line across (grammar-goto-table gram)
	    do (let ((nonterm (car line))
		     (array (cdr line)))
		 (format s "~%~11s" nonterm)
		 (dotimes (n nstates)
		   (let ((x (svref array n)))
		     (if x (format s "~3d " x) (format s "  . "))))))
	(format s "~%Actions:  ")
	(dotimes (n nstates) (format s "~4,'-d" n))
	(format s "~%Defaults: ")
	(dotimes (n nstates)
	  (if (svref (grammar-defaults gram) n)
	      (format s "   X")
	    (format s "   .")))
	(format s "~%")
	(dolist (action (grammar-action-table gram))
	  (let ((term (car action))
		(array (cdr action)))
	    (if (numberp term)
		(if (char-name (code-char term))
		    (format s "~10a" (char-name (code-char term)))
		  (format s "~c~10t" (code-char term)))
	      (format s "~10s" term))
	    (dotimes (n nstates)
	      (let ((act (svref array n)))
		(case (car act)
		  (error (format s "   E"))
		  (accept (format s "   A"))
		  (reduce (format s "~3DR" (production-number (cdr act))))
		  (ireduce (format s "~3DI" (production-number (cdr act))))
		  (shift (format s "~3DS" (cdr act)))
		  (t (format s "   ?")))))
	    (format s "~%")))))
    (values)))
