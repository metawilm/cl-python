;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON; Readtable: PY-AST-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)
(in-syntax *ast-user-readtable*)

;;;; Python compiler

;;; Compilation context macros: reader, host compiler settings

(defun call-with-python-code-reader (initial-forms func)
  "Let the Python parser handle all reading."
  (with-sane-debugging ("Error occured while reading input with the Python readtable:~%  ~A")
    ;; The below SETUP-OMNIVORE-READMACRO can't be shared (e.g. inside LOAD-TIME-VALUE), as
    ;; it contains state (the initial forms).
    (let ((*readtable* (setup-omnivore-readmacro :function #'clpython.parser:parse
                                                 :initial-forms initial-forms
                                                 :readtable (copy-readtable nil))))
      (funcall func))))

(defmacro with-proper-compiler-settings (&body body)
  `(call-with-proper-compiler-settings (lambda () ,@body)))

(defun call-with-proper-compiler-settings (func)
  "The idea is to save as much debug information as possible."
  (handler-bind (#+sbcl (sb-int:simple-compiler-note #'muffle-warning)
                 #+allegro (warning (lambda (c)
                                      (when (loop for arg in (slot-value c 'excl::format-arguments)
                                                thereis (and (stringp arg)
                                                             (search "is incompatible for numeric operation" arg)))
                                        ;; The compiler macro for != expands into inline guarded to /=.
                                        ;; When arg is not a number, like for:  if x != 'a'
                                        ;; this warning will come. Spurious because of the (numberp etc) guard.
                                        (muffle-warning c)))))
    (let #+clpython-source-level-debugging ((compiler:save-source-level-debug-info-switch t)
                                            (compiler:save-local-names-switch t)
                                            (compiler:tail-call-self-merge-switch nil)
                                            (compiler:tail-call-non-self-merge-switch nil)
                                            #+(or) ;; also?
                                            (excl::*record-source-file-info* t))
         #-clpython-source-level-debugging ()
         
         (funcall func))))


;;; Compiling Python source files, and loading their fasls

(defparameter *current-module-name* *__main__-module-name*
  "The name of the module now being compiled; module.__name__ is set to it.")

(defun compile-py-source-file-to-lisp-source (&key filename output-file)
  (with-standard-io-syntax ;; No surprises for e.g. *print-length*
    (let ((progn-form (call-with-python-code-reader
                       `((in-package :clpython))
                       (lambda ()
                         (let (#+ecl (c::*debug-compiler* t))
                           (with-open-file (in filename :direction :input)
                             (read in)))))))
      (assert (and (listp progn-form)
                   (eq (car progn-form) 'progn)))
      (with-open-file (out output-file :direction :output :element-type 'character :if-exists :supersede)
        (loop for form in (cdr progn-form)
            do (write form :stream out)
               (write-char #\Newline out))))))

(defun compile-py-source-file (&key filename mod-name output-file)
  (assert (and filename mod-name output-file))
  (let ((*current-module-name* mod-name))  ;; used by compiler
    (with-auto-mode-recompile (:filename filename)
      (with-proper-compiler-settings
          (clpython.parser:with-source-locations
              
              (let ((*package* #.(find-package :clpython))) ;; Package during compilation
                (call-with-python-code-reader ;; .. must be equal to
                 `((in-package :clpython) ;; .. package while loading (CLHS 3.2.4.4)
                   #+(or)(in-module :name ',mod-name
                                    :src-pathname ',filename
                                    :bin-pathname ',output-file))
                 (lambda ()
                   (let (#+ecl (c::*debug-compiler* t))
                     (compile-file filename
                                   :output-file output-file))))))))))

#+(or) ;; intended as high-level interface for users
(defun compile-py-file (fname &key (verbose t) source-information)
  (let* ((module (pathname-name fname))
         (fasl-file (compiled-file-name :module module fname))
         (*import-force-recompile* t))
    (declare (special *import-force-recompile*))
    (flet ((do-compile ()
             (%compile-py-file fname :mod-name module :output-file fasl-file)))
      (if source-information
          (clpython.parser:with-source-locations
              (do-compile))
        (do-compile)))))

(defun load-python-file-with-hooks (&key filename on-pre-import on-import-success on-import-fail)
  "Loads given FILENAME (either a source or fasl file) with given hooks.
Hook arguments:
 (ON-PRE-IMPORT MODULE NEW-MODULE-P)
 (ON-IMPORT-SUCCESS MODULE NEW-MODULE-P SOURCE-FUNC SOURCE)
 (ON-IMPORT-FAIL MODULE NEW-MODULE-P)
Returns the module loading success, or NIL after failure.
Callers can intercept the condition MODULE-IMPORT-PRE to further customize loading behaviour."
  (assert filename)
  (let #1=(module new-module-p source-func source)
    (flet ((handle-module-import-pre (c)
             ;; Being muffled means condition was intended for an earlier handler,
             ;; corresponding to a nested inner import action.
             (unless (mip.muffled c)
               (unless (fasl-matches-compiler-p (mip.compiler-id c))
                 (when (mip.is-compiled c)
                   (whereas ((r (find-restart 'delete-fasl-try-again)))
                            (format t "~&;; Recompiling obsolete Python fasl file.~%")
                            (invoke-restart r)))
                 (fasl-mismatch-cerror filename))
               (setf module (mip.module c)
                     new-module-p (mip.module-new-p c)
                     source-func (mip.source-func c)
                     source (mip.source c)
                     (mip.muffled c) t)
               (when on-pre-import
                 (funcall on-pre-import module new-module-p)))))
      (let ((load-success))
        (unwind-protect
            (handler-bind ((module-import-pre #'handle-module-import-pre))
              (with-auto-mode-recompile (:filename filename :restart-name delete-fasl-try-again)
                (let (#+lispworks (system:*binary-file-type* (string-downcase *py-compiled-file-type*)))
                  ;; LOAD can e.g. return NIL, if loading fails and user invoked a restart to let LOAD return NIL.
                  (when (setf load-success (load filename))
                    (when on-import-success
                      (funcall on-import-success . #1#))))))
          (unless load-success
            (when on-import-fail
              (funcall on-import-fail module new-module-p))))
        (when load-success module)))))

(defparameter *compile-python-ast-before-running* nil
  "Whether to compile an AST before running it.")

(defparameter *run-module-globals* nil
  "The module namespace in which RUN forms are evaluated.")

(defun run-python-ast (ast &key (habitat *habitat*)
                                (compile *compile-python-ast-before-running*)
                                (module-globals *run-module-globals*)
                                time
                                (args nil args-p))
  "Run Python AST in freshly bound habitat.
HABITAT is the execution environment; a fresh one will be used otherwie.
If COMPILE is true, the AST is compiled into a function before running.
MODULE-RUN-ARGS is a list with options passed on to the module-function; e.g. %module-globals, module-name, src-module-path.
ARGS are the command-line args, available as `sys.argv'; can be a string (which will be splitted on spaces) or a list of strings."
  
  (unless (and (listp ast) (eq (car ast) '[module-stmt]))
    (error "Not an AST: ~S" ast))
  
  ;; At the moment there are only hashtable or package module namespaces:
  (with-compiler-generated-syntax-errors ()
    (handler-bind (#+sbcl
                   (sb-kernel:redefinition-with-defun #'muffle-warning))
      (let* ((get-module-f `(lambda () ,ast))
             fc module-init-func module-run-tlv-func result)
        ;; Set FC
        (when compile
          ;; Same context as for importing a module
          (with-proper-compiler-settings
              (multiple-value-bind (func)
		  (block compilation
		    (handler-bind (#+ecl 
				   ((or c:compiler-error c:compiler-internal-error simple-error)
                                    (lambda (c)
                                      (warn "Compilation failed: ~S" c)
                                      (return-from compilation nil))))
                      (compile nil get-module-f)))
                (cond (func
                       (setf fc func))
                      (t
                       (warn "CLPython detected a compilation failure, possibly indicating a bug in the Lisp compiler.")
                       (warn "The Python code will be run interpreted instead."))))))
        (unless fc
          (setf fc (coerce get-module-f 'function)))
        (assert fc)
        (handler-bind ((module-import-pre (lambda (c)
                                            ;; This handler just saves the relevant functions,
                                            ;; unwinding the import state with restarts like
                                            ;; continue-loading, abort-loading, so the user is not
                                            ;; bothered by these restarts.
                                            (setf module-init-func (mip.init-func c)
                                                  module-run-tlv-func (mip.run-tlv-func c))
                                            (invoke-restart 'abort-loading))))
          (funcall fc))
        (assert (and module-init-func module-run-tlv-func) () "Unexpected module import behaviour")
        (let ((*habitat* (or habitat (make-habitat))))
          (unless module-globals
            (setf module-globals (habitat-module-globals *habitat*)))
	  (assert (with-py-dict (hash-table-p module-globals)))
          (when args-p
            (setf (habitat-cmd-line-args *habitat*) args))
          (flet ((run ()
                   (funcall module-init-func module-globals) ;; always set __name__, __debug__
                   (setf result (funcall module-run-tlv-func module-globals))))
            (if time (time (run)) (run))))
        result))))


;;; Python source files are compiled to fasl files. A way is needed to mark
;;; the compiler version used to produce the fasl, so outdated fasls can be
;;; automatically recompiled if the corresponding source file is available.
;;;
;;; As the "compiler version" is mostly determined by the macros below, it
;;; seems sound to take the hash of _this_ source file as compiler id.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *clpython-compiler-version-id* 
      #.(let* ((old-id (when (boundp '*clpython-compiler-version-id*)
                         (symbol-value '*clpython-compiler-version-id*)))
               (new-id (cond (*compile-file-truename* (sxhash (slurp-file *compile-file-truename*)))
                             (*load-truename* (sxhash (slurp-file *load-truename*)))
                             (t (warn "Can't calculate CLPython compiler version id.")
                                -1))))
          (when (and old-id (/= old-id new-id))
            (format t "~&;; CLPython compiler version id has changed, from ~A to ~A." old-id new-id))
          new-id)
      "CLPython compiler version, stored in Python fasl files and checked at load time."))

(eval-when (:load-toplevel)
  #+(or)(format t "~&;; CLPython compiler version id: ~A.~%" *clpython-compiler-version-id*))

(defun fasl-matches-compiler-p (fasl-compiler-version-id)
  (= fasl-compiler-version-id *clpython-compiler-version-id*))

(defun fasl-mismatch-cerror (fasl-name)
  (cerror "Continue anyway with outdated fasl file ~A"
          "Python fasl file ~A was created by another version of the CLPython compiler. ~
           The fasl code may not work with the current CLPython runtime. ~
           It is best to recompile the Python source, then load the new fasl."
          fasl-name))
  

;;; Translates a Python module AST into a Lisp function.
;;; 
;;; Each node in the s-expression returned by the
;;; parse-python-{file,string} corresponds to a macro defined below
;;; that generates the corresponding Lisp code.
;;; 
;;; Each such AST node has a name ending in "-expr" or "-stmt", they are
;;; in the :clpython.ast.node package.
;;; 
;;; In the macro expansions, lexical variables that keep context state
;;; have a name like +NAME+.

;;; Language Semantics

(defvar *allow-indirect-special-call* nil
  "Whether `eval', `locals' and `globals' can be called indirectly, like:
 x = locals; x()
If true, the compiler must generate additional code for every call,
and execution will be slower. It is very rare for Python code to do
such indirect special calls.")
;; This is similar to the Javscript restriction on `eval' (ECMA 262, paragraph 15.1.2.1)

(defvar *mangle-private-variables-in-class* t
  "In class definitions, replace __foo by _CLASSNAME__foo, like CPython does")

(defmacro with-complete-python-semantics (&body body)
  `(let ((*allow-indirect-special-call* t)
	 (*mangle-private-variables*    t))
     ,@body))

(defvar *__debug__* t
  "The ASSERT-STMT uses the value of *__debug__* to determine whether
or not to include the assertion code.")

(defconstant-once +standard-module-globals+ '({__name__} {__debug__})
  "Names of global variables automatically created for every module")

;;; Compiler optimizations

(defvar *inline-number-arithmetic* t
  "For common arithmetic operations (+,-,*,/) the (often common) two-fixnum case is inlined")

(defvar *inline-builtin-methods* t
  "Inline method calls to common builtin methods (with a run-time check) for method calls
like .join (string.join), .sort (list.sort), etc")

(defvar *inline-getattr-call* t
  "Inline getattr(x,y).(zzz) calls, which usually saves creation of a temporary bound method.")

(defvar *inline-print* t
  "Inline calls to `print', which will improves efficiency of printing strings and fixnums.")

(defvar *optimize-function-arg-checking* t
  "Whether to optimize the check on function receiving correct number of arguments.")

(defmacro without-inlining (&body body)
  `(let ((*inline-number-arithmetic* nil)
         (*inline-builtin-methods* nil)
         (*inline-getattr-call* nil)
         (*inline-print* nil)
         (*optimize-function-arg-checking* nil))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant-once +optimize-debug+   '(optimize (speed 0) (safety 3) (debug 3) (space 0)))
  (defconstant-once +optimize-std+     '(optimize (speed 3) (safety 1) (debug 1)))
  (defconstant-once +optimize-fast+    '(optimize (speed 3) (safety 1)))
  (defconstant-once +optimize-fastest+ '(optimize (speed 3) (safety 0) (debug 0))))

(defmacro fastest (&body body)
  `(locally (declare ,+optimize-fastest+)
     ,@body))

(defmacro fixnump (x)
  #+allegro `(excl::fixnump ,x)
  #-allegro `(typep ,x 'fixnum))

(define-compiler-macro fixnump (&whole whole x)
  (typecase x
    (number (typep x 'fixnum))
    (string nil)
    (t     whole)))

;;; `Exex' statement handling

(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *exec-early-parse-constant-string* t
  "Whether a constant string argument to the `exec' statement is parsed at compile time already.")
)

(defvar *exec-stmt-compile-before-run* t
  "Whether the code for `exec' statements is compiled before being run.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setf expansion

;; In addition to the standard 5 values, there is also a "delete" form
;; relevant in Python. This form used to be returned as the sixth form
;; by the setf expanders defined in this file. However, that is not a
;; portable mechanism; it turns out CMUCL does not communicate the
;; sixth value to the caller of "get-setf-expansion". Thus this hack.

(defparameter *want-DEL-setf-expansion* nil
  "Whether the requested setf expansion is for a 'delete' stmt, not for
an assigment statement. This changes at least the returned 'store' form.")
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  The macros corresponding to AST nodes

(defun assert-stmt-1 (test test-ast raise-arg)
  (with-simple-restart (:continue "Ignore the assertion failure")
    (unless (py-val->lisp-bool test)
      (py-raise '{AssertionError} (or raise-arg 
				    (format nil "Failing test: ~A"
					    (with-output-to-string (s)
					      (py-pprint test-ast s))))))))

(defmacro [assert-stmt] (test raise-arg)
  ;; The decision whether or not to execute `assert' statements
  ;; is taken at compile-time.
  ;; WISH: if assertion is a comparison like "a < b+3" then show values
  ;;       of left and right side in error message.
  (when *__debug__*
    `(assert-stmt-1 ,test ',test ,raise-arg)))

(defun assign-stmt-list-vals (iterable num-targets)
  ;; This function is called to parse tuple args, like (x,y) in "def f((x,y)): ..
  ;; therefore optimize.
  (declare #.+optimize-fastest+ (fixnum num-targets))
  (let ((val-list (if (listp iterable) iterable (py-iterate->lisp-list iterable))))
    (declare (list val-list))
    (unless (= (length val-list) num-targets)
      (py-raise '{ValueError}
                "Assignment to multiple targets: wanted ~A values, but iteration gave ~A, from object ~A."
                num-targets (length val-list) (py-repr-string iterable)))
    val-list))

(defun target-get-bound-vars (tg)
  (loop with todo and res
      for x = tg then (pop todo)
      while x
      do (when (listp x) ;; Usually true, but not in [with-stmt] expansion.
           (ecase (first x)
             ([attributeref-expr] )
             ([subscription-expr] )
             ([identifier-expr]          (push (second x) res))
             (([list-expr] [tuple-expr]) (setf todo (nconc todo (second x))))))
      finally (return res)))
             
(defun assign-stmt-get-bound-vars (ass-stmt)
  ;; Valid for ASSIGN-STMT targets and DEL-STMT target.
  (with-matching (ass-stmt ([assign-stmt] ?value ?targets))
    (declare (ignore ?value))
    (mapcan #'target-get-bound-vars ?targets)))

(defun check-valid-assignment-targets (targets &key augmented)
  (labels ((check (tg)
             (when (listp tg)
               (let ((node (car tg)))
                 (when (member node '([attributeref-expr] [subscription-expr]
                                      [identifier-expr]))
                   (return-from check))
                 (unless augmented
                   (when (member node '([tuple-expr] [list-expr]))
                     (dolist (tg (second tg))
                       (check tg))
                     (return-from check)))))
             ;; Surround source code with quotes, unless it is a string itself.
             (raise-syntax-error
              "Invalid ~Aassignment target: ~A~A~A."
              (if augmented "augmented " "")
              (if (stringp tg) "" #\`)
              (py-pprint tg)
              (if (stringp tg) "" #\'))))
    (dolist (tg targets)
      (check tg)))
  t)

(defmacro [assign-stmt] (value targets)
  (check-valid-assignment-targets targets)
  (with-gensyms (assign-val)
    `(let ((,assign-val ,value))
       ,@(loop for tg in targets collect `(setf ,tg ,assign-val)))))

(define-compiler-macro [assign-stmt] (&whole whole value targets)
  ;; Shortcut the case "a,b,.. = 1,2,.." where left and right same number of
  ;; items. This saves creation of a tuple for RHS.
  (check-valid-assignment-targets targets)
  (if (and (or (match-p value '([tuple-expr] ?items))
               (match-p value '([list-expr] ?items)))
           (or (match-p targets '(([tuple-expr] ?items)))
               (match-p targets '(([list-expr] ?items))))
           (= (length (second value)) (length (second (car targets)))))
      
      ;; Note that by using PSETF, we force values to be evaluated before targets.
      `(psetf ,@(mapcan #'list (second (car targets)) (second value)))
    
    whole))

(defmacro [attributeref-expr] (item attr)
  (with-matching (attr ([identifier-expr] ?name))
    `(attr ,item ',?name)))

(define-setf-expander [attributeref-expr] (item attr)
  (with-matching (attr ([identifier-expr] ?name))
    (with-gensyms (prim store)
      (let ((store-form `(with-pydecl ((:inside-setf-py-attr t))
                           (setf (attr ,prim ',?name) ,store)))
            (del-form `(with-pydecl ((:inside-setf-py-attr t))
                         (setf (attr ,prim ',?name) nil))))
        (values `(,prim) ;; temps
                `(,item) ;; values
                `(,store)   ;; stores
                (if *want-DEL-setf-expansion* del-form store-form) ;; store/delete form
                `(attr ,prim ',?name)))))) ;; read-form

(defmacro [augassign-stmt] (&whole whole op place val &environment env)
  (check-valid-assignment-targets (list place) :augmented t)
  (case (car place)
    
    (([attributeref-expr] [subscription-expr] [identifier-expr])
     ;; XXX should also use __coerce__ http://www.python.org/dev/peps/pep-0203/
     (let ((py-@= (get-binary-iop-func-name op))
	   (py-@  (get-binary-op-func-name-from-iop op)))
       (multiple-value-bind (vars vals stores writer reader)
	   (get-setf-expansion place env)
	 (assert (null (cdr stores)))
	 (with-gensyms (place-val-now op-val new-val)
	   `(let* (,@(mapcar #'list vars vals)
                   (,op-val ,val)
                   (,place-val-now ,reader)
                   (,new-val))
              (tagbody
                
                ;; The @= functions are not defined on numbers and strings.
                ;; Check for fixnum inline.
                (when (fixnump ,place-val-now)
                  (go non-aug))
                
                ;; py-@= returns NIL iff __i@@@__ not found
                (setf ,new-val (,py-@= ,place-val-now ,op-val))
                (cond ((not ,new-val)
                       (go non-aug))
                      (t
                       (go set-new-val)))
               non-aug
                (setf ,new-val (,py-@ ,place-val-now ,op-val))
                
               set-new-val
                (let ((,(car stores) ,new-val))
                  ,writer)))))))
    
    (t (raise-syntax-error "Invalid augmented assignment: ~A"
                           (py-pprint whole nil)))))

(defmacro [backticks-expr] (item)
  `(py-repr ,item))

(defmacro [binary-expr] (op left right)
  `(,(get-binary-op-func-name op) ,left ,right))

(defmacro [binary-lazy-expr] (op left right)
  (ecase op
    ([or] `(let ((.left ,left))
	     (if (py-val->lisp-bool .left)
		 .left
               ,right)))
    ([and] `(let ((.left ,left))
	      (if (py-val->lisp-bool .left)
		  ,right
		.left)))))

(defmacro [bracketed-expr] (expr)
  ;; Brackets are only used to force parsing precedence.
  expr)
  
(defmacro [break-stmt] (&environment e)
  (if (get-pydecl :inside-loop-p e)
      `(go .break)
    (raise-syntax-error "Statement `break' was found outside loop.")))

;; EXEC-STMT here, because CALL-EXPR needs it

(defmacro [exec-stmt] (code-string globals locals &key (allowed-stmts t))
  ;; TODO:
  ;;   - allow code object etc as CODE
  ;;
  ;; An EXEC-STMT is translated into a Python suite containing a
  ;; function definition and a subsequent call of the function.
  ;;
  ;; ALLOWED-STMTS: if T:      allow all statements
  ;;                   a list: allow only those statements
  ;;                   NIL:    allow no statements
  ;;  (not evaluated)
  `(multiple-value-bind (glo loc)
       ,(cond ((and globals locals) `(values ,globals ,locals))
              (globals              `(let ((x ,globals))
                                       (values x x))) ;; globals also used for locals
              (t                    `(values (%globals) (%locals))))
     (when (typep glo 'symbol-hash-table)
       (setf glo (sht-ht glo)))
     (when (typep loc 'symbol-hash-table)
       (setf loc (sht-ht loc)))
     #+(or)(exec-stmt-check-namespaces glo loc)
     (exec-stmt-string ,code-string glo loc ',allowed-stmts)))

(defun exec-stmt-check-namespaces (globals locals)
  (check-type globals dict) ;; todo: support any mapping for globals, locals
  (check-type locals dict)
  (flet ((check-is-namespace-dict (d)
           ;; Ensure dict has only string keys.
           (check-type d dict)
           ;; XXX todo
           #+(or)
           (dikt-map (py-dict-dikt d)
                     (lambda (k v)
                       (declare (ignore v))
                       (unless (typep k '(or string symbol))
                         (py-raise
                          '{TypeError}
                          "Cannot use ~A as namespace dict for `exec', due to non-string key: ~A."
                          d k))))))
    (check-is-namespace-dict globals)
    (unless (eq globals locals)
      (check-is-namespace-dict locals))))

(defun exec-stmt-string (code-string globals locals allowed-stmts)
  (check-type code-string string)
  (let ((ast (parse code-string)))
    (exec-stmt-check-ast code-string ast allowed-stmts)
    (exec-stmt-ast ast globals locals)))

#-ecl
;; Test case "x = 3; exec 'assert x == 3'" goes wrong:
;; HANDLE-PARSER-CONDITION called for NEXT-EOF-FAKE-AFTER-TOPLEVEL-FORM with garbled arguments.
(define-compiler-macro exec-stmt-string (&whole whole code-string globals locals allowed-stmts)
  (assert (or (eq allowed-stmts t)
              (and (listp allowed-stmts)
                   (eq (car allowed-stmts) 'quote))))
  ;; Move compilation of string to compile-time. Warn if string contains errors.
  ;; (Actually the whole string could be inlined without any runtime exec call,
  ;; maybe should even do that.)
  (labels ((warn-static-error (s error)
             (let ((nice-code (if (> (length s) 40)
                                  (concatenate 'string (subseq s 0 40) "...")
                                s)))
               (warn "The statement `exec ~S' will raise [~A]." nice-code error))))
    (when *exec-early-parse-constant-string*
      (whereas ((s (string-literal-p code-string)))
        (multiple-value-bind (ast error)
            (ignore-errors (with-compiler-generated-syntax-errors ()
                             (parse s)))
          (if error
              (warn-static-error s error)
            (multiple-value-bind (ok error)
                (if (eq allowed-stmts t)
                    (values t nil)
                  (ignore-errors (exec-stmt-check-ast code-string ast (second allowed-stmts))))
              (declare (ignore ok))
              (if error
                  (warn-static-error s error)
                (return-from exec-stmt-string
                  `(exec-stmt-ast ',ast ,globals ,locals))))))))
    whole))

(defun exec-stmt-check-ast (string ast allowed-stmts)
  (whereas ((s (ast-contains-stmt-p ast :allowed-stmts allowed-stmts)))
    (py-raise '{TypeError}
              "Statements are not allowed in this Python code string (found `~A' in \"~A\")." s string))
  (with-py-ast (form ast :into-nested-namespaces nil)
    (case (car form)
      ([return-stmt] (raise-syntax-error
                      "Statement `return' was found outside function (in `exec')."))
      (t form)))
  t)

(defun exec-stmt-ast (ast globals locals)
  (with-matching (ast ([module-stmt] ?suite))
    (multiple-value-bind (globals-ns globals-param)
        (typecase globals
          (symbol-hash-table (error "never"))
          (eq-hash-table     (values (make-hash-table-ns
                                      :dict-form '%exec-globals-ns
                                      :scope :exec-globals
                                      :parent (make-builtins-namespace))
                                     globals))
          (t                 (values (make-mapping-ns
                                      :mapping-form '%exec-globals-ns
                                      :scope :exec-globals
                                      :parent (make-builtins-namespace))
                                     globals)))
      (let ((locals-excluded-names  (multiple-value-bind (locals new-locals globals)
                                        (suite-globals-locals ?suite nil nil)
                                      (declare (ignore locals new-locals))
                                      globals)))
        (multiple-value-bind (locals-ns locals-param)
            (typecase locals
              (symbol-hash-table (error "never"))
              (eq-hash-table     (values (make-hash-table-w/excl-ns
                                          :dict-form '%exec-locals-ns
                                          :excluded-names locals-excluded-names
                                          :parent globals-ns
                                          :scope :exec-locals)
                                         locals))
              (t                 (values (make-mapping-w/excl-ns
                                          :mapping-form '%exec-locals-ns
                                          :excluded-names locals-excluded-names
                                          :parent globals-ns
                                          :scope :exec-locals)
                                         locals)))
        
          (let ((lambda-expr `(lambda (%exec-globals-ns %exec-locals-ns)
                                (with-pydecl ((:context-type-stack (:module)))
                                  (with-namespace (,globals-ns)
                                    (with-namespace (,locals-ns)
                                      ,?suite))))))
            (with-compiler-generated-syntax-errors ()
              ;; WITH-COMPILER-GENERATED-SYNTAX-ERRORS is needed for e.g. making sure
              ;; SyntaxError gets raised about the misplaced "yield" in:
              ;;    exec 'yield 1'
              ;; which might occur either during compilation (macroexpand) or runtime
              ;; (if interpreted).
              (let ((func (if *exec-stmt-compile-before-run*
                              (compile nil lambda-expr)
                            (coerce lambda-expr 'function))))
                (funcall func globals-param locals-param)))))))))

;;; `Call' expression

(defvar *special-calls* '({locals} {globals} {eval}))

(defmacro [call-expr] (&whole whole primary pos-args kwd-args *-arg **-arg)
  ;; For complete Python semantics, we should check for every call if
  ;; the function being called is one of the built-in functions EVAL,
  ;; LOCALS or GLOBALS, because they access the variable scope of the
  ;; caller.
  ;; 
  ;; As a compromise, by default we only check in case the name is
  ;; literally used, so "x = locals()" will work, while
  ;; "y = locals; y()" will not.
  ;;
  ;; But when *allow-indirect-special-call* is true, all calls
  ;; are checked regardless the primitive's name
  (flet ((do-maybe-special-call (prim which)
           (let ((there-are-args (cond ((or pos-args kwd-args) t)
                                       ((and *-arg **-arg)     `(or (py-iterate->lisp-list ,*-arg)
                                                                    (py-iterate->lisp-list ,**-arg)))
                                       (*-arg                  `(py-iterate->lisp-list ,*-arg))
                                       (**-arg                 `(py-iterate->lisp-list ,**-arg))
                                       (t                      nil)))
                 (there-are-key-args (cond (kwd-args  t)
                                           (**-arg    `(py-iterate->lisp-list ,**-arg))
                                           (t         nil)))
                 (pos-args `(nconc (list ,@pos-args) ,(when *-arg `(py-iterate->lisp-list ,*-arg))))
                 (locals-dict '(%locals))
                 (globals-dict '(%globals)))
               
             `(cond ,@(when (member '{locals} which)
                        `(((eq ,prim (function {locals}))
                           (call-expr-locals ,locals-dict ,there-are-args))))
                    ,@(when (member '{globals} which)
                        `(((eq ,prim (function {globals}))
                           (call-expr-globals ,globals-dict ,there-are-args))))
                    ,@(when (member '{eval} which)
                        `(((eq ,prim (function {eval}))
                           (call-expr-eval ,locals-dict ,globals-dict
                                           ,pos-args ,there-are-key-args))))
                    (t (call-expr-1 ,prim ,@(cddr whole)))))))
    
    (let ((specials-to-check (if *allow-indirect-special-call*
                                 *special-calls*
                               (with-perhaps-matching (primary ([identifier-expr] ?name))
                                 (intersection (list ?name) *special-calls*)))))
      (if specials-to-check
          `(let* ((.prim ,primary))
             ,(do-maybe-special-call '.prim specials-to-check))
        `(call-expr-1 ,@(cdr whole))))))

(defun call-expr-locals (locals-dict args-p)
  (when args-p (py-raise '{TypeError} "Built-in function `locals' does not take args."))
  (if (typep locals-dict 'hash-table)
      (make-symbol-hash-table locals-dict)
    locals-dict))

(defun call-expr-globals (globals-dict args-p)
  (when args-p (py-raise '{TypeError} "Built-in function `globals' does not take args."))
  (if (typep globals-dict 'hash-table)
      (make-symbol-hash-table globals-dict)
    globals-dict))

(defun call-expr-eval (locals-dict globals-dict pos-args key-args-p)
  "Handle call to `Eval' at runtime."
  ;; Uses exec-stmt, therefore below it.
  (when (or key-args-p 
	    (not pos-args)
	    (> (length pos-args) 3))
    (py-raise '{TypeError} "Built-in function `eval' takes between 1 and 3 positional args."))
  (let* ((string (pop pos-args))
	 (glob-d (or (pop pos-args) globals-dict))
	 (loc-d  (or (pop pos-args) locals-dict)))
    ;; Make it an EXEC stmt which returns the value of the expression
    ([exec-stmt] string glob-d loc-d :allowed-stmts ([module-stmt] [suite-stmt]))))

(defmacro call-expr-1 (primary pos-args kwd-args *-arg **-arg)
  (let ((kw-args (loop for ((i-e key) val) in kwd-args
		     do (assert (eq i-e '[identifier-expr]))
		     collect (intern (symbol-name key) :keyword)
		     collect val)))
    (cond
     ((or kw-args **-arg)  `(call-expr-pos+*+kw+** ,primary 
						   (list ,@pos-args) ,*-arg
						   (list ,@kw-args) ,**-arg))
     ((and pos-args *-arg) `(call-expr-pos+* ,primary (list ,@pos-args) ,*-arg))
     (*-arg                `(call-expr-* ,primary ,*-arg))
     (t                    `(py-call ,primary ,@pos-args)))))

(defun call-expr-pos+*+kw+** (prim pos-args *-arg kw-args **-arg)
  (apply #'py-call prim
	 (nconc pos-args
		(when *-arg (py-iterate->lisp-list *-arg))
		kw-args
		(when **-arg (py-**-mapping->lisp-arg-list **-arg)))))

(defun call-expr-pos+* (prim pos-args *-arg)
  (apply #'py-call
	 prim
	 (nconc pos-args (py-iterate->lisp-list *-arg))))

(defun call-expr-* (prim *-args)
  (apply #'py-call prim (py-iterate->lisp-list *-args)))

#+(or)
(define-compiler-macro [call-expr] (&whole whole primary pos-args kwd-args *-arg **-arg)
  (declare (ignore primary pos-args kwd-args *-arg **-arg))

  ;; The transformations below inline common cases, but
  ;; there are still run-time checks to verify whether
  ;; the inline case should be taken.

  ;; Optimize calls of the form OBJ.ATTR(POS-ARGS..)
  ;; where ATTR is usually a built-in method,
  ;; so "x.sort()" gets inlined call to `py-list.sort'.
  (when *inline-builtin-methods*
    (with-perhaps-matching (whole
                            ([call-expr] ([attributeref-expr] ?obj ([identifier-expr] ?attr-name))
                                         ?pos-args () nil nil))
      (when (inlineable-method-p ?attr-name (length ?pos-args))
        (return-from [call-expr]
          (inlined-method-code ?obj ?attr-name ?pos-args)))))
          
  ;; Optimize "getattr(OBJ, ATTR)(POSARGS...)", to save allocation of bound method.
  (when *inline-getattr-call*
    (with-perhaps-matching (whole ([call-expr]
                                   ([call-expr] ?id-getattr (?obj ?attr) () nil nil)
                                   ?pos-args () nil nil))
      (with-perhaps-matching (?id-getattr ([identifier-expr] {getattr}))
        (return-from [call-expr]
          `(if (eq ,?id-getattr (symbol-function '{getattr}))
               (multiple-value-bind (.a .b .c)
                   (getattr-nobind ,?obj ,?attr nil)
                 (if (eq .a :class-attr)
                     (funcall .b .c ,@?pos-args)
                   (py-call .a ,@?pos-args)))
             (py-call ,?id-getattr ,@?pos-args))))))
  
  ;; XXX todo: Optimize obj.__get__(...)
  whole)

(defmacro [classdef-stmt] (&whole whole &rest args)
  ;; Enable reuse by CPS version of macro
  (declare (ignore args))
  `(classdef-stmt-1 ,@(cdr whole)))

(defmacro classdef-stmt-1 (name inheritance suite &environment e)
  (multiple-value-bind (all-class-locals new-locals class-cumul-declared-globals)
      (suite-globals-locals suite () (get-pydecl :lexically-declared-globals e))
    (assert (equal new-locals all-class-locals))
    (let* ((cname (with-matching (name ([identifier-expr] ?name))
                    ?name))
	   (new-context-name-stack (cons cname (get-pydecl :context-name-stack e)))
	   (context-cname (ensure-user-symbol 
                           (format nil "~{~A~^.~}" (reverse new-context-name-stack)))))
      `(multiple-value-bind (namespace-ht cls-dict.__metaclass__)
           ;; Need a nested LET, as +cls-namespace+ may not be set when the ASSIGN-STMT
           ;; below is executed, as otherwise nested classes don't work.
           (let ((%class-namespace (make-eq-hash-table)))
             (with-namespace (,(make-hash-table-w/excl-ns
                                 :dict-form '%class-namespace
                                 :excluded-names class-cumul-declared-globals
                                 :parent (get-pydecl :namespace e)
                                 :scope :class))
               ;; XXX enforce string-only keys
               ;; First, run the statements in the body of the class
               ;; definition. This will fill +cls-namespace+ with the
               ;; class attributes and methods.
               
               ;; Local class variables are not locally visible (they don't extend ":lexically-visible-vars")
               ;; Variables declared `global' in a class scope are not global in sub-scopes.
               (with-pydecl ((:context-type-stack ,(cons :class (get-pydecl :context-type-stack e)))
                             (:context-name-stack ,new-context-name-stack)
                             (:declared-globals-current-scope ,class-cumul-declared-globals)
                             ;; :lexically-declared-globals is unchanged
                             )
                 ,(if *mangle-private-variables-in-class*
                      (mangle-suite-private-variables cname suite)
                    suite))
               
               (values %class-namespace (namespace-get {__metaclass__}))))
         
         ;; Second, now that +cls-namespace+ is filled, make the
         ;; class with that as namespace.
         (let ((cls (make-py-class :name ',cname
                                   :context-name ',context-cname
                                   :namespace namespace-ht
                                   :supers ,(with-matching (inheritance ([tuple-expr] ?supers))
                                              `(list ,@?supers))
                                   :cls-metaclass cls-dict.__metaclass__
                                   :mod-metaclass (module-namepace-get {__metaclass__}))))
           (record-source-file-loc ',context-cname :type)
           ([assign-stmt] cls (,name)))))))

(defun mangle-suite-private-variables (cname suite)
  "Rename all attributes `__foo' to `_CNAME__foo'. Attributes names can be __foo, __foo_, but not __foo__ (2 suffix _)"
  (check-type cname symbol)
  (check-type suite list)
  (flet ((mangle-candidate-p (identifier-name)
           (check-type identifier-name symbol)
           (let ((name (symbol-name identifier-name)))
             (and (>= (length name) 3)
                  (char= (aref name 0) #\_)
                  (char= (aref name 1) #\_)
                  (or (char/= (aref name (- (length name) 1)) #\_)
                      (char/= (aref name (- (length name) 2)) #\_)))))
         (replacement-symbol (identifier-name)
           (check-type identifier-name symbol)
           (intern (format nil "_~A~A" cname identifier-name) (symbol-package identifier-name))))
    (let (candidates) ;; (old . new) symbols
      (with-py-ast ((form &key value target) suite :into-nested-namespaces t)
        (declare (ignore value target))
        (case (car form)
          ([identifier-expr] (let ((name-sym (second form)))
                               (when (mangle-candidate-p name-sym)
                                 (pushnew name-sym candidates)))
                             (values nil t))
          ([attributeref-expr] (destructuring-bind (item attr) (cdr form)
                                 (declare (ignore item))
                                 (assert (and (listp attr)
                                              (eq (first attr) '[identifier-expr])
                                              (symbolp (second attr))))
                                 (when (mangle-candidate-p (second attr))
                                   (pushnew (second attr) candidates)))
                               form) ;; recurse into ITEM
          (t form)))
      (setf suite (copy-tree suite)) ;; gonna modify it - probably screws up source recording
      (dolist (c candidates suite)
        (setf suite (nsubst (replacement-symbol c) c suite))))))

(defun apply-comparison-brackets (whole)
  (let (args cmps)
    (labels ((apply-brackets (form)
			     (if (not (ast-p form '[comparison-expr]))
				 (push form args)
			       (with-matching (form ([comparison-expr] ?cmp ?left ?right))
                                 (progn (apply-brackets ?left)
                                        (push ?cmp cmps)
                                        (apply-brackets ?right))))))
      (apply-brackets whole)
      (values (nreverse args) (mapcar #'get-binary-comparison-func-name (nreverse cmps))))))

(defmacro [comparison-expr] (&whole whole cmp left right)
  ;; "Formally, if a, b, c, ..., y, z are expressions and
  ;; op1, op2, ..., opN are comparison operators, then
  ;; a op1 b op2 c ... y opN z is equivalent to
  ;; a op1 b and b op2 c and ... y opN z,
  ;; except that each expression is evaluated at most once."
  ;; http://python.org/doc/current/reference/expressions.html
  (declare (ignore cmp left right))
  (multiple-value-bind (args cmp-func-names)
      (apply-comparison-brackets whole)
    (with-gensyms (x y cmp-res comparison-expr)
      `(block ,comparison-expr
         (let* ((,x ,(pop args))
                (,y ,(pop args))
                (,cmp-res (,(pop cmp-func-names) ,x ,y)))
           ,#1=(let ((exit-condition (if (null cmp-func-names) t `(not (py-val->lisp-bool ,cmp-res)))))
                 `(when ,exit-condition
                    (return-from ,comparison-expr ,cmp-res)))
           ,@(loop for cmp-func-name = (pop cmp-func-names)
                 while cmp-func-name
                 collect `(progn (shiftf ,x ,y ,(pop args))
                                 (setf ,cmp-res (,cmp-func-name ,x ,y))
                                 ,#1#)))))))

(defmacro [continue-stmt] (&environment e)
  (if (get-pydecl :inside-loop-p e)
      `(go .continue)
    (raise-syntax-error "Statement `continue' was found outside loop.")))

(defmacro [del-stmt] (item &environment e)
  (multiple-value-bind (temps values stores del-form read-form)
      (let ((*want-DEL-setf-expansion* t))
        (get-setf-expansion item e))
    (declare (ignore stores read-form))
    (assert del-form () "No DEL form for: ~A" item)
    `(let ,(mapcar #'list temps values)
       ,del-form)))

(defun init-dict (vk-list)
  (with-py-dict 
      (let ((dict (make-py-hash-table)))
        (loop for (v k) on vk-list by #'cddr
            do (setf (gethash k dict) v))
        dict)))

(defmacro [dict-expr] (vk-list)
  (with-gensyms (list)
    `(with-stack-list (,list ,@vk-list)
       (init-dict ,list))))

(defmacro with-iterator ((target source) &body body)
  ;; (with-iterator (var object) (...var...))
  ;; VAR is NIL if object iteration exhausted; or non-NIL if broken out of.
  (assert (symbolp target))
  (with-gensyms (it-fun)
    `(locally (declare #.+optimize-fastest+)
       (let ((,it-fun (get-py-iterate-fun ,source)))
         (loop for ,target = (funcall (the function ,it-fun))
             while ,target
             do (locally (declare #.+optimize-std+)
                  ,@body))))))

(define-compiler-macro with-iterator (&whole whole (target source) &body body &environment e)
  "Optimize some common iteration patterns."
  (unless (get-pydecl :may-inline-iteration e) ;; Can't declare a macro as (not)inline
    (return-from with-iterator whole))
  (with-perhaps-matching (source ([call-expr] ([identifier-expr] {range}) ?pos-args () () ()))
    (let ((n-args (length ?pos-args)))
      (when (<= 1 n-args 2)
        (return-from with-iterator
          `(flet ((run-it (val)
                    (with-pydecl ((:may-inline-iteration t))
                      (let ((,target val))
                        ,@body))))
             (declare (dynamic-extent #'run-it))
             (let ((.range ([identifier-expr] {range}))
                   (.p1 ,(first ?pos-args))
                   ,@(when (= n-args 2)
                       `((.p2 ,(second ?pos-args)))))
               (if (and (eq .range (load-time-value (builtin-value '{range})))
                        (fixnump .p1)
                        ,@(when (= n-args 2)
                            `((fixnump .p2))))
                   (loop for .i fixnum from ,(if (= n-args 2) `.p1 0) below ,(if (= n-args 2) `.p2 `.p1)
                       do (run-it .i))
                 (with-pydecl ((:may-inline-iteration nil))
                   (with-iterator (,target (py-call .range .p1 ,@(when (= n-args 2) `(.p2))))
                     (run-it ,target))))))))))
  whole)

(defmacro [for-in-stmt] (target source suite else-suite &environment e)
  (with-gensyms (x)
   `(tagbody (with-pydecl ((:may-inline-iteration t))
               (with-iterator (,x ,source)
                 ([assign-stmt] ,x (,target))
                 (tagbody
                   (with-pydecl ((:inside-loop-p t)
                                 (:safe-lex-visible-vars
                                  ,(union (set-difference
                                           (target-get-bound-vars target)
                                           (nconc (ast-deleted-variables suite)
                                                  (get-pydecl :declared-globals-current-scope e)))
                                          (get-pydecl :safe-lex-visible-vars e))))
                     ,suite)
                   (go .continue) ;; prevent warning about unused tag
                  .continue)))
       ,@(when else-suite `(,else-suite))
       (go .break) ;; prevent warning about unused tag
      .break)))

(defun lambda-args-and-destruct-form (f-pos-args f-key-args)
  ;; Replace "def f( (x,y), z):  .." 
  ;; by "def f( |(x,y)|, z):  x, y = |(x,y)|; ..".
  (let (nested-vars)
    (labels ((find-nested-vars (tup)
               ;; Convert tuple with identifiers to symbol:  (a,(b,c)) -> |(a,(b,c))|
               ;; Returns the symbol and a list with the "included" symbols (here: a, b and c)
               (assert (match-p tup '([tuple-expr] ?items)))
               (labels ((rec (x)
                          (ecase (car x)
                            ([tuple-expr] (loop for v in (second x) do (rec v)))
                            ([identifier-expr] (push (second x) nested-vars)))))
                 (rec tup)))
             
             (analyze-args (args start-ix)
               (let (new-arglist normal-args destructs)
                 (loop
                     for arg in args
                     for ix from start-ix
                     do (ecase (car arg)
                          ([identifier-expr] (let ((name (second arg)))
                                               (push name new-arglist)
                                               (push name normal-args)))
                          ([tuple-expr] (find-nested-vars arg)
                                        (let ((tuple-var (make-symbol (format nil ".~D" ix))))
                                          (push tuple-var new-arglist)
                                          (push `([assign-stmt] ,tuple-var (,arg)) destructs)))))
                 (values (nreverse new-arglist)
                         (nreverse normal-args)
                         (nreverse destructs)))))
      
      (multiple-value-bind (lambda-pos-args normal-pos-args pos-destructs)
          (analyze-args f-pos-args 0)
        (multiple-value-bind (lambda-key-args normal-key-args key-destructs)
            (analyze-args (mapcar #'car f-key-args) (length f-pos-args))
          (values lambda-pos-args ;; LAMBDA args
                  lambda-key-args ;;  are in the same order as original lists
                  (when (or pos-destructs key-destructs)
                    `(progn ,@(nconc pos-destructs key-destructs)))
                  (nconc normal-pos-args normal-key-args)
                  (nreverse nested-vars)))))))

(defun suite-globals-locals (suite sure-locals sure-globals)
  "Returns three lists: LOCALS, NEW-LOCALS, GLOBALS.
LOCALS are the variables assigned to within the function body.
LOCALS shares share tail structure with input arg locals."
  (declare (optimize (debug 3)))
  (assert (match-p suite '([suite-stmt] ?items)))
  (let (new-locals)
    (with-py-ast ((form &key value target) suite :value t)
      ;; Use :VALUE T, so the one expression for lambda suites is handled correctly.
      (declare (ignore value))
      (case (car form)

	(([classdef-stmt] [funcdef-stmt])
	 (multiple-value-bind (name kind)
	     (ecase (pop form)
	       ([classdef-stmt]
                (with-matching (form (([identifier-expr] ?cname) ?inheritance ?csuite))
                  (values ?cname "class")))
	       ([funcdef-stmt]
                (with-matching (form (?decorators ([identifier-expr] ?fname) ?fargs ?fsuite))
                  (values ?fname "function"))))
	   (when (member name sure-globals)
             (raise-syntax-error
              "The ~A name `~A' may not be declared `global'." kind name))
           (unless (member* name sure-locals new-locals)
             (push name sure-locals)
             (push name new-locals)))
	 (values nil t))
	
	([identifier-expr]
	 (let ((name (second form)))
	   (when (and target
                      (not (member* name sure-locals new-locals sure-globals)))
	     (push name sure-locals)
	     (push name new-locals)))
	 (values nil t))
	
	([global-stmt]
         (with-matching ((second form) ([tuple-expr] ?identifiers))
           (dolist (x ?identifiers)
             (assert (match-p x '([identifier-expr] ?_))))
           (let* ((sym-list (mapcar #'second ?identifiers))
                  (erroneous (intersection sym-list sure-locals :test 'eq)))
             (when erroneous
               ;; CPython gives SyntaxWarning, and seems to internally move the `global'
               ;; declaration before the first use. Let us signal an error; it's easy
               ;; for the user to fix this.
               (raise-syntax-error
                "This `global' declaration for variable `~A' is not allowed: ~
                 declaration must be given before first use in function body."
                (car erroneous)))
             (setf sure-globals (nconc sym-list sure-globals)))
           (values nil t)))

        ([del-stmt]
         ;; Local variables are determined by looking at assignments.
         ;; Deletions play no role, so don't walk into them.
         (values nil t))
              
	(t form)))
  
    (values sure-locals new-locals sure-globals)))

(defmacro [funcdef-stmt] (&whole whole &rest args)
  ;; Enable reuse by CPS version of macro
  (declare (ignore args))
  `(funcdef-stmt-1 ,@(cdr whole)))

(defmacro funcdef-stmt-1 (decorators
                          fname (pos-args key-args *-arg **-arg)
                          suite
                          &key (return-default-none t)
                          &environment e)
  ;; The resulting function is returned.
  ;; 
  ;; If FNAME is a keyword symbol (like :lambda), then an anonymous
  ;; function (like from LAMBDA-EXPR) is created. The function is thus
  ;; not bound to a name. Decorators are not allowed then.
  ;;
  ;; You can rely on the whole function body being included in
  ;;  (block function-body ...).
  (if (keywordp fname)
      (assert (null decorators))
    (with-matching (fname ([identifier-expr] ?name))
      (setf fname ?name)))
  
  (multiple-value-bind (lambda-pos-args lambda-key-args tuples-destruct-form
                        normal-pos-key-args destructed-pos-key-args)
      (lambda-args-and-destruct-form pos-args key-args)
    ;; Example:  def f((x,y), z, (m,n)=4): ...
    ;;  => lambda-pos-args: |(x,y)|, z
    ;;     lambda-key-args: |(m,n)|, p
    ;;     tuples-destruct-form: (progn ([assign-stmt] |(x,y)| (([tuple-expr] .. x .. y)))
    ;;                                  ([assign-stmt] |(m,n)| (([tuple-expr] .. m .. n))))
    ;;     normal-pos-key-args: z, p
    ;;     destructed-pos-key-args: x, y, m, n
    (let ((nontuple-arg-names (nconc normal-pos-key-args destructed-pos-key-args)))
      (when *-arg (push (second *-arg) nontuple-arg-names))
      (when **-arg (push (second **-arg) nontuple-arg-names))

      (multiple-value-bind (all-nontuple-func-locals new-locals func-cumul-declared-globals)
	  (suite-globals-locals suite nontuple-arg-names (get-pydecl :lexically-declared-globals e))
	(assert (null (intersection destructed-pos-key-args new-locals)))
	(let* ((new-context-name-stack (cons fname (get-pydecl :context-name-stack e)))
	       (context-fname (ensure-user-symbol
                               (format nil "~A/~{~A~^.~}" *current-module-name*
                                       (reverse new-context-name-stack))))
               (generator-p (generator-ast-p suite))
	       (body-decls `((:lexically-declared-globals ,func-cumul-declared-globals)
                             (:declared-globals-current-scope ,func-cumul-declared-globals)
                             (:context-type-stack ,(cons :function (get-pydecl :context-type-stack e)))
                             (:context-name-stack ,new-context-name-stack)
                             (:lexically-visible-vars
                              ,(let ((sum (remove-duplicates
                                           (append all-nontuple-func-locals
                                                   (get-pydecl :lexically-visible-vars e)))))
                                 ;; def f(x):
                                 ;;   def g(y):
                                 ;;     <Here G is locally visibe because it is a /local variable/
                                 ;;      in F. In general the name of a function is not visible
                                 ;;      in its body.>
                                 ;; 
                                 ;; See also the testcases for the :LEXICALLY-VISIBLE-VARS declaration.
                                 #+(or);; think superfluous
                                 (when (eq (car (get-pydecl :context-type-stack e)) :function)
                                   (assert (member :function (get-pydecl :context-type-stack e)))
                                   (pushnew fname sum))
                                 sum))
                             (:safe-lex-visible-vars
                              ,(nset-difference
                                (append nontuple-arg-names
                                        (get-pydecl :safe-lex-visible-vars e))
                                (ast-deleted-variables suite)))))
               (func-lambda
		`(py-arg-function
                  ,context-fname
		  (,lambda-pos-args
		   ,(loop for lambda-key-arg in lambda-key-args
                        for ((nil nil) key-default-arg) in key-args
                        collect `(,lambda-key-arg ,key-default-arg))
		   ,(when *-arg  (second *-arg))
		   ,(when **-arg (second **-arg)))
                  (with-namespace (,(make-let-w/locals-ns
                                     :parent (get-pydecl :namespace e)
                                     :names all-nontuple-func-locals
                                     :let-names (append destructed-pos-key-args new-locals)
                                     :scope :function)
                                   :define-%locals ,(funcdef-should-save-locals-p suite))
                    (block function-body
		      (with-pydecl ,body-decls
                        ,tuples-destruct-form
                        ,(if generator-p
                             `([return-stmt] ,(rewrite-generator-funcdef-suite
                                               context-fname suite))
                           `(progn ,suite
                                   ,@(when return-default-none
                                       `((load-time-value *the-none*))))))))))
               (func-code `(make-instance 'func-code
                             :name ,(symbol-name fname)
                             :arg-count ,(+ (length lambda-pos-args) (length lambda-key-args))
                             :nlocals ,(length (append destructed-pos-key-args new-locals)) ;; TODO: check this
                             :varnames (make-tuple-from-list ',(mapcar #'symbol-name
                                                                       (append lambda-pos-args
                                                                               lambda-key-args
                                                                               (when *-arg (list (second *-arg)))
                                                                               (when **-arg (list (second **-arg)))
                                                                               destructed-pos-key-args
                                                                               new-locals)))
                             :cellvars *the-empty-tuple* ;; FIXME
                             :freevars *the-empty-tuple* ;; FIXME
                             :code ""
                             :consts *the-empty-tuple*
                             :names *the-empty-tuple*
                             :filename "FIXME"
                             :lnotab ""
                             :stacksize 1
                             :flags ,(+ (if *-arg  #x04 0)
                                        (if **-arg #x08 0)
                                        (if generator-p #x20 0)))))
          (when (keywordp fname)
	    (return-from funcdef-stmt-1 `(make-py-function :name ',fname
                                                           :context-name ',context-fname
                                                           :lambda ,func-lambda
                                                           :func-globals ,(get-module-namespace e)
                                                           :func-code ,func-code
                                                           :module-name ,*current-module-name*)))
          (let ((art-deco '.undecorated-func))
            (dolist (x (reverse decorators))
              (setf art-deco `(py-call ,x ,art-deco)))
            `(let* ((.undecorated-func (make-py-function :name ',fname
                                                         :context-name ',context-fname
                                                         :lambda ,func-lambda
                                                         :func-globals ,(get-module-namespace e)
                                                         :func-code ,func-code
                                                         :module-name ,*current-module-name*))
                    (.decorated-func ,art-deco))
               ;; Ugly special case:
               ;;  class C:
               ;;   def __new__(..):    <-- the __new__ method inside a class
               ;;      ...                  automatically becomes a 'static-method'
               ;; XXX check whether this works correctly when user does same explicitly
               ,@(when (and (eq (car (get-pydecl :context-type-stack e)) :class)
                            (eq fname '{__new__}))
                   `((setf .decorated-func (py-call (find-class 'py-static-method) .decorated-func))))
               
               (setf ([identifier-expr] ,fname) .decorated-func)
               
               (record-source-file-loc ',context-fname :operator)
               .decorated-func))))))) ;; return the function


(defmacro [generator-expr] (&whole whole item for-in/if-clauses)
  (declare (ignore item for-in/if-clauses))
  (rewrite-generator-expr-ast whole))
       
(defmacro [global-stmt] (names &environment e)
  ;; GLOBAL statements are already determined and used at the moment a
  ;; FUNCDEF-STMT/CLASSDEF-STMT/EXEC-STMT is handled.
  (declare (ignore names))
  (unless (intersection '(:function :class) (get-pydecl :context-type-stack e))
    (warn "Bogus `global' statement found at top-level.")))

(defparameter *debug-assume-variables-bound* nil
  "A hack to analyze compiler output; don't use.")

(define-setf-expander [identifier-expr] (name &environment e)
  ;; XXX safe-lexical-vars
  ;; XXX built-in non-shadowable values like True, False, None, Ellipsis, NotImplemented
  (let* ((ns (get-pydecl :namespace e))
         (global-p (member name (get-pydecl :declared-globals-current-scope e)))
         (module-ns (get-module-namespace e)))
    (with-gensyms (store)
      (let ((store-form (if global-p
                            (ns.write-form module-ns name store)
                          (ns.find-form #'ns.write-form ns name store)))
            (del-form `(if ,(if global-p
                                (ns.del-form module-ns name)
                              (ns.find-form #'ns.del-form ns name))
                           nil
                         ,(unless *debug-assume-variables-bound*
                            `(unbound-variable-error ',name :expect-value nil)))))
        (values () ;; temps
                () ;; values
                `(,store) ;; stores
                (if *want-DEL-setf-expansion* del-form store-form) ;; write/delete form

                ;; Can have that in deepest namespace it's global,
                ;; but a higher namespace has local with that name.
                `(or ,(if global-p
                          (ns.read-form module-ns name)
                        (ns.find-form #'ns.read-form ns name))
                     ,(unless *debug-assume-variables-bound*
                        `(unbound-variable-error ',name :expect-value t)))))))) ;; read form

(defmacro [identifier-expr] (&whole whole name &environment e)
  ;; The identifier is used for its value.
  ;; (Assignent targets are handled by the setf expander.)
  (check-type name symbol)
  (multiple-value-bind (temps values stores store-form read-form)
      (get-setf-expansion whole e)
    (declare (ignore temps values stores store-form))
    read-form))

(defmacro [if-expr] (condition then else)
  `(if (py-val->lisp-bool ,condition) ,then ,else))
       
(defmacro [if-stmt] (if-clauses else-clause)
  `(cond ,@(loop for (cond body) in if-clauses
	       collect `((py-val->lisp-bool ,cond) ,body))
	 ,@(when else-clause
	     `((t ,else-clause)))))

(defmacro [import-stmt] (items)
  ;; The effects of "import x.y.z" are:
  ;;   1. modules "x", "x.y" and "x.y.z" are loaded (if they were not already)
  ;;   2. name "x" is bound to the first module object, <module x>
  ;; But, if a bindname is given, the effect of "import x.y.z as foo":
  ;;   1. imports modules, x, x.y, x.y.z
  ;;   2. binds x.y.z to "foo"; does not bind "x"
  ;;  
  ;; One import statement can contain multiple submodules to import (the items).
  ;; Returns the imported (sub)modules as multiple values: <module x.y.z>, <module a.b>.
  `(let ((*module-namespace* nil)) ;; hack
     (values ,@(loop for (mod-name-as-list bind-name) in items
                   for top-name = (car mod-name-as-list)
                   collect `(let* ((args (list :within-mod-path ,*compile-file-truename*
                                               :within-mod-name ',*current-module-name*))
                                   (top-module (apply #'py-import '(,top-name) args))
                                   (deep-module ,(if (cdr mod-name-as-list)
                                                     `(apply #'py-import ',mod-name-as-list args)
                                                   `top-module)))
                              ,(if bind-name
                                   `([assign-stmt] deep-module (([identifier-expr] ,bind-name)))
                                 `([assign-stmt] top-module (([identifier-expr] ,top-name))))
                              deep-module)))))

(defvar *inside-import-from-stmt* nil) ;; hack

(defmacro [import-from-stmt] (mod-name-as-list items)
  (when (eq (car mod-name-as-list) '[.])
    (error "Relative imports are not yet supported by the compiler: ~A" 
           (with-output-to-string (s)
             (py-pprint `([import-from-stmt] ,mod-name-as-list ,items) s))))
  `(let* ((*module-namespace* nil) ;; hack
          (args (list :within-mod-path ,*compile-file-truename*
                      :within-mod-name ',*current-module-name*))
          (m (apply #'py-import '(,(car mod-name-as-list)) args)))
     (declare (ignorable m)) ;; Ensure topleve module is imported relative to current mod
     (whereas ((mod-obj ,(if (= (length mod-name-as-list) 1)
                             'm
                           `(apply #'py-import ',mod-name-as-list args))))
       ,(case items
          ([*] `(loop for (k . v) in (dir-items mod-obj)
                    do (namespace-set-runtime (ensure-user-symbol k) v)))
          (t `(progn
                ,@(loop for (item bind-name) in items
                      collect `([assign-stmt] (let ((*inside-import-from-stmt* t))
                                                ([attributeref-expr] mod-obj ([identifier-expr] ,item)))
                                              (([identifier-expr] ,(or bind-name item)))))))))))
       
(defmacro [lambda-expr] (args expr)
  ;; XXX Treating lambda as a funcdef-stmt results in way more
  ;; code than necessary for the just one expression it contains.
  `([funcdef-stmt] nil :lambda ,args ([suite-stmt] (,expr))
                   :return-default-none nil))
  
(defmacro [listcompr-expr] (item for-in/if-clauses)
  (with-gensyms (list)
    `(let ((,list ()))
       ,(loop
	    with res = `(push ,item ,list)
	    for clause in (reverse for-in/if-clauses)
	    do (setf res (ecase (car clause)
			   ([for-in-clause] `([for-in-stmt] ,(second clause) ,(third clause) ,res nil))
			   ([if-clause]     `([if-stmt] ((,(second clause) ,res)) nil))))
	    finally (return res))
       (make-py-list-from-list (nreverse ,list)))))

(defmacro [list-expr] (items)
  `(make-py-list-unevaled-list ,items))

(define-setf-expander [list-expr] (items &environment e)
  (get-setf-expansion `(list/tuple-expr ,items) e))

(defmacro [literal-expr] (kind value)
  (ecase kind
    (:string (check-type value string)
             value)
    (:bytes  (check-type value string)
             ;; In Python 2.x this is treated the same as strings. It's intended as mental preparation for 3.0.
             value)
    (:number (check-type value number)
             value)
    (:lisp   value)))

(defvar *module-preload-hook*)

(defvar *signal-unbound-variable-errors* nil
  "Whether to signal UNBOUND-VARIABLE when a Python variable is unbound, before a NameError is raised.")

(defun unbound-variable-error (name &key (expect-value t))
  (declare (special *py-signal-conditions*))
  (if expect-value
      (restart-case
          (progn (when *signal-unbound-variable-errors*
                   (signal 'unbound-variable :name name))
                 (py-raise '{NameError} "Variable `~A' is unbound." name))
        (cl:use-value (val)
            :report (lambda (stream)
                      (format stream "Enter a Lisp value to use for `~A'." name))
            :interactive (lambda () 
                           (format t "Enter new Lisp value for `~A': " name)
                           (multiple-value-list (eval (read))))
          (return-from unbound-variable-error val))
        (import (val)
            :report (lambda (stream) (format stream "Use the built-in module named `~A'." name))
            :test (lambda (c)
                    (declare (ignore c))
                    (not (null (lisp-package-as-py-module name))))
          :interactive (lambda () (list (lisp-package-as-py-module name)))
          (return-from unbound-variable-error val)))
    (with-simple-restart (continue "Continue as if `~A' is currently bound." name)
      (py-raise '{NameError} "Variable `~A' is unbound." name))))

(defparameter *module-function-hook* nil)

(defparameter *module-source-hook* nil
  "Called when module is imported, with source/binary path names and module source code as args.")

(defvar *module-namespace* nil)

(defparameter *muffle-sbcl-compiler-notes* nil)

(defvar *compile-time-module-name* nil)

(defvar %compile-time-module% nil
  "For compilation use")


(define-condition module-import-pre ()
  ((compiler-id :initarg :compiler-id :accessor mip.compiler-id)
   (module :initarg :module :accessor mip.module)
   (module-new-p :initarg :module-new-p :accessor mip.module-new-p)
   (source :initarg :source :accessor mip.source)
   (source-func :initarg :source-func :accessor mip.source-func)
   (init-func :initarg :init-func :accessor mip.init-func)
   (run-tlv-func :initarg :run-tlv-func :accessor mip.run-tlv-func)
   (muffled :initform nil :accessor mip.muffled)
   (is-compiled :initarg :is-compiled :reader mip.is-compiled)))

(defmethod print-object ((x module-import-pre) stream)
  (print-unreadable-object (x stream :type t)
    (format stream ":module ~A" (mip.module x))))

(defun describe-py-fasl (fasl)
  (check-type fasl pathname)
  (handler-case (progn (load fasl) (values))
    (module-import-pre (mip)
      (format t "~&Python fasl file~% ~A~%" fasl)
      (let ((m (mip.module mip)))
        (format t "  Python module: `~A'~%" (module-name m))
        (format t "  original source: ~A~%" (module-src-pathname m))
        (format t "  original fasl: ~A~%" (module-src-pathname m))
        (format t "  CLPython compiler version id: ~A~@[~A~]~%"
                (mip.compiler-id mip)
                (unless (fasl-matches-compiler-p (mip.compiler-id mip))
                  " (incompatible with current CLPython version)"))))
    (:no-error ()
      (error "~A is not a Python fasl file." fasl))))

(defvar *load-file->module* (make-hash-table :test 'equal)
  "Mapping from loading file name to Python module")

#||
(defmacro in-module (&rest args)
  `(eval-when (:load-toplevel :execute)
     (in-module-1 ,@args)))

(defun in-module-1 (&rest args)
  "Ensures the module exists"
  (unless *load-truename*
    (error "*LOAD-TRUENAME* is not set?!"))
  (setf (gethash (derive-pathname *load-truename*) *load-file->module*)
    (apply #'ensure-module args))
  (unless *habitat*
    (warn "IN-MODULE-1: Creating *habitat*")
    (setf *habitat* (make-habitat))))
||#

(defun careful-derive-pathname (pathname default &rest options)
  (if pathname
      (apply #'derive-pathname pathname options)
    default))
  
(defmacro with-module-toplevel-context (() &body body)
  ;; Consider *module-namespace* ?
  `(with-pydecl ((:context-type-stack (:module)))
     (with-namespace (,(or *module-namespace*
                           (make-hash-table-ns
                            :dict-form '%module-globals
                            :scope :module
                            :parent (make-builtins-namespace)))
                      :define-%globals t)
       ,@body)))

;;; TODO: parse __future__ imports

(defmacro with-stmt-decl (() &body body)
  `(locally (declare #+sbcl ,@(when *muffle-sbcl-compiler-notes*
                                `((sb-ext:muffle-conditions sb-ext:compiler-note))))
     ,@body))

(defun module-init (&key src-pathname bin-pathname current-module-name
                         defun-wrappers source source-func
                         (compiler-id (error "compiler-id is required"))
                         is-compiled)
  (multiple-value-bind (module module-new-p)
      (ensure-module :src-pathname src-pathname :bin-pathname bin-pathname :name current-module-name)
    (let ((%module-globals (module-ht module)))
      (assert (with-py-dict (hash-table-p %module-globals)))
      (flet ((init-module (&optional (module-globals %module-globals))
               (init-module-namespace module-globals current-module-name))
             (run-top-level-forms (&optional (module-globals %module-globals))
               (let (result)
                 (dolist (f defun-wrappers result)
                   (loop named tlf
                       do #+(or)(format t "Evaluating top-level form ~A in ~A~%" (incf i) current-module-name)
                          (restart-bind ((error (lambda () (return-from tlf *the-none*))
                                             :report-function
                                               (lambda (stream)
                                                 (format stream "Skip this top-level form in module `~A'"
                                                         current-module-name))))
                            (with-simple-restart (retry "Retry evaluating this top-level form in module `~A'"
                                                        current-module-name)
                              (setf result (funcall f module-globals))
                              (return-from tlf))))))))
        ;; Give outer function a chance to influence module loading actions:
        (restart-case
            (progn (signal 'module-import-pre
                           :compiler-id compiler-id
                           :module module
                           :module-new-p module-new-p
                           :source source
                           :source-func source-func
                           :init-func #'init-module
                           :run-tlv-func #'run-top-level-forms
                           :is-compiled is-compiled)
                   ;; If not overruled, take the normal loading steps:
                   (unless (fasl-matches-compiler-p compiler-id)
                     (fasl-mismatch-cerror bin-pathname))
                   (invoke-restart 'continue-loading
                                   :init module-new-p
                                   :run-tlv t
                                   :globals %module-globals))
          (continue-loading (&key (init t) (run-tlv t) (globals %module-globals))
            (assert (with-py-dict (hash-table-p globals)))
            (setf %module-globals globals)
            (when init
              (init-module))
            (when run-tlv
              (run-top-level-forms))
            t)
          (abort-loading ()
            nil))))))
    
(defmacro [module-stmt] (suite &environment e)
  "If *MODULE-NAMESPACE* is bound, it is used."
  (declare (ignorable e))
  (assert (ast-p suite '[suite-stmt]))
  (labels ((stmt-func-name (i stmt suite-hash)
             (ensure-user-symbol
              (format nil "{~A[~A] \"~A\" #~A}"
                      *current-module-name* i
                      (abbreviate-to-one-line (py-pprint stmt))
                      suite-hash)))
           (wrap-in-funcs (suite-hash)
             "Reason for wrapping top-level forms in functions, is that Allegro's upcoming
            source-level debugging probably only works for code inside defuns."
             (loop for stmt in (with-matching (suite ([suite-stmt] ?stmts)) ?stmts)
                 for i from 0
                 for func-name = (stmt-func-name i stmt suite-hash)
                 collect (list `(defun ,func-name (%module-globals)
                                  (declare (optimize debug))
                                  (declare (ignorable %module-globals)) ;; when using a package-ns for module globals
                                  (with-module-toplevel-context ()
                                    (with-py-errors (:name ,(intern (format nil "python-module ~A" *current-module-name*) #.*package*))
                                      (with-stmt-decl ()
                                        ,stmt))))
                               func-name))))
    ;; Because using uninterned symbols gives some problems with Allegro's source form recording, give 
    ;; the functions a practically unique name based on the suite.
    (let* ((suite-hash (sxhash (format nil "~A" suite)))
           (module-function-name #+(or)(make-symbol (format nil "~A.__module_init__" *current-module-name*))
                                (intern (format nil "~A.__module_init__.~A" *current-module-name* suite-hash)
                                        #.*package*))
          (defun-wrappers (wrap-in-funcs suite-hash)))
      `(progn
         ,@(mapcar #'first defun-wrappers) ;; One function for each top-level form
         
         (defun ,module-function-name ()
           (declare (optimize debug))
           
           #+clpython-source-level-debugging
           ,(create-python-source-location-table-pydecl suite)
           
           (module-init :src-pathname ,*compile-file-truename*
                        :bin-pathname (load-time-value (careful-derive-pathname *load-truename* #P"__main__" :case #+sbcl :local #-sbcl :common))
                        :current-module-name ,*current-module-name*
                        :defun-wrappers ',(mapcar #'second defun-wrappers)
                        :source ,(when *compile-file-truename*
                                       (slurp-file *compile-file-truename*))
                        :source-func ',module-function-name
                        :compiler-id ,*clpython-compiler-version-id*
                        :is-compiled ,(not (null *compile-file-truename*))))
         
         ;; suppress spurious warning about undefined function, by going via symbol-function
         (funcall (symbol-function ',module-function-name))))))

(defmacro [pass-stmt] ()
  nil)

(defmacro [print-stmt] (dest items comma?)
  ;; XXX todo: use methods `write' of `dest' etc
  `(py-print ,dest (list ,@items) ,(not (null comma?))))

(defmacro [return-stmt] (val &environment e)
  (if (eq :function (car (get-pydecl :context-type-stack e)))
      `(return-from function-body ,(or val `(load-time-value *the-none*)))
    (raise-syntax-error "Statement `return' was found outside function.")))

(defmacro [slice-expr] (start stop step)
  `(make-slice ,start ,stop ,step))

(defmacro [subscription-expr] (item subs)
  `(py-subs ,item ,subs))

(define-setf-expander [subscription-expr] (item subs &environment e)
  (declare (ignore e))
  (with-gensyms (it su store)
    (let ((store-form `(setf (py-subs ,it ,su) ,store))
          (del-form `(setf (py-subs ,it ,su) nil)))
      (values `(,it ,su) ;; temps
              `(,item ,subs) ;; values
              `(,store) ;; stores
              (if *want-DEL-setf-expansion* del-form store-form) ;; delete/store-form
              `(py-subs ,it ,su))))) ;; read-form

(defmacro [suite-stmt] (stmts)
  `(progn ,@stmts))

#+(or)
(define-compiler-macro [suite-stmt] (&whole whole stmts &environment e)
  ;; Skip checks for bound-ness, when a lexical variable is certainly bound.
  (unless (eq (car (get-pydecl :context-type-stack e)) :function)
    (return-from [suite-stmt] whole))
  
  (let* ((deleted-vars (ast-deleted-variables whole))
         (deleted-safe (intersection deleted-vars (get-pydecl :safe-lex-visible-vars e)))
         (global-safe (intersection (get-pydecl :declared-globals-current-scope e) 
                                    (get-pydecl :safe-lex-visible-vars e))))
    ;; This is a nice place for some sanity checks
    (assert (not deleted-safe) () "Bug: deleted vars ~A found in :safe-lex-visible-vars" deleted-safe)
    (assert (not global-safe) () "Bug: global vars ~A found in :safe-lex-visible-vars" global-safe)

    (unless (some (lambda (s) (match-p s '([assign-stmt] ?value ?targets)))
                  (butlast stmts))
      (return-from [suite-stmt] whole))
    
    ;; Collect the stmts before the assignment, and those after
    (multiple-value-bind (before-stmts ass-stmt after-stmts)
        (loop for sublist on stmts
            for s = (car sublist)
            until (match-p s '([assign-stmt] ?value ?targets))
            collect s into before
            finally (return (values before s (cdr sublist))))
      (assert ass-stmt)
      `(progn ,@(when before-stmts
                  `(([suite-stmt] ,before-stmts))) ;; recursive, but doesn't contain assign-stmt
              ,ass-stmt
              ,@(when after-stmts
                  (let ((bound-vars (assign-stmt-get-bound-vars ass-stmt)))
                    `((with-pydecl ((:safe-lex-visible-vars
                                     ,(let ((new-safe-vars (get-pydecl :safe-lex-visible-vars e)))
                                        (dolist (v bound-vars new-safe-vars)
                                          (when (and (member v (get-pydecl :lexically-visible-vars e))
                                                     (not (member v deleted-vars)))
                                            (assert (not (member v (get-pydecl :lexically-declared-globals e))) 
                                                () "Bug: variable ~A both lexicaly-visible and lexically-global." v)
                                            (unless (member v (get-pydecl :safe-lex-visible-vars e))
                                              (push v new-safe-vars)))))))
                        ([suite-stmt] ,after-stmts))))))))) ;; recursive, but 1 assign-stmt less

(defvar *last-raised-exception* nil)

(defmacro with-last-raised-exception ((exc) &body body)
  `(progn (setf *last-raised-exception* ,exc)
          (unwind-protect (progn ,@body)
            (setf *last-raised-exception* nil))))

(defun raise-stmt-1 (exc &optional var tb)
  (when tb (warn "Traceback arg to RAISE ignored"))
  
  ;; ERROR does not support _classes_ as first condition argument; it
  ;; must be an _instance_ or condition type _name_.
  (cond ((stringp (deproxy exc))
         (error "String exceptions are not supported (got: ~S)" (deproxy exc))
         (py-raise '{TypeError}
                   "String exceptions are not supported (got: ~S)" (deproxy exc)))
        
        ((and exc var)
         (etypecase exc
           (class  (error (make-exception exc var)))
           (error  (progn (warn "RAISE: ignored arg, as exc was already an instance, not a class")
                          (error exc)))))
        (exc
         (etypecase exc
           (class    (error (make-exception exc)))
           (error    (error exc))))
        
        (t
         ;; "raise" only allowed in the dynamic scope of an except block
         (if *last-raised-exception*
             (error *last-raised-exception*)
           (py-raise '{ValueError} "There is no outstanding exception to re-raise by bare `raise'.")))))

(defmacro [raise-stmt] (exc var tb)
  (whereas ((s (string-literal-p exc)))
    (warn "Raising string exceptions not supported (got: 'raise ~S')" s))
  `(raise-stmt-1 ,exc ,var ,tb))

(defmacro [try-except-stmt] (suite except-clauses else-suite)
  ;; The Exception class in a clause is evaluated only after an
  ;; exception is thrown.
  (with-gensyms (the-exc)
    (flet ((handler->cond-clause (except-clause)
	     (destructuring-bind (exc var handler-suite) 
                 except-clause
               ;; Every handler should store the exception, so it can be returned
               ;; in sys.exc_info().
	       (setq handler-suite `(with-last-raised-exception (,the-exc)
                                      ,handler-suite))
               (cond ((null exc)
		      `(t (progn ,handler-suite
				 (return-from try-except-stmt nil))))
		   
		     ((and (listp exc)
                           (eq (car exc) '[tuple-expr]))
                      ;; Because the names in EXC may be any variable that is bound to an exception
                      ;; class, not possible to use `(typep ,the-exc (or ,@names))
		      `((or ,@(loop for cls in (second exc)
                                  collect `(typep ,the-exc ,cls)))
                        ,@(when var `(([assign-stmt] ,the-exc (,var))))
                        ,handler-suite
                        (return-from try-except-stmt nil)))
               
                     (t
		      `((progn (try-except-ensure-valid-exception-class ,exc)
			       (typep ,the-exc ,exc))
                        ,@(when var `(([assign-stmt] ,the-exc (,var))))
                        ,handler-suite
                        (return-from try-except-stmt nil)))))))
      
      (let ((handler-form `(lambda (,the-exc)
			     (declare (ignorable ,the-exc))
			     (cond ,@(mapcar #'handler->cond-clause except-clauses)))))
      
	`(block try-except-stmt
	   (tagbody
	     (handler-bind (({Exception} ,handler-form))
	       
	       (progn (with-py-errors (:name try-except-function) ,suite)
		      ,@(when else-suite `((go :else)))))
	     
	     ,@(when else-suite
		 `(:else ,else-suite))))))))

(defun try-except-ensure-valid-exception-class (exc)
  (unless (and (typep exc 'class)
               (subtypep exc '{Exception}))
    (py-raise '{TypeError} "The `except' argument must be a subclass of `Exception' (got: ~A)." exc)))

(defmacro [try-finally-stmt] (try-suite finally-suite)
  `(unwind-protect
       ,try-suite
     ,finally-suite))

(defmacro [tuple-expr] (items)
  `(make-tuple-unevaled-list ,items))

(define-setf-expander [tuple-expr] (items &environment e)
  (get-setf-expansion `(list/tuple-expr ,items) e))

(define-setf-expander list/tuple-expr (items &environment e)
  (with-gensyms (store val-list)
    (let ((store-form `(let ((,val-list (assign-stmt-list-vals ,store ,(length items))))
                         ,@(mapcar (lambda (it)
                                     (multiple-value-bind (temps values stores store-form read-form)
                                         (get-setf-expansion it e)
                                       (declare (ignore read-form))
                                       (assert (null (cdr stores)))
                                       `(let* (,@(mapcar #'list temps values)
                                               (,(car stores) (pop ,val-list)))
                                          ,store-form)))
                                   items)
                         ,store))
          (del-form `(progn ,@(loop for it in items collect `([del-stmt] ,it)))))
      (values () ;; temps
              () ;; values
              (list store)
              (if *want-DEL-setf-expansion* del-form store-form)
              'setf-tuple-read-form-unused))))
  
(defmacro [unary-expr] (op item)
  (let ((py-op-func (get-unary-op-func-name op)))
    (assert py-op-func)
    `(funcall (function ,py-op-func) ,item)))

(defmacro [while-stmt] (test suite else-suite)
  `(tagbody
    .continue
     (if (py-val->lisp-bool ,test)
         (go .body)
       (go .else))
     
    .body
     (with-pydecl ((:inside-loop-p t))
       ,suite)
     (go .continue)
     
    .else
     ,@(when else-suite `(,else-suite))

     (go .break) ;; prevent warning
    .break
     ))

(defmacro [with-stmt] (expr var block &environment e)
  ;; When this expansion changes from the form:
  ;;  (WITH-NAMESPACE (...) normal-python-ast)
  ;; then the cps macro needs to be updated.
  (with-gensyms (mgr exit value exc)
    `(with-namespace (,(make-let-ns :names (list mgr exit value exc)
                                    :scope :with-stmt
                                    :parent (get-pydecl :namespace e))
                      :define-%locals nil
                      :define-%globals nil)
       ,(let ((clpython.parser:*extra-identifier-char2-p* (list #\$)))
          (with-matching ((clpython.parser:parse-with-replacements
                           (concatenate 'string "mgr$ = EXPR$
exit$ = mgr$.__exit__  # Not calling it yet
value$ = mgr$.__enter__()
exc$ = True
try:
  try:
"
                                        (when var "
    VAR$ = value$  ## Only if 'as VAR' is present
")
                                        "
    BLOCK$
  except:
    # The exceptional case is handled here
    exc$ = False
    if not exit$(*exc_info$()):
      raise
    # The exception is swallowed if exit() returns true
finally:
  # The normal and non-local-goto cases are handled here
  if exc$:
    exit$(None, None, None)")
                           `((([identifier-expr] {EXPR$})  . ,expr)
                             ,@(when var `((([identifier-expr] {VAR$})   . ,var)))
                             (([identifier-expr] {BLOCK$}) . ,block)
                             ({mgr$}   . ,mgr)
                             ({exit$}  . ,exit)
                             ({value$} . ,value)
                             ({exc$}   . ,exc)
                             (([identifier-expr] {exc_info$})
                              . ([literal-expr] :lisp (symbol-function
                                                       (find-symbol "exc_info" :clpython.module.sys))))))
                          ([module-stmt] ?suite-stmt))
            ?suite-stmt)))))

;; We should not come here during normal macroexpansion, as generators
;; are handled by the CPS convertor. However, some implementations
;; (Allegro) check CONSTANTP of the KEY form in (gethash KEY ..), and
;; KEY might be ([YIELD-EXPR] ..).  To handle those situation, macro
;; WITH-CPS-CONVERSION sets a pydecl, and we return a dummy
;; non-constant form.

(defvar *dummy-nonconstant-form* '(random 2))

(defmacro [yield-expr] (val &environment e)
  (declare (ignore val))
  (if (get-pydecl :inside-cps-conversion e)
      *dummy-nonconstant-form*
    (raise-syntax-error "Expression `yield' was found outside function.")))

(defmacro [yield-stmt] (val &environment e)
  (declare (ignore val))
  (if (get-pydecl :inside-cps-conversion e)
      *dummy-nonconstant-form*
    (raise-syntax-error "Statement `yield' was found outside function.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helper functions for the compiler

(defun stmt-p (sym)
  (check-type sym symbol)
  (let ((sym.name (symbol-name sym)))
    (cond ((<= (length sym.name) 5) nil)
          ((string-equal (subseq sym.name (- (length sym.name) 5)) "-stmt") sym)
          (t nil))))

(defun ast-contains-stmt-p (ast &key allowed-stmts)
  "Returns the forbidden statement, or NIL"
  (when (eq allowed-stmts t)
    (return-from ast-contains-stmt-p nil))
  (labels ((test (ast)
	     (typecase ast
	       (list (loop for x in ast when (test x) return it finally (return nil)))
	       (symbol (unless (member ast allowed-stmts :test #'eq)
			 (whereas ((s (stmt-p ast)))
			   (return-from test s))))
	       (t    nil))))
    (test ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Inlining of method calls on built-in objects

(defparameter *inlineable-methods* (make-hash-table :test #'eq))

(defun register-inlineable-methods ()
  (clrhash *inlineable-methods*)
  (loop for item in
	'(({isalpha}    0 stringp      py-string.isalpha)
	  ({isalnum}    0 stringp      py-string.isalnum)
	  ({isdigit}    0 stringp      py-string.isdigit)
	  ({islower}    0 stringp      py-string.islower)
	  ({isspace}    0 stringp      py-string.isspace)
	  ({join}       0 stringp      py-string.join   )
	  ({lower}      0 stringp      py-string.lower  )
	  ({strip}      0 stringp      py-string.strip  )
	  ({upper}      0 stringp      py-string.upper  )
	  	     
	  ({keys}       0 dict-p    dict.keys     )
	  ({items}      0 dict-p    dict.items    )
	  ({values}     0 dict-p    dict.values   )
	  	     
	  ({next}       0 py-func-iterator-p py-func-iterator.next)
	  
	  ({read}       (0 . 1) filep    py-file.read      )
	  ({readline}   (0 . 1) filep    py-file.readline  )
	  ({readlines}  (0 . 1) filep    py-file.readlines )
	  ({xreadlines}  0      filep    py-file.xreadlines)
	  ({write}       1      filep    py-file.write  )
	  
	  ({append}      1      vectorp  py-list.append )
	  ({sort}        0      vectorp  py-list.sort   )
	  ({pop}        (0 . 1) vectorp  py-list.pop    ))
	
      do (when (gethash (car item) *inlineable-methods*)
	   (warn "Replacing existing entry in *inlineable-methods* for attr ~A:~% ~A => ~A"
		 (car item) (gethash (car item) *inlineable-methods*) (cdr item)))
	 (setf (gethash (car item) *inlineable-methods*) (cdr item))))

(register-inlineable-methods)

(defun inlineable-method-p (attr num-pos-args)
  (let ((item (gethash attr *inlineable-methods*)))
    (and item
         (destructuring-bind (req-args check inline-func) 
             item
           (declare (ignore check inline-func))
           (etypecase req-args
             (integer (= num-pos-args req-args))
             (cons    (<= (car req-args) num-pos-args (cdr req-args))))))))

(defun inlined-method-code (prim attr args)
  (assert (inlineable-method-p attr (length args)))
  (destructuring-bind (req-args check inline-func)
      (gethash attr *inlineable-methods*)
    (declare (ignore req-args))
    `(let ((.prim ,prim))
       (if ,(ecase check
              ((stringp vectorp)  `(,check .prim))
              (filep              `(eq (class-of .prim) (ltv-find-class 'py-func-iterator)))
              (dict-p             `(eq (class-of .prim) (ltv-find-class 'dict)))
              (py-func-iterator-p `(eq (class-of .prim) (ltv-find-class 'py-func-iterator))))
           (,inline-func .prim ,@args)
         (py-call (attr .prim ',attr) ,@args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun py-**-mapping->lisp-arg-list (**-arg)
  ;; Return list: ( :|key1| <val1> :|key2| <val2> ... )
  ;; 
  ;; XXX CPython checks that ** args are unique (also w.r.t. k=v args supplied before it).
  ;;     We catch errors while the called function parses its args.
  (let* ((items-meth (or (x.class-attr-no-magic.bind **-arg '{items})
			 (py-raise '{TypeError}
				   "The ** arg in a call must be a mapping, ~
                                   supporting 'items' (got: ~S)" **-arg)))
	 (items-list (py-iterate->lisp-list (py-call items-meth))))
    (loop with res = ()
	for k-v in items-list
	do (let ((k-and-v (py-iterate->lisp-list k-v)))
	     (unless (= (length k-and-v) 2)
	       (py-raise '{TypeError}
			 "The ** arg must be list of 2-element tuples (got: ~S)"
			 k-v))
	     (destructuring-bind (k v) k-and-v
	       (push v res)
	       (push (intern (py-val->string k) :keyword)
		     res)))
	finally (return res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Detecting the globals and locals of modules, functions and classes

#+(or) ;; Unused, maybe move to ast-util or something.
(defun module-stmt-suite-globals (suite)
  "A list of the global variables of the module."

  ;; We make use of the fact that every global variable must be _set_
  ;; sometime: at the toplevel of the module, or in a function or
  ;; classdef.
  ;; 
  ;; The first way, at toplevel, can be detected by looking for the
  ;; variables used at the top level. The latter two (func/class) can
  ;; be detected by looking for the required `global' declaration.
  ;; 
  ;; However, the resulting list of names is a subset (underestimate)
  ;; of the total list of global variables in the module, as more can be
  ;; created dynamically from outside the module by another module,
  ;; and also by code in an "exec" stmt in this module.
  
  (declare (optimize (debug 3)))
  (assert (eq (car suite) '[suite-stmt]))
  
  (let ((globals ()))
    
    ;; Variables assigned/looked up at module level
    
    (with-py-ast (form suite)
      (case (car form)

	(([classdef-stmt]) 
	 ;; name of this class, but don't recurse
         (with-matching ((cdr form) (([identifier-expr] ?cname) ?inheritance ?csuite))
	   (pushnew ?cname globals))
	 (values :dummmy-classdef t))

	([funcdef-stmt]
	 ;; name of this function, but don't recurse
         (with-matching ((cdr form) (?deco ([identifier-expr] ?fname) ?args ?fsuite))
           (pushnew ?fname globals))
	 (values nil t))
	
	([identifier-expr] (let ((name (second form)))
			     (pushnew name globals))
			   (values :dummy-funcdef t))
	
	(t form)))
    
    ;; Variables explicitly declared `global', somewhere arbitrarily deeply nested.
    (with-py-ast (form suite :into-nested-namespaces t)
      (case (car form)

	([global-stmt] (with-matching ((second form) ([tuple-expr] ?identifiers))
                         (dolist (name (mapcar #'second ?identifiers))
                           (pushnew name globals))
                         (values :dummy-global t)))
	
	(t form)))
    
    ;; Every module has some special names predefined
    (dolist (n '({__name__} {__debug__}))
      (pushnew n globals))
    
    globals))

(defun member* (item &rest lists)
  (loop for x in lists thereis (member item x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function argument handling

(defun only-pos-args (args)
  "Returns NIL if not only pos args;
Non-negative integer denoting the number of args otherwise."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((num 0))
    (declare (type (integer 0 #.most-positive-fixnum) num))
    (dolist (a args)
      (cond ((symbolp a)
	     ;; Regular Python values are never symbols,
	     ;; so a symbol is a keyword.
	     (return-from only-pos-args nil))
	    (t (incf num))))
      num))
(declaim (ftype (function (list) (or (integer 0 #.most-positive-fixnum) null)) only-pos-args))

(defun raise-wrong-args-error ()
  (py-raise '{TypeError} "Wrong number of arguments, or wrong keyword, supplied to function."))

(defun raise-invalid-keyarg-error (kw)
  (py-raise '{TypeError}
	    "Function got unexpected keyword argument `~A'." kw))

(defun raise-double-keyarg-error (kw)
  (py-raise '{TypeError}
	    "Function got multiple values for argument `~A'." kw))

(defmacro py-arg-function (name (pos-args key-args *-arg **-arg) &body body)
  ;; Non-consing argument parsing! (except when *-arg or **-arg
  ;; present)
  ;; 
  ;; POS-ARGS: list of symbols
  ;; KEY-ARGS: list of (key-symbol default-val) pairs
  ;; *-ARG, **-ARG: a symbol or NIL 
  ;; 
  ;; XXX todo: the generated code can be cleaned up a bit when there
  ;; are no arguments (currently zero-length vectors are created).
  (let* ((num-pos-args (length pos-args))
	 (num-key-args (length key-args))
	 (num-pos-key-args  (+ num-pos-args num-key-args))
	 (some-args-p (or pos-args key-args *-arg **-arg))
	 (pos-key-arg-names (nconc (copy-list pos-args) (mapcar #'first key-args)))
	 (key-arg-default-asts (mapcar #'second key-args))
         (arg-kwname-vec (make-array num-pos-key-args
                                     :initial-contents (loop for x in pos-key-arg-names
                                                           collect (intern (string x) :keyword)))))
    ;; This MAKE-FA can not be moved inside the fuction as load-time-value, because
    ;;  1. it will probably not be evaluated at the right moment (top-level forms executed);
    ;;  2. some issue with default argument values and namespaces.
    `(locally
	 (declare #.+optimize-fast+)
       (let (,@(when (or *-arg **-arg some-args-p)
		     `((fa (make-fa
			    :func-name        ',name
			    :num-pos-args     ,num-pos-args
			    :num-key-args     ,num-key-args
			    :num-pos-key-args ,num-pos-key-args
			    :pos-key-arg-names ',(make-array (length pos-key-arg-names)
							     :initial-contents pos-key-arg-names)
			    :key-arg-default-vals (make-array ,num-key-args
							      :initial-contents (list ,@key-arg-default-asts))
			    :arg-kwname-vec   ,arg-kwname-vec
			    :*-arg            ',*-arg
			    :**-arg           ',**-arg)))))
	 ,(let ((fname (intern (format nil "~A.~A" *current-module-name* name) #.*package*)))
	       `(flet ((,fname (&rest %args)
			 (declare #+(or)(dynamic-extent %args)
				  #.+optimize-std+)
			 ,(let ((let-variables (append pos-key-arg-names
						       (when *-arg (list *-arg))
						       (when **-arg (list **-arg))))
				(maybe-only-pos-args-case (and some-args-p (not *-arg) (not **-arg))))
			       `(let (,@let-variables
				      ,@(when maybe-only-pos-args-case
					      `((only-pos-args (only-pos-args %args)))))
				  (declare (ignorable ,@let-variables))
				  ;; There are two ways to parse the argument list:
				  ;; - The pop way, which quickly assigns the variables a local name (only usable
				  ;;   when there are a correct number of positional arguments).
				  ;; - The array way, where a temporary array is created and a arg-parse function
				  ;;   is called (used everywhere else).
				  ,(let ((the-array-way
					  `(let ((arg-val-vec (make-array ,(+ num-pos-key-args
									      (if (or *-arg **-arg) 1 0)
									      (if **-arg 1 0)) :initial-element nil)))
					     (declare (dynamic-extent arg-val-vec))
					     (parse-py-func-args %args arg-val-vec fa)
					     ,@(loop for p in pos-key-arg-names and i from 0
						  collect `(setf ,p (svref arg-val-vec ,i)))
					     ,@(when  *-arg
						      `((setf ,*-arg (svref arg-val-vec ,num-pos-key-args))))
					     ,@(when **-arg
						     `((setf ,**-arg (svref arg-val-vec ,(1+ num-pos-key-args)))))))
					 (the-pop-way
					  `(progn ,@(loop for p in pos-key-arg-names collect `(setf ,p (pop %args))))))
					
					(cond ((or *-arg **-arg)  the-array-way)
					      (some-args-p        `(if (and only-pos-args
									    (= (the fixnum only-pos-args) ,num-pos-key-args))
								       ,the-pop-way
								       ,the-array-way))
					      (t `(when %args (raise-wrong-args-error)))))
				  
				  (locally #+(or)(declare (optimize (safety 3) (debug 3)))
					   ,@body)))))
		  #',fname))))))

#+allegro
(progn
(defun check-1-kw-call (got-kw nargs-mi want-kw)
  (unless (and (= (excl::ll :mi-to-fixnum nargs-mi) 2)
               (eq got-kw want-kw))
    (raise-wrong-args-error)))

(defun slow-1-kw-call (f a1 a2 kw)
  (declare (optimize #.+optimize-fast+))
  (block .func-body
    (tagbody
      (when a2
        (go .2-args))
     .body
      (return-from .func-body
        (funcall f a1))
     .2-args
      (if (eq a1 kw)
          (progn (setq a1 a2)
                 (go .body))
        (raise-wrong-args-error)))))

(defun slow-2-kw-call (f a1 a2 a3 a4 kw12)
  (declare (optimize #.+optimize-fast+))
  (let ((ka (car kw12))
        (kb (cdr kw12)))
    (block .func-body
      (tagbody
        (cond (a3 (go .>=3-args))
              ((symbolp a1) (go .error)))
        ;; 2 non-symbol args
        ;; XXX that pb can't be a symbol is enforced by CLPython
        ;; compiler, but calls directly from Lisp code might
        ;; violate this assumption.
       .body
        (return-from .func-body
          (funcall f a1 a2))
        
       .>=3-args
        (if a4
            (go .4-args)
          (go .3-args))
        
       .3-args
        (if (eq a2 kb)
            (progn (setq a2 a3) ;; f(a, b=2)
                   (go .body))
          (go .error)) ;; f(a, xxx=2)
        
       .4-args
        (progn (when (eq a1 ka)
                 (if (eq a3 kb)
                     (progn (setq a1 a2 ;; f(a=1,b=2)
                                  a2 a4)
                            (go .body))
                   (go .error)))
               (when (and (eq a1 kb) 
                          (eq a3 ka)) ;; f(b=2, a=1)
                 (progn (setq a1 a4)
                        (go .body))))
       .error
        (raise-wrong-args-error)))))

(defmacro with-nof-args-supplied-as-mi ((n) &body body)
  "Bind N to nofargs, as machine integer (not regular fixnum)"
  `(let* ((,n (excl::ll :register :nargs)))
     ,@body))

#-clpython-source-level-debugging ;; named-function goes wrong
(define-compiler-macro py-arg-function (&whole whole
                                               name (pos-args key-args *-arg **-arg) &body body)
  ;; More efficient argument-parsing, for functions that take only a few positional arguments.
  ;; Allegro passes the number of supplied args in a register; the code below makes use of
  ;; that register value.
  ;; 
  ;; If BODY creates closures, then the register value will be overwritten before we have
  ;; a chance to look at it. Therefore, if we read the :nargs register, the BODY is wrapped
  ;; in FLET.
  (flet ((case-0-pos ()
           `(named-function ,name
              (lambda ()
                ,@body)))
         (case-1-pos ()
           (let* ((pa (car pos-args))
                  (ka (intern (symbol-name pa) :keyword)))
             `(named-function ,name
                (lambda (,pa &optional .e)
                  (declare ,+optimize-fastest+) ;; surpress default arg checking
                  (if .e
                      (slow-1-kw-call (excl::ll :register :function) ,pa .e ',ka)
                    (locally (declare ,+optimize-std+)
                      ,@body))))))
         (case-2-pos ()
           (destructuring-bind (pa pb)
               pos-args
             (let ((ka (intern (symbol-name pa) :keyword))
                   (kb (intern (symbol-name pb) :keyword)))
               `(named-function ,name ;; f(a,b)
                  (lambda (,pa ,pb &optional .e1 .e2)
                    ;; Python functions never get NIL as argument, therefore
                    ;; as presence predicates the argument themselves can be used.
                    (symbol-macrolet ((.e1? .e1)
                                      (.e2? .e2))
                      (locally (declare ,+optimize-fastest+)
                        (if (or .e1? (symbolp ,pa))
                            (slow-2-kw-call (excl::ll :register :function)
                                            ,pa ,pb .e1 .e2 '(,ka . ,kb))
                          ;; 2 non-symbol args
                          ;; XXX that pb can't be a symbol is enforced by CLPython
                          ;; compiler, but calls directly from Lisp code might
                          ;; violate this assumption.
                          (locally (declare ,+optimize-std+) ;; but run body with safety
                            ,@body))))))))))
    
    (if (or (not *optimize-function-arg-checking*)
               (>= (length pos-args) 3)
               key-args *-arg **-arg)
        whole
      (ecase (length pos-args)
        (0 (case-0-pos))
        (1 (case-1-pos))
        (2 (case-2-pos))))))
);; #+allegro

(defstruct (func-args (:type vector) (:conc-name fa-) (:constructor make-fa))
  (num-pos-args          0 :type fixnum :read-only t)
  (num-key-args          0 :type fixnum :read-only t)
  (num-pos-key-args      0 :type fixnum :read-only t)
  (pos-key-arg-names    nil :type (or null vector) :read-only t)
  (key-arg-default-vals nil :type (or null vector) :read-only nil) ;; filled at load time
  (arg-kwname-vec       nil :type (or null vector) :read-only t)
  (*-arg                nil :type symbol :read-only t)
  (**-arg               nil :type symbol :read-only t)
  (func-name            nil :type symbol :read-only t))
  

(defun parse-py-func-args (%args arg-val-vec fa)
  ;; %ARGS: the (&rest) list containing pos and ":key val" arguments
  ;; ARG-VAL-VEC: (dynamic extent) vector to store final argument values in
  ;;              => the penultimate item will get *-arg value (if any)
  ;;                 the last item **-arg value (if any)
  ;;                 so ARG-VAL-VEC must be larger than just num-pos-and-key-args! 
  ;; FA: func-args struct
  ;; Returns nothing
  (declare (optimize (safety 3) (debug 3))
	   (dynamic-extent %args)
	   (type list %args))
  
  (let ((num-filled-by-pos-args 0) for-* for-**)
    (declare (type (integer 0 #.most-positive-fixnum) num-filled-by-pos-args))
    
    ;; Match standard pos-args and *-arg
    (loop
	with max-to-fill-with-pos = (the fixnum (fa-num-pos-key-args fa))
	until (or (= num-filled-by-pos-args max-to-fill-with-pos)
		  (symbolp (car %args))) ;; the empty list NIL is a symbol, too
	      
	do (setf (svref arg-val-vec num-filled-by-pos-args) (fastest (pop %args)))
	   (incf num-filled-by-pos-args)
	   
	finally
	  (unless (symbolp (car %args))
	    (cond ((fa-*-arg fa)
		   (setf for-*
		     ;; Reconsing because %args might be dynamic-extent.
		     (loop until (symbolp (car %args)) collect (fastest (pop %args)))))
		  (t (raise-wrong-args-error)))))
    
    ;; All remaining arguments are keyword arguments;
    ;; they have to be matched to the remaining pos and
    ;; key args by name.
    (loop
      	for key = (fastest (pop %args))
        for val = (fastest (pop %args))
	while key do
	  ;; `key' is a keyword symbol
	  (or (block find-key-index
		(when (> (the fixnum (fa-num-pos-key-args fa)) 0)
		  (loop with name-vec = (fa-pos-key-arg-names fa)
		      with kwname-vec = (fa-arg-kwname-vec fa)
		      for i fixnum from num-filled-by-pos-args below
			(the fixnum (fa-num-pos-key-args fa))
		      when (eq (svref kwname-vec i) key)
		      do (when (svref arg-val-vec i)
			   (raise-double-keyarg-error (svref name-vec i)))
			 (setf (svref arg-val-vec i) val)
			 (return-from find-key-index t))))
	      
	      (when (fa-**-arg fa)
                (push (cons key val) for-**)
		t)

              ;; Error.. either an unknown keyword, or we already got a pos arg for it
              (loop for i fixnum from 0 below num-filled-by-pos-args
                  when (eq (svref (fa-arg-kwname-vec fa) i) key)
                  do (raise-double-keyarg-error (svref (fa-pos-key-arg-names fa) i)))
              
	      (raise-invalid-keyarg-error key)))
    
    ;; Ensure all positional arguments covered
    (loop for i fixnum from num-filled-by-pos-args below (the fixnum (fa-num-pos-args fa))
	unless (svref arg-val-vec i)
	do (raise-wrong-args-error))
    
    ;; Use default values for missing keyword arguments
    (loop for i fixnum from (fa-num-pos-args fa) below (the fixnum (fa-num-pos-key-args fa))
	unless (svref arg-val-vec i)
	do (setf (svref arg-val-vec i)
	     (svref (fa-key-arg-default-vals fa)
		    (the fixnum (- i (the fixnum (fa-num-pos-args fa)))))))

    ;; Create * arg
    (when (fa-*-arg fa)
      (setf (svref arg-val-vec (fa-num-pos-key-args fa))
	(make-tuple-from-list for-*)))

    ;; Create ** arg
    (when (fa-**-arg fa)
      (setf (svref arg-val-vec (1+ (the fixnum (fa-num-pos-key-args fa))))
        (with-py-dict
            (loop with d = (make-py-hash-table)
                for (k . v) in for-** 
                do (setf (gethash (symbol-name k) d) v)
                finally (return d))))))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exceptions: convert Lisp conditions to Python exceptions

(defparameter *max-py-error-level* 1000) ;; max number of nested try/except; for b1.py
(defvar *with-py-error-level* 0)
(defvar *debug-with-py-errors-enabled* nil)

(defun check-max-with-py-error-level ()
  (fastest
   (when (> (the fixnum *with-py-error-level*) (the fixnum *max-py-error-level*))
     (py-raise '{RuntimeError} "Stack overflow (~A)" *max-py-error-level*))))

(defmacro with-py-errors ((&key (name 'with-py-errors-func)) &body body)
  (check-type name (or symbol list))
  (with-gensyms (f)
    `(let ((,f #+clpython-source-level-debugging (lambda () ,@body) ;; goes wrong with source lookup
               #-clpython-source-level-debugging (named-function ,name (lambda () ,@body))))
       (declare (dynamic-extent ,f))
       (call-with-py-errors ,f))))

(defun call-with-py-errors (f)
  (unless *debug-with-py-errors-enabled*
    (return-from call-with-py-errors (funcall f)))
  (let ((*with-py-error-level* (fastest (1+ (the fixnum *with-py-error-level*)))))
    (check-max-with-py-error-level)
    ;; Using handler-bind, so uncatched errors are shown in precisely
     ;; the context where they occur.
    (handler-bind
        ((division-by-zero (lambda (c) 
                             (declare (ignore c))
                             (py-raise '{ZeroDivisionError}
                                       "Division or modulo by zero")))
         
         #+(or) ;; Don't try to raise new Python exception.
         (storage-condition (lambda (c)
                              (declare (ignore c))
                              (py-raise-runtime-error)))
         #+allegro
         (excl:synchronous-operating-system-signal
          (lambda (c)
            (if (string= (simple-condition-format-control c)
                         "~1@<Stack overflow (signal 1000)~:@>")
                (py-raise '{RuntimeError} "Stack overflow")
              (py-raise '{RuntimeError} "Synchronous OS signal: ~A" c))))
         #+allegro
         (excl:interrupt-signal
          (lambda (c)
            (let ((args (simple-condition-format-arguments c)))
              (when (string= (cadr args) "Keyboard interrupt")
                (py-raise '{KeyboardInterrupt} "Keyboard interrupt")))))
         #+(or)
         (error (lambda (c)
                  (warn "with-py-handlers passed on error: ~A" c))))
      (funcall f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;  Generator rewriting

(defun generator-ast-p (ast)
  "Is AST a function definition for a generator? Returns set of ([yield-expr] [yield-stmt]) of nodes found."
  ;; Note that LAMBDA-EXPR can't contain (yield) statements
  (assert (not (ast-p ast '[module-stmt])) () "GENERATOR-AST-P on MODULE-STMT: does not make sense.")
  (let (res)
    (with-py-ast (form ast)
      (case (car form)
        
        (([yield-expr] [yield-stmt])
         (progn (pushnew (car form) res)
                form))
        
        ([classdef-stmt]
         ;; Don't enter the namespace, but do check the superclasses
         ;; which might be yield expressions (don't ask :)
         (with-matching (form ([classdef-stmt] ([identifier-expr] ?cname) ?inheritance ?csuite))
           (with-matching (?inheritance ([tuple-expr] ?supers))
             (dolist (node (mapcan #'generator-ast-p ?supers))
               (pushnew node res))))
         (values nil t))
        
        ([funcdef-stmt]
          ;; Don't enter the namespace, but do check the keyword default
          ;; values which might be yield expressions (don't ask :)
          (with-matching (form ([funcdef-stmt] ?decorators ?fname (?pos-args ?key-args ?*-arg ?**-arg) ?suite))
            (loop for (nil key-default-arg) in ?key-args
                do (dolist (node (generator-ast-p key-default-arg))
                     (pushnew node res))))
          (values nil t))
        
        ([lambda-expr]
         (with-matching (form ([lambda-expr] (?pos-args ?key-args ?*-arg ?**-arg) ?expr))
           (loop for (nil key-default-arg) in ?key-args
               do (dolist (node (generator-ast-p key-default-arg))
                    (pushnew node res))))
         (values nil t))
        
        (t form)))
    res))

(defun ast-deleted-variables (ast)
  "Is there a DEL statement in the AST? If so, returns a list of all
symbol names which are deleted. (Some compiler optimizations are possible
in the absence of DEL statements, as then variables can be guaranteed to
be bound."
  (let (deleted-names)
    (with-py-ast ((form &key value target) ast)
      (case (car form)
	([identifier-expr] (when (eq target +delete-target+)
			     (assert (not value))
			     (push (second form) deleted-names))
			   form)
	(t form)))
    deleted-names))
  
(defun funcdef-should-save-locals-p (ast)
  (when *allow-indirect-special-call*
    (return-from funcdef-should-save-locals-p t))
  
  (with-py-ast (form ast)
    (case (car form)
      ([call-expr] (destructuring-bind (primary . args)
		       (cdr form)
		     (declare (ignore args))
		     ;; `locals()' or `globals()'
                     (with-perhaps-matching (primary ([identifier-expr] ?name))
                       (when (member ?name '({locals} {globals} {eval}))
                         ;; We could check for num args here already, but that is a bit hairy,
                         ;; e.g. locals(*arg) is allowed if arg evaluates to e.g. [].
                         (return-from funcdef-should-save-locals-p t)))
		     form))
      ([exec-stmt] (return-from funcdef-should-save-locals-p t))
      (t form)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Source locations of classes and functions

(defun record-source-file-loc (context-name kind)
  ;; Make source location known to Allegro, using "fi:lisp-find-definition".
  ;; Also record upper case version, apparently otherwise lower case names must
  ;; be |escaped| in ANSI mode.
  ;; Besides in :clpython.user, the sources are also recorded as symbols in
  ;; the :clpython package. That eases the use.
  (declare (ignorable kind))
  (check-type context-name symbol)
  #+(or)
  (let ((syms (list context-name 
                    (ensure-user-symbol (string-upcase context-name))
                    (intern (string context-name) (load-time-value (find-package :clpython)))
                    (intern (string-upcase context-name) (load-time-value (find-package :clpython))))))
    
    (without-redefinition-warnings
     (dolist (s syms)
       #+(or)(eval `(excl:record-source-file ',s :type ',kind))
       ))))
