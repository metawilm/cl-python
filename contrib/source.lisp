;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON; Readtable: PY-AST-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Source-level debugging in Allegro

(in-package :clpython)
(in-syntax *ast-user-readtable*)

#-clpython-source-level-debugging
(eval-when (:compile-toplevel :load-toplevel :execute)
  (break "This module is for Allegro 8.2+"))


;;; The unit of source information is the SUBFORM, which links one Python AST node
;;; (a statement or expression) to a function and assembly offset. SUBFORMS are only
;;; available if a module is loaded.
;;;
;;; The source information of a module is stored in a MODULE-SOURCE-INFORMATION (MSI)
;;; struct. From there the SUBFORMS can be reached.
;;;
;;; Additionally the source information can be looked up with the function as key.
;;; This is for example used when a Python breakpoint is hit.

(defparameter *module->source-positions* (make-hash-table :test 'equal)
  "Mapping from pathname to source data. Filled during loading.")

(defstruct (module-source-information
            (:conc-name msi.)
            (:constructor make-msi (original-filename original-source subforms))
            (:print-object print-msi))
  "A MODULE-SOURCE-INFORMATION, or MSI, contains the Python SUBFORMS of one source file."
  original-filename original-source subforms)

(defun print-msi (msi stream)
  (print-unreadable-object (msi stream :type t)
    (format stream "~S (~A records)~% "
            (msi.original-filename msi)
            (length (msi.subforms msi)))
    (pprint-logical-block (stream (msi.subforms msi) :per-line-prefix "    ")
      (let ((*print-level* 5))
        (dolist (subform (msi.subforms msi))
          (format stream "~A~%" subform))))))

(defun msi-for-source-file (file &key error)
  "When ERROR, returns value must be MSI."  
  (check-type file pathname)
  (loop for py-file being the hash-key in *module->source-positions*
      using (hash-value msi)
      when (equalp (pathname file) (pathname py-file))
      return msi
      finally (when error (error "File ~A is not a loaded Python module." file))))


(defvar *lisp-function->subforms* (make-hash-table :test 'eq)
  "Mapping from Lisp fuction to list of SUBFORMS.
Subforms are ordered from outer to inner, so parent ASTs go before children. 
This is the same order as provided by Allegro CL.")

(defstruct (subform
            (:conc-name subform.)
            (:print-object print-subform))
  "A SUBFORM links a Python abstract syntax tree node, with the function/asm where it is loaded."
  form start-char end-char bp-func bp-pc bp-next-pc bp-own msi)

(defun print-subform (subform stream)
  (print-unreadable-object (subform stream :type t)
    ;; Don't print msi: infinite recursion
    (format stream ":form ~A ~_:form-python \"~A\" :chars [~A, ~A] ~_:bp-func ~A ~_:bp-pc [~A, ~A] :bp-own ~A"
            (subform.form subform)
            (clpython.parser::abbreviated-python-code (subform.form subform))
            (subform.start-char subform) (subform.end-char subform)
            (subform.bp-func subform) (subform.bp-pc subform) (subform.bp-next-pc subform)
            (subform.bp-own subform))))

(defun subform.pc-range (subform)
  "Returns VALID-P, PC, NEXT-PC, PC-RANGE 
If subform corresponds to non-empty range of pc offsets, and returned values are:
 T, PC, NEXT-PC, (NEXT-PC - PC)
If invalid:
 NIL, PC, NEXT-PC, MOST-POSITIVE-FIXNUM"
  (check-type subform subform)
  (let ((pc (subform.bp-pc subform))
        (next-pc (subform.bp-next-pc subform)))
    (if (and (typep pc '(integer 0))
             (typep next-pc '(integer 0))
             (> next-pc pc))
        (values t pc next-pc (- next-pc pc))
      (values nil pc next-pc most-positive-fixnum))))

(defun subform.pc-contained-p (subform pc)
  (check-type subform subform)
  (check-type pc integer)
  (multiple-value-bind (valid-p subform.pc subform.next-pc range)
      (subform.pc-range subform)
    (declare (ignore range))
    (and valid-p (<= subform.pc pc subform.next-pc))))

(defun subform.source-line-offset (subform)
  "Returns position of SUBFORM in the full source code as LINE, OFFSET; both start at zero."
  (check-type subform subform)
  (let ((source (msi.original-source (subform.msi subform)))
        (char-ix (subform.start-char subform)))
    (check-type source string)
    (check-type char-ix integer)
    (assert (<= 0 char-ix (1- (length source))))
    (let ((line-no (count #\Newline source :end char-ix)))
      (if (zerop line-no)
          (values 0 char-ix)
        (let ((newline-pos (position #\Newline source :from-end t :end char-ix)))
          (values line-no (- char-ix (1+ newline-pos))))))))

(defun subform-for-func-pc (func pc &key (exact-start-pc t))
  ;; FUNC must be the actual function, so an FLET instead of the outer DEFUN, etc.
  (let (best best-range)
    (dolist (subform (gethash func *lisp-function->subforms*) best)
      (multiple-value-bind (s.valid-p s.pc s.next-pc s.pc-range)
          (subform.pc-range subform)
        (declare (ignore s.next-pc))
        (when (and s.valid-p
                   (if exact-start-pc (= s.pc pc) (subform.pc-contained-p subform pc))
                   (or (not best) (< s.pc-range best-range)))
          (setf best subform
                best-range s.pc-range))))))


;;; A Python module includes a table mapping source code character index to AST form.
;;; This table is created during the macroexpansion of [module-stmt], and stored as a
;;; custom declaration (pydecl).
;;;
;;; The table is recorded in the Lisp source code records, and can be retrieved upon loading.

(defun create-python-source-location-table-pydecl (suite)
  (let ((table (create-python-source-location-table suite)))
    `(locally (declare (pydecl (:python-source-location-table ,table))))))

(defun create-python-source-location-table (suite)
  "Returns the Python AST subforms of SUITE together with their source location. Parents are
 returned before their children, as a list of (:form <AST form> :start <ix> :end <ix2>)"
  (when *python-form->source-location*
    (let (python-source-location-table)
      (with-py-ast ((subform &key value target) suite :into-nested-namespaces t)
        (declare (ignore value))
        (unless target
          (whereas ((source-position (gethash subform *python-form->source-location*)))
            (push (list* :form subform (copy-list source-position)) python-source-location-table)))
        subform)
      (reverse python-source-location-table))))

(defun retrieve-python-source-location-table (module-func-name)
  "Retrieve the Python source location table for the module designated by MODULE-FUNC-NAME.
Returned table is a list of entries: (:FORM <AST form> :START <ix> :END <ix2>)
The table is assumed to be stored as PYDECL by the [module-stmt] macro.
Error when no table found."
  (check-type module-func-name symbol)
  (labels ((find-it (x)
             (when (listp x)
               (when (eq (car x) :python-source-location-table)
                 (return-from retrieve-python-source-location-table (second x)))
               (dolist (item x)
                 (find-it item)))))
    (loop for record across (function-source-records (symbol-function module-func-name) :lisp)
        for src = (excl::ldb-code-source record)
        when (and (listp src)
                  (eq (first src) 'defun)
                  (eq (second src) module-func-name))
        do (or (find-it src)
               (break "Found DEFUN record but not :PYTHON-SOURCE-LOCATION-TABLE in src ~S for func ~S"
                      src module-func-name))
        finally (error "No Python source information found for module-function ~S." module-func-name))))

(defun function-source-records (function kind)
  "Return the Lisp source debug records for FUNCTION as vector.
KIND is :LISP or :PYTHON; the latter only includes records for Python STMT or EXPR nodes.
The :PYTHON records are thus a subset of the :LISP records."
  (setf function (coerce-to-function function))
  (let ((lisp-records (or (excl::function-source-debug-info function)
                          (error "No Lisp source info for ~s" function))))
    (ecase kind
      (:lisp lisp-records)
      (:python (coerce (remove-if-not #'clpython.parser::ast-p lisp-records
                                      :key #'excl::ldb-code-source)
                       'vector)))))

;;; At module loading time the Lisp and Python source information is linked together.
;;;
;;; The aspect that makes it all work, is that the AST forms in our table are EQ to
;;; the source forms stored in the Lisp source debug info records. The latter maps
;;; from lisp form to function/assembly offset.

(defparameter *debug-dump-python-asm-mapping* nil
  "Whether to print the Python -> ASM offset mapping for a module (for debugging).")

(defvar *debug-latest-msi* nil
  "The MSI last augmented with ASM information.")

(defun register-python-module-source (&key module-function-name source-path source)
  (check-type module-function-name symbol)
  (check-type source string)
  (check-type source-path pathname #+(or)(or string pathname))
  (when *module->source-positions*
    (let* ((subforms (loop for psi in (retrieve-python-source-location-table module-function-name)
                         collect (destructuring-bind (&key form start end) psi
                                   (make-subform :form form :start-char start :end-char end))))
           (msi (make-msi source-path source subforms)))
      (dolist (subform subforms)
        (setf (subform.msi subform) msi))
      (setf (gethash source-path *module->source-positions*) msi)
      (augment-msi-with-asm module-function-name msi)
      (when *debug-dump-python-asm-mapping*
        (print-python-asm-mapping module-function-name msi)))))

(defun augment-msi-with-asm (module-function msi)
  "Augments the MSI with assembly information."
  (setf module-function (coerce-to-function module-function))
  (let ((func module-function))
    (loop with vec = (function-source-records func :lisp)
        with assigned-bps = (make-hash-table :test #'equal) ;; (func . pc) -> t
        for record across (function-source-records func :python)
        for ldb-code.src = (excl::ldb-code-source record)
        do (dolist (subform (msi.subforms msi))
             
             ;; The core step in linking together the Lisp and Python source information:
             (when (eq (subform.form subform) ldb-code.src)
               
               (if (subform.bp-pc subform)
                   (warn "Assembly information already set in subform ~A" subform)
                 (let* ((curr-func (loop for i from (excl::ldb-code-index record) downto 0
                                       when (excl::ldb-code-func (aref vec i))
                                       return it
                                       finally 
                                               ;;; XXX is this always correct, if no function is set at all?
                                         (return func)))
                        (start-pc (or (excl::ldb-code-pc record)
                                      (error "Missing pc in record ~A for ~A" record func)))
                        (next-pc (or (excl::ldb-code-next-pc record)
                                     (progn #+(or) ;; for debugging
                                            (warn "Missing next-pc in record ~A for ~A" record func)
                                            ;; (excl::ldb-code-pc record)
                                            most-positive-fixnum)))
                        (own-p (not (gethash (cons curr-func start-pc) assigned-bps))))
                   (setf (subform.bp-func subform) curr-func
                         (subform.bp-pc subform) start-pc
                         (subform.bp-next-pc subform) next-pc
                         (subform.bp-own subform) own-p)
                   ;; Ensure the subforms are stored from outer to inner.
                   (setf (gethash curr-func *lisp-function->subforms*)
                     (nconc (gethash curr-func *lisp-function->subforms*) (list subform)))
                   
                   (setf (gethash (cons curr-func start-pc) assigned-bps) t)
                   #+(or) ;; debug
                   (format t "Python source fragment chars [~A, ~A]:~_  ~A~_maps to asm offsets [~A, ~A] in func ~S.~%"
                           (subform.start-char subform)
                           (subform.end-char subform)
                           (clpython:py-pprint ldb-code.src)
                           (excl::ldb-code-pc record) (excl::ldb-code-next-pc record) curr-func)))))
           ;; finally (format t "PC - subform mapping: ~A" assigned-bps)
           ))
  (setf *debug-latest-msi* (format nil "Calculated Python-asm mapping: ~A" msi))
  msi)

(defun print-python-asm-mapping (module-function msi)
  (flet ((find-func-for-pc (bp-vector ix)
           (check-type bp-vector vector)
           (check-type ix integer)
           (loop named find
               for i from ix downto 0
               for bp = (aref bp-vector i)
               do (whereas ((f (excl::ldb-code-func bp)))
                    (return-from find f))
                  (let ((src (excl::ldb-code-source bp)))
                    (when (and (listp src)
                               (eq (car src) 'defun))
                      (return-from find (second src))))
                  (break "no func for ix ~A" ix)))
         (sum-asm-ranges (vec start end)
           (loop with result
               for ix from start to end
               for pc = (excl::ldb-code-pc (aref vec ix))
               for pc-next = (excl::ldb-code-next-pc (aref vec ix))
               do (cond ((null pc-next) (push pc result))
                        ((= pc pc-next) (push pc result))
                        ((< pc pc-next) (push (cons pc pc-next) result))
                        (t              (push pc result)
                                        (push (cons pc-next pc-next) result)))
               finally 
                 (flet ((sort-them (res)
                          (sort res #'< :key (lambda (x) (if (consp x) (- (car x) 0.5) x)))))
                   (let ((res (sort-them (remove-duplicates result :test 'equal))))
                     (return (let (to-remove)
                               (tagbody
                                .repeat
                                 (loop for sublist on res
                                     do (let ((a (first sublist))
                                              (b (second sublist)))
                                          (when (and (consp a)
                                                     (or (and (numberp b)
                                                              (<= (car a) b (cdr a)))
                                                         (and (consp b)
                                                              (= (car b) (cdr b))
                                                              (<= (car a) (car b) (cdr a)))))
                                            (setf res (remove b res))
                                            (go .repeat)))))
                               (setf res (set-difference res to-remove))
                               (setf res (sort-them res))
                               (return res))))))))
    (declare (ignorable #'find-func-for-pc))
    (let ((func module-function))
      (when (excl::closurep func)
        (setq func (excl::cl_shared func)))
      (loop with vec = (function-source-records func :lisp)
          for record across (function-source-records func :python)
          do (dolist (subform (msi.subforms msi))
               (when (eq (subform.form subform) (excl::ldb-code-source record))
                 (format t "~&~%Python source form [chars ~A~@[-~A~]]: ~A~%" 
                         (subform.start-char subform) 
                         (let ((end (subform.end-char subform)))
                           (when (/= end (subform.start-char subform))
                             end))
                         (py-pprint (subform.form subform)))
                 (when (excl::ldb-code-end record)
                   (format t "Asm ranges: ~A~%" (sum-asm-ranges vec (excl::ldb-code-index record) (excl::ldb-code-end record))))
                 (do* ((record record (when child-ix (aref vec child-ix)))
                       (child-ix #1=(when record (excl::ldb-code-child record)) #1#)
                       (src #2=(when record (excl::ldb-code-source record)) #2#))
                     ((not record))
                   #+(or)(format t "Lisp func: ~S~%" (find-func-for-pc vec (excl::ldb-code-index record)))
                   (format t "Lisp (#~A) [asm ~A~@[-~A~]]: ~S~%" 
                           (excl::ldb-code-index record)
                           (excl::ldb-code-pc record)
                           (let ((next (excl::ldb-code-next-pc record)))
                             (when (/= next (excl::ldb-code-pc record))
                               next))
                           src))))))))

;;; Once a module is registered, we have the mapping from source file location to subform.
;;; As subform contains FUNC and PC, mapping effectively goes from source location to AST to func/pc.

(defun subform-for-source-file-char (file char &key (error t))
  "When ERROR, return value must be SUBFORM."
  (check-type file pathname)
  (let ((msi (msi-for-source-file file :error error)))
    (subform-for-msi-char msi char :error error)))

(defun subform-for-msi-char (msi char &key error)
  "When ERROR, return value must be SUBFORM."
  (check-type char integer)
  (check-type msi module-source-information)
  ;; XXX For now requires exact match
  (let (best-subform)
    (dolist (subform (msi.subforms msi))
      (when (and (clpython.parser::ast-p (subform.form subform)) ;; XXX needed?
                 (<= (subform.start-char subform) char (subform.end-char subform))
                 (or (null best-subform)
                     (<= (- (subform.end-char subform) (subform.start-char subform))
                         (- (subform.end-char best-subform) (subform.start-char best-subform)))))
        (setf best-subform subform)))
    (or best-subform
        (when error
          (error "Not a valid breakpoint position (no subform record found): char ~A file ~S."
                 char (msi.original-filename msi))))))

;;; Python breakpoints

(defvar *inside-python-set-breakpoint* nil
  "For consistency checking.")

(defun python-breakpoint-p (bpt)
  (check-type bpt excl::breakpoint)
  (eq (excl::ldb-language-name (excl::breakpoint-language bpt)) :python))

(defun ldb-python.find-pc (func pc case-table &optional return-address)
  "PC must be the start of a Python AST form (derived from Python source info)."
  (declare (ignorable case-table return-address))
  (check-type func function)
  (check-type pc (or excl::breakpoint integer))
  (unless *inside-python-set-breakpoint*
    (warn "Hmm, LDB-PYTHON.FIND-PC without *INSIDE-PYTHON-SET-BREAKPOINT*"))
  ;; Every Python AST form is a Lisp form and the start of an ASM instruction.
  ;; Therefore adapt the asm breakpoint.
  (make-bp-from-asm-bp func pc))

(defun make-bp-from-asm-bp (func pc &key return-address source)
  (check-type func function)
  (check-type pc (or excl::breakpoint integer))
  (let ((case-table (make-array 257 :element-type '(unsigned-byte 32))))
    (multiple-value-bind (asm-start-pc asm-next-pc branched-to branch-type)
        (excl::ldb-asm-find-pc func pc case-table return-address)
      (when (excl::breakpoint-p pc)
        (multiple-value-setq (asm-start-pc asm-next-pc branched-to branch-type)
          (values (excl::breakpoint-pc pc) (excl::breakpoint-next-pc pc)
                  (excl::breakpoint-branch-pc pc) (excl::breakpoint-branch-type pc))))
      (excl::make-ldb-code :language (ensure-python-language)
                           :func func
                           :pc asm-start-pc
                           :next-pc asm-next-pc
                           :branch-pc branched-to
                           :branch-type branch-type
                           :temporary t
                           :source source))))

(defun ldb-python.print-pc (func pc &optional sliding)
  (setf pc (coerce-to-breakpoint-pc pc))
  (let ((subform (subform-for-func-pc func pc)))
    (cond (subform
           (ldb-python-print-subform subform))
          ((excl::ldb-lisp-source-p func) ;; Fall back on Lisp LDB
           (excl::ldb-lisp-print-pc func pc sliding))
          (t
           (format t "[ldb-python.print-pc: default case] Function ~A pc=~A" func pc)))))

(defun ldb-python-print-subform (subform)
  (check-type subform subform)
  (unless (eq :python (excl::ldb-backend-name excl::*current-ldb-backend*))
    (warn "LDB-PYTHON-PRINT-SUBFORM called with backend != :python: ~A" excl::*current-ldb-backend*))
  (funcall (excl::ldb-backend-format excl::*current-ldb-backend*)
           :info-source subform))

;;; Breakpoint instruction classificatin

(defun bpt.call-p (bpt)
  (check-type bpt excl::breakpoint)
  (member (excl::breakpoint-branch-type bpt) '(:call :funcall :funcall-check)))

(defun bpt.return-p (bpt)
  (check-type bpt excl::breakpoint)
  (eq (excl::breakpoint-branch-type bpt) :ret))

(defun bpt.jump-p (bpt)
  (check-type bpt excl::breakpoint)
  (eq (excl::breakpoint-branch-type bpt) :jmp))


;;; Stepper: define behaviour when a breakpoint is hit.

(defvar *possible-step-commands* '(:cont :return :into :over)
  "The possible commands a user can give to the stepper.")

(defvar *current-step-mode* nil
  "Possible values: nil :into :over")

(defun ldb-python.set-next (bpt command)
  (declare (optimize (debug 3)))
  ;; Mostly copied from LDB-ASM-SET-NEXT
  ;; XXX Must take into account the :into, :over commands.
  (let ((func (excl::breakpoint-func bpt))
        (language (excl::breakpoint-language bpt)) ;; enables reuse by other languages
        (temporary-kind (if (command.continue-p command) :ratchet :temporary))
        (next-pc-valid (and (not (bpt.jump-p bpt))
                            (not (command.return-p command))
                            (excl::breakpoint-next-pc bpt)))
        (*inside-python-set-breakpoint* t))
    ;;              ......command........................
    ;; branch-type  return  cont           into 
    ;; -----------  ------- -------------- --------------
    ;;  jmp         rl=tmp 
    ;;  call        rl=tmp  nxt=rat;cl=rat nxt=tmp;cl=tmp
    ;;  bcc         rl=tmp  nxt=rat;br=rat nxt=tmp;br=tmp
    ;;  ret         rl=tmp  nxt=rat;rl=rat nxt=tmp;rl=tmp
    ;;  no-stop     rl=tmp  nxt=rat        nxt=tmp
    ;;  case        rl=tmp  nxt=rat;br=rat nxt=tmp;br=tmp
    ;;  nil         rl=tmp  nxt=rat        nxt=tmp
    
    ;; Try to set the next-pc breakpoint, if appropriate
    (when (bpt.call-p bpt)
      (when (or (command.continue-p command) (command.into-p command))
        (excl::add-call-link bpt temporary-kind)))

    (when (or (command.return-p command) (bpt.return-p bpt))
      (excl::add-return-link bpt temporary-kind))

    (when next-pc-valid
      (excl::add-breakpoint func next-pc-valid temporary-kind nil language))
    
    ;; Now set branch-pc breakpoints, if any
    (unless (command.return-p command)
      (whereas ((branch (excl::breakpoint-branch-pc bpt)))
        (etypecase branch
          (vector (loop for case across branch
                      do (excl::add-breakpoint func case temporary-kind nil language)))
          (integer (excl::add-breakpoint func branch temporary-kind nil language)))))))

;;; Step command classification

(defun command.return-p (command)
  (check-type command keyword)
  (eq command :return))

(defun command.continue-p (command)
  (check-type command keyword)
  (eq command :cont))

(defun command.into-p (command)
  (check-type command keyword)
  (eq command :into))

(defvar *current-bpt*)

(defparameter *not-into-functions*
    nil
  #+(or)(list #'excl::format-engine #'excl::process-format-string #'format #'excl::gethash_2op_1ret
              #'(method make-instance (class)) #'(method py-call (class)) #'excl::coerce-to-condition
              #+(or) #'py-call))

(defgeneric ldb-python.prompt-loop (bpt &optional old-bpt)
  (:method (bpt &optional old-bpt)
           ;; Executed upon a breakpoint hit. Must return a command from:
           ;;   :cont :return :into :over
           ;; This command is input for SET-NEXT. Command :this is handled by itself.
           (declare (ignore old-bpt))
           (setf *current-step-mode* :into)
           (setf *current-bpt* bpt)
           #+(or)
           (let ((*print-level* 3))
             (format t "===~&Python prompt loop at bpt exptr:  ~A~%" (plc-python-or-lisp-code bpt)))
           (let ((subform (subform-for-func-pc (excl::breakpoint-func bpt) (excl::breakpoint-pc bpt)))
                 (into-mode (eq *current-step-mode* :into))
                 (user-set-bp (not (excl::breakpoint-temporary bpt))))
    
             (when (and subform (or into-mode user-set-bp))
               (ldb-python-print-subform subform)
               (loop for str = (read-line *standard-input* nil nil)
                   while (plusp (length str))
                   when (string= str ":p")
                   do (print-python-stacktrace))
               (return-from ldb-python.prompt-loop
                 (progn (warn "Calling excl::ldb-backend-prompt-loop")
                        (let ((res (funcall (excl::ldb-backend-prompt-loop excl::*current-ldb-backend*))))
                          (warn "excl::ldb-backend-prompt-loop res=~S" res)
                          res))))
             (unless subform
               (assert (not user-set-bp) ()
                 "Not a subform, so can't be a user-set Python breakpoint: ~A." bpt)
               (assert into-mode ()
                 "Stopped at a PC without Python form, should imply INTO-mode.")
               (let ((*print-level* 4))
                 #+(or)(format t "skipping bpt form lacking python form: ~A" bpt)
                 (when (member (excl::breakpoint-func bpt) *not-into-functions*)
                   #+(or)(format t "~&SKIPPED: stepping over ~A~%" (excl::breakpoint-func bpt))
                   (return-from ldb-python.prompt-loop :over))))
             
             (return-from ldb-python.prompt-loop *current-step-mode*))))

(defun ldb-python.list-exits (bpt sliding)
  (declare (ignore bpt sliding))
  (error "Never"))

(defun ldb-python.slide (bpt command verbose)
  (declare (ignore bpt command verbose))
  (error "Never"))

(defun add-python-language ()
  ;; Lacking remove-ldb-language...
  (setf excl::*ldb-languages* (remove :python excl::*ldb-languages* :key #'excl::ldb-language-name))
  (excl::add-ldb-language :python
                          :predicate (constantly t) ;; also handle PCs in the middle of Python forms, for :step-into
                          
                          ;; breakpoint behaviour:
                          :find-pc #'ldb-python.find-pc
                          :print-pc #'ldb-python.print-pc
                          
                          ;; stepper behaviour:
                          :prompt-loop #'ldb-python.prompt-loop
                          :set-next #'ldb-python.set-next
                          :list-exits #'ldb-python.list-exits
                          :slide #'ldb-python.slide))

(add-python-language)

(defun ensure-python-language ()
  (or (excl::find-language :python)
      (error "LDB language :python not defined")))


;;; Defining the LDB backend, which does the user interaction after a breakpoint is hit.
;;;
;;; For example, it deals with "presenting" messages to the user, which does 
;;; not necessarily mean printing them to stdout.

(defgeneric python-backend.format (kind &rest args)
  (:method (kind &rest args)
           (case kind
             (:info-source (let ((subform (pop args)))
                             (check-type subform subform)
                             (let ((msi (subform.msi subform)))
                               (format t "~&;; Hit breakpoint at: ~%")
                               (py-pprint (subform.form subform) t)
                               (multiple-value-bind (line pos)
                                   (subform.source-line-offset subform)
                                 (format t "~&;; File ~A at line ~A, char ~A~%" 
                                         (msi.original-filename msi) line pos)
                                 #+(or)(subform.start-char subform)))))
             (:deleting (apply #'format t args))
             (t (format t  "[python-backend.format] ~A ~{~A~^, ~}" kind args)))))

(defgeneric python-backend.prompt-loop ()
  (:method ()
           "Returns a value in *debug-interaction-commands*"
           (assert *current-bpt*)
           ;; XXX reuse? (excl::read-eval-print-loop :stepping :ldb-step)
           (if (not (excl::breakpoint-temporary *current-bpt*))
              (progn (format t "current-bp func = ~A~%" *current-bpt*)
                     (read))
             :into)))

(defun add-python-backend ()
  (excl::add-ldb-backend :name :python
                         :format #'python-backend.format
                         :prompt-loop #'python-backend.prompt-loop))

(add-python-backend)


;;; Debugger interface: setting and removing breakpoints.
;;; Also safe functions for use with Emacs.

#+(or)
(defun print-python-breakpoints ()
  (let (some)
    (map-over-python-breakpoints
     (lambda (&key func bp ix ix-in-func &allow-other-keys)
       (setf some t)
       (when (zerop ix-in-func)
         (unless (zerop ix)
           (format t "~%"))
         (format t "~A~%" func))
       (format t "  ~3D  ~A~%" (plc-pc bp) (plc-python-or-lisp-code bp :abbreviated t))))
    (unless some
      (format t "No Python breakpoints set.~%"))))

#+(or)
(defun list-python-breakpoints-for-emacs ()
  (with-output-to-string (*standard-output*)
    (print-python-breakpoints)))

(defun map-over-python-breakpointed-subforms (f)
  "Call F on all Python breakpoints, with two arguments: BP-STRUCT, SUBFORM.
Calls are ordered by function, and within functions by increasing pc."
  (maphash (lambda (func bp-map)
             (check-type func function)
             (check-type bp-map hash-table)
             (let (func-bps-subforms)
               (maphash (lambda (asm-pc bp-struct)
                          (check-type asm-pc integer)
                          (check-type bp-struct excl::breakpoint)
                          (when (python-breakpoint-p bp-struct)
                            (let ((subform (or (subform-for-func-pc func asm-pc)
                                               (error "No subform for ~A ~A" func asm-pc))))
                              (assert (eq (subform.bp-func subform) func) ()
                                "Unexpected subform function: SUBFORM-FOR-FUNC-PC gave ~A for :func ~A :pc ~A"
                                subform func asm-pc)
                              (assert (eq (subform.bp-pc subform) asm-pc) ()
                                "Unexpected subform pc: SUBFORM-FOR-FUNC-PC gave ~A for :func ~A :pc ~A"
                                subform func asm-pc)
                              (push (list bp-struct subform) func-bps-subforms))))
                        bp-map)
               (setf func-bps-subforms (sort func-bps-subforms #'< :key (lambda (x) (excl::ldb-code-pc (car x)))))
               (loop for (bp-struct subform) in func-bps-subforms
                   do (funcall f bp-struct subform))))
           excl::*ldb-breakpoints*))

(defun set-breakpoint (&key file character)
  "Set a Python breakpoint for the Python source form that is at the given CHARACTER in FILE.
Note that CHARACTER is zero-based, while e.g. Emacs starts counting at 1.
The source information for FILE is assumed to be up to date.
The (new) breakpoint is enabled and returned."
  (let ((subform (subform-for-source-file-char file character :error t))
        (*inside-python-set-breakpoint* t))
    (assert subform)
    (prog1 (excl:add-breakpoint (subform.bp-func subform) (subform.bp-pc subform)
                                nil ;; temporary
                                nil ;; verbose -- error otherwise, not good
                                :python) ;; language
      ;; Necessary to activate it.
      (excl::install-breakpoints))))

(defun set-breakpoint-for-emacs (&rest args)
  (let ((bp (apply #'set-breakpoint args)))
    (message-for-emacs "Breakpoint set: ~A  [:func ~S :pc ~A]"
                       (clpython.parser::abbreviated-python-code (excl::breakpoint-source bp))
                       (excl::breakpoint-func bp) (excl::breakpoint-pc bp)))
  t)

(defun remove-breakpoint (&key file char)
  (let ((subform (subform-for-source-file-char file char file char :error t)))
    (assert subform)
    (excl::delete-breakpoint (subform.bp-func subform) (subform.bp-pc subform) :verbose)
    t))

(defun remove-breakpoint-for-emacs (&rest args)
  (apply #'remove-breakpoint args)
  t)

(defun message-for-emacs (string &rest args)
  (flet ((escape-amp-and-quote (string)
           (coerce (loop for ch across string
                       if (char= ch #\%)
                       collect #\% and collect #\%
                       else if (char= ch #\")
                       collect #\\ and collect #\"
                       else
                       collect ch)
                   'string)))
    (let* ((save-msg (escape-amp-and-quote (apply #'format nil string args)))
           (cmd (concatenate 'string "(message\" " save-msg "\")")))
      (lep::eval-in-emacs cmd)))
  t)


;;; Printing Python stack trace
;;; Based on Slime's swank-allegro.lisp

(defun find-topframe ()
  (let ((skip-frames 0))
    (do ((f (excl::int-newest-frame) (next-frame f))
         (i 0 (1+ i)))
        ((= i skip-frames) f))))

(defun next-frame (frame)
  (let ((next (excl::int-next-older-frame frame)))
    (cond ((not next) nil)
          ((debugger:frame-visible-p next) next)
          (t (next-frame next)))))

(defun print-frame (frame stream)
  (debugger:output-frame stream frame :moderate))

(defun print-frame-locals (frame)
  (loop with some = nil
      for i from 0 below (debugger:frame-number-vars frame)
      for name = (debugger:frame-var-name frame i)
      when (and (symbolp name)
                (eq (symbol-package name) #.(find-package :clpython.user)))
      do (unless some
           (setf some t)
           (format t "  Local variables:~%"))
         (format t "    ~A = ~A~%"
                 (debugger:frame-var-name frame i)
                 #+(or)(debugger:frame-var-type frame i)
                 (debugger:frame-var-value frame i))))

(defun print-frame-python?-source (frame)
  (flet ((found-python-expr (source ast)
           (format t "~&~%  ~A" source)
           (format t "~&    ~A~%" (if (stringp ast) ast (py-pprint ast)))
           (return-from print-frame-python?-source)))
    (multiple-value-bind (func pc)
        (frame-get-func-pc frame)
      #+(or)(format t "PFP?S: fr=~A func=~A pc=~A~%" frame func pc)
      (when (functionp func)
        (whereas ((subform (subform-for-func-pc func pc :exact-start-pc nil))
                  (msi (subform.msi subform)))
          ;; Found Python source code
          (found-python-expr (format nil "File ~A, line ~A:~%"
                                     (msi.original-filename msi)
                                     (subform.source-line-offset subform)
                                     #+(or)(subform.start-char subform))
                             (subform.form subform)))
        
        (whereas ((bp (let ((*inside-python-set-breakpoint* t)) ;; consistency check hack
                        (ldb-python.find-pc func pc nil)))
                  (src (excl::breakpoint-source bp))
                  (ign1 (not (eq src :no-source)))
                  (ign2 (and (listp src)
                             (symbolp (car src))
                             (clpython.parser::stmt-or-expr-node-p (car src)))))
          (found-python-expr "?" src))
        
        ;; XXX Ugly but useful special-cases
        (cond ((eq func #'run-python-ast) ;; interactive input (repl)
               (loop for i from 0 below (debugger:frame-number-vars frame)
                   when (eq (debugger:frame-var-name frame i) 'ast)
                   do (found-python-expr "Interactive input:" (debugger:frame-var-value frame i))))
              
              ((eq func #'(method py-call (function))) ;; calling function without source info (e.g. entered in repl)
               (loop with func
                   for i from 0 below (debugger:frame-number-vars frame)
                   when (and (eq (debugger:frame-var-name frame i) 'f)
                             (setf func (debugger:frame-var-value frame i))
                             (typep func 'py-function)
                             (setf func (py-function-lambda func))
                             (progn (when (excl::closurep func)
                                      (setq func (excl::cl_shared func)))
                                    t)
                             (not (excl::ldb-lisp-source-p func)))
                   do (found-python-expr (format nil "Interpreted function ~A:" func)
                                         "(expression unavailable, TODO)"))))))))

(defun frame-get-func-pc (frame)
  ;; Result of debugger:frame-expression is a list, not the function itself.
  (multiple-value-bind (class func nargs bnp pc)
      (excl::fd-analyze frame)
    (declare (ignorable class nargs bnp))
    (values func pc)))

(defgeneric print-python-stacktrace ()
  (:method ()
           (format t "~&Python backtrace:")
           (loop for frame = (find-topframe) then (next-frame frame)
               for i from 0
               while frame
               do #+(or)(format t "~&======================================================~%")
                  #+(or)(format t "~&FRAME ~A:~%" i)
                  #+(or)(print-frame frame t)
                  (print-frame-python?-source frame)
                  (print-frame-locals frame))))

(setf (top-level:alias "p") #'print-python-stacktrace)


;;; Utilities

(defun coerce-to-function (function)
  "Coerce symbols and closures to the corresponding function."
  (when (symbolp function)
    (setf function (symbol-function function)))
  (when (excl::closurep function)
    (setq function (excl::cl_shared function)))
  (check-type function function)
  function)

(defun coerce-to-breakpoint-pc (pc)
  (etypecase pc
    (integer pc)
    (excl::breakpoint (excl::breakpoint-pc pc))))


;;; Test
#+(or)
(defun source-test ()
  (progn (clpython::%reset-import-state)
         (clrhash (clpython::find-symbol-value '#:|modules| :clpython.module.sys))
         (clpython:run "import foo"))
  (set-breakpoint :file "c:/willem/lisp/cl-python/core/foo.py" :character 14))

#|| ;; to test:
=== foo.py ===
def foo(x):
  a = 2
  print a + x

def zut(y):
  zutx = foo(1 + y)
  print zutx

foo(3)
===

(aload :clpython)
:pa clpython
:cd cl-python/core
(repl)
import foo
 (clpython::set-breakpoint :file "c:/willem/lisp/cl-python/core/foo.py" :character 14)
foo.foo(3)
||#
