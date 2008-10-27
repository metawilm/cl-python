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

;;; Raising exceptions

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *exceptions-are-python-objects*
    #+allegro t
    #+lispworks t
    #+cmu nil    ;; CMUCL does not allow arbitrary meta/superclasses in conditions
    #-(or allegro lispworks cmu) nil)

(if *exceptions-are-python-objects*
    (pushnew :clpython-exceptions-are-python-objects *features*)
  (setf *features* (remove :clpython-exceptions-are-python-objects *features*)))
)


(define-compiler-macro py-call (&whole whole prim &rest args)
  (with-perhaps-matching (prim ([attributeref-expr] ?x ([identifier-expr] ?attr-sym)))
    ;; Optimize "obj.attr(..args..)" = (py-call (py-attr obj attr) ..args..)
    ;; so the allocation of a bound method object is skipped.
    (let ((res `(let* ((.x ,?x)
                       (.val (class.attr-no-magic (py-class-of .x) ',?attr-sym)))
                  (if (and (functionp .val) (not (instance.attr-no-magic .x ',?attr-sym)))
                      (funcall .val .x ,@args)
                    (locally (declare (notinline py-call))
                      (py-call (attr .x ',?attr-sym) ,@args))))))
      #+(or)(warn "py-call cm res: ~A" res)
      (return-from py-call res)))
  whole)

#||
         #+(or)
         `(multiple-value-bind (.a .b .c)
              (py-attr ,x ,attr :bind-class-attr nil)
            (if (eq .a :class-attr)
                (progn #+(or)(assert (functionp .b))
                       #+(or)(warn "saving bound method ~A ~A" .b .c)
                       (funcall .b .c ,@args))
              (locally (declare (notinline py-call))
                (py-call .a ,@args))))
         
         #+(or)
         ((and (listp prim)
               (eq (first prim) 'bind-val)
               (= (length prim) 4))
          ;; Optimize  (py-call (bind-val val x x.class) ..args..)
          ;; where val is a function and x an instance, so it doesn't allocate
          ;; bound method.
          #+(or)(warn "inlining (py-call (bind-val ..) ..) ~A" whole)
          (destructuring-bind (val x x.class) (cdr prim)
            `(let ((.val ,val)
                   (.x ,x)
                   (.x.class ,x.class))
               (if (functionp .val) ;; XXX Maybe check for user-defined subclasses of function?
                   (progn #+(or)(warn "saving binding ~A" ',whole)
                          (funcall .val .x ,@args))
                 (py-call (bind-val .val .x .x.class) ,@args)))))

         #+(or)
         (t 
          ;; Optimize case where PRIM is a function.
          (let ((a (gensym "args"))
                (p (gensym "prim")))
            (let ((res `(locally (declare (optimize (speed 3)))
                          (let ((,p ,prim))
                            (with-stack-list (,a ,@args)
                              (if (functionp ,p)
                                  (progn #+(or)(warn "inlined py-call <function> ~A" ,p)
                                         (apply 'funcall (the function ,p) ,a))
                                (apply 'py-call ,p ,a)))))))
              #+(or)(format t ";; py-call => ~A" res)
              res))))))
||#