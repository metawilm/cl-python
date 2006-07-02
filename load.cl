;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :user)

;; Ensure read-time conditionals are not compiled away.
(eval-when (compile)
  (error "This file should be loaded interpreted, not compiled."))

#-(and allegro-version>= (version>= 7 0))
(cerror "continue anyway"
	"CLPython requires Allegro Common Lisp 8.0.")

#+(and allegro-version>= (version>= 7 0) (not (version>= 8 0)))
(warn "CLPython has been tested in Allegro Common Lisp 8.0, ~
       not in 7.0. (Nevertheless it might still work in 7.0.)")

#+(and allegro-version>= (version>= 7 0))
(let ((mode excl::*current-case-mode*))
  (ecase mode
    (:case-sensitive-lower) ;; Modern, ok
    
    ((:case-insensitive-upper :case-insensitive-lower)
     (cerror "continue anyway"
	     "CLPython should be run in Allegro CL `Modern' mode; ~
              it will not run correctly in ~A mode (yet)."
	   (ecase mode
	     (:case-insensitive-upper "`ANSI'")
	     (:case-insensitive-lower))))))


(load "defsys")

(compy)
(loadpy)
