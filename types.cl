(in-package :python)

basestring
bool
buffer

#+(or) ;; type
(defun py-classmethod (func)
  "Return a class method for function FUNC. ~@
   A class method receives the class as implicit first argument, just ~@
   like an instance method receives the instance."
  (declare (ignore func))
  (error "todo: classmethod"))


#+(or) ;; str is a type
(defun py-str (x)
  (handler-case (__str__ x)
    (%magic-method-missing% ()
      (handler-case (__repr__ x)
	(%magic-method-missing% ()
	  (error "There should always be some __repr__ that's applicable?!"))))))

#+(or) ;; not needed: class constructor already provides this functionality.
(defun complex (&optional real imag)
  (make-complex real imag))

#+(or) ;; not needed: class constructor already provides this functionality.
(defun dict (mapping-or-sequence)
  (make-dict mapping-or-sequence))

#+(or) ;; XXX this is a type!
(defun enumerate (x)
  "Return an `enumerate object' for X. ~@
   Successively calling method `next' will return (0, x[0]), (1, x[1]), (2, x[2]), ..."
  (declare (ignore x))
  (error "todo: enumerate"))

#+(or) ;; not needed: class constructor already provides this functionality.
(defun file (filename &optional (mode "r") bufsize)
  "Returns a new object of type `file'. ~@
   MODE is a string that could contain any of the following:
      'r' [reading], 'w' [truncating and writing], 'a' [appending]
      'r+', 'w+', 'a+' open for updating (XXX which means: appending ?)
      'b' for binary files
      'U' means all newlines (\r, \n, \r\n) are converted to \n and
          all encountred newline types are stored in attribute `newlines'
          which has as value one of None, '\r', '\n', '\r\n', or a tuple
          containing two or more of the three newline types as string.
   BUFSIZE: 0 = unbuffered, 1 = line buffered,
            n > 1 = buffer of approx size n
            n < 1 = system default"
  (declare (ignore filename mode bufsize))
  (error "todo: file"))

#+(or) ;; not needed: class constructor already provides this functionality.
(defun float (&optional (val 0.0))
  (make-float val))
