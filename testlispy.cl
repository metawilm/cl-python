(in-lisp-py-syntax)

(format t "Lisp says: hello!~%")
print "Python says: hello!"

;; Calling a Lisp function from Python or Lisp
(defun lispa () 42)

(format t "(lispa) = ~A~%" (lispa))
print "lispa() = %s" % lispa()

;; Calling a Python function (taking optional second argument)
;; from Python or Lisp.

def f(x, y=1): return x+y

print "f(3) = %d" % f(3)
(format t "(f 3) = ~D~%" (f 3))

# Python-style "#..." comments are supported, too.
# Lisp function calling the Python function f
(defun lispwow ()
  (coerce (list (f 3)         ;; regular Lisp function call syntax
		{f(3)}        ;; In Lisp code, put {...} around Python expressions
		{f(x=1, y=4)} ;; Supplying keyword arguments the Python way
		(f :x 1 :y 4) ;; Supplying keyword arguments the Lisp way
		)
	  'vector))

(format t "Lispwow = ~A~%" (lispwow))
print "Lispwow() = %s" % lispwow()

;; Equivalent Python function
def pywow():
  return [ $(f 3)$, f(3), $(f :x 1 :y 4)$, f(x=1, y=4) ]

print "pywow = %s" % pywow()
(format t "(pywow) = ~A~%" (pywow))


;; Python lists are Lisp vectors

(format t "Concatenation (L) = ~A~%" (concatenate 'vector (pywow) (lispwow)))
print "Concatenation (P) = %s" % (pywow() + lispwow())


;; Python and Lisp functions can work together in a straightforward way

def pyfact(x):
  if x <= 1:
    return 1
  else:
    return x * lispfact(x-1) # In Python code, undefined functions are looked up in the Lisp world

(defun lispfact (x)
  (if (<= x 1)
      1
    (* x (pyfact (1- x))))) ;; In Lisp code, undefined functions are looked up in the Python world


print pyfact(6), $(lispfact 6)$

print pyfact(25) == lispfact(25)
(format t "~D~%" (if (= {pyfact(25)} (lispfact 25)) 1 0))
