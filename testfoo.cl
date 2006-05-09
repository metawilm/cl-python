(in-lisp-py-syntax :module "foo")

;; Simple Python function
def f(x, y=1):
  return x+y

;; Lisp function accessing Python function
(defun lispwow ()
  (list (f 3)    ;; regular Lisp syntax
	{f(3)}   ;; In Lisp code, put {...} around Python expressions
	(f :x 1 :y 4)    ;; Supplying keyword arguments the Lisp way
	{f(x=1, y=4)} )) ;; Supplying keyword arguments the Python way

;; Equivalent Python function
def pywow():
  [ $(f 3)$, f(3), $(f :x 1 :y 4)$, f(x=1, y=4) ]


print pywow() + lispwow()

(format t (concatenate 'vector (pywow) (lispwow)))

;; Python and Lisp like to work together:

def pyfact(x):
  if x <= 1:
    return 1
  else:
    return x * lispfact(x-1) # In Python code, undefined functions are looked up in the Lisp world

(defun lispfact (x)
  (if (<= x 1)
      1
    (* x (pyfact (1- x))))) ;; In Lisp code, undefined functions are looked up in the Python world

print pyfact(6), $(lispfact 6)$, $(= {pyfact(25)} (lispfact 25))$
