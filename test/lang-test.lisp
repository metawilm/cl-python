;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.TEST; Readtable: PY-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Python language semantics test

(in-package :clpython.test)
(in-syntax *ast-user-readtable*)

(defun run-lang-test ()
  (with-subtest (:name "CLPython-Lang")
    (dolist (node '(:assert-stmt :assign-stmt :attributeref-expr :augassign-stmt
		    :backticks-expr :binary-expr :binary-lazy-expr :break-stmt
		    :call-expr :classdef-stmt :comparison-expr :continue-stmt
		    :del-stmt :dict-expr :exec-stmt :for-in-stmt :funcdef-stmt
		    :generator-expr :global-stmt :identifier-expr :if-expr :if-stmt
		    :import-stmt :import-from-stmt :lambda-expr :listcompr-expr
                    :list-expr :module-stmt :print-stmt :return-stmt :slice-expr
                    :subscription-expr :suite-stmt :return-stmt :raise-stmt
                    :try-except-stmt :try-finally-stmt :tuple-expr :unary-expr
                    :while-stmt :with-stmt :yield-expr :yield-stmt
                    :attribute-semantics :number-method-lookups :getitem-methods))
      (test-lang node))))

(defmacro with-all-compiler-variants-tried (&body body)
  (let ((g1 (gensym))
        (g2 (gensym)))
    `(dolist (,g1 (clpython.util::all-use-environment-accessor-values))
       (dolist (,g2 '(t nil))
         (let ((clpython.util::*use-environment-acccessors* ,g1)
               (clpython:*compile-python-ast-before-running* ,g2))
           ,@body)))))

(defmacro run-error (string condtype &rest options)
  `(with-all-compiler-variants-tried
       (test-error (run ,string) :condition-type ',condtype ,@options)))

(defmacro run-no-error (string &rest options)
  `(with-all-compiler-variants-tried
       (test-no-error (run ,string) ,@options)))

(defmacro run-test (val string &rest options)
  `(with-all-compiler-variants-tried
       (test ,val (run ,string) ,@options)))


(defgeneric test-lang (kind))

(defmethod test-lang :around (kind)
  (with-subtest (:name (format nil "CLPython-Lang-~A" kind))
    (assert (next-method-p))
    (call-next-method)))

(defmethod test-lang ((kind (eql :assert-stmt)))
  (declare (ignorable kind))
  (run-error        "assert 0" {AssertionError} )
  (run-no-error     "assert 1")
  (run-error "assert \"\"" {AssertionError})
  (run-no-error     "assert \"s\"")
  (run-error "assert []" {AssertionError})
  (run-no-error     "assert [1,2]")
  (run-no-error     "assert True")
  (run-error "assert not True" {AssertionError})
  (run-no-error     "assert not not True")
  (run-no-error     "assert not False")
  (run-no-error     "assert 1 < 2")
  (run-no-error     "assert not 1 > 2")
  
  (multiple-value-bind (x err) 
      (ignore-errors (run "assert 0, 'abc'"))
    (test-false x)
    (test-true err)
    (test-true (string= (pop (exception-args err)) "abc")
               :fail-info (format nil "~A = ~A" 'clpython:*exceptions-are-python-objects*
                                  clpython:*exceptions-are-python-objects*))))

(defmethod test-lang ((kind (eql :assign-stmt)))
  (declare (ignorable kind))
  (run-test 3 "a = 3; a")
  (run-test 3 "a, = 3,; a")
  (run-test 3 "[a] = [3]; a")
  (run-test 3 "(a,) = (3,); a")
  (run-test 3 "a,b = 3,4; a")
  (run-test 3 "a,b = [3,4]; a")
  (run-error "a,b = 3" {TypeError} :fail-info "Iteration over non-sequence.")
  (run-error "a,b = 3,4,5" {ValueError})
  (run-error "a,b = [3,4,5]" {ValueError})
  (run-no-error "def f(): pass")
  (run-no-error "
g = 3
def f():
  global g
  g = 2
f()
assert g == 2"))

(defmethod test-lang ((kind (eql :attributeref-expr)))
  (declare (ignorable kind))
  (run-no-error "class C: pass
x = C()
C.a = 3
assert (x.a == 3)
x.a = 4
assert (x.a == 4)
del x.a
assert (x.a == 3)
del C.a
assert not hasattr(C, 'a')"))

(defmethod test-lang ((kind (eql :augassign-stmt)))
  (declare (ignorable kind))
  (run-no-error "x = 3; x+= 2; assert x == 5")
  (run-no-error "x = 3; x*= 2; assert x == 6")
  (run-no-error "x = [1,2]; x[1] -= 2; assert x[1] == 0")
  (run-error    "x,y += 3" {SyntaxError})
  (run-no-error "x = 3; x **= 3; assert x == 27"))

(defmethod test-lang ((kind (eql :backticks-expr)))
  (declare (ignorable kind))
  (run-no-error "x = `3`; assert x == '3'")
  (run-no-error "x = `(1,3)`; assert x == '(1, 3)'")
  (run-no-error "
class C:
  def __repr__(self): return 'r'
  def __str__(self): return 'str'
x = C()
assert `x` == 'r'"))

(defmethod test-lang ((kind (eql :binary-expr)))
  (declare (ignorable kind))
  (run-no-error "assert 1 + 2 == 3")
  (run-no-error "assert 1 - 2 * 3 == -5")
  (run-no-error "assert 1 ^ 3 == 2")
  (run-no-error "assert 1 | 2 == 3")
  (run-no-error "assert 4 * 'ax' == 'axaxaxax'")
  (run-no-error "assert -4 * 'ax' == ''")
  (run-no-error "
# https://codespeak.net/issue/pypy-dev/issue412
class Base(object):
    '''analogous to sympy.core.basic.Basic'''
    def __init__(self, value):
        self.value = value
    def __mul__(self, other):
        return self.value * other.value
    def __rmul__(self, other):
        return other.value * self.value

class Doubler(Base):
    '''analogous to sympy.core.numbers.Rational'''
    def __mul__(self, other):
        return 2 * (self.value * other.value)

class AnotherDoubler(Doubler):
    '''analogous to sympy.core.numbers.Half'''

a = Doubler(2)
b = AnotherDoubler(3)
assert a * b == 12"
                :fail-info "Wrong lookup logic for __r...__ methods"
                )
  (run-no-error "[1,2,3] * 2 == [1,2,3,1,2,3]")
  (run-no-error "(1,2,3) * 2 == (1,2,3,1,2,3)"))

(defmethod test-lang ((kind (eql :binary-lazy-expr)))
  (declare (ignorable kind))
  (run-no-error "assert not (0 or 0)")
  (run-no-error "assert not (0 and 0)")
  (run-no-error "1 or 3 / 0")
  (run-no-error "0 and 3/0")
  (run-no-error "assert ([] or '') == ''")
  (run-no-error "assert (1 or '') == 1")
  (run-no-error "assert ('' or 1) == 1")
  (run-no-error "assert (1 or 2) == 1")
  (run-no-error "assert (1 and 2) == 2")
  (run-no-error "assert (0 and 2) == 0")
  (run-no-error "assert (1 and []) == []"))

(defmethod test-lang ((kind (eql :break-stmt)))
  (declare (ignorable kind))
  (run-error "break" {SyntaxError})
  (run-no-error "
for i in [1,2]:
  break
assert i == 1"))

(defmethod test-lang ((kind (eql :call-expr)))
  (declare (ignorable kind))
  (run-no-error "def f(x,y,z=3,*arg,**kw): return x,y,z,arg,kw
assert (1,2,3,(),{}) == f(1,2)")
  (run-no-error "
class C:
  def __call__(self, *args):
    return args
x = C()
x(1,2,3) == (1,2,3)"))

(defmethod test-lang ((kind (eql :classdef-stmt)))
  (declare (ignorable kind))
  (run-no-error "
class C:
  def m(self): return 'C.m'
assert C().m() == 'C.m'
assert C.__mro__ == (C, object)")
  (run-no-error "
class C: pass
class D(C): pass
assert D.__mro__ == (D, C, object)")
  (run-no-error "
class C:
  x = 3          # this variable X can not be closed over by methods
  def g(self):
    return x   # so this should give an error

try:
  print C().g()
  assert False
except NameError:
  'ok'
")
  (run-no-error "
def f():
  class C:
    x = 3          # this variable X can not be closed over by methods
    def g(self):
	return x   # so this should give an error
  return C().g

try:
  print f()()
  assert False
except NameError:
  'ok'
")
  (run-no-error "
class M(type): pass

class C:
  __metaclass__ = M

assert type.__class__ == type
assert M.__class__ == type
assert M.__class__.__class__ == type
assert C.__class__ == M")
    (let ((clpython::*mro-filter-implementation-classes* t))
      (run-no-error "
class C(int):
  pass
x = C()
assert x.__class__ == C
assert C.__class__ == type
assert C.__mro__ == (C, int, object)"))
    (run-no-error "
class C( type(1+2)): pass
assert C() == 0")
    (run-no-error "
x = []
class Meta(type):
  def __init__(cls,*args,**kw):
    Meta   ## check no name error
    x.append(cls)
    type.__init__(cls, *args, **kw)

class C():
  __metaclass__ = Meta

assert len(x) == 1
assert x[0] == C")
    (run-no-error "
class C:
  a = 3
  assert locals()['a'] == 3"))

(defmethod test-lang ((kind (eql :comparison-expr)))
  (declare (ignorable kind))
  ;; Ensure py-list.__eq__ can handle non-lists, etc.
  (run-no-error "assert [] != ()")
  (run-no-error "assert () != []")
  (run-no-error "assert [] == []")
  (run-no-error "assert [] != {}")
  (run-no-error "assert {} != []")
  (run-no-error "assert [] != None")
  (run-no-error "assert '' != None")
  (run-no-error "assert [] != 3")
  (run-no-error "assert 3 != None")
  (run-no-error "assert (1 < 2 < 3)")
  (run-no-error "assert 1 < 2 < 3")
  (run-no-error "assert (3 > 2 > 1)")
  (run-no-error "assert 3 >=3 > 2 > 1 < 2 <= 2 < 3 > 2 >= 1")
  (run-no-error "
for x in range(5):
  for y in range(5):
    for z in range(5):
      assert (x == y == z) == ((x == y) and (y == z)) == (x == y and y == z)
      assert (x <= y <= z) == ((x <= y) and (y <= z)) == (x <= y and y <= z)
      assert (x < y > z) == ((x < y) and (y > z)) == (x < y and y > z)")
  (run-no-error "
for x in range(5):
  for y in range(5):
    for z in range(5):
      if x < y:
        assert ((x < y) < z) == (1 < z)
      else:
        assert ((x < y) < z) == (0 < z)")
  (run-no-error "
le = 0
class C:
  def __le__(self, other):
    global le
    le += 1
    return []
x, y = C(), C()
assert (x <= y) == []
assert le > 0" :known-failure t :fail-info "<= should use __le__, not __cmp__."))

(defmethod test-lang ((kind (eql :continue-stmt)))
  (declare (ignorable kind))
  (run-error "break" {SyntaxError})
  (run-no-error "for i in []: continue")
  (run-no-error "
for i in [1]: continue
assert i == 1")
  (run-no-error "
for i in [1,2,3]:
  continue
  1 / 0")
  (run-no-error "
sum = 0
for i in [0,1,2,3]:
  if i == 0:
    continue
  sum += i
  continue
  i / 0
assert sum == 1 + 2 + 3
assert i == 3"))

(defmethod test-lang ((kind (eql :del-stmt)))
  (declare (ignorable kind))
  (run-error "del x" {NameError})
  (run-no-error "x = 3; del x")
  (run-error "x = 3; del x; x" {NameError})
  (run-no-error "x,y,z = 3,4,5; del x,y; z")
  (run-error "x,y,z = 3,4,5; del x,y; y" {NameError})
  (run-no-error "
x,y,z = 3,4,5
del x,y,z
try:
  z
  assert False
except NameError:
  pass")
  (run-no-error "
def f():
  global x,y,z
  del x,y,z" :fail-info "Should not warn about unused local vars.")
  (run-no-error "
x = [1,2,3,4,5]
del x[-2:]
assert x == [1,2,3]
")
  (run-no-error "
x = range(10)
del x[1:8:2]
assert x == [0, 2, 4, 6, 8, 9]")
  (run-no-error "
x = range(10)
del x[-1:-6:-2]
assert x == [0, 1, 2, 3, 4, 6, 8]"))

(defmethod test-lang ((kind (eql :dict-expr)))
  (declare (ignorable kind))
  (run-no-error "{}")
  (run-no-error "{1: 3}")
  (run-no-error "{1+2: 3+4}")
  (run-no-error "assert {1: 3}[1] == 3")
  (run-no-error "assert {1: 3, 2: 4}[1] == 3")
  (run-no-error "
d = {}
d[3] = 1
assert d[3] == 1
del d[3]
assert d == {}
d[3] = 2
assert d[3] == 2")
  (run-no-error "
# make sure user-defined subclasses of string work okay as key
class C(str): pass
x = C('a')
d = {}
d[x] = 3
assert d['a'] == 3
y = C('b')
assert d.get(y) == None
d[y] = 42
assert d[y] == 42
assert d['b'] == 42")
  (run-no-error "assert {None: 3}[None] == 3"))

(defmethod test-lang ((kind (eql :exec-stmt)))
  (declare (ignorable kind))
  (run-no-error "
def f():
  x = (1,2)
  exec 'print x'" :fail-info "Make sure tuple `(1 2) is quoted in code generated for `exec'")
  (run-no-error "exec 'assert x == 3' in {'x': 3}")
  (run-no-error "
glo = {'x': 3}
loc = {'x': 4}
exec 'assert x == 4' in glo, loc" :fail-info "Locals higher priority than globals.")
  (run-no-error "
exec \"
try:
  1/0
  assert 0
except ZeroDivisionError:
  'ok'\"")
  (run-no-error "
x = 3
exec 'assert x == 3'")
  (run-no-error "
x = 3
def f():
  x = 4
  exec 'assert x == 4'
f()")
  (run-no-error "
x = 3
def f():
  x = 4
  def g():
    exec 'assert x == 3'
  g()
f()")
  (run-no-error "# http://mail.python.org/pipermail/python-dev/2008-October/082951.html
class C:
  a = 3
  assert locals().has_key('a')
  exec 'b = 4'
  assert locals().has_key('b')
assert C.a == 3
assert C.b == 4")
  (run-no-error "
exec 'def f(): return 3'
assert f() == 3")
  (run-no-error "# not quite exec-stmt, but implementation of eval is much the same
a = eval('2+3')
assert a == 5"))

(defmethod test-lang ((kind (eql :for-in-stmt)))
  (declare (ignorable kind))
  (run-no-error "for i in []: 1/0")
  (run-no-error "for i in '': 1/0")
  (run-no-error "
for k in {1: 3}:
  x = k
assert k == 1")
  (run-no-error "
for x in []:
  pass
else:
  x = 3
assert x == 3")
  (run-no-error "
for x in [1]:
  break
else:
  x = 3
assert x == 1")
  (run-no-error "
def f():
  for x in [1]:
    break
  else:
    x = 3
  assert x == 1
  yield x
g = f()
assert g.next() == 1")
  (run-no-error "
def f():
  for x in []:
    pass
  else:
    x = 3
  yield x
g = f()
assert g.next() == 3")
  (run-no-error "
def f():
  yield 1
  yield 2
  raise StopIteration('stop')
res = [x for x in f()]
assert res == [1,2]"))

(defmethod test-lang ((kind (eql :funcdef-stmt)))
  (declare (ignorable kind))
  ;; *-arg, **-arg
  (run-no-error "
def f(a, b, c=13, d=14, *e, **f): return [a,b,c,d,e,f]
x = f(1,2,3,4,5,6)
assert x == [1,2,3,4,(5,6),{}], 'x = %s' % x"
)
  (run-no-error "
def f(a, b, c=13, d=14, *e, **f): return [a,b,c,d,e,f]
x = f(a=1,b=2,c=3,d=4,e=5,f=6)
assert x == [1,2,3,4,(),{'e': 5, 'f': 6}], 'x = %s' % x"
                )
  (run-no-error "
def f(): return f
f()
assert f() == f")
  (run-no-error "
def f((x,y)=[1,2]): return x+y
assert f() == 3
assert f((1,2)) == 3
x = (1,2)
assert f(x) == 3")
  (run-no-error "
def f(x, **kw): return x, kw
assert f(1,a=3) == (1, {'a': 3})")
  ;; todo: check evaluation order of decorators vs. keyword argument default values.
  (run-no-error "
def f():
  pass
f.__name__ = 'g'
assert f.__name__ == 'g'"))


(defmethod test-lang ((kind (eql :generator-expr)))
  (declare (ignorable kind))
  )

(defmethod test-lang ((kind (eql :global-stmt)))
  (declare (ignorable kind))
  (test-some-warning (run "global x")) ;; useless at toplevel
  (run-error "
def f():
  x = 3
  global x" {SyntaxError}) ;; global decl must be before first usage
  (run-error "def f(x): global x" {SyntaxError})
  (run-no-error "
def f(y):
  global x
  x = y
f(3)
assert x == 3")
  (run-no-error "
def f():
  global x
  def g(y):
    x = y
  return g
f()(4)
assert x == 4" :fail-info "Global decl is also valid for nested functions")
  (test-some-warning (run "
global y  # bogus declaration; check it does not leak into f
def f(a):
  y = a
f(3)
try:
  print y
  assert False
except NameError:
  pass"
  ))
  (run-no-error "
def f():
  x = 'fl'
  class C:
    global x  # does not hold for method m 
    y = x
    def m(self):
      return x
  return C().m()

x = 'gl'

assert f() == 'fl'" :fail-info "`global' in a class def must not leak into the methods within")
  (run-no-error "
a = 'global'
  
def f():
    a = 'af'
    def g():
      global a
      def h():
        assert a == 'global'
      return h()
    return g()

f()")
  (run-no-error "
x = 0

def f():
  x = 'local'
  def g():
    global x
    print x
    x += 1
  g()

f()

assert x == 1"))

(defmethod test-lang ((kind (eql :identifier-expr)))
  (declare (ignorable kind))
  (run-no-error "
def f():
   x = 3
   class C:
     assert x == 3")
  (run-no-error "
def f():
  x = 1
  def g():
    y = 2
    def h():
      return (x,y)
    return h
  return g

assert f()()() == (1,2)")
  (run-no-error "
ok = ''
class C:
  x = 1
  class D:
    y = 2
    class E:
      global ok
      try:
        x
      except NameError:
        ok += 'x'
      try:
        y
      except NameError:
        ok += 'y'

assert ok == 'xy'"))

(defmethod test-lang ((kind (eql :if-expr)))
  (declare (ignorable kind))
  (run-no-error "x = (1 if True else 0); assert x == 1")
  (run-no-error "x = 1 if True else 0; assert x == 1")
  (run-no-error "x = 1 if False else 0; assert x == 0")
  (progn
    ;; Grammar test cases from PEP http://www.python.org/dev/peps/pep-0308/.
    (run-no-error "
[a, b] = [f for f in (1, lambda x: x if x >= 0 else -1)]
assert a == 1
assert b(1) == 1
assert b(-2) == -1")
    (run-no-error "
[a, b] = [f for f in 1, lambda x: (x if x >= 0 else -1)]
assert a == 1
assert b(1) == 1
assert b(-2) == -1")
    (run-no-error "
[a, b] = [f for f in 1, (lambda x: x if x >= 0 else -1)]
assert a == 1
assert b(1) == 1
assert b(-2) == -1")
    (run-error "[f for f in 1, lambda x: x if x >= 0 else -1]" {SyntaxError})))

(defmethod test-lang ((kind (eql :if-stmt)))
  (declare (ignorable kind))
  (run-no-error "def f(): pass
if f(): pass" :fail-info "Functions inherit __nonzero__ from object."))

(defmethod test-lang ((kind (eql :import-stmt)))
  (declare (ignorable kind))
  #.(progn (unless (string= (pathname-name (or *compile-file-truename*
                                               *load-truename*))
                            "lang-test")
             (error "Compile file lang-test.lisp using compile-file (or asdf), not using temp file, ~
                     otherwise import paths are incorrect: ~A." *compile-file-truename*))
           nil)
  (let* ((new-dir #.(directory-namestring (clpython.util:derive-pathname
                                           (or *compile-file-truename* *load-truename*)
                                           :type nil :name nil)))
         (prefix (concatenate 'string "
import sys
sys.path.append('" (coerce (loop for c across new-dir if (char= c #\\) collect #\\ and collect #\\ else collect c) 'string) "data')" (string #\Newline))))
    (format t "prefix: ~S~%" prefix)
    
    (clpython::%reset-import-state)
    (run-no-error "import sys
assert sys" :fail-info "Should work in both ANSI and Modern mode.")
    
    (clpython::%reset-import-state)
    ;; run compilation outside run-no-error, to prevent allegro style warning from failing the test
    (clpython:run (concatenate 'string prefix "
print 'import'
import bar
assert bar.i
print 'reload'
reload(bar)
print 'del bar.i'
del bar.i"))
    
    (clpython::%reset-import-state)
    ;; When importing a module, the conditions of type clpython::module-import-pre
    ;; make run-no-error fail. Therefore rely on statements returning nil (?!) by using test-false.
    (test-true (prog1 t
                 (run `,(concatenate 'string prefix "
import bar
for i in xrange(3):
  print 'bar.i=', bar.i, 'i=', i
  assert bar.i == i+1
  reload(bar)"))))
    
    (clpython::%reset-import-state)
    ;; run outside run-no-error
    (clpython:run (concatenate 'string prefix "
print '4a'
import zut.bla
print '4b'"))
  
    (clpython::%reset-import-state)
    (test-true (prog1 t
                 (run `,(concatenate 'string prefix "
print '5a'
for i in xrange(3):
  import zut.bla
  assert zut.bla.x
  print '5b'"))))))

(defmethod test-lang ((kind (eql :import-from-stmt)))
  (declare (ignorable kind))
  (run-no-error "from sys import path; path.append('/foo'); del path[-1]"))

(defmethod test-lang ((kind (eql :lambda-expr)))
  (declare (ignorable kind))
  (run-no-error "lambda: None")
  (run-no-error "lambda: 3*x")
  (run-error "(lambda: 3*x)()" {NameError})
  (run-no-error "assert (lambda x: x)(0) == 0")
  (run-no-error "
f = lambda x, y=3: x+y
assert f(1) == 4
assert f(1,2) == 3")
  (run-no-error "
f = lambda x, y=lambda: 42: x + y()
assert f(1) == 1 + 42")
  (run-no-error "
f = lambda x, y=lambda: 42: x + y()
assert f(1, lambda: 2) == 1 + 2")
  (run-no-error "assert (lambda x, y: x+y)(x=3, y=4) == 7")
  (run-no-error "print (lambda:42).func_globals" :known-failure t))

(defmethod test-lang ((kind (eql :listcompr-expr)))
  (declare (ignorable kind))
  (run-no-error "assert [x for x in [1,2] if x > 1] == [2]")
  (run-no-error "assert [(x,y) for x in [1,2] for y in [x]] == [(1,1), (2,2)]"))

(defmethod test-lang ((kind (eql :list-expr)))
  (declare (ignorable kind))
  (run-no-error "
x = []
y = x
x += [2,3]
assert x == [2,3]
assert y == [2,3]")
  (run-no-error "
x = []
x += '12'
assert x == ['1', '2']"))

(defmethod test-lang ((kind (eql :module-stmt)))
  (declare (ignorable kind))
  )

(defmethod test-lang ((kind (eql :print-stmt)))
  (declare (ignorable kind))
  )

(defmethod test-lang ((kind (eql :return-stmt)))
  (declare (ignorable kind))
  (run-error "
def f():
 class C:
  return
f()" {SyntaxError} :fail-info "return outside function"))

(defmethod test-lang ((kind (eql :slice-expr)))
  (declare (ignorable kind))
  (run-no-error "
x = range(10)
x[1:10:2] = [0,0,0,0,0]
assert x == [0, 0, 2, 0, 4, 0, 6, 0, 8, 0]")
  (run-no-error "
x = range(10)
x[1:3] = []
assert x == [0, 3, 4, 5, 6, 7, 8, 9]")
  (run-no-error "
x = [1,2]
x[:0] = 'abc'
assert x == ['a', 'b', 'c', 1, 2]"))

(defmethod test-lang ((kind (eql :subscription-expr)))
  (declare (ignorable kind)))

(defmethod test-lang ((kind (eql :suite-stmt)))
  (declare (ignorable kind)))

(defmethod test-lang ((kind (eql :raise-stmt)))
  (declare (ignorable kind)))

(defmethod test-lang ((kind (eql :try-except-stmt)))
  (declare (ignorable kind)))

(defmethod test-lang ((kind (eql :try-finally-stmt)))
  (declare (ignorable kind))
  (progn 
    ;; These two test cases taken from Mike Stall,
    ;;  http://blogs.msdn.com/jmstall/archive/2007/12/16/return-vs-finally-2.aspx
    (run-no-error "
def f1():
  try:
    return 10
  finally:
    return 5
assert f1() == 5")
    (run-no-error "
def f2():
  try:
    raise Exception # like 'throw'
  finally:
    return 5
assert f2() == 5")))

(defmethod test-lang ((kind (eql :tuple-expr)))
  (declare (ignorable kind))
  (run-no-error "assert (1,2,3)[0:1] == (1,)"))

(defmethod test-lang ((kind (eql :unary-expr)))
  (declare (ignorable kind))
  (run-no-error "x = 3; +x; -x")
  (run-no-error "assert +3 == 3")
  (run-no-error "x = 3; assert +x == 3")
  (run-no-error "x = 3; assert -x == -3")
  (run-no-error "x = 3; assert ++x == 3")
  (run-no-error "x = 3; assert --x == 3")
  )

(defmethod test-lang ((kind (eql :while-stmt)))
  (declare (ignorable kind))
  (run-no-error "while 0: 1/0")
  (run-no-error "while 1: break")
  (run-no-error "
x = 3
while x > 0:
  x -= 1
  if x == 1:
    break
assert x == 1"
  )
  (run-no-error "
x = 3
while x > 0:
  x -= 1
  if x == 1:
    break
else:
  x = 42
assert x == 1"
  )
  (run-no-error "
x = 3
while x > 0:
  x -= 1
else:
  x = 42
assert x == 42"
  )
  (run-no-error "
def f():
  x = 3
  while x > 0:
    x -= 1
  else:
    x = 42
  assert x == 42
f()")
  (run-no-error "
def f():
  x = 3
  while x > 0:
    x -= 1
  else:
    x = 42
  assert x == 42
  yield 42
g = f()
assert g.next() == 42"))

(defmethod test-lang ((kind (eql :with-stmt)))
  (declare (ignorable kind))
  (run-no-error "
x = []
class C:
  def __enter__(self):
    x.append('enter')
    return 42
  def __exit__(self, _x,_y,_z):
    x.append('exit')

with C() as y:
  x.append(y)
assert x == ['enter', 42, 'exit']")
  (run-error "
x = []
class C:
  def __enter__(self):
    x.append('enter')
    return 42
  def __exit__(self, _x,_y,_z):
    x.append('exit')

with C() as y:
  x.append(y)
  1/0
" {ZeroDivisionError})
  (run-no-error "
def f():
 yield 1
 try:
  yield 2
  raise NameError
 finally:
  yield 3
g = f()
assert g.next() == 1
assert g.next() == 2
assert g.next() == 3
try:
  g.next()
  assert False
except NameError:
  pass
")
  (run-no-error "
x = None
def f():
 yield 1
 try:
  yield 2
 finally:
  global x
  x = 3
g = f()
assert list(f()) == [1,2]
assert x == 3")
  (run-error "
x = []
class C:
  def __enter__(self):
    x.append('enter')
    return 42
  def __exit__(self, _x,_y,_z):
    x.append('exit')
def f():
 with C() as y:
  x.append(y)
  yield 1/0
list(f())" {ZeroDivisionError})
  (run-error "
x = []
class C:
  def __enter__(self):
    x.append('enter')
    return 42
  def __exit__(self, _x,_y,_z):
    x.append('exit')
def f():
 with C():
  x.append('y')
  yield 1/0
list(f())" {ZeroDivisionError}))

(defparameter *cps-tests*
    `(("pass" ())
      ("yield 1" (1))
      ("yield" ,(list *the-none*))
      ("
if 0:
  yield 1
elif []:
  yield 2
else:
  yield 3
  yield 31
  if 1:
    yield 4
    yield 41
  else:
    yield 5" (3 31 4 41))
      ("
try:
  1/0
  print 'a'
  yield 2
except:
  print 'x'
  print 'y'
  yield 3
  print 'z'
print 'below'" (3))
      ("
yield 1
class C: pass
class D:
  pass
yield 2" (1 2))
      ("
yield 1
class C:
  pass
try:
  print 'aap'
  yield 2
  1/0
except C, e:
  print 'never'
  yield 20
except:
  yield 3
  yield 4" (1 2 3 4))
      ("
y = (yield 3)
yield y" ,(list 3 *the-none*))
      ("print [1,2,3,4]
print (1,2,3,4)" ())
      ("
for x in [1,2,3,4]:
  yield x" (1 2 3 4))
      ("
for x in [1,2,3,4]:
  if x != 3:
    yield x" (1 2 4))
      ("
x = 1
while x < 4:
  yield x
  x = x + 1
" (1 2 3))
      ("
for x in [1,2,3,4,5]:
  if x <= 3:
     continue
  if x == 5:
     break
  yielded = 0
  for y in 'abcde':
     while not yielded:
       yield x
       yielded = 1
" (4))
      ("assert ((yield 1), (yield 2)) == (None, None)" (1 2))
      ("assert [(yield 1), (yield 2)] == [(yield 3), (yield 4)], 'Never' % (yield 5)" (1 2 3 4))
      ("
try:
  yield 1
  assert 0
  yield 3
except AssertionError:
  yield 2" (1 2))
      ("
class C: pass
yield 1
x = C()
x.a = 3
yield x.a
x.a += 1
yield x.a" (1 3 4))
      ("
x = []
def f(i):
  x.append(i)
  return i
assert `(f(1), (yield f(2)), f(3), (yield f(4)))` == `(1, None, 3, None)`
assert x == [1, 2, 3, 4]" (2 4))
      ("
x = []
try:
  try:
    yield 1
    1/0
    yield 999
  finally:
    x.append('finally')
    x.append((yield 2))
except:
  x.append('except')
  x.append((yield 3))
assert x == ['finally', None, 'except', None]" (1 2 3))
      ("assert (1 and (yield 1)) == None" (1))
      ("assert (1 or (yield 1)) == 1" ())
      ("yield 1
try:
  try:
    pass
    pass
    exec 'yield 1'
  except KeyError:
    yield 'never'
    pass
except SyntaxError:
  yield 2
" (1 2))
      ("
try:
  yield 1
  yield 2
except:
  yield 'except'
else:
  yield 'else'" (1 2 "else"))
      ("
try:
  yield 1
  yield 2
finally:
  yield 'finally'" (1 2 "finally"))
      ("
try:
  try:
    yield 1
    1/0
    yield 2
  except:
    yield 'except inner'
  finally:
    yield 'finally inner'
except:
  yield 'except outer'
finally:
  yield 'finally outer'" (1 "except inner" "finally inner" "finally outer"))))

(defmacro with-dummy-namespace (&body body)
  `(let ((%dummy-cps-namespace (make-hash-table :test 'eq)))
     (declare (ignorable %dummy-cps-namespace))
     (clpython::with-namespace (,(clpython::make-hash-table-ns
                                   :dict-form '%dummy-cps-namespace
                                   :parent (clpython::make-hash-table-ns
                                            :dict-form '(make-hash-table)
                                            :scope :module
                                            :parent (clpython::make-builtins-namespace))
                                   :scope :function)
                                   :define-%globals t)
       ,@body)))

(defun make-test-gen (string &key debug (max 10))
  "Returns yielded values. The values send as the result of each yield expression
are increasing integers starting from 1, up to MAX.
MAX is a safey limit, in case the code erroneously goes into an endless loop."
  (flet ((wformat (&rest args)
           (when debug (apply #'format t args)))) 
    (let* ((suite-stmt (with-matching ((parse string) ([module-stmt] ?suite-stmt))
                         ?suite-stmt))
           (gener (eval `(with-dummy-namespace
                             (clpython::make-generator ,suite-stmt))))
           (yielded ()))
      (block iterate
        (wformat "G: ~A~%" gener)
        (wformat "Starting G with next()~%")
        (let ((values (multiple-value-list
                       (handler-case (clpython:generator.next gener)
                         ({StopIteration} ()
                           (wformat " exhausted~%")
                           :exhausted)))))
          (wformat "  yield #1: ~S~%" values)
          (unless (keywordp (car values))
            (wformat "First value not a keyword; assuming yielded value: ~A" values)
            (setf values (cons :yield values)))
          (ecase (car values)
            (:exhausted
             (return-from iterate))
            (:implicit-return
             (return-from iterate))
            (:yield
             (push (second values) yielded)
             (loop for val = (multiple-value-list
                              (handler-case (clpython:generator.send gener *the-none*)
                                ({StopIteration} ()
                                  (wformat " exhausted~%")
                                  :exhausted)))
                 for i from 1 below (1+ max)
                 while val do
                   (unless (keywordp (car val))
                     (wformat "First value not a keyword; assuming yielded value: ~A" val)
                     (setf val (cons :yield val)))
                   (case (car val)
                     (:exhausted
                      (return-from iterate))
                     (:yield
                      (wformat "  yield #~A: ~A~%" (1+ i) (second val))
                      (push (second val) yielded))
                     (:implicit-return
                      (return-from iterate))
                     (t (error "invalid values from gener: ~A" val)))
                 finally (when (> i max)
                           (warn "Generator manually stopped (but perhaps not exhausted).~%")))))))
      (nreverse yielded))))

(defmethod test-lang ((kind (eql :yield-expr)))
  (declare (ignorable kind))
  (loop for (src expected) in *cps-tests*
      do (eval `(test-equal (make-test-gen ',src :debug nil) ',expected)))
  (run-no-error "
def f(): 
  assert [2,4,6] == [(yield item) for item in (yield list) if (yield (item, item))]
g = f()
print 1, g.next()
print 2, g.send('abc')
print 3, g.send(1)
print 4, g.send(2)
print 5, g.send(3)
print 6, g.send(4)
print 7, g.send(5)
try:
  print 8, g.send(6)
except StopIteration:
  print 'ok'")
  (run-no-error "
res = list(x for x in range(3) if x != 2)
assert res == [0,1]")
  (run-no-error "
def f(x,y,z,q=99,quux=100):
  global a
  a = x,y,z,q,quux
def g():
  f((yield 1), q=(yield 2), *[(yield 3), (yield 4)])
gener = g()
assert gener.next() == 1
assert gener.send(10) == 2
assert gener.send(40) == 3
assert gener.send(20) == 4
ok = 0
try:
  gener.send(30)
except StopIteration:
  ok = 1
assert ok == 1
print a
assert a == (10, 20, 30, 40, 100)
")
  (run-no-error "
ok = 0
def f(x,y,z):
  assert (x,y,z) == (1,2,3)
  global ok
  ok = 1
def g():
  f( **{ (yield 'y_key'): (yield 'y_val'),
         (yield 'x_key'): (yield 'x_val'),
         (yield 'z_key'): (yield 'z_val') })
gen = g()
request = gen.next()
while 1:
  if request.endswith('_key'):
    val = request[0]
  else:
    val = {'x': 1, 'y': 2, 'z': 3}[request[0]]
  try:
    request = gen.send(val)
  except StopIteration:
    break
assert ok == 1")
  (run-no-error "
class A: pass
class B: pass
def f():
  class C((yield 'A'), (yield 'B')):
    pass
  global theC
  theC = C
g = f()
assert g.next() == 'A'
assert g.send(A) == 'B'
ok = 0
try:
  g.send(B)
except StopIteration:
  ok = 1
assert ok == 1
print theC.__mro__
assert theC.__mro__ == (theC,A,B,object)
")
  (run-no-error "
def f():
  yield ((yield 1) < (yield 2) < (yield 3))
g = f()
assert g.next() == 1
assert g.next() == 2
assert g.next() == False" :fail-info "short-circuit comparison expr")
  (run-no-error "
def f():
  yield 1
  x = 3
  del x
  try:
    x
    global ok
    ok = 0
  except NameError:
    ok = 1
  print 'ok=%s' % ok
  yield 2
print list(f())  
assert list(f()) == [1,2]
assert ok == 1")
  (run-no-error "
d = {1:3}
def f():
  del (yield 1)[(yield 2)]
g = f()
g.next()
g.send(d)
try:
  g.send(1)
  assert False
except StopIteration:
  pass
assert d == {}")
  (run-no-error "
def f():
  (yield 2) if (yield 1) else (yield 3)

g = f()
assert g.next() == 1
assert g.send(True) == 2

g = f()
assert g.next() == 1
assert g.send(False) == 3")
  (run-no-error "
def f():
  import math
  yield math.pi
g = f()
assert 3 < g.next() < 4")
  (run-no-error "
def f():
  from math import pi
  yield pi
g = f()
assert 3 < g.next() < 4")
  (run-no-error "
f = lambda: (yield 2)
assert list(f()) == [2]")
  (run-no-error "
def f():
  yield lambda x, y=(yield 1): (yield x + y + 100)
g = f()
assert g.next() == 1
lm = g.send(10)
assert type(lm) == type(lambda: None)

# lm is a lambda that generates a generator:
#  lambda x, y=10: yield x+y+100
lmg = lm(3)
assert lmg.next() == 3 + 10 + 100
try:
  lmg.next()
  assert False
except StopIteration:
  pass

h = f()
h.next()
lm = h.send(20)
# lm:  lambda x, y=20: yield x+y+100

lmg2 = lm(13, 99)
assert lmg2.next() == 13 + 99 + 100
try:
  lmg2.next()
  assert False
except StopIteration:
  pass
" :fail-info "Returning a lambda generator from a function")
  (run-error "
class C:
 def f():
  def g():
   lambda x, y=(yield 2): x+y
   return 2" {SyntaxError}
   :known-failure (not clpython:*compile-python-ast-before-running*)
   :fail-info "In a generator, returning a value disallowed. Detected by macroexpansion.")
  (run-error "
def f():
   lambda x, y=(yield 2): x+y
   return 2
list(f())" {SyntaxError} :fail-info "In a generator, returning a value disallowed. Detected by macroexpansion.")
  (run-no-error "
def f(x, limit):
  for i in x:
    if i < limit:
      yield i
    else:
      return
g = f([1,2,3,4], 3)
assert list(g) == [1,2]

h = f([1,2,3,4], 0)
assert list(h) == []")
  (run-no-error "
def f(x, limit):
  try:
    for i in x:
      if i < limit:
        yield i
      else:
        return
  except:
    assert False

g = f([1,2,3,4], 3)
assert list(g) == [1,2]

h = f([1,2,3,4], 0)
assert list(h) == []
" :fail-info "Expicit return in subgenerator")
  (run-no-error "
def f():
  yield range(100)[(yield 1):(yield 2):(yield 3)]
g = f()
assert g.next() == 1
assert g.send(0) == 2
assert g.send(10) == 3
assert g.send(3) == [0,3,6,9]" :fail-info "Slice-expr")
  (run-no-error "
x = (x for x in 'y')
assert x == iter(x)" :fail-info "Generator is its own iterator")
  (run-no-error "
def f(): yield 1
g = f()
assert g == iter(g)" :fail-info "Generator is its own iterator")
  (run-no-error "
def f(): yield 1
g = f()
assert g.__iter__() == g" :fail-info "Generator is its own iterator")
  (run-no-error "
def f(): yield 1
try:
  iter(f)
  assert False
except TypeError:
  pass" :fail-info "Generator function does not support iterating")
  (run-no-error "
assert list((x+y) for x in range(3) for y in range(2)) == [0, 1, 1, 2, 2, 3]")
  (run-error "
def f():
 yield 1
 class C:
  yield 2
list(f())" {SyntaxError} :fail-info "Yield outside function")
  (run-error "
def f():
 x = (yield 1)
 class C:
  y = (yield 2)
list(f())" {SyntaxError} :fail-info "Yield outside function"))
  
(defmethod test-lang ((kind (eql :yield-stmt)))
  (declare (ignorable kind))
  (run-no-error "
def f():
  return
  yield 1
g = f()
try:
  g.next()
  assert False
except StopIteration:
  pass")
  (run-no-error "
def f():
  i = 0
  while 1:
    yield i
    i += 1
g = f()
assert g.next() == 0
assert g.next() == 1
")
  (run-no-error "
def f():
  'asdfas'  # in the resulting tagbody, no strings may occur at toplevel
  yield 2")
  (run-no-error "
def f():
  0  # ensure loose integers don't screw up tagbody tags
  yield 2
  0
  1
  2
  3
  yield 3")
  (run-no-error "
def f():
  yield 1
  yield 2
g = f()
error = 0
try:
  g.send(1)
except ValueError:
  error = 1
assert error == 1
")
  (run-no-error "
def f():
  yield 1
  yield 2
g = f()
error = 0
try:
  g.throw(KeyError)
except KeyError:
  error = 1
assert error == 1
")
  (run-no-error "
def f():
  yield 1
g = f()
assert g.next
assert g.send
")
  (run-no-error "
x = 0
def f():
  try:
    yield 1
    yield 2
  finally:
    global x
    x = 1
g = f()
assert g.next() == 1
g.close()
assert x == 1")
  (run-no-error "
def f(x):
  for i in x:
    class C:
      for j in x:
        break
    yield i
g = f([1,2,3])
list(g) == [1,2,3]"))

(defmethod test-lang ((kind (eql :attribute-semantics)))
  (declare (ignorable kind))  
  (run-no-error "
class C: pass
x = C()
x.a = 3
assert x.a == 3")
  (run-no-error "
class C:
  pass
setattr(C, 'a', 3)
try:
  setattr(C, 1, 2)
  assert 0
except TypeError:
  None
"))

(defmethod test-lang ((kind (eql :number-method-lookups)))
  (declare (ignorable kind))
  (run-no-error "
# http://bugs.jython.org/issue1159
class DummyNumber(object):
    '''
    Minimal implementation of a number that works with SymPy.

    If one has a Number class (e.g. Sage Integer, or some other custom class)
    that one wants to work well with SymPy, one has to implement at least the
    methods of this class DummyNumber, resp. it's subclasses I5 and F1_1.

    Basically, one just needs to implement either __int__() or __float__() and
    then one needs to make sure that the class works with Python integers and
    with itself.
    '''

    def __radd__(self, a):
        if isinstance(a, (int, float)):
            return a + self.number
        return NotImplemented

    def __add__(self, a):
        if isinstance(a, (int, float, DummyNumber)):
            return self.number + a
        return NotImplemented

    def __rsub__(self, a):
        if isinstance(a, (int, float)):
            return a - self.number
        return NotImplemented

    def __sub__(self, a):
        if isinstance(a, (int, float, DummyNumber)):
            return self.number - a
        return NotImplemented

    def __rmul__(self, a):
        if isinstance(a, (int, float)):
            return a * self.number
        return NotImplemented

    def __mul__(self, a):
        if isinstance(a, (int, float, DummyNumber)):
            return self.number * a
        return NotImplemented

    def __rdiv__(self, a):
        if isinstance(a, (int, float)):
            return a / self.number
        return NotImplemented

    def __div__(self, a):
        if isinstance(a, (int, float, DummyNumber)):
            return self.number / a
        return NotImplemented

    def __rpow__(self, a):
        if isinstance(a, (int, float)):
            return a ** self.number
        return NotImplemented

    def __pow__(self, a):
        if isinstance(a, (int, float, DummyNumber)):
            return self.number ** a
        return NotImplemented

    def __pos__(self):
        return self.number

    def __neg__(self):
        return - self.number

class I5(DummyNumber):
    number = 5
    def __int__(self):
        return self.number

class F1_1(DummyNumber):
    number = 1.1
    def __float__(self):
        return self.number

i5 = I5()
f1_1 = F1_1()

print f1_1**i5
print f1_1**f1_1"
                :known-failure t
                :fail-info "__pow__ lookup goes wrong somewhere"))

(defmethod test-lang ((kind (eql :getitem-methods)))
  (declare (ignorable kind))
  (run "
class C:
  def __getslice__(x, frm, to):
    return 's', frm, to
  def __getitem__(x, item):
    return 'i', item
  def __len__(x):
    return 10

val = C()[:-1]
print 'val=', val
assert val == ('s', 0, 9)

val = C()[3]
print 'val=', val
assert val == ('i', 3)")
  (run "
res = []
class C:
  def __setslice__(x, frm, to, item):
    print 'setslice'
    global res
    res += ['s', frm, to, item]
    print 'after setslice'
  def __setitem__(x, item, val):
    print 'setitem'
    global res
    res += ['i', item, val]
  def __len__(x):
    return 10

x = C()
x[1:4] = 5
x[2] = 4

assert res == ['s', 1, 4, 5, 'i', 2, 4]"))