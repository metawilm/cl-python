;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.TEST -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Python function test

(in-package :clpython.test)

(defun run-function-test ()
  (run-no-error "
def a(): pass
c = a.func_code
assert c.co_argcount == 0
assert c.co_cellvars == ()
assert c.co_flags & 0x04 == 0 # *-arg
assert c.co_flags & 0x08 == 0 # **-arg
assert c.co_flags & 0x20 == 0 # generator
assert c.co_freevars == ()
assert c.co_name == 'a'
assert c.co_names == ()
assert c.co_varnames == ()
print 'a: ok'

def a2(): return 1
c = a2.func_code
assert c.co_argcount == 0
assert c.co_cellvars == ()
assert c.co_flags & 0x04 == 0 # *-arg
assert c.co_flags & 0x08 == 0 # **-arg
assert c.co_flags & 0x20 == 0 # generator
assert c.co_freevars == ()
assert c.co_name == 'a2'
assert c.co_names == ()
assert c.co_varnames == ()
print 'a2: ok'


def a3(): 
  x = 5
  return x
c = a3.func_code
assert c.co_argcount == 0
assert c.co_cellvars == ()
assert c.co_flags & 0x04 == 0 # *-arg
assert c.co_flags & 0x08 == 0 # **-arg
assert c.co_flags & 0x20 == 0 # generator
assert c.co_freevars == ()
assert c.co_name == 'a3'
assert c.co_names == ()
assert c.co_varnames == ('x',)
print 'a3: ok'

def b(x): pass
c = b.func_code
assert c.co_argcount == 1
assert c.co_cellvars == ()
assert c.co_flags & 0x04 == 0 # *-arg
assert c.co_flags & 0x08 == 0 # **-arg
assert c.co_flags & 0x20 == 0 # generator
assert c.co_freevars == ()
assert c.co_name == 'b'
assert c.co_names == ()
assert c.co_varnames == ('x',)
print 'b: ok'

def d(x, y): pass
c = d.func_code
assert c.co_argcount == 2
assert c.co_cellvars == ()
assert c.co_flags & 0x04 == 0 # *-arg
assert c.co_flags & 0x08 == 0 # **-arg
assert c.co_flags & 0x20 == 0 # generator
assert c.co_freevars == ()
assert c.co_name == 'd'
assert c.co_names == ()
assert c.co_varnames == ('x','y')
print 'd: ok'

def e((x, y)): pass
c = e.func_code
assert c.co_argcount == 1
assert c.co_cellvars == ()
assert c.co_flags & 0x04 == 0 # *-arg
assert c.co_flags & 0x08 == 0 # **-arg
assert c.co_flags & 0x20 == 0 # generator
assert c.co_freevars == ()
assert c.co_name == 'e'
assert c.co_names == ()
assert c.co_varnames == ('.0', 'x', 'y')
print 'e: ok'

def e2(a, (x, y)): pass
c = e2.func_code
assert c.co_argcount == 2
assert c.co_cellvars == ()
assert c.co_flags & 0x04 == 0 # *-arg
assert c.co_flags & 0x08 == 0 # **-arg
assert c.co_flags & 0x20 == 0 # generator
assert c.co_freevars == ()
assert c.co_name == 'e2'
assert c.co_names == ()
assert c.co_varnames == ('a', '.1', 'x', 'y')
print 'e2: ok'

def e3(a, (x, y), b, (p,(q,r))): pass
c = e3.func_code
assert c.co_argcount == 4
assert c.co_cellvars == ()
assert c.co_flags & 0x04 == 0 # *-arg
assert c.co_flags & 0x08 == 0 # **-arg
assert c.co_flags & 0x20 == 0 # generator
assert c.co_freevars == ()
assert c.co_name == 'e3'
assert c.co_names == ()
assert c.co_varnames == ('a', '.1', 'b', '.3', 'x', 'y', 'p', 'q', 'r')
print 'e3: ok'

def i(*args): pass
c = i.func_code
assert c.co_argcount == 0
assert c.co_cellvars == ()
assert c.co_flags & 0x04 == 0x04 # *-arg
assert c.co_flags & 0x08 == 0 # **-arg
assert c.co_flags & 0x20 == 0 # generator
assert c.co_freevars == ()
assert c.co_name == 'i'
assert c.co_names == ()
assert c.co_varnames == ('args',)
print 'i: ok'

def i2(**kwargs): pass
c = i2.func_code
assert c.co_argcount == 0
assert c.co_cellvars == ()
assert c.co_flags & 0x04 == 0 # *-arg
assert c.co_flags & 0x08 == 0x08 # **-arg
assert c.co_flags & 0x20 == 0 # generator
assert c.co_freevars == ()
assert c.co_name == 'i2'
assert c.co_names == ()
assert c.co_varnames == ('kwargs',)
print 'i2: ok'

def i3(*args, **kwargs): pass
c = i3.func_code
assert c.co_argcount == 0
assert c.co_cellvars == ()
assert c.co_flags & 0x04 == 0x04 # *-arg
assert c.co_flags & 0x08 == 0x08 # **-arg
assert c.co_flags & 0x20 == 0 # generator
assert c.co_freevars == ()
assert c.co_name == 'i3'
assert c.co_names == ()
assert c.co_varnames == ('args', 'kwargs',)
print 'i3: ok'

def i4(a,b, c=1, d=2, *args, **kwargs): pass
c = i4.func_code
assert c.co_argcount == 4
assert c.co_cellvars == ()
assert c.co_flags & 0x04 == 0x04 # *-arg
assert c.co_flags & 0x08 == 0x08 # **-arg
assert c.co_flags & 0x20 == 0 # generator
assert c.co_freevars == ()
assert c.co_name == 'i4'
assert c.co_names == ()
assert c.co_varnames == ('a', 'b', 'c', 'd', 'args', 'kwargs',)
print 'i4: ok'

def i5(a,(b1,b2), c=1, d=2, *args, **kwargs): pass
c = i5.func_code
assert c.co_argcount == 4
assert c.co_cellvars == ()
assert c.co_flags & 0x04 == 0x04 # *-arg
assert c.co_flags & 0x08 == 0x08 # **-arg
assert c.co_flags & 0x20 == 0 # generator
assert c.co_freevars == ()
assert c.co_name == 'i5'
assert c.co_names == ()
assert c.co_varnames == ('a', '.1', 'c', 'd', 'args', 'kwargs', 'b1', 'b2')
print 'i5: ok'

def j(a, (p,q)=(1,2), z=3): pass
c = j.func_code
assert c.co_argcount == 3
assert c.co_cellvars == ()
assert c.co_flags & 0x04 == 0 # *-arg
assert c.co_flags & 0x08 == 0 # **-arg
assert c.co_flags & 0x20 == 0 # generator
assert c.co_freevars == ()
assert c.co_name == 'j'
assert c.co_names == ()
assert c.co_varnames == ('a', '.1', 'z', 'p', 'q')
print 'j: ok'

def k(): yield 1
c = k.func_code
assert c.co_argcount == 0
assert c.co_cellvars == ()
assert c.co_flags & 0x04 == 0 # *-arg
assert c.co_flags & 0x08 == 0 # **-arg
assert c.co_flags & 0x20 == 0x20 # generator
assert c.co_freevars == ()
assert c.co_name == 'k'
assert c.co_names == ()
assert c.co_varnames == ()
print 'k: ok'"))

