# basic things: numbers

def a():
  a,b = 3,4                   # relevant methods:
  print a, "+", b, "=", a+b   # a.__add__(b)   a.__str__()  b.__str__()
  print a, "<<", b, "=", a << b   # a.__lshift__(b)

a()

# strings

def a2 ():
  s = "Bush"
  t = "Reagan"
  print s != t

a2()


# lists

def b():
  x = [2,3,4,5]
  print "'3' has index ", x.index(3), "in list ", x
  y = [6,7]
  z = x + y     # different use of `+':  <list>.__add__ != <number>.__add__ 
  print x, "+", y, "=", z
  z.reverse()
  print "reversed: ", z

b()


# nice: list comprehension
# (compare with LOOP)

def c():
  x = [n for n in range(0,15) if n & 1 == 0]
  print "even numbers from 0 to 15: ", x

c()


# subclass and customize the built-in dictionary type

class D(dict):
  def __setitem__(self, key, val):
    dict.__setitem__(self, len(key), val)

  def __getitem__(self, key):
    return dict.__getitem__(self, len(key))

def f():
  d = D()
  d["foo"] = 3                     # d.__setitem__("foo", 3)
  print "d[bar] = 3? ", d["bar"]   # d.__getitem__("bar")

f()


# exceptions
# (The Lisp exceptions are catched and replaced by Python exceptions)

def g ():
  try:
    a = 1 + 2 + 5/0               #  5.__div__(0)
  except ZeroDivisionError, e:
    print "got error:", e         # e.__str__()

g()


# generators: return a value, but remember state of execution

def xrange_(x):
  i = 0
  while i < x:
    yield i       # <- 'yield' instead of 'return'
    i = i + 1

def h ():
  x = xrange(1,10,2)
  i = iter(x)          # i = x.__iter__()

  print "x: ", x
  print "i: ", i

  a = i.next()
  b = i.next()
  print "a:", a, "b:", b

h()


