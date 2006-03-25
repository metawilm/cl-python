def f(x):
  print "f %s" % x
  return [x,x]

@f
def g():
  return 3
