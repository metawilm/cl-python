print "running tests..."

# IronPython 20060602
class x(dict): pass
c = x(a=3)
assert type(c) is x
assert c['a'] == 3

