class Random:

    def __init__(self, x, y, z):
        self._seed = (x, y, z)

    def random(self):
        x, y, z = self._seed
        x = (171 * x) % 30269
        y = (172 * y) % 30307
        z = (170 * z) % 30323
        self._seed = x, y, z
        return (x/30269.0 + y/30307.0 + z/30323.0) % 1.0

    def uniform(self, a, b):
        return a + (b-a) * self.random()

    def randint(self, a, b):
        return a + int((b+1-a) * self.random())

    def choice(self, seq):
        return seq[int(self.random() * len(seq))]

rgen = Random(57, 86, 708 % 650)

compares = 0

T = int

class TT(T):
    def __repr__(self):
        return "T(%d)" % self

class Int(TT):
    def __new__(cls, value=None):
        if value is None:
            value = rgen.randint(0, 0x7ffffffe)
        return TT.__new__(cls, value)

def icmp(a, b):
    global compares
    compares += 1
    return T.__cmp__(a, b)

N = 20000
K = 1

##if __debug__: import time

def sortum(data, cmp=None):
    global compares
    compares = 0
    data = data[:]
    ##if __debug__: t0 = time.time()
    if cmp is None:
        print "using None"
        data.sort()
    else:
        print "using", cmp.__name__
        data.sort(cmp)
    ##if __debug__: t1 = time.time()
    print "Z", data[:K], data[N//2:N//2+K], data[-K:]
    print compares,
    ##if __debug__:  print "%.3f" % (t1-t0),
    print

def main():
    ##if __debug__: t0 = time.time()
    data = [Int() for x in xrange(N)]
    ##if __debug__: t1 = time.time()
    ##if __debug__: print "%.3f" % (t1-t0)
    print "A", data[:K], data[N//2:N//2+K], data[-K:]
    sortum(data)
    sortum(data, cmp)
    sortum(data, icmp)
    TT.__cmp__ = icmp
    sortum(data)
    TT.__cmp__ = T.__cmp__
    sortum(data)

if __name__ == '__main__':
    main()
