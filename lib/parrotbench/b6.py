from b5 import check

def main():
    print 111
    L = [1]*1000000
    L[-1] = 42
    print 151
    n = 0
    for i in L:
        n += i
    check(i, 42)
    check(n, 1000041)
    print 222

    n = 0
    for i in xrange(1000000):
        n += i
    print 333

    check(i, 999999)
    check(n, 999999*1000000//2)
    print 444
    d = dict.fromkeys(xrange(1000000))
    print 555
    n = 0
    for i in d:
        n += i
    print 666
    check(n, 999999*1000000//2)

if __name__ == '__main__':
    main()
