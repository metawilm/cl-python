from b5 import check

def main():
    L = [1]*1000000
    L[-1] = 42
    n = 0
    for i in L:
        n += i
    check(i, 42)
    check(n, 1000041)
    n = 0
    for i in xrange(1000000):
        n += i
    check(i, 999999)
    check(n, 999999*1000000//2)
    d = dict.fromkeys(xrange(1000000))
    n = 0
    for i in d:
        n += i
    check(n, 999999*1000000//2)

if __name__ == '__main__':
    main()
