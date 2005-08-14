# Test existence and performance of builtins that aren't used elsewhere

show = True

def check(a, b):
    if __debug__:
        if show:
            print `a`, "==", `b`
    if not a == b:
        raise AssertionError("%.30r != %.30r" % (a, b))

def exception(exc, f, *args):
    try:
        f(*args)
    except exc:
        if __debug__:
            if show:
                print "%s%r raised %s" % (f.__name__, args, exc.__name__)
    else:
        raise AssertionError("%s not raised by %s%r",
                             exc.__name__, f.__name__, args)

def check_functions(i=0, j=0):
    check(abs(42*i), 42*j)
    check(abs(-42*i), 42*j)
    check(abs(-12345678910*i), 12345678910*j)
    check(abs(-3.14*i), 3.14*i)
    check(abs((3j+4)*i), 5*j)

    check(bool(1+i), True)
    check(bool(100+j), True)
    check(bool(i-j), False)
    check(bool([i, j]), True)
    check(bool([i, j][i:j]), False)
    check(bool({i: j}), True)
    check(bool({}), False)

    check(complex(3*i, 4*j), 3*i+4j*j)

    check(dict([(1,2), (3,4)]), {1: 2, 3: 4})
    check(dict.fromkeys("abc"), {'a': None, 'b': None, 'c': None})

    check(divmod(7, 4), (1, 3))

    check(list(enumerate("abc")), [(0, 'a'), (1, 'b'), (2, 'c')])

    check(filter(None, range(10)), range(1, 10))
    check(filter(lambda x: x < 5, range(10)), range(5))

    check(float("1.5"), 1.5)
    check(float(15*i), 15.0*j)
    check(float(10**100), 1e100)

    check(hash(42), hash(42L))
    check(hash(42), hash(42.0))
    check(hash(42+0j), hash(42.0))
    check(hash("abc"), hash(u"abc"))

    check(hex(42).lower(), "0x2a")
    check(hex(42L).lower(), "0x2al")

    check(int("42"), 42)
    check(int("12345678910"), 12345678910)
    check(int("42", 0), 42)
    check(int("042", 0), 34)
    check(int("0x42", 0), 66)
    check(int("42", 8), 34)
    check(int("42", 16), 66)

    check(list((1, 2, 3)), [1, 2, 3])
    check(list("abc"), ['a', 'b', 'c'])
    check(list(u"abc"), ['a', 'b', 'c'])

    check(long("42"), 42)
    check(long("12345678910"), 12345678910)
    check(long("42", 0), 42)
    check(long("042", 0), 34)
    check(long("0x42", 0), 66)
    check(long("42", 8), 34)
    check(long("42", 16), 66)

    check(isinstance(42, int), True)
    check(isinstance(42, long), False)
    check(isinstance(42L, int), False)
    check(isinstance(42L, long), True)
    check(isinstance(12345678910, int), False)
    check(isinstance(12345678910, long), True)
    check(isinstance(3.14, int), False)
    check(isinstance(3.14, float), True)
    check(isinstance(int, type), True)
    check(isinstance(int, object), True)
    check(isinstance(type, object), True)

    check(issubclass(int, object), True)
    check(issubclass(int, int), True)
    check(issubclass(bool, int), True)
    check(issubclass(int, str), False)
    check(issubclass(str, int), False)
    check(issubclass(type, object), True)

    it = iter("abcdef")
    for i, c in enumerate(it):
        check(c, chr(ord('a') + i))
        if i == 2:
            break
    check(it.next(), "d")
    check(it.next(), "e")
    check(it.next(), "f")
    exception(StopIteration, it.next)
    exception(StopIteration, it.next)

    check(map(None, range(5)), range(5))
    check(map(None, range(5), range(5)),
          [(0, 0), (1, 1), (2, 2), (3, 3), (4, 4)])
    check(map(lambda x: x+1, range(5)), range(1, 6))
    check(map(len, ("", "a", "ab", "abc")), range(4))

    check(max(1, 5), 5)
    check(max([3, 1, 2]), 3)
    check(max("Bac"), "c")
    check(max(u"aBc"), u"c")

    check(min(1, 5), 1)
    check(min([3, 1, 2]), 1)
    check(min("Bac"), "B")
    check(min(u"aBc"), u"B")

    a = object()
    b = object()
    if id(a) == id(b):
        raise AssertionError("objects not unique")

    check(oct(42), '052')
    check(oct(42L), '052L')

    check(ord("a"), 97)
    check(ord(u"a"), 97)
    check(ord(u"\u1234"), 0x1234)

    check(pow(2, 10), 1024)
    check(pow(2, 100), 1L<<100)
    check(pow(2, 10, 100), 24)

    check(reduce(lambda a, b: a+b, ("a", "b", "c", "d", "e")), "abcde")

    check(repr(42), "42")
    check(repr(42L), "42L")
    check(repr(3.5), "3.5")
    check(repr(4.5j), "4.5j")
    check(repr(4j+3), "(3+4j)")
    check(repr(4j-3), "(-3+4j)")
    check(repr(-4j), "-4j")
    check(repr(3.5-0j), "(3.5+0j)")
    check(repr("abc"), "'abc'")
    check(repr("abc\012"), "'abc\\n'")
    check(repr(u"abc"), "u'abc'")
    check(repr(u"abc\u1234"), "u'abc\u1234'")
    check(repr(range(5)), "[0, 1, 2, 3, 4]")
    check(repr(('a', 'b', 'c')), "('a', 'b', 'c')")
    check(repr({1: 42}), "{1: 42}")

    for x in 42, 42L, 3.5, 4.5j, 4j+3, "abc", range(3), (1, 2, 'c'), {}:
        check(repr(x), `x`)

    check(round(3.14), 3.0)
    check(round(3.14, 1), 3.1)
    check(round(31.4, -1), 30.0)

    check(str(42), "42")
    check(str(42L), "42")
    check(str(3.5), "3.5")
    check(str(4.5j), "4.5j")
    check(str(4j+3), "(3+4j)")
    check(str(4j-3), "(-3+4j)")
    check(str(-4j), "-4j")
    check(str(3.5-0j), "(3.5+0j)")
    check(str("abc"), "abc")
    check(str(range(5)), "[0, 1, 2, 3, 4]")
    check(str(('a', 'b', 'c')), "('a', 'b', 'c')")
    check(str({1: 42}), "{1: 42}")

    check(sum(range(1, 11)), 55)
    check(sum((3.5, 2, 4.5)), 10)

    check(tuple("abc"), ('a', 'b', 'c'))
    check(tuple(range(5)), (0, 1, 2, 3, 4))
    check(tuple({1: 2}), (1,))
    check(tuple(u"abc\u1234"), (u'a', u'b', u'c', u'\u1234'))

    check(type(1 == 1), bool)
    check(type(42), int)
    check(type(42L), long)
    check(type(3.14), float)
    check(type(0j), complex)
    check(type(''), str)
    check(type(u''), unicode)
    check(type(()), tuple)
    check(type(range(10)), list)
    check(type({}), dict)
    check(type(type), type)
    check(type(object), type)
    check(type(lambda: None), type(check_functions))
    class C(object):
        pass
    class MC(type):
        pass
    class D:
        __metaclass__ = MC
    class E(object):
        __metaclass__ = MC
    check(type(C), type)
    check(type(D), MC)
    check(type(E), MC)

    check(unicode("abc"), u"abc")
    check("abc".decode(), u"abc")
    check(unicode("abc", "ASCII"), u"abc")
    check("abc".decode("646"), u"abc")
    check(unicode("abc\xff", "Latin-1"), u"abc\u00ff")
    check("abc\xff".decode("latin-1"), u"abc\u00ff")
    check(unicode("abc\xc3\xbf", "utf8"), u"abc\xff")
    check("abc\xc3\xbf".decode("Utf8"), u"abc\xff")
    exception(UnicodeError, unicode, "abc\xff")
    exception(UnicodeError, "abc\xff".decode)
    exception(UnicodeError, unicode, "abc\xff", "us_ascii")
    exception(UnicodeError, "abc\xff".decode, "Ascii")
    exception(UnicodeError, unicode, "abc\xff", "UTF-8")
    exception(UnicodeError, "abc\xff".decode, "utf8")
    check(u"abc\xff".encode("utf-8"), "abc\xc3\xbf")
    check(u"abc\xff".encode("latin1"), "abc\377")
    exception(UnicodeError, u"abc\xff".encode, "Ascii")
    check(u"abc".encode("us"), "abc")

    check(list(xrange(10)), range(10))

    check(zip("abc", "def"), [('a', 'd'), ('b', 'e'), ('c', 'f')])
    check(zip("abc", "def", "ghi"),
          [('a', 'd', 'g'), ('b', 'e', 'h'), ('c', 'f', 'i')])

def check_descriptors(i, j):

    class C0(object):

        def __getattribute__(self, name):
            if name == "spam":
                raise IndexError("no way")
            return super(C0, self).__getattribute__(name)

    class C(C0):

        hello = 42

        def __new__(cls, *args):
            if args:
                return None
            return super(C, cls).__new__(cls)

        def __init__(self):
            self.__dict__["foo"] = 42
            self.spam = 42

        def __setattr__(self, name, value):
            if name == "foo":
                raise RuntimeError("forget it")
            super(C, self).__setattr__(name, value)

        def __getattr__(self, name):
            if name == "bar":
                return self.foo
            raise AttributeError(name)

        def __getattribute__(self, name):
            if name == "hello":
                return "booh"
            return super(C, self).__getattribute__(name)

        def getx(self):
            return self.__x
        def setx(self, x):
            self.__x = x
        def delx(self):
            del self.__x
        x = property(getx, setx, delx)
        xx = property(getx)

        def f(*args, **kwds):
            return list(args), dict(kwds)
        fc = classmethod(f)
        fs = staticmethod(f)

        def __repr__(self):
            s = super(C, self).__repr__()
            s = s.replace("<" + __name__ + ".", "<")
            i = s.index(" at ")
            s = s[:i] + ">"
            return s

    def checks():
        check(C(1), None)
        c1 = C()
        exception(AttributeError, getattr, c1, 'booh')
        exception(AttributeError, getattr, c1, "x")
        exception(AttributeError, getattr, c1, "xx")
        exception(AttributeError, setattr, c1, "xx", 42)
        setattr(c1, "x", 42)
        check(c1.x, 42)
        check(c1._C__x, 42)
        check(c1.xx, 42)
        exception(AttributeError, delattr, c1, "xx")
        del c1.x
        exception(AttributeError, getattr, c1, "x")
        exception(AttributeError, getattr, c1, "xx")
        check(getattr(c1, "x", None), None)
        check(getattr(c1, "xx", None), None)
        exception(AttributeError, delattr, c1, "x")

        check(c1.f(42), ([c1, 42], {}))
        check(c1.fc(42, foo=42), ([C, 42], {"foo": 42}))
        check(c1.fs(42, a=1, b=2), ([42], {'a': 1, 'b': 2}))
        check(repr(c1), "<C object>")

        check(getattr(c1, 'foo'), 42)
        check(getattr(c1, 'bar'), 42)
        exception(RuntimeError, setattr, c1, "foo", 42)
        c1.bar = "hello"
        check(c1.bar, "hello")
        exception(IndexError, getattr, c1, "spam")
        check(getattr(c1, "hello"), "booh")
        B = C.__bases__[-1]
        save = B.__getattribute__
        del B.__getattribute__
        check(c1.spam, 42)
        check(getattr(c1, "hello"), "booh")
        save2 = C.__getattribute__
        del C.__getattribute__
        check(c1.hello, 42)
        C.__getattribute__ = save2
        B.__getattribute__ = save
        exception(IndexError, getattr, c1, "spam")
        exception(IndexError, getattr, c1, "spam", None)
        check(getattr(c1, "hello"), "booh")

    checks()
    checks()
    class A(object):
        pass
    class B(object):
        def __getattribute__(self, name):
            if name == "spam":
                raise IndexError("no way")
            return super(B, self).__getattribute__(name)
    C.__bases__ = (A, B)
    checks()

    c2 = C()
    c2.x = 42
    check(c2.x, 42)
    check(c2._C__x, 42)
    c2.__class__ = C0
    check(getattr(c2, 'x', None), None)
    check(c2._C__x, 42)

def main():
    global show
    show = True
    for i in range(500):
        check_functions(j=long(i*1000000), i=i*1000000)
        check_descriptors(j=long(i*1000000), i=i*1000000)
        show = False
    print "OK."

if __name__ == '__main__':
    main()
