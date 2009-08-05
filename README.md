CLPython - an implementation of Python in Common Lisp
=====================================================

CLPython is an open-source implementation of Python written in Common Lisp.
With CLPython you can run Python programs in a Lisp environment. Libraries written
in Lisp are available to Python code, and Python libraries can be accessed by Lisp code.

CLPython is developed by Willem Broekema and is released as open source under the
[LLGPL](http://opensource.franz.com/preamble.html).

Requirements
------------

CLPython runs successfully on each of the following platforms:

* [Allegro CL 8.1](http://franz.com/products/allegrocl/)
* [Clozure CL 1.3](http://clozure.com/clozurecl.html)
* [CMUCL 19E (2009-01)](http://www.cons.org/cmucl/)
* [LispWorks 5.1](http://www.lispworks.com/)
* [SBCL 1.0.29](http://sbcl.sourceforge.net/)

There are dependencies on:

* [CL-Yacc](http://www.pps.jussieu.fr/~jch/software/cl-yacc/)
* [Closer to MOP](http://common-lisp.net/project/closer/closer-mop.html)
* [ptester](http://www.cliki.net/ptester)

Install
-------

To compile and load CLPython you need [asdf](http://www.cliki.net/asdf). First create a link from
the repository to files _clpython.asd_ and _clpython-test.asd_. Then load the system:

    (asdf:operate 'asdf:load-op :clpython)

To run the test suite:

    (asdf:operate 'asdf:test-op :clpython)

The test result printed at the end should be a message like "Errors detected in this test: 4" (which
are a handful known issues) and many successes. Unintended test errors are marked as "unexpected"
in the summary; please report them if they occur.

Mailing Lists
-------------

There are two mailing lists, both low-traffic:

* [clpython-devel](http://common-lisp.net/cgi-bin/mailman/listinfo/clpython-devel) for general discussion and bug reports;
* [clpython-announce](http://common-lisp.net/cgi-bin/mailman/listinfo/clpython-announce) for announcements of new releases.
