CLPython - an implementation of Python in Common Lisp
=====================================================

CLPython is an open-source implementation of Python written in Common Lisp.
With CLPython you can run Python programs in a Lisp environment. Libraries written
in Lisp are available to Python code, and Python libraries can be accessed by Lisp code.
Also Python and Lisp code can be mixed.

CLPython is developed by Willem Broekema and is released as open source under the
[LLGPL](http://opensource.franz.com/preamble.html).

The project was started in 2006, and is currently (2013) not under active development anymore.

The git address changed on Feb 4, 2014 from: github.com/franzinc/cl-python.git to: github.com/metawilm/cl-python.git

Documentation
-------------

Please see the [Introduction](http://common-lisp.net/project/clpython/index.html) and
[Manual](http://common-lisp.net/project/clpython/manual.html) on *common-lisp.net*.

Requirements
------------

CLPython runs successfully on:

* [Allegro CL 8.2 (ANSI and Modern)](http://franz.com/products/allegrocl/)
* [Clozure CL 1.5-r13651](http://clozure.com/clozurecl.html)
* [CMUCL 20b-pre2](http://www.cons.org/cmucl/)
* [ECL (git: 2011.02.11)](http://ecls.sourceforge.net/)
* [LispWorks 6.0](http://www.lispworks.com/)
* [SBCL 1.0.45](http://sbcl.sourceforge.net/)

Install
-------

Using QuickLisp:

    (ql:quickload "clpython")
