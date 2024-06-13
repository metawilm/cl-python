CLPython - an implementation of Python in Common Lisp
=====================================================

CLPython is an open-source implementation of Python written in Common Lisp.
With CLPython you can run Python programs in a Lisp environment. Libraries written
in Lisp are available to Python code, and Python libraries can be accessed by Lisp code.
Also Python and Lisp code can be mixed.

For rough documentation, please see the [Introduction](http://common-lisp.net/project/clpython/index.html) and
[Manual](http://common-lisp.net/project/clpython/manual.html) on *common-lisp.net*.

To install using QuickLisp: `(ql:quickload "clpython")` (require ASDF 3.3 or higher).

CLPython is developed by Willem Broekema and is released as open source under the [LLGPL](http://opensource.franz.com/preamble.html).
The project was started in 2006, and is since 2013 not under active development anymore.

See the [build status](https://travis-ci.org/metawilm/cl-python) on Travis-CI, and the [coverage status](https://coveralls.io/github/metawilm/cl-python?branch=master) on Coveralls:

| Common Lisp Implementation | Build + Test Status | |
|:-:|:-:|:-:|
| [ABCL](https://common-lisp.net/project/armedbear/) | ? | |
| [Allegro CL](http://franz.com/products/allegrocl/) | ? | |
| [Clozure CL](http://clozure.com/clozurecl.html)    | ? | |
| [CLISP 2.49.93](https://clisp.sourceforge.io)      | &#10060; | Dependency `cl-fad` &rarr; `bordeaux-threads` not supported |
| [CMUCL](http://www.cons.org/cmucl/)                | ? | |
| [ECL 23.9.9](https://ecl.common-lisp.dev)          | ? | |
| [LispWorks](http://www.lispworks.com/)             | ? | |
| [SBCL 2.1.11](https://www.sbcl.org)                | &#9989; | |
