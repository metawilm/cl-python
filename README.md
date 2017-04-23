CLPython - an implementation of Python in Common Lisp
=====================================================

CLPython is an open-source implementation of Python written in Common Lisp.
With CLPython you can run Python programs in a Lisp environment. Libraries written
in Lisp are available to Python code, and Python libraries can be accessed by Lisp code.
Also Python and Lisp code can be mixed.

For rough documentation, please see the [Introduction](http://common-lisp.net/project/clpython/index.html) and
[Manual](http://common-lisp.net/project/clpython/manual.html) on *common-lisp.net*.

To install using QuickLisp: `(ql:quickload "clpython")`

CLPython is developed by Willem Broekema and is released as open source under the [LLGPL](http://opensource.franz.com/preamble.html).
The project was started in 2006, and is since 2013 not under active development anymore.

| Common Lisp Implementation | Build + Test Status | |
|:-:|:-:|:-:|
| [ABCL](https://common-lisp.net/project/armedbear/) | [![Build Status](https://travis-build-job-badge.herokuapp.com/badge?user=metawilm&repo=cl-python&branch=master&envContains=abcl+CATCH&label=ABCL)](https://travis-ci.org/metawilm/cl-python) | Fails due to NullPointerException |
| [Allegro CL](http://franz.com/products/allegrocl/) | [![Build Status](https://travis-build-job-badge.herokuapp.com/badge?user=metawilm&repo=cl-python&branch=master&envContains=allegro+CATCH&label=Allegro+CL)](https://travis-ci.org/metawilm/cl-python) | |
| [Clozure CL](http://clozure.com/clozurecl.html)    | [![Build Status](https://travis-build-job-badge.herokuapp.com/badge?user=metawilm&repo=cl-python&branch=master&envContains=ccl&label=Clozure+CL)](https://travis-ci.org/metawilm/cl-python) | |
| [CLISP](http://clisp.sourceforge.net)              | [![Build Status](https://travis-build-job-badge.herokuapp.com/badge?user=metawilm&repo=cl-python&branch=master&envContains=clisp&label=CLISP)](https://travis-ci.org/metawilm/cl-python) | Fails due to stack overflow |
| [CMUCL](http://www.cons.org/cmucl/)                | ? | |
| [ECL](http://ecls.sourceforge.net/)                | [![Build Status](https://travis-build-job-badge.herokuapp.com/badge?user=metawilm&repo=cl-python&branch=master&envContains=ecl+CATCH&label=ECL)](https://travis-ci.org/metawilm/cl-python) | |
| [LispWorks](http://www.lispworks.com/)             | ? | |
| [SBCL](http://sbcl.sourceforge.net/)               | [![Build Status](https://travis-build-job-badge.herokuapp.com/badge?user=metawilm&repo=cl-python&branch=master&envContains=sbcl&label=SBCL)](https://travis-ci.org/metawilm/cl-python) [![Coverage Status](https://coveralls.io/repos/metawilm/cl-python/badge.svg?branch=master)](https://coveralls.io/r/metawilm/cl-python?branch=master) | |


