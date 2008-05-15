(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :aserve))

(defpackage #:clpython-web
  (:use #:common-lisp #:net.html.generator))

(in-package #:clpython-web)

(defun run ()
  (create-pages))

(defun create-pages ()
  (loop for (fname name) in `(("index.html" :main)
                              #+(or)("tech.html" :tech))
      do  (with-open-file (f fname
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
            (format t "Creating file ~A ...~%" fname)
	    (html-stream f (fill-page name)))))

(defparameter *style*
    "<style type=\"text/css\">
 body, td {line-height: 1.4em}
 h1, h2, pre {font-family: Georgia, Palatino, \"Times New Roman\", Times, serif; font-weight: normal;}
 h1 {color: navy}
 body, div {color: #000; background-color: #fff}
 h2     {font-family: Georgia, Palatino, Times, serif; font-size: 1.2em; color: navy;
         position: relative; xleft: -1em; margin-right: 0.2em; display: inline }
 prex {xfont-family: xfont-family: Monaco, monospace; font-size: 1.0em; xline-height: 1.4em; background-color: #dedede;
        position: relative; left: 0%; width: 100%; overflow: auto}
 .input {color: brown}
 h2 + p {display: inline}
 h2:after {content: \" \\02014 \"} /* 02014 = mdash */
 .params {position: relative; left: 15px}
 .dict {margin-bottom: 30px}
 p {margin-bottom: 1.4em; xmargin-top: 1.4em}
 xpre {padding: 0.7em 0em 0.7em 1em; margin: -0.7em 0em; color: #333}
 xul {margin-top: 0.4em}
 li > pre {display: inline; margin: 0 0.7em; padding: 0 0.7em}
 a.external {padding-right: 13px; background: url(external.png) center right no-repeat; }
</style>")

(defgeneric fill-page (page)
  (:documentation "Emit html"))

(defmacro with-page-template (options &body body)
  (let ((title (second (find :title options :key #'first)))
        (page (second (find :page options :key #'first))))
    (check-type title string)
    `(html
      (:html
       (:head (:title ,title)
              (:princ *style*))
       (:body
        ((:div style "position: absolute; left: 5%; width: 90%")
         ((:div style "margin-bottom: 35px")

          #+(or)((:div style "font-size: small")
                 ,(if (eq page :main) "Introduction" '((:a href "./index.html") "Introduction"))
                 " | "
                 ,(if (eq page :tech) "Technical Background" '((:a href "./tech.html") "Technical Background"))
                 ;;" | "
                 ;; ,(if (eq page :manual) "Reference Manual" '((:a href "./manual.html") "Reference Manual"))
                 ;; " | "
                 ;;,(if (eq page :dictionary) "Dictionary" '((:a href "./dictionary.html") "Dictionary"))
                 ;; " | "
                 ;;,(if (eq page :status) "Status" '((:a href "./status.html") "Status"))
                 )
          
          (:h1 ,title))
         ,@body
         ((:br clear "all"))
         ((:div style "font-size: small; text-align: center")
          "&mdash; Updated "
          (:princ (multiple-value-bind (second minute hour date month year day)
                      (get-decoded-time)
                    (declare (ignore second minute hour day))
                    (format nil "~4,'0D-~2,'0D-~2,'0D" year month date)))
          " &mdash;")))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun make-anchor-text (string)
  (string-downcase (substitute #\_ #\Space string :test #'char=)))

(defun anchor-link (string)
  `((:a href ,(concatenate 'string "#" (make-anchor-text string)))
    ,string))
)

(defmethod fill-page ((page (eql :main)))
  (with-page-template ((:page :main)
                       (:title "CLPython - an implementation of Python in Common Lisp"))
    ((:div style "font-size: 120%")
     (:p "CLPython is an open-source implementation of <a href=\"http://www.python.org\" class=\"external\">Python</a>
written in Common Lisp.<br>With CLPython you can run Python programs in a Lisp environment. Libraries written<br> in Lisp
are available to Python code, and Python libraries can be accessed by Lisp code."))
    
    ((:div style "width: 54%; float: left")
     
     (:h2 "Requirements")
     (:p "CLPython runs successfully on the following platforms:")
     (:ul (:li ((:a href "http://franz.com/products/allegrocl/" class "external") "Allegro CL 8.1"))
          (:li ((:a href "http://www.lispworks.com/" class "external") "LispWorks 5.0"))
          (:li ((:a href "http://sbcl.sourceforge.net/" class "external") "SBCL 1.0.16")))
     (:p "CLPython does not run yet on the following platforms::")
     (:ul (:li ((:a href "http://www.cons.org/cmucl/" class "external") "CMUCL")))
     (:p "There are dependencies on "
         ((:a href "http://common-lisp.net/project/closer/closer-mop.html" class "external") "Closer to MOP")
         " and " ((:a href "http://www.cliki.net/ptester" class "external") "ptester") ".")
     
     (:h2 "Download")
     (:p "To get the source code from CVS:")
     (:ul (:li (:i "cvs -d :pserver:cvspublic@cvspublic.franz.com:/cvs-public login"))
          (:li "Password:" (:i "cvspublic"))
          (:li (:i "cvs -d :pserver:cvspublic@cvspublic.franz.com:/cvs-public checkout clpython")))
     
     (:h2 "Install")
     (:p "To compile and load CLPython you need " ((:a href "http://www.cliki.net/asdf" class "external") "asdf") ": "
         "first create a link from the repository to file " (:i "clpython.asd") ", then load the system using
using " (:i "(asdf:operate 'asdf:load-op :clpython)") ". ")
     
     (:p "Read the sections below on " ((:a href "#getting_started") "getting started") " with CLPython, "
         "and some " ((:a href "#technical_details") "technical details") " on the implementation.")
     (:p "To run the test suite, evaluate " (:i "(asdf:operate 'asdf:test-op :clpython)") ". The test
result printed at the end should be the message \"Errors detected in this test: 5\" (which are the 5 or so known issues)
and several hundred test successes. Unintended test errors will have the note \"unexpected\".")
     
     #+(or)(:h2 "Documentation")
     #+(or)(:p "The following documents are currently available:" (:br)
               "&#187; " ((:a href "manual.html") "Reference Manual") ": a complete description of CLPython;" (:br)
               "&#187; " ((:a href "dictionary.html") "Dictionary") ": the CLPython interface;" (:br)
               "&#187; " ((:a href "status.html") "Status") ": progress report of the implementation of Python functions and modules.")
     (:p)
     
     (:h2 "Mailing Lists")
     (:p "There are two mailing lists, both low-traffic:")
     (:ul (:li ((:a href "http://common-lisp.net/cgi-bin/mailman/listinfo/clpython-devel") "clpython-devel")
               ": for general discussion and bug reports;")
          (:li ((:a href "http://common-lisp.net/cgi-bin/mailman/listinfo/clpython-announce") "clpython-announce")
               ": for announcements of new releases."))
     (:p)

     (:h2 "About")
     (:p "CLPython is developed by Willem Broekema with support from "
         ((:a href "http://franz.com/" class "external") "Franz Inc.")
         " and released as open source under the "
         ((:a href "http://opensource.franz.com/preamble.html" class "external") "LLGPL") ".")
     (:p))

    ((:div style "width: 38%; float: right; background-color: #dddddd; padding: 15px; margin: 5px")
     (:h2 "Completeness")
     (:p "Almost all Python language features are implemented, like generators, classes, metaclasses, modules, 
list comprehensions, and of course " (:i "lambda") ". A few fairly new Python features introduced
with " ((:a href "http://docs.python.org/whatsnew/whatsnew25.html" class "externab") "Python 2.5")
" are missing:")
     (:ul (:li "Absolute and relative imports: &nbsp;" (:i "from .. import x"))
          (:li "Generators that use " (:i "yield") " as expression: &nbsp;" (:i "y = (yield x)")))

     (:p "Some of the modules in the "
         ((:a href "http://python.org/doc/current/modindex.html"
              class "external") "Python standard library")
         " are written in C; they have to be ported to Lisp before they can be used in CLPython. "
         "Not much time has been spent on this yet. "
         "Here's a rough overview of the porting progress: " (:br)
         
         (let (status)
           (with-package-details (rel-name is-first percentage details)
             (push (cons rel-name (1+ percentage)) status))
           #+(or)(html (:princ (format nil "~A" status)))
           (html (:br)
                 ((:img src (format nil "http://chart.apis.google.com/chart?chd=t:~{~A~^,~}&chg=25,0,1,3&chs=200x300&cht=bhs&chxl=0:||100%|1:|~{~A|~}&chxt=x,y&chf=bg,s,dddddd"
                                    (mapcar #'cdr status)
                                    (mapcar #'car status)))))))
     #+(or)((:h2 "Performance")
            (:p "CLPython is as fast as CPython for numerical calculations, but slower in object handling.")))

    ((:br clear "all"))

    ((:div style "background-color: #dddddd; position: relative; left: 10%; width: 80%; padding: 10px; margin: 30px 0px")
     ((:h1 id "getting_started") "Getting started with CLPython")
     ((:table cellpadding "5")
      ((:tr valign "top")
       ((:td width "50%")
        "This is how to run a single Python file, e.g. the file <i>b2.py</i>
from the Pie-thon benchmarks which calculates the digits of pi:")
       (:td "
clpython(1): <i>(run #p\"lib/parrotbench/b2.py\")</i><br>
3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4 ..."))
      ((:tr valign "top")
       (:td "The same in a read-eval-print loop (\"interpreter\"):")
       (:td "clpython(2): <i>(repl)</i><br>
[CLPython -- type `:q' to quit, `:help' for help]<br>
>>> <i>import sys</i><br>
>>> <i>sys.path.append(\"./lib/parrotbench/\")</i><br>
>>> <i>import b2</i><br>
#&lt;module `b2' from file #P\"lib/parrotbench/b2.py\"><br>
>>> <i>b2.main()</i><br>
3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4 ..."))
      ((:tr valign "top")
       (:td "You can enter arbitrary Python statements, including function and class definitions. The variables
\"_\", \"__\" and \"___\" are bound to the last three results:")
       (:td "
>>> <i>1+2</i><br>
3<br>
>>> <i>_ + 2</i><br>
5"))
      ((:tr valign "top")
       (:td "You can enter arbitrary Lisp code at the prompt, like a function definition:")
       (:td ">>> <i>(defun foo (x y) (+ x y))</i><br>
clpython.app.repl::foo"))

      ((:tr valign "top")
       (:td "But such definitions are not yet accessible to Python:")
       (:td ">>> <i>foo</i><br>
Error: NameError: Variable `foo' is unbound."))
      
      ((:tr valign "top")
       (:td "Input that can be interpreted as both Python or Lisp code, like <i>#'foo</i> will normally be interpreted as a Python comment, but if a line starts with a space, it is always interpreted as Lisp code:")
       (:td ">>> <span style=\"border-bottom: 1px dotted black\">&nbsp;&nbsp;</span><i>#'foo</i><br>
#&lt;Interpreted Function foo>"))
      
      ((:tr valign "top")
       (:td "In that case also the variable \"_\" is set. That gives a way to call Lisp functions from within Python:")
       (:td "
>>> _<br>
#&lt;function foo @ #x11412672><br>
>>> <i>foo = _</i><br>
>>> <i>foo(1,2)</i><br>
3"))
      
      ((:tr valign "top")
       (:td)
       (:td ">>> <i>print \"Have fun!\"</i><br>
Have fun!"))))

    ((:br clear "all"))
    
    ((:div style "position: relative; left: 5%; width: 90%")
     ((:h1 id "technical_details") "CLPython - Some Technical Details")
     #+(or)(make-anchor-links)
     (list #6="Python Object Representation" #+(or)#11="Packages" #+(or)#1="Readtables" #+(or)#2="Parser and Pretty Printer"
           #4="Compiler" #55="Compiler optimizations" #5="Compiled vs. Interpreted Code")
    
     ((:div style "width: 45%; float: left")
      (h2-anchor #6#)
      (:p "Python objects are represented by an equivalent Lisp value where possible, and as "
          ((:a href "http://en.wikipedia.org/wiki/CLOS" class "external") "CLOS")
          " instances otherwise:")
      (:p)
      ((:table style "position: relative; left: 1em; font-size: small")
       (:tr ((:td style "border-top: 1px solid black; border-bottom: 1px dotted #999")
             "Python data type")
            ((:td style "border-top: 1px solid black; border-bottom: 1px dotted #999")
             "Representation in CLPython"))
       (loop with data = '(("Class"              "CLOS Class")
                           ("Instance of user-defined class" "CLOS instance")
                           ("Function"           "Function")
                           ("Dict"               "Hashtable")
                           ("(Unicode) String"   "Unicode string")
                           ("List"               "Adjustable vector")
                           ("Tuple"              "Consed list")
                           ("Long, Integer, Boolean" "Integer")
                           ("Complex"            "Complex")
                           ("Float"              "Double-float")
                           ("Complex"            "Complex"))
           for (k v) in data
           for i from 1
           do (let ((style (if (= i (length data))
                               "border-bottom: 1px solid black"
                             "")))
                (html (:tr ((:td style style)
                            (:princ k))
                           ((:td style style)
                            (:princ v)))))))
      (:p)
     
      #+(or)((h2-anchor #11#)
             (:p "CLPython offers one package named <i>clpython</i> to the outside world. But internally the functionality is spread over separate packages:")
             ((:ul style "font-size: small")
              (:li "<i>clpython:</i> &nbsp;main package"
                   (:ul (:li "<i>clpython.parser:</i> &nbsp; parser and code walker")
                        (:li "<i>clpython.ast:</i>"
                             (:ul (:li "<i>clpython.ast.reserved:</i> &nbsp;reserved words")
                                  (:li "<i>clpython.ast.operator:</i> &nbsp;mathematical operators")
                                  (:li "<i>clpython.ast.punctuation:</i> &nbsp;punctionation characters")
                                  (:li "<i>clpython.ast.node:</i> &nbsp;AST nodes")))
                        (:li "<i>clpython.user:</i> &nbsp; identifiers in Python code"
                             (:ul (:li "<i>clpython.user.builtin</i>")
                                  (:li "<i>clpython.user.builtin.type</i>")
                                  (:li "<i>clpython.user.builtin.type.exception</i>")
                                  (:li "<i>clpython.user.builtin.value</i>")))
                        (:li "<i>clpython.module:</i> &nbsp;aggregation package of Python modules")
                        (:li "<i>clpython.app:</i> &nbsp;programs and utilities"
                             (:ul (:li "<i>clpython.app.repl:</i>&nbsp; read-eval-print loop")))
                        (:li "<i>clpython.package:</i> &nbsp;utilities")))))
     
      #+(or)((h2-anchor #1#)
             (:p "Programs that deal with abstract syntax trees of Python programs have to deal with symbols in different packages: "
                 "the nodes of the AST are in package <i>clpython.<b>ast</b></i> or a subpackage, e.g. <i>clpython.ast.node:if-stmt</i>."
                 "Identifiers used in the Python code, on the other hand, are interned in package <i>clpython.<b>user</b></i>, like "
                 "<i>clpython.user::foo</i>.")
             (:p "Some of these symbols conflict with those from the <i>Common Lisp</i> package. For that reason they are not exported "
                 "from the <i>clpython</i> package. Nevertheless there is an easy way to refer to these symbols, namely by using a "
                 "handy " ((:a href "http://www.lisp.org/HyperSpec/Body/sec_2-1-1.html" class "external") "readtable") " defined by CLPython. "
                 "This allows writing <i>[foo]</i> to refer to <i>clpython.ast::foo</i>, and <i>{bar}</i> for <i>clpython.user::bar</i>. ")
             (:p "There are three such readtables: <i>*ast-readtable*</i> for the <i>{foo}</i> styntax, "
                 "<i>*user-readtable*</i> for the <i>[if-stmt]</i> syntax, and <i>*ast-user-readtable*</i> for both. "
                 "These readtables have been "
                 ((:a href "http://www.franz.com/support/documentation/8.0/doc/operators/excl/named-readtable.htm"
                      class "external") "named" )
                 " <i>:py-ast-readtable</i>, <i>:py-user-readtable</i> and <i>:py-ast-user-readtable</i>, "
                 "and these names can be used in the file mode line in Emacs source files:")
             ((:p style "font-size: small; position: relative; left: 5%")
              ";; -*- package: clpython; readtable: py-ast-user-readtable -*-<br>
\(in-package :clpython)<br>
\(in-syntax *ast-user-readtable*)"))

      #+(or)((h2-anchor #2#)
             (:p "The parser (function <i>parse</i>) translates Python source code into an abstract syntax tree. ")
             (:p "The pretty printer (function <i>py-pprint</i>) does the reverse: translating an AST into properly formatted Python source.
         The outputted source code is normalized:")
             ((:ul style "font-size: small")
              (:li "Comments (i.e. lines starting with `#') are currently not included in the AST,
thus the emitted source code will not contain them. (But docstrings, being regular strings, are included.)")
              (:li "String delimiters are by default single quotes: \"a\" will be printed as 'a'.")
              (:li "Brackets put around subexpresions are printed only if the priority of operations requires it: '(1 * 2) + 3' will be printed as '1 * 2 + 3'.")
              (:li "Whitespace is normalized: '1+2' is printed as '1 + 2'.")
              (:li "Indentation is normalized, to use multiples of "
                   (:princ clpython.parser::*tab-width-spaces*)
                   " characters, and no tabs.")
              (:li "Consecutive string constants are concatenated: \"x = 'a' 'b'\" will be printed as \"x = 'ab'\".")))
    
      (h2-anchor #4#)
      (:p "CLPython first translates Python code into an abstract syntax tree (AST), and then translates the AST into "
          "Lisp code. Most of the compilation work is carried out by macros:")
      ((:p style "font-size: small; position: relative; left: 5%")
       "if 4 > 3:<br>
  &nbsp; print 'y'<br>
else:<br>
&nbsp; print 'n'")
      (:p "...macroexpands into a <i>cond</i> form:")
      ((:p style "font-size: small; position: relative; left: 5%")
       "(cond<br>
 &nbsp; ((py-val-&gt;lisp-bool (py-&gt; 4 3))<br>
 &nbsp; &nbsp; (py-print nil (list \"y\") nil))<br>
 &nbsp; (t<br>
 &nbsp; &nbsp; (py-print nil (list \"n\") nil)))")
      (:p "...as defined by the macro:")
      ((:p style  "font-size: small; position: relative; left: 5%") 
       "(defmacro [if-stmt] (if-clauses else-clause)<br>
  &nbsp; `(cond ,@(loop for (cond body) in if-clauses<br>
  &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; collect `((py-val->lisp-bool ,cond) ,body))<br>
  &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; ,@(when else-clause <br>
  &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; `((t ,else-clause)))))")
      (:p "Note that the expansion contains calls to functions <i>py-val-&gt;lisp-bool</i>, <i>py-&gt;</i> and <i>py-print</i>. These function are part of CLPython \"runtime environment\", and not part of the generated code. CLPython must be loaded every time this code is executed."))

     ((:div style "width: 45%; float: right")
      (h2-anchor #55#)
      (:p "Sometimes the generated Python code can be simplified because values of expressions are known at compile time. "
          "This is where " ((:a href "http://www.lisp.org/HyperSpec/Body/sec_3-2-2-1.html" class "external") "compiler macros")
          " play a role. In the previous example, as 4 > 3 always holds, first the compiler macro for <i>py-&gt;</i> replaces "
          "<i>(py-&gt; 4 3)</i> by the Python value <i>True</i>. Then the compiler macro for <i>py-val-&gt;lisp-bool</i> "
          "sees <i>True</i> is a constant value, and replaces <i>(py-val-&gt;lisp-bool True)</i> by <i>t</i>. "
          "The Lisp compiler then deduces that always the first branch of the <i>if</i> expression is taken, "
          "and replace the whole <i>(cond ...)</i> by <i>(py-print nil (list \"y\") nil)</i>, which is a call to the <i>print</i> function "
          "in the CLPython runtime environment. (Actually there is a compiler macro for py-print too, which also optimizes certain cases.)")
      (:p "In this example the compiler macros were able to remove a lot of the Lisp code at compile time. In practice there is often not that much that can be decided at compile time, due to Python as language being very dynamic.
For example: in the expression <i>5 + x</i> the value of <i>x</i> can be anything. As classes are able to redefine how the <i>+</i> operator
behaves (with the <i>__add__</i> and <i>__radd__</i> methods), the value of <i>5 + x</i> can be anything as well.
Unless the context gives more information about the type of <i>x</i>, the Lisp code must contain a call to the generic addition function <i>py-+</i>.")
      (:p "Nevertheless, the compiler macro will inline \"common\" case, and make the generic call only for \"uncommon\" arguments.
If small integers are common for the <i>+</i> operator, the compiler macro for <i>py-+</i> could emit:")
      ((:p style "font-size: small; position: relative; left: 5%")
       "(if (typep x 'fixnum)<br>
  &nbsp; &nbsp;  (+ 5 x)<br>
  &nbsp; (py-+ 5 x))")
      (:p "The check for x being <i>fixnum</i> is very fast, as is the addition in that case. If x is not a <i>fixnum</i> it could another kind of number, or even a Pythonic object simulating "
          "numeric behavior. The generic <i>py-+</i> will handle those types, and raise an exception if addition fails.")
      (:p)
      (h2-anchor #5#)
      (:p "CLPython can run Python code in two Lisp modes, <i>interpreted</i> or <i>compiled</i>. In the latter case Lisp code is "
          "translated into assembly (or byte code, depending on the Lisp implementation). The advantage of interpreted code is "
          "that debugging is easier (the stack trace contains more information), but execution of compiled code "
          "is much faster. When a Python module is compiled, the functions are compiled into assembly code and written to an "
          "implementation-dependent <i>fasl</i> file.")
      (:p)))))


(defmacro with-package-details ((rel-name is-first perc details) &body body)
  `(let ((.pkgs (sort (excl:package-children :clpython.module) #'string<
                      :key #'package-name)))
     (loop for .p in .pkgs
         for ,is-first = t then nil
         do (multiple-value-bind (.summary .ratio ,details)
                (clpython::package-impl-status .p)
              (declare (ignore .summary))
              (let ((,perc (round (* 100 (or .ratio 0))))
                    (,rel-name (clpython::relative-package-name .p :clpython.module)))
                (declare (ignorable ,is-first ,details ,perc))
                ,@body)))))
(defmacro make-anchor-links (&rest strings)
   (assert strings)
   `((:p style "font-size: small")
     "[ " ,(anchor-link (car strings))
        ,@(loop for s in (cdr strings)
              collect " | "
              collect (anchor-link s))
        " ]"))

(defmacro h2-anchor (string)
  `((:h2 id ,(make-anchor-text string)) ,string))

(defmethod fill-page ((page (eql :status)))
  (with-page-template ((:page :status)
                       (:title "CLPython Status")
                       (:backlink t))
    (make-anchor-links #1="Language Features" #2="Modules" )

    (h2-anchor #1#)
    (:p "Almost all language features are implemented, including the <i>yield</i> statement (to create generators). Some new features, introduced with "
        ((:a href "http://docs.python.org/whatsnew/whatsnew25.html")
         "Python 2.5")
        ", have not been implemented yet. This is due to lack of time, not for technical reasons. 
The missing language features are:")
    (:ul
     (:li "Absolute and relative Imports: " (:pre "from .. import x"))
     (:li "Generators using <i>yield</i> as expression: " (:pre "y = (yield x)")))

    (h2-anchor #2#)
    (:p "The following modules of the "
        ((:a href "http://python.org/doc/current/modindex.html")
         "Python library")
        " have been implemented, to some degree:")

    (html (:table
           (with-package-details (rel-name is-first percentage details)
             (html (:tr (:td ((:a href (format nil "#~A" rel-name)) (:princ rel-name))
                             "&nbsp; &nbsp;")
                        (:td (:princ percentage)
                             "% "
                             (when is-first (html (:princ "completed "))))
                        )))))
    
    (html (:p "More specific, the implementation status per module is as follows:")
          ((:table style "font-size: small; border: 1px solid black; border-width: 1px 0px")
           (with-package-details (rel-name is-first percentage details)
             (loop with first-row = t
                 for (code . desc) in clpython::+impl-statuses+
                 for syms = (cdr (assoc code details))
                 when syms do
                   (let ((st (if (and (not is-first) first-row)
                                 "border-top: 1px dotted #999" "")))
                     (html ((:tr valign "top")
                            ((:td style st)
                             (when first-row (html (:br) ((:div id rel-name) (:princ rel-name)) "&nbsp;")))
                            (html ((:td style st)
                                   (when first-row
                                     (html (:br)))
                                   (:nobr "&nbsp;" (:princ desc) ":&nbsp;"))
                                  ((:td style st)
                                   (when first-row
                                     (html (:br)))
                                   (let (prev)
                                     (dolist (s (sort syms #'string<))
                                       (when prev
                                         ;; linebreaks between different sets of names
                                         (let ((first-chars (list (aref (string s) 0)
                                                                  (aref (string prev) 0))))
                                           (when (or (= 1 (count #\_ first-chars :test #'char=))
                                                     (= 1 (count-if #'upper-case-p first-chars)))
                                             (html (:br)))))
                                       (html (:princ s) "&nbsp; ")
                                       (setf prev s)))
                                   (:br) "&nbsp;"))))
                     (setf first-row nil))))))))
  


(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro func-entry (&rest symbols)
  (apply #'concatenate 'string
         (let (res)
           (dolist (s symbols (nreverse res))
             (cond ((listp s)
                    (push "<i>(</i>" res)
                    (let ((s (eval `(func-entry ,@s))))
                      (when (and (> (length s) 0)
                                 (char= (aref s (1- (length s))) #\Space))
                        (setf s (subseq s 0 (- (length s) 2))))
                      (push s res))
                    (push "<i>) </i>" res))
                   (t (push (case s
                              (:func "[Function] ")
                              (:macro "[Macro] ")
                              (:result " &nbsp;=> &nbsp;")
                              ((&key &optional &rest &body) (format nil "~A " s))
                              (t (if (and (member (first symbols) '(:func :macro))
                                          (eq s (second symbols)))
                                     (format nil "<b>~A</b> &nbsp;" s)
                                   (format nil "<i>~A</i> " s))))
                            res))))))))

(defmacro params (&rest data)
  `((:p class "params")
    ,@(loop for (k v) in data
          collect (format nil "<i>~A</i> &mdash; " k)
          collect v
          collect '(:br))))
             
(defmethod fill-page ((page (eql :dictionary)))
  (with-page-template ((:page :dictionary)
                       (:title "CLPython Dictionary"))
    
    (make-anchor-links #1="Parser" #2="Code walker" #201="Pretty Printer" #3="Compiler")

    #|
    (h2-anchor #102#)
    (:p "Abstract syntax trees are represented by a nested list of symbols, strings and numbers. The following symbols can occur in them:")
    (:ul (:li "<b>Reserved words</b>" (:br)
              "Package <i>clpython.ast.reserved</i>: &nbsp;" 
              #.(let (res)
                  (do-external-symbols (s :clpython.ast.reserved res)
                    (push (symbol-name s) res))
                  (setf res (sort res #'string<)) 
                  (format nil "<i>~{~A~^ ~}</i>" res))
              (:br)
              (:br))
         (:li "<b>Binary operators</b>" (:br)
              #.(let (res)
                  (do-external-symbols (s :clpython.ast.operator res)
                    (push (symbol-name s) res))
                  (format nil "Package <i>clpython.ast.operator</i>: ~{~A~^, ~}." res))
              (:br)))
              |#
    
    (h2-anchor #1#)
    (:ul ((:li class "dict")
          (:p (:princ (func-entry
                           :func parse-python-file
                           file &key tab-width-spaces incl-module include-line-numbers
                           :result ast))
                  (:br)
                  (:princ (func-entry
                           :func parse-python-string
                           string
                           &key tab-width-spaces incl-module include-line-numbers
                           :result ast)))
              (params
               (file "a pathname or a stream.")
               (string "a string.")
               (tab-width-spaces #.(concatenate 'string "a non-negative integer. "
                                                "The default is "
                                                (format nil "~A"
                                                        clpython.parser::*tab-width-spaces*)
                                                "."))
               (include-line-numbers "a boolean. The default is nil.")
               (ast "an abstract syntax tree."))
              
              (:p "<i>Parse-python-file</i> and <i>parse-python-string</i>
 return the result of parsing the given file or string, in the form of an abstract syntax tree.")
              (:p "When encountered as indentation, one <i>Tab</i> character will be treated
 as <i>tab-width-spaces</i> times a <i>Space</i> character. (This is only relevant if in
 the source the indentation consists of both spaces and tabs.)")
              (:p "If <i>include-line-numbers</i> is true, then the generated AST will contain
 tokens representing line numbers in the source. (This is intended for debuggers and source code
 coverage analyses.) Note that not every source line number will be included in the AST."))
         
         (:li (:p (:princ (func-entry
                           :macro with-python-code-reader
                           () &body body)))
              (:p "The macro <i>with-python-code-reader</i> sets up an environment in which
<i>*readtable*</i> is bound to a Python source code readtable, so that every call to <i>read</i>
in <i>body</i> will return a Python abstract syntax tree as result.")
              (:p "This macro is intended to be used for reading in Python source files or
interactively entered Python commands.")))
               

    (h2-anchor #2#)
    (:ul (:li (:p (:princ (func-entry :macro with-py-ast
                                      (subform ast &key
                                               build-result lists-only into-nested-namespaces)
                                      &body body)))
              (params
               (subform "a symbol <i>sym</i>, or the list <i>(sym &key clpython::value clpython::target)</i> with <i>sym</i> a symbol.")
               (ast "an abstract syntax tree.")
               (build-result "a boolean. The default is false.")
               (lists-only "a boolean. The default is true.")
               (into-nested-namespaces "a boolean. The default is false."))
              
              (:p "<i>Body</i> will be executed each time the code walker recurses into the <i>ast</i>; variable <i>sym</i> is bound to the current sub-AST.")
              (:p "If the list form of <i>subform</i> is used, then <i>value</i> and <i>target</i> are bound to the context in which the sub-AST <i>sym</i> is used:")
              (:ul (:li "<i>value</i> will be <i>true</i> if the sub-AST is used for its value (e.g. \"y\" in \"x = y\")")
                   (:li "<i>target</i> will be <i>true</i> if the sub-AST is used to store a value (e.g. \"x\" in \"x = y\")"))
              (:p "For statements, both <i>value</i> and <i>target</i> are usually false. For some expressions, both are true (e.g. \"x\" in \"x += 2\"). The possible values for <i>value</i> are
<i>+normal-value+</i>, <i>+augassign-value+</i> and <i>+no-value+</i>. For <i>target</i> they are
<i>+normal-target+</i>, <i>+delete-target+</i>, <i>+augassign-target+</i>, <i>+global-decl-target+</i> and
<i>+no-target+</i>.")
              (:p "The return values of <i>body</i> are used as follows:")
              (:ul (:li "The first value is the form that the code walker will recurse into next. Typically it is the subform to which <i>sym</i> is bound.")
                   (:li "The second value indicates whether the code walker should consider this recursion branch finished: if <i>true</i>, then the code walker will not recurse deeper into the form returned as first value; otherwise it will recurse in the first value returned (if possible);"))
              (:p "If <i>lists-only</i> is true, then the body will only be executed for subforms that are lists; this will skip numbers and strings.")
              (:p "If <i>into-nested-namespaces</i> is true, then recursion into nested namespaces will take place; otherwise recursion stops just before entering the nested namespace. (A nested namespace is the body of an inner function or inner class.)")
              (:p "If <i>build-result</i> is true, then on each invocation the (first) return value of <i>body</i> is collected. After walking, walk-py-ast returns the AST that is a copy of the original AST, except that every time when <i>body</i> returned a different subform than the value it got for <i>sym</i>, the form returned by <i>body</i> replaces the form of <i>sym</i> in the returned AST. (Think of it as a \"tree-replace\" function with special knowledge of the AST structure.)")))

    (h2-anchor #201#)
    (:ul (:li (:p (:princ (func-entry :func py-pprint
                                      (ast &optional stream))))
              (params (ast "an abstract syntax tree.")
                      (stream "a stream, or nil."))
              (:p "<i>Py-pprint</i> pretty-prints the ast as Python source code to <i>stream</i>.")
              (:p "If <i>stream</i> is not supplied or <i>nil</i>, the emitted source code is returned as a
 string. Otherwise, <i>stream</i> is treated as an <i>output stream designator</i>.")))
    (h2-anchor #3#)
    (:p "todo")
    (:p)))
