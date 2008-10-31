;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(defpackage :clpython.module.array
  (:use :clpython :common-lisp)
  (:import-from :clpython
		#:py-val->string #:def-py-method
		#:py-iterate->lisp-list #:py-raise )
  (:shadow #:|array|)
  (:export #:|array|))

(defpackage :clpython.module.binascii
  (:use :clpython :common-lisp)
  (:export #:|hexlify|))

(defpackage :clpython.module.gc
  (:use :clpython :common-lisp)
  (:export #:|disable| )
  (:import-from :clpython #:*try-except-currently-handled-exception*))

(defpackage :clpython.module.math
  (:use :clpython :common-lisp)
  (:export #:|pi| #:|e|
           #:|pow| #:|exp| #:|log| #:|log10| #:|sqrt|
           #:|sin| #:|cos| #:|tan| #:|asin| #:|acos| #:|atan| #:|sinh| #:|cosh| #:|tanh| #:|atan2|
           #:|ceil| #:|floor| #:|degrees| #:|radians|
           #:|fmod| #:|fabs| #:|modf| #:|frexp| #:|hypot| #:|ldexp| )
  (:shadow #:|sin| #:|cos| #:|tan| #:|asin| #:|acos| #:|atan| #:|sinh| #:|cosh| #:|tanh|))

(defpackage :clpython.module.operator
  (:use :clpython :common-lisp)
  (:export) ;; symbols will be filled upon package loading
  (:shadow #:|eq| #:|mod| #:|index| #:|abs| #:|not|))

(defpackage :clpython.module.os
  (:use :clpython :common-lisp)
  (:export #:|name|
           #:|path|
           #:|error| #:|strerror|
           #:|confstr| #:|confstr_names| #:|getloadavg| #:|sysconf| #:|sysconf_names|
           #:|curdir| #:|pardir| #:|sep| #:|altsep| #:|extsep| #:|pathsep| #:|defpath|
           #:|linese| #:|devnull|
           #:|urandom|

           ;; OS functionality
           #:|environ| #:|getenv| #:|putenv| #:|unsetenv|
           #:|ctermid|
           #:|umask| #:|uname|
           
           #:|getegid| #:|geteuid| #:|getgid| #:|getgroups| #:|getlogin| #:|getpgid|
           #:|getpgrp| #:|getpid| #:|getppid| #:|getuid|
           #:|setegid| #:|seteuid| #:|setgroups| #:|setpgrp| #:|setpgid| #:|setreuid|
           #:|setregid| #:|getsid| #:|setsid| #:|setuid|

           #:|abort| #:|execl| #:|execle| #:|execlp| #:|execlpe| #:|execv| #:|execve|
           #:|execvp| #:|execvpe| #:|_exit| #:|fork| #:|forkpty| #:|kill| #:|killpg|
           #:|nice| #:|plock| #:|popen| #:|popen2| #:|popen3| #:|popen3| #:|popen4|
           #:|spawnl| #:|spawnle| #:|spawnlp| #:|spawnlpe| #:|spawnv| #:|spawnve|
           #:|spawnvp| #:|spawnvpe| #:|startfile| #:|system| #:|times| #:|wait|
           #:|waitpid| #:|wait3| #:|wait4|

           ;; File handling (14.1.1)
           #:|fdopen| #:|tmpfile|
           
           #:|close| #:|dup| #:|dup2| #:|fdatasync| #:|fpathconf| #:|fstat| #:|fstatvfs|
           #:|fsync| #:|ftruncate| #:|isattylseek| #:|open| #:|openpty| #:|pipe| #:|read|
           #:|tcgetpgrp| #:|tcsetpgrp| #:|ttyname| #:|write|
           
           #:|O_RDONLY| #:|O_WRONLY| #:|O_RDWR| #:|O_APPEND| #:|O_CREAT| #:|O_EXCL|
           #:|O_TRUNC| #:|O_DSYNC| #:|O_RSYNC| #:|O_SYNC| #:|O_NDELAY| #:|O_NONBLOCK|
           #:|O_NOCTTY| #:|O_SLOCK| #:|O_EXLOCK| #:|O_BINARY| #:|O_NOINHERIT|
           #:|O_TEMPORARY| #:|O_RANDOM| #:|O_SEQUENTIAL| #:|O_TEXT| #:|SEEK_SET|
           #:|SEEK_CUR| #:|SEEK_END|

           ;; 14.1.4 ...
           #:|chdir| #:|fchdir| #:|getcwd| #:|getcwdu| #:|chroot| #:|chmod| #:|chown|
           #:|lchown| #:|link| #:|listdir| #:|lstat| #:|mkfifo| #:|mknod| #:|major|
           #:|minor| #:|makedev| #:|mkdir| #:|makedirs| #:|pathconf| #:|pathconf_names|
           #:|readlink| #:|remove| #:|removedirs| #:|rename| #:|renames| #:|rmdir|
           #:|stat| #:|stat_float_times| #:|statvfs| #:|symlink| #:|tempnam| #:|tmpnam|
           #:|TMP_MAX| #:|unlink| #:|utime| #:|walk| ))

(defpackage :clpython.module._random
  (:use :clpython :common-lisp)
  (:export #:|Random|)
  (:import-from :clpython #:def-py-method))

(defpackage :clpython.module.re
  (:use :clpython :common-lisp)
  (:shadow #:|compile| #:|error| #:|search|)
  (:export #:|compile| #:|search| #:|match| #:|split| #:|findall|
           #:|finditer| #:|sub| #:|subn| #:|escape| #:|error|
           #:|I| #:|IGNORECASE|
           #:|L| #:|LOCALE|
           #:|M| #:|MULTILINE|
           #:|S| #:|DOTALL|
           #:|U| #:|UNICODE|
           #:|X| #:|VERBOSE|
           )
  (:import-from :clpython #:def-proxy-class #:def-py-method))

(defpackage :clpython.module.string
  (:use :clpython :common-lisp)
  (:export "ascii_letters" "ascii_lowercase" "ascii_uppercase" "digits"
           "hexdigits" "octdigits" "lowercase" "uppercase" "letters"
           "punctuation" "whitespace" "printable")
  (:import-from :clpython.package #:defconstant-once +max-char-code+))

(defpackage :clpython.module.symbol
  (:use :clpython :common-lisp))

(defpackage :clpython.module.sys
  (:use :clpython :common-lisp)
  (:export #:|argv| #:|byteorder| #:|exc_info| #:|copyright|
	   #:|builtin_module_names| #:|displayhook| #:|excepthook|
	   #:|__displayhook__| #:|exc_info| #:|__excepthook__| #:|exit| #:|exitfunc|
	   #:|setcheckinterval| #:|getcheckinterval|
	   #:|getdefaultencoding| #:|setdefaultencoding|
	   #:|getfilesystemencoding|
	   #:|getrecursionlimit| #:|setrecursionlimit|
	   #:|hexversion| #:|maxint|
	   #:|maxunicode| #:|modules| #:|path| #:|platform|
	   #:|prefix| #:|ps1| #:|ps2| #:|stdin| #:|stdout| #:|stderr|
	   #:|__stdin__| #:|__stdout__| #:|__stderr__|
	   #:|api_version| #:|version_info| #:|version|
           #:|warnoptions|)
  (:import-from :clpython #:*try-except-currently-handled-exception*))

(defpackage :clpython.module.time
  (:use :clpython :common-lisp)
  (:shadow #:|sleep| #:|time|)
  (:export #:|clock| #:|sleep| #:|time|))
