(defpackage :clpython.module.os.process
  (:use :clpython :common-lisp)
  (:export #:|environ| #:|getenv| #:|putenv| #:|unsetenv|
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
           #:|waitpid| #:|wait3| #:|wait4| ))

(defpackage :clpython.module.os.file
  (:use :clpython :common-lisp)
  (:export ;; 14.1.1 
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

(defpackage :clpython.module.os
  (:use :clpython :common-lisp
        :clpython.module.os.process
        :clpython.module.os.file)
  (:export #:|name|
           #:|path|
           #:|error| #:|strerror|
           #:|confstr| #:|confstr_names| #:|getloadavg| #:|sysconf| #:|sysconf_names|
           #:|curdir| #:|pardir| #:|sep| #:|altsep| #:|extsep| #:|pathsep| #:|defpath|
           #:|linese| #:|devnull|
           #:|urandom|))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (user::cascade-external-symbols :clpython.module.os
                                  (set-difference (package-use-list :clpython.module.os)
                                                  '(:clpython :common-lisp)
                                                  :key #'package-name)))

(in-package :clpython)
(in-syntax *user-readtable*)

;;; OS
(in-package :clpython.module.os)

(defvar |name| "common lisp")
(defvar |error| (find-class '{OSError}))
(set-impl-status '(|name| |error|) t)

(set-impl-status '(|path|) :todo)

(in-package :clpython.module.os.process)

(in-package :clpython.module.os.file)

(do-external-symbols (s :clpython.module.os)
  (unless (clpython::impl-status s)
    (set-impl-status s :todo)))

