;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(defpackage :clpython.module.__builtin__
  (:use :clpython)
  (:export #:|basestring| #:|bool| #:|buffer| #:|classmethod| #:|complex| #:|dict|
           #:|enumerate| #:|file| #:|float| #:|frozenset| #:|int| #:|list| #:|long|
           #:|number| #:|object| #:|property| #:|slice| #:|set| #:|staticmethod| #:|str|
           #:|super| #:|tuple| #:|type| #:|unicode| #:|xrange|
           
           #:|__import__| #:|abs| #:|all| #:|any| #:|apply| #:|callable| #:|chr| #:|cmp|
           #:|coerce| #:|compile| #:|delattr| #:|dir| #:|divmod| #:|eval| #:|execfile|
           #:|filter| #:|getattr| #:|globals| #:|hasattr| #:|hash| #:|hex| #:|id| #:|input|
           #:|intern| #:|isinstance| #:|issubclass| #:|iter| #:|len| #:|map| #:|max|
           #:|min| #:|oct| #:|open| #:|ord| #:|pow| #:|range| #:|raw| #:|reduce| #:|reload|
           #:|repr| #:|round| #:|setattr| #:|sorted| #:|sum| #:|unichr| #:|vars| #:|zip| 
           
           . #.(loop for class in clpython::*exception-classes*
                 collect (class-name class))
           ))
  
(defpackage :clpython.module._ast
  (:use :clpython :common-lisp))

(defpackage :clpython.module.array
  (:use :clpython :common-lisp)
  (:import-from :clpython #:py-val->string)
  (:shadow #:|array|)
  (:export #:|array|))

(defpackage :clpython.module._bsddb
  (:use :clpython :common-lisp))

(defpackage :clpython.module.binascii
  (:use :clpython :common-lisp)
  (:export #:|hexlify|))

(defpackage :clpython.module._codecs
  (:use :clpython :common-lisp)
  (:export #:|lookup_error| #:|register|))

(defpackage :clpython.module._csv
  (:use :clpython :common-lisp))

(defpackage :clpython.module._collections
  (:use :common-lisp :clpython)
  (:export #:|deque| #:|defaultdict|))

(defpackage :clpython.module.cStringIO
  (:use :common-lisp)
  (:export ))

(defpackage :clpython.module.datetime
  (:use :clpython :common-lisp))

(defpackage :clpython.module.errno
  (:use :common-lisp)
  (:export ))

(defpackage :clpython.module.exceptions
  (:use :common-lisp)
  (:export #:|ValueError| #:|SyntaxError|))

(defpackage :clpython.module._functools
  (:use :clpython :common-lisp)
  (:shadow #:|reduce|)
  (:export #:|partial| #:|reduce|))

(defpackage :clpython.module._fileio
  (:use :clpython :common-lisp))

(defpackage :clpython.module.fcntl
  (:use :clpython :common-lisp))

(defpackage :clpython.module.gc
  (:use :clpython :common-lisp)
  (:export #:|disable| ))

(defpackage :clpython.module.imp
  (:use :clpython :common-lisp)
  (:export #:|get_magic| #:|find_module| #:|load_module|))

(defpackage :clpython.module.itertools
  (:use :clpython :common-lisp)
  (:export #:|count| #:|ifilter| #:|imap| #:|izip| #:|islice| #:|repeat| #:|tee|))

(defpackage :clpython.module._lsprof
  (:use :clpython :common-lisp))

(defpackage :clpython.module._md5
  (:use :clpython :common-lisp)
  (:export #:|new|))

(defpackage :clpython.module.marshal
  (:use :clpython :common-lisp)
  (:export #:|dump| #:|load| #:|dumps| #:|loads| #:|version|))

(defpackage :clpython.module.math
  (:use :clpython :common-lisp)
  (:export #:|pi| #:|e|
           #:|pow| #:|exp| #:|log| #:|log10| #:|sqrt|
           #:|sin| #:|cos| #:|tan| #:|asin| #:|acos| #:|atan| #:|sinh| #:|cosh| #:|tanh| #:|atan2|
           #:|ceil| #:|floor| #:|degrees| #:|radians|
           #:|fmod| #:|fabs| #:|modf| #:|frexp| #:|hypot| #:|ldexp| #:|sqrt|)
  (:shadow #:|sin| #:|cos| #:|tan| #:|asin| #:|acos| #:|atan| #:|sinh| #:|cosh| #:|tanh| #:|log| #:|exp| #:|sqrt|))

(defpackage :clpython.module.operator
  (:use :clpython :common-lisp)
  (:export) ;; symbols will be filled upon package loading
  (:shadow #:|eq| #:|mod| #:|index| #:|abs| #:|not|))

(defpackage :clpython.module.posix
  (:use :clpython :common-lisp)
  (:shadow #:|open| #:|read| #:|close| #:|remove| #:|error|)
  (:export #:|open| #:|read| #:|close| #:|remove| #:|unlink| #:|stat| #:|error|
           #:|O_RDONLY| #:|O_WRONLY| #:|O_RDWR| #:|O_NONBLOCK| #:|O_NDELAY| #:|O_APPEND|
           #:|O_SHLOCK| #:|O_EXLOCK| #:|O_SYNC| #:|O_NOFOLLOW| #:|O_CREAT| #:|O_TRUNC|
           #:|O_EXCL| #:|O_NOCTTY| #:|O_DIRECTORY|))

(defpackage :clpython.module.pwd
  (:use :clpython :common-lisp))

(defpackage :clpython.module._random
  (:use :clpython :common-lisp)
  (:export #:|Random|))

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
  (:import-from :clpython #:def-proxy-class))

(defpackage :clpython.module._sha
  (:use :clpython :common-lisp)
  (:export #:|new|))

(defpackage :clpython.module._sha256
  (:use :clpython :common-lisp)
  (:export #:|sha256| #:|sha224|))

(defpackage :clpython.module._sha512
  (:use :clpython :common-lisp)
  (:export #:|sha512| #:|sha384|))

(defpackage :clpython.module._socket
  (:use :clpython :common-lisp)
  (:shadow #:|error|)
  (:export #:|error| #:|socket| #:|gethostname| #:|gethostbyaddr|
           #:|AF_UNSPEC| #:|AF_UNIX| #:|AF_INET| #:|AF_DECnet| #:|AF_APPLETALK| #:|AF_SNA| #:|AF_ROUTE|
           #:|AF_IPX| #:|AF_INET6|
           #:|AI_PASSIVE| #:|AI_CANONNAME| #:|AI_NUMERICHOST| #:|AI_MASK| #:|AI_ALL| #:|AI_V4MAPPED_CFG|
           #:|AI_ADDRCONFIG| #:|AI_DEFAULT| #:|AI_V4MAPPED|
           #:|EAI_ADDRFAMILY| #:|EAI_AGAIN| #:|EAI_BADFLAGS| #:|EAI_FAIL| #:|EAI_FAMILY| #:|EAI_MEMORY| #:|EAI_NODATA|
           #:|EAI_NONAME| #:|EAI_SERVICE| #:|EAI_SOCKTYPE| #:|EAI_SYSTEM| #:|EAI_BADHINTS| #:|EAI_PROTOCOL| #:|EAI_MAX|
           #:|INADDR_ALLHOSTS_GROUP| #:|INADDR_ANY| #:|INADDR_BROADCAST| #:|INADDR_LOOPBACK| #:|INADDR_MAX_LOCAL_GROUP|
           #:|INADDR_NONE| #:|INADDR:_UNSPEC_GROUP| #:|IPPORT_RESERVED| #:|IPPORT_USERRESERVED|
           #:|IPPROTO_HOPOPTS| #:|IPPROTO_IP| #:|IPPROTO_ICMP| #:|IPPROTO_IGMP| #:|IPPROTO_GGP| #:|IPPROTO_IPIP|
           #:|IPPROTO_IPV4| #:|IPPROTO_TCP| #:|IPPROTO_EGP| #:|IPPROTO_PUP| #:|IPPROTO_UDP| #:|IPPROTO_IDP|
           #:|IPPROTO_TP| #:|IPPROTO_XTP| #:|IPPROTO_IPV6| #:|IPPROTO_ROUTING| #:|IPPROTO_FRAGMENT| #:|IPPROTO_RSVP|
           #:|IPPROTO_GRE| #:|IPPROTO_ESP| #:|IPPROTO_AH| #:|IPPROTO_ICMPV6| #:|IPPROTO_NONE| #:|IPPROTO_DSTOPTS|
           #:|IPPROTO_HELLO| #:|IPPROTO_ND| #:|IPPROTO_EON| #:|IPPROTO_PIM| #:|IPPROTO_IPCOMP| #:|IPPROTO_RAW|
           #:|IPPROTO_MAX|
           #:|IPV6_RTHDR_TYPE_0| #:|IPV6_UNICAST_HOPS| #:|IPV6_MULTICAST_IF| #:|IPV6_MULTICAST_HOPS| #:|IPV6_MULTICAST_LOOP| 
           #:|IPV6_JOIN_GROUP| #:|IPV6_LEAVE_GROUP| #:|IPV6_PKTINFO| #:|IPV6_HOPLIMIT| #:|IPV6_NEXTHOP| #:|IPV6_HOPOPTS| 
           #:|IPV6_DSTOPTS| #:|IPV6_RTHDR| #:|IPV6_CHECKSUM| #:|IPV6_V6ONLY|
           #:|IP_DEFAULT_MULTICAST_LOOP| #:|IP_DEFAULT_MULTICAST_TTL| #:|IP_OPTIONS| #:|IP_TOS| #:|IP_TTL| #:|IP_RECVOPTS| 
           #:|IP_RECVRETOPTS| #:|IP_RECVDSTADDR| #:|IP_RETOPTS| #:|IP_HDRINCL| #:|IP_MULTICAST_IF| #:|IP_MULTICAST_TTL| 
           #:|IP_MULTICAST_LOOP| #:|IP_ADD_MEMBERSHIP| #:|IP_DROP_MEMBERSHIP| #:|IP_MAX_MEMBERSHIPS|
           #:|MSG_OOB| #:|MSG_PEEK| #:|MSG_DONTROUTE| #:|MSG_EOR| #:|MSG_TRUNC| #:|MSG_CTRUNC| #:|MSG_WAITALL| #:|MSG_DONTWAIT|
           #:|NI_NOFQDN| #:|NI_NUMERICHOST| #:|NI_NAMEREQD| #:|NI_NUMERICSERV| #:|NI_DGRAM| #:|NI_MAXSERV| #:|NI_MAXHOST|
           #:|SHUT_RD| #:|SHUT_WR| #:|SHUT_RDWR| 
           #:|SOCK_STREAM| #:|SOCK_DGRAM| #:|SOCK_RAW| #:|SOCK_RDM| #:|SOCK_SEQPACKET| 
           #:|SOL_IP| #:|SOL_TCP| #:|SOL_UDP| #:|SOL_SOCKET| 
           #:|SO_DEBUG| #:|SO_ACCEPTCONN| #:|SO_REUSEADDR| #:|SO_KEEPALIVE| #:|SO_DONTROUTE| #:|SO_BROADCAST|
           #:|SO_USELOOPBACK| #:|SO_LINGER| #:|SO_OOBINLINE| #:|SO_REUSEPORT| #:|SO_SNDBUF| #:|SO_RCVBUF|
           #:|SO_SNDLOWAT| #:|SO_RCVLOWAT| #:|SO_SNDTIMEO| #:|SO_RCVTIMEO| #:|SO_ERROR| #:|SO_TYPE| 
           #:|TCP_NODELAY| #:|TCP_MAXSEG|))

(defpackage :clpython.module._sre
  (:use :clpython :common-lisp))

(defpackage :clpython.module._ssl
  (:use :clpython :common-lisp)
  (:export #:|SSLError| #:|CERT_NONE| #:|CERT_OPTIONAL| #:|CERT_REQUIRED|
           #:|PROTOCOL_SSLv2| #:|PROTOCOL_SSLv3| #:|PROTOCOL_SSLv23| #:|PROTOCOL_TLSv1|
           #:|SSL_ERROR_SSL| #:|SSL_ERROR_WANT_READ| #:|SSL_ERROR_WANT_WRITE| #:|SSL_ERROR_WANT_X509_LOOKUP|
           #:|SSL_ERROR_SYSCALL| #:|SSL_ERROR_ZERO_RETURN| #:|SSL_ERROR_WANT_CONNECT| #:|SSL_ERROR_EOF|
           #:|SSL_ERROR_INVALID_ERROR_CODE|
           #:|RAND_status| #:|RAND_egd| #:|RAND_add|
           ))

(defpackage :clpython.module._struct
  (:use :clpython :common-lisp)
  (:export #:|_clearcache| #:|__doc__|))

(defpackage :clpython.module._symtable
  (:use :clpython :common-lisp))

(defpackage :clpython.module.select
  (:use :clpython :common-lisp))

(defpackage :clpython.module.signal
  (:use :clpython :common-lisp))

(defpackage :clpython.module.string
  (:use :clpython :common-lisp)
  (:export #:|ascii_letters| #:|ascii_lowercase| #:|ascii_uppercase| #:|digits|
           #:|hexdigits| #:|octdigits| #:|lowercase| #:|uppercase| #:|letters|
           #:|punctuation| #:|whitespace| #:|printable| #:|translate| #:|maketrans|
           #:|replace|)
  (:import-from :clpython.util #:+max-char-code+)
  (:shadow #:|replace|))

(defpackage :clpython.module.symbol
  (:use :clpython :common-lisp)
  (:export #:|Symbol| #:|Wild| #:|symbols|))

(defpackage :clpython.module.sys
  (:use :clpython :common-lisp)
  (:export #:|argv| #:|byteorder| #:|exc_info| #:|exc_clear| #:|copyright|
	   #:|builtin_module_names| #:|displayhook| #:|excepthook|
	   #:|__displayhook__| #:|exc_info| #:|__excepthook__| #:|exit| #:|exitfunc|
           #:|flags| #:|_getframe|
	   #:|setcheckinterval| #:|getcheckinterval|
	   #:|getdefaultencoding| #:|setdefaultencoding|
	   #:|getfilesystemencoding|
	   #:|getrecursionlimit| #:|setrecursionlimit|
	   #:|hexversion| #:|maxint|
	   #:|maxunicode| #:|modules| #:|path| #:|platform|
	   #:|prefix| #:|ps1| #:|ps2| #:|stdin| #:|stdout| #:|stderr|
	   #:|__stdin__| #:|__stdout__| #:|__stderr__|
	   #:|api_version| #:|version_info| #:|version|
           #:|warnoptions| #:|py3kwarning|)
  (:import-from :clpython #:*last-raised-exception*))

(defpackage :clpython.module.termios
  (:use :clpython :common-lisp))

(defpackage :clpython.module.thread
  (:use :clpython :common-lisp)
  (:import-from :clpython #:def-py-method)
  (:export #:|allocate_lock| #:|start_new_thread| #:|get_ident| #:|error|))

(defpackage :clpython.module.time
  (:use :clpython :common-lisp)
  (:shadow #:|sleep| #:|time|)
  (:export #:|clock| #:|sleep| #:|time|))

(defpackage :clpython.module.unicodedata
  (:use :clpython :common-lisp))

(defpackage :clpython.module._weakref
  (:use :clpython :common-lisp))

(defpackage :clpython.module.zlib
  (:use :clpython :common-lisp))
