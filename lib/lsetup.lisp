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
  (:import-from :clpython #:py-val->string)
  (:shadow #:|array|)
  (:export #:|array|))

(defpackage :clpython.module.binascii
  (:use :clpython :common-lisp)
  (:export #:|hexlify|))

(defpackage :clpython.module.cStringIO
  (:use :common-lisp)
  (:export ))

(defpackage :clpython.module.errno
  (:use :common-lisp)
  (:export ))

(defpackage :clpython.module.exceptions
  (:use :common-lisp)
  (:export #:|ValueError| #:|SyntaxError|))

(defpackage :clpython.module.gc
  (:use :clpython :common-lisp)
  (:export #:|disable| )
  (:import-from :clpython #:*try-except-currently-handled-exception*))

(defpackage :clpython.module.imp
  (:use :clpython :common-lisp)
  (:export #:|get_magic| #:|find_module| #:|load_module|))

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

(defpackage :clpython.module.posix
  (:use :clpython :common-lisp)
  (:shadow #:|open| #:|read| #:|close| #:|remove| #:|error|)
  (:export #:|open| #:|read| #:|close| #:|remove| #:|unlink| #:|stat| #:|error|
           #:|O_RDONLY| #:|O_WRONLY| #:|O_RDWR| #:|O_NONBLOCK| #:|O_NDELAY| #:|O_APPEND|
           #:|O_SHLOCK| #:|O_EXLOCK| #:|O_SYNC| #:|O_NOFOLLOW| #:|O_CREAT| #:|O_TRUNC|
           #:|O_EXCL| #:|O_NOCTTY| #:|O_DIRECTORY|))

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

(defpackage :clpython.module.string
  (:use :clpython :common-lisp)
  (:export #:|ascii_letters| #:|ascii_lowercase| #:|ascii_uppercase| #:|digits|
           #:|hexdigits| #:|octdigits| #:|lowercase| #:|uppercase| #:|letters|
           #:|punctuation| #:|whitespace| #:|printable| #:|translate| #:|maketrans|)
  (:import-from :clpython.package +max-char-code+))

(defpackage :clpython.module.symbol
  (:use :clpython :common-lisp)
  (:export #:|Symbol| #:|Wild| #:|symbols|))

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

(defpackage :clpython.module.thread
  (:use :clpython :common-lisp)
  (:import-from :clpython #:def-py-method)
  (:export #:|allocate_lock| |start_new_thread|))

(defpackage :clpython.module.time
  (:use :clpython :common-lisp)
  (:shadow #:|sleep| #:|time|)
  (:export #:|clock| #:|sleep| #:|time|))
