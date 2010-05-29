;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE._SOCKET; Readtable: PY-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module._socket)
(in-syntax *user-readtable*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clpython:define-exception-subclass '{SocketError} '{Exception}))

(defconstant-once |error| (find-class '{SocketError}))

(defun |gethostname| ()
  #+allegro (excl.osi:gethostname)
  #-allegro (break "todo"))

(defun |gethostbyaddr| (address)
  (check-type address string)
  (let* ((hostname #+allegro (if (socket:dotted-address-p address)
                                 (excl.osi:gethostbyaddr address)
                               (excl.osi:gethostbyname address))
                   #-allegro (break "todo"))
         (alias-list (clpython:make-py-list-from-list ())) ;; todo
         (ip-addr-list (clpython:make-py-list-from-list (list #+allegro (socket:ipaddr-to-dotted (socket:lookup-hostname hostname))
                                                     #-allegro (break "todo")))))
    (make-tuple-from-list (list hostname alias-list ip-addr-list)))
        
  #-allegro (break "todo"))


;; Socket class

(defclass |socket| (clpython:object)
  ((address :accessor socket.address :initform nil)
   (socket :accessor socket.socket :initform nil)
   (stream :accessor socket.stream :initform nil :initarg :stream))
  (:metaclass clpython:py-type))

(def-py-method |socket.__doc__| :attribute (x)
  "Socket")

(def-py-method |socket.close| (x)
  (break "todo"))

(def-py-method |socket.accept| (x)
  #+allegro (let* ((stream (socket:accept-connection (socket.socket x) :wait t))
                   (remote-address (format nil "~A:~A" (socket:lookup-hostname (socket:remote-host stream)) (socket:remote-port stream))))
              (make-tuple-from-list (list (make-instance '|socket| :stream stream)
                                          remote-address)))
  #-allegro (break "todo"))

(def-py-method |socket.bind| (x address)
  (setf (socket.address x) address))

(def-py-method |socket.connect| (x address)
  (break "todo"))

(def-py-method |socket.connect_ex| (x address)
  (break "todo"))

(def-py-method |socket.fileno| (x)
  (break "todo"))

(def-py-method |socket.getpeername| (x)
  (break "todo"))

(def-py-method |socket.getsockname| (x)
  (whereas ((socket (socket.socket x)))
    (let ((hostname #+allegro (socket:ipaddr-to-dotted (socket:local-host socket))
                    #-allegro (break "todo"))
          (port #+allegro (socket:local-port socket)
                #-allegro (break "todo")))
      (return-from |socket.getsockname|
        (make-tuple-from-list (list hostname port)))))
   ;; default same as CPython does
  (make-tuple-from-list (list "0.0.0.0" 0)))

(def-py-method |socket.getsockopt| (x opt-name &optional buflen)
  (break "todo"))

(defparameter *all-sockets* ())

(def-py-method |socket.listen| (x backlog)
  (let ((address (socket.address x)))
    (assert (and (listp address)
                 (= (length address) 2)
                 (integerp (second address)))
        (address) "Cannot socket.listen() without an address: ~S." address)
    (let ((socket #+allegro (socket:make-socket :connect :passive :local-port (+ (second address) (random 100))) ;; XXX random
                  #-allegro (break "todo")))
      (push socket *all-sockets*)
      (setf (socket.socket x) socket))))

(def-py-method |socket.makefile| (x &optional mode bufsize)
  (break "todo"))

(def-py-method |socket.recv| (x bufsize &optional flags)
  (unless (socket.stream x)
    (break "Can't socket.recv(): no socket stream."))
  ;; todo: multiple bytes
  (read-byte (socket.stream x)))

(def-py-method |socket.recvfrom| (x bufsize &optional flags)
  (break "todo"))

(def-py-method |socket.recvfrom_into| (x buffer &optional nbytes flags)
  (break "todo"))

(def-py-method |socket.recv_into| (x buffer &optional nbytes flags)
  (break "todo"))

(def-py-method |socket.send| (x string &optional flags)
  (break "todo"))

(def-py-method |socket.sendall| (x string &optional flags)
  (break "todo"))

(def-py-method |socket.sendto| (x string &optional flags address)
  (break "todo"))

(def-py-method |socket.setblocking| (x flag)
  (break "todo"))

(def-py-method |socket.settimeout| (x value)
  (break "todo"))

(def-py-method |socket.gettimeout| (x)
  (break "todo"))

(def-py-method |socket.setsockopt| (x level optname value)
  (warn "socket.setsockopt(..) ignored"))

(def-py-method |socket.shutdown| (x how)
  (break "todo"))


;;; Constants

(defconstant |AF_UNSPEC| 0)
(defconstant |AF_UNIX| 1)
(defconstant |AF_INET| 2)
(defconstant |AF_DECnet| 12)
(defconstant |AF_APPLETALK| 16)
(defconstant |AF_SNA| 11)
(defconstant |AF_ROUTE| 17)
(defconstant |AF_IPX| 23)
(defconstant |AF_INET6| 30)

(defconstant |AI_PASSIVE| 1)
(defconstant |AI_CANONNAME| 2)
(defconstant |AI_NUMERICHOST| 4)
(defconstant |AI_MASK| 7)
(defconstant |AI_ALL| 256)
(defconstant |AI_V4MAPPED_CFG| 512)
(defconstant |AI_ADDRCONFIG| 1024)
(defconstant |AI_DEFAULT| 1536)
(defconstant |AI_V4MAPPED| 2048)

(defconstant |EAI_ADDRFAMILY| 1)
(defconstant |EAI_AGAIN| 2)
(defconstant |EAI_BADFLAGS| 3)
(defconstant |EAI_FAIL| 4)
(defconstant |EAI_FAMILY| 5)
(defconstant |EAI_MEMORY| 6)
(defconstant |EAI_NODATA| 7)
(defconstant |EAI_NONAME| 8)
(defconstant |EAI_SERVICE| 9)
(defconstant |EAI_SOCKTYPE| 10)
(defconstant |EAI_SYSTEM| 11)
(defconstant |EAI_BADHINTS| 12)
(defconstant |EAI_PROTOCOL| 13)
(defconstant |EAI_MAX| 14)

(defconstant |INADDR_ALLHOSTS_GROUP| -536870911)
(defconstant |INADDR_ANY| 0)
(defconstant |INADDR_BROADCAST| -1)
(defconstant |INADDR_LOOPBACK| 2130706433)
(defconstant |INADDR_MAX_LOCAL_GROUP| -536870657)
(defconstant |INADDR_NONE| -1)
(defconstant |INADDR_UNSPEC_GROUP| -536870912)

(defconstant |IPPORT_RESERVED| 1024)
(defconstant |IPPORT_USERRESERVED| 5000)

(defconstant |IPPROTO_HOPOPTS| 0)
(defconstant |IPPROTO_IP| 0)
(defconstant |IPPROTO_ICMP| 1)
(defconstant |IPPROTO_IGMP| 2)
(defconstant |IPPROTO_GGP| 3)
(defconstant |IPPROTO_IPIP| 4)
(defconstant |IPPROTO_IPV4| 4)
(defconstant |IPPROTO_TCP| 6)
(defconstant |IPPROTO_EGP| 8)
(defconstant |IPPROTO_PUP| 12)
(defconstant |IPPROTO_UDP| 17)
(defconstant |IPPROTO_IDP| 22)
(defconstant |IPPROTO_TP| 29)
(defconstant |IPPROTO_XTP| 36)
(defconstant |IPPROTO_IPV6| 41)
(defconstant |IPPROTO_ROUTING| 43)
(defconstant |IPPROTO_FRAGMENT| 44)
(defconstant |IPPROTO_RSVP| 46)
(defconstant |IPPROTO_GRE| 47)
(defconstant |IPPROTO_ESP| 50)
(defconstant |IPPROTO_AH| 51)
(defconstant |IPPROTO_ICMPV6| 58)
(defconstant |IPPROTO_NONE| 59)
(defconstant |IPPROTO_DSTOPTS| 60)
(defconstant |IPPROTO_HELLO| 63)
(defconstant |IPPROTO_ND| 77)
(defconstant |IPPROTO_EON| 80)
(defconstant |IPPROTO_PIM| 103)
(defconstant |IPPROTO_IPCOMP| 108)
(defconstant |IPPROTO_RAW| 255)
(defconstant |IPPROTO_MAX| 256)

(defconstant |IPV6_RTHDR_TYPE_0| 0)
(defconstant |IPV6_UNICAST_HOPS| 4)
(defconstant |IPV6_MULTICAST_IF| 9)
(defconstant |IPV6_MULTICAST_HOPS| 10)
(defconstant |IPV6_MULTICAST_LOOP| 11)
(defconstant |IPV6_JOIN_GROUP| 12)
(defconstant |IPV6_LEAVE_GROUP| 13)
(defconstant |IPV6_PKTINFO| 19)
(defconstant |IPV6_HOPLIMIT| 20)
(defconstant |IPV6_NEXTHOP| 21)
(defconstant |IPV6_HOPOPTS| 22)
(defconstant |IPV6_DSTOPTS| 23)
(defconstant |IPV6_RTHDR| 24)
(defconstant |IPV6_CHECKSUM| 26)
(defconstant |IPV6_V6ONLY| 27)

(defconstant |IP_DEFAULT_MULTICAST_LOOP| 1)
(defconstant |IP_DEFAULT_MULTICAST_TTL| 1)
(defconstant |IP_OPTIONS| 1)
(defconstant |IP_TOS| 3)
(defconstant |IP_TTL| 4)
(defconstant |IP_RECVOPTS| 5)
(defconstant |IP_RECVRETOPTS| 6)
(defconstant |IP_RECVDSTADDR| 7)
(defconstant |IP_RETOPTS| 8)
(defconstant |IP_HDRINCL| 2)
(defconstant |IP_MULTICAST_IF| 9)
(defconstant |IP_MULTICAST_TTL| 10)
(defconstant |IP_MULTICAST_LOOP| 11)
(defconstant |IP_ADD_MEMBERSHIP| 12)
(defconstant |IP_DROP_MEMBERSHIP| 13)
(defconstant |IP_MAX_MEMBERSHIPS| 20)

(defconstant |MSG_OOB| 1)
(defconstant |MSG_PEEK| 2)
(defconstant |MSG_DONTROUTE| 4)
(defconstant |MSG_EOR| 8)
(defconstant |MSG_TRUNC| 16)
(defconstant |MSG_CTRUNC| 32)
(defconstant |MSG_WAITALL| 64)
(defconstant |MSG_DONTWAIT| 128)

(defconstant |NI_NOFQDN| 1)
(defconstant |NI_NUMERICHOST| 2)
(defconstant |NI_NAMEREQD| 4)
(defconstant |NI_NUMERICSERV| 8)
(defconstant |NI_DGRAM| 16)
(defconstant |NI_MAXSERV| 32)
(defconstant |NI_MAXHOST| 1025)

(defconstant |SHUT_RD| 0)
(defconstant |SHUT_WR| 1)
(defconstant |SHUT_RDWR| 2)

(defconstant |SOCK_STREAM| 1)
(defconstant |SOCK_DGRAM| 2)
(defconstant |SOCK_RAW| 3)
(defconstant |SOCK_RDM| 4)
(defconstant |SOCK_SEQPACKET| 5)

(defconstant |SOL_IP| 0)
(defconstant |SOL_TCP| 6)
(defconstant |SOL_UDP| 17)
(defconstant |SOL_SOCKET| 65535)

(defconstant |SO_DEBUG| 1)
(defconstant |SO_ACCEPTCONN| 2)
(defconstant |SO_REUSEADDR| 4)
(defconstant |SO_KEEPALIVE| 8)
(defconstant |SO_DONTROUTE| 16)
(defconstant |SO_BROADCAST| 32)
(defconstant |SO_USELOOPBACK| 64)
(defconstant |SO_LINGER| 128)
(defconstant |SO_OOBINLINE| 256)
(defconstant |SO_REUSEPORT| 512)
(defconstant |SO_SNDBUF| 4097)
(defconstant |SO_RCVBUF| 4098)
(defconstant |SO_SNDLOWAT| 4099)
(defconstant |SO_RCVLOWAT| 4100)
(defconstant |SO_SNDTIMEO| 4101)
(defconstant |SO_RCVTIMEO| 4102)
(defconstant |SO_ERROR| 4103)
(defconstant |SO_TYPE| 4104)

(defconstant |TCP_NODELAY| 1)
(defconstant |TCP_MAXSEG| 2)
