(in-package :protoform.libc)

(defun get-str-errno ()
  (sb-int:strerror (sb-alien:get-errno)))

;; RECHECK SIZE_T

(defcstruct sockaddr
  (sa-family :ushort)
  (sa-data :char :count 14))

(defcstruct sockaddr-un
  (sun-family :ushort) ; sa_family_t = unsigned short int
  (sun-path :char :count 108))

(defcfun ("socket" c-socket) :int
  (domain :int)
  (type :int)
  (protocol :int))

(defcfun ("bind" c-bind) :int
  (sockfd :int)
  (addr (:pointer sockaddr)) ; sockaddr_t
  (addr-len :int))

(defcfun ("listen" c-listen) :int
  (sockfd :int)
  (backlog :int))

(defcfun ("accept" c-accept) :int
  (sockfd :int)
  (addr :pointer)
  (addr-len (:pointer :int)))

(defcfun ("accept4" c-accept4) :int
  (sockfd :int)
  (addr :pointer)
  (addr-len (:pointer :int))
  (flags :int))

(defcfun ("recv" c-recv) :int ; ssize_t
  (sockfd :int)
  (buf :pointer)
  (len :int) ; size_t
  (flags :int))

(defcfun ("connect" c-connect) :int
  (sockfd :int)
  (addr :pointer)
  (addr-len :int))

(defcfun ("send" c-send) :int
  (fd :int)
  (buf :pointer)
  (n :int)
  (flags :int))  

(defcfun ("shutdown" c-shutdown) :int
  (socket :int)
  (how :int))

(defcfun ("close" c-close) :int
  (filedes :int))

(defcfun ("unlink" c-unlink) :int
  (pathname (:pointer :char)))

(defconstant +af-unix+ 1)
(defconstant +sock-stream+ 1)
(defconstant +sock-nonblock+ #o4000)
(defconstant +e-again+ 11)
(defconstant +e-wouldblock+ 11)
(defconstant +shut-rd+ 0)
(defconstant +shut-wr+ 1)
(defconstant +shut-rdwr+ 2)
