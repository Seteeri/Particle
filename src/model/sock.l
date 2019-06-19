(def AF-UNIX 1)
(def SOCK-STREAM 1)
(def SOCK-NONBLOCK (oct "04000"))
(def E-AGAIN 11)
(def E-WOULDBLOCK 11)
(def SHUT-RD 0)
(def SHUT-WR 1)
(def SHUT-RDWR 2)

#{
(defcstruct sockaddr
  (sa-family :ushort)
  (sa-data :char :count 14))

(defcstruct sockaddr-un
  (sun-family :ushort) ; sa_family_t = unsigned short int
  (sun-path :char :count 108))
}#

(de socket (Domain Type Protocol)
    (native "@" "socket" 'I Domain Type Protocol))

(de bind-sock (Sockfd Addr Addr-len)
    #(addr (:pointer sockaddr)) ; sockaddr_t
    (native "@" "bind" 'I Sockfd Addr Addr-len))

(de listen (Sockfd Backlog)
    (native "@" "listen" 'I Sockfd Backlog))

(de accept (Sockfd Addr Addr-len)
    (native "@" "accept" 'I Sockfd Addr Addr-len))

(de accept-4 (Sockfd Addr Addr-len Flags)
    (native "@" "accept4" 'I Sockfd Addr Addr-len Flags))

(de recv (Sockfd Buf Len Flags)
    (native "@" "recv" 'I Sockfd Buf Len Flags))

(de connect (Sockfd Addr Addr-len)
    (native "@" "connect" 'I Sockfd Addr Addr-len))

(de send-sock (Fd Buf N Flags)
    (native "@" "send" 'I Fd Buf N Flags))

(de shutdown (Socket How)
    (native "@" "shutdown" 'I Socket How))

#See os.l close-fd
(de close-sock (Filedes)
    (native "@" "close" 'I Filedes))

(de unlink (Pathname)
    (native "@" "unlink" 'I Pathname))
