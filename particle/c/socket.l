(symbols 'sock 'pico)

(local) (bind listen acpt connect send)

(def 'AF-UNIX 1)
(def 'SOCK-STREAM 1)
(def 'SOCK-NONBLOCK (oct "04000"))
(def 'E-AGAIN 11)
(def 'E-WOULDBLOCK 11)
(def 'SHUT-RD 0)
(def 'SHUT-WR 1)
(def 'SHUT-RDWR 2)

(def 'MSG-OOB      (hex "01"))   # process out-of-band data
(def 'MSG-PEEK     (hex "02"))   # peek at incoming message
(def 'MSG-DONTWAIT (hex "40"))
(def 'MSG-WAITALL  (hex "100"))


(def 'SOCKADDR-LEN 110)
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

(de bind (Sockfd Addr Addr-len)
    #(addr (:pointer sockaddr)) ; sockaddr_t
    (native "@" "bind" 'I Sockfd Addr Addr-len))

#(de listen (Sockfd Backlog)
#    (native "@" "listen" 'I Sockfd Backlog))

#(de acpt (Sockfd Addr Addr-len)
#    (native "@" "acpt" 'I Sockfd Addr Addr-len))

(de accept-4 (Sockfd Addr Addr-len Flags)
    (native "@" "accept4" 'I Sockfd Addr Addr-len Flags))

(de connect (Sockfd Addr Addr-len)
    (native "@" "connect" 'I Sockfd Addr Addr-len))

(de recv (Sockfd Buf Len Flags)
    (native "@" "recv" 'I Sockfd Buf Len Flags))

(de send (Fd Buf N Flags)
    (native "@" "send" 'I Fd Buf N Flags))

(de shutdown (Socket How)
    (native "@" "shutdown" 'I Socket How))

(de unlink (Pathname)
    (native "@" "unlink" 'I Pathname))

###############################

(de init-sockaddr (Family Path)
  #{
    (sun-family :ushort) #sa_family_t = unsigned short int
    (sun-path :char :count 108)
  }#

  (let Ptr (native "@" "malloc" 'N (+ 2 108))

    (struct Ptr 'N
      (cons Family 2) #1=LOCAL_SOCKET, 2 bytes
      (cons Path 108))

    Ptr))


#Function for typical scenarios
(de init-socket (Block)
  (let Sock (socket AF-UNIX
                    (if Block (| SOCK-STREAM) (| SOCK-STREAM SOCK-NONBLOCK))
                     0)
    (when (= Sock -1) "(= socket -1)")
  Sock))


(de connect-sock (Path
                  Block
                  Retry)

  (let (Sock     (init-socket Block)
        Sockaddr (init-sockaddr 1 Path))

    #{
      Retry:
      >N= Delay
      <0 = Once
    }#

    (if (> Retry 0)

        (until (=0 (connect Sock
                            Sockaddr
                            SOCKADDR-LEN))
          (wait Retry))

        (when (= -1 (connect Sock
                             Sockaddr
                             SOCKADDR-LEN))
          (native "@" "close" NIL Sock)
          (off Sock)))

    (native "@" "free" NIL Sockaddr)

    Sock))


(de listen (Path
            Block
            Backlog)

  # See notes about unlink
  (unlink Path)

  (let (Sock     (init-socket Block)
        Sockaddr (init-sockaddr 1 Path))

    (let Ret (bind Sock
                   Sockaddr
                   SOCKADDR-LEN)

      (when (= Ret -1) "(= bind -1)"))

    (let Ret (native "@" "listen" 'I Sock Backlog)
      (when (= Ret -1) "(= bind -1)"))

    (native "@" "free" NIL Sockaddr)

    Sock))


(de acpt (Sock Block)
  #{
    For nonblocking, will either return a valid fd (> 0)
    or it will fail and set errno to e-again or e-wouldblock
    Any other errno indicates failure somewhere else

    flags=0 - same as acpt
  }#

  (let (Sockaddr     (native "@" "malloc" 'N 110)
        Sockaddr-Len (native "@" "malloc" 'N 4))  #socklen_t

    (struct Sockaddr-Len 'N (cons SOCKADDR-LEN 4))

    (let Ret (accept-4 Sock
                        Sockaddr
                        Sockaddr-Len
                        (if Block 0 SOCK-NONBLOCK))

      (native "@" "free" NIL Sockaddr)
      (native "@" "free" NIL Sockaddr-Len)

      Ret)))


(de end (Sock)
  (shutdown Sock SHUT-RDWR)
  (native "@" "close" NIL Sock))


# single-str: (in '(ls) (till NIL T))
(de get-rmem-def ()
  (in '("cat" "/proc/sys/net/core/rmem_default")
      (until (eof)
        (format (line T)))))

(de get-wmem-def ()
  (in '("cat" "/proc/sys/net/core/wmem_default")
      (until (eof)
        (format (line T)))))
