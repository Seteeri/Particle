(def 'AF-UNIX 1)
(def 'SOCK-STREAM 1)
(def 'SOCK-NONBLOCK (oct "04000"))
(def 'E-AGAIN 11)
(def 'E-WOULDBLOCK 11)
(def 'SHUT-RD 0)
(def 'SHUT-WR 1)
(def 'SHUT-RDWR 2)

(def 'ADDR-LEN 110)
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

(de bind-sockfd (Sockfd Addr Addr-len)
    #(addr (:pointer sockaddr)) ; sockaddr_t
    (native "@" "bind" 'I Sockfd Addr Addr-len))

(de listen-sockfd (Sockfd Backlog)
    (native "@" "listen" 'I Sockfd Backlog))

(de accept-sockfd (Sockfd Addr Addr-len)
    (native "@" "accept" 'I Sockfd Addr Addr-len))

(de accept-4 (Sockfd Addr Addr-len Flags)
    (native "@" "accept4" 'I Sockfd Addr Addr-len Flags))

(de recv (Sockfd Buf Len Flags)
    (native "@" "recv" 'I Sockfd Buf Len Flags))

(de connect-sockfd (Sockfd Addr Addr-len)
    (native "@" "connect" 'I Sockfd Addr Addr-len))

(de send-fd (Fd Buf N Flags)
    (native "@" "send" 'I Fd Buf N Flags))

(de shutdown (Socket How)
    (native "@" "shutdown" 'I Socket How))

#See os.l close-fd
(de close-filedes (Filedes)
    (native "@" "close" 'I Filedes))

(de unlink (Pathname)
    (native "@" "unlink" 'I Pathname))

###############################

(de init-sockaddr (Family Path)
    #{
    (sun-family :ushort) #sa_family_t = unsigned short int
    (sun-path :char :count 108)
    }#
    (let (Ptr (native "@" "malloc" 'N (+ 2 108)))
      (struct Ptr 'N
	      (cons Family 2) #1=LOCAL_SOCKET, 2 bytes
	      (cons Path 108))
      (println (struct Ptr '(I)))
      Ptr))

#{
(prog
    (setq P (native "@" "malloc" 'N (+ 2 108)))
   (struct P 'N (cons 1 2) (cons "/tmp/protoform-render.socket" 108))
   (struct P '(B . 110)))
(struct P '((B . 2) (C . 108)))
}#


#Function for typical scenarios
(de init-socket (Nonblock)
    (let (Sock (socket AF-UNIX
		 (if (= Nonblock 'nonblock)
		     (| SOCK-STREAM SOCK-NONBLOCK)
	             (| SOCK-STREAM))
		 0))
    (when (= Sock -1)
      (quit "(= socket -1)"))
    Sock))


(de init-connection (Path
		     Nonblock)
    
    (let (Sock (init-socket Nonblock)
	  Sockaddr (init-sockaddr 1 Path))
      
      #{
      ;; timeout
      ;; 0 = return immediately
      ;; > 0 = wait indefinitely with delay
      ;;
      ;; Can connect block?
      }#

      (let (Ret (connect-sockfd Sock
		  Sockaddr
		  ADDR-LEN))

	(when (= Ret -1)
	  (quit "(= connect-sockfd -1)"))
	
	(native "@" "free" NIL Sockaddr)
	
	Sock)))
