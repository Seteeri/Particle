(def 'AF-UNIX 1)
(def 'SOCK-STREAM 1)
(def 'SOCK-NONBLOCK (oct "04000"))
(def 'E-AGAIN 11)
(def 'E-WOULDBLOCK 11)
(def 'SHUT-RD 0)
(def 'SHUT-WR 1)
(def 'SHUT-RDWR 2)

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


(de connect-sock (Path
		  Nonblock)
    
    (let (Sock (init-socket Nonblock)
	  Sockaddr (init-sockaddr 1 Path))
      
      #{
      timeout:
        0 = return immediately
      > 0 = wait indefinitely with delay
      }#

      (let (Ret (connect-sockfd Sock
		                Sockaddr
				SOCKADDR-LEN))
	(when (= Ret -1)
	  (quit "(= connect-sockfd -1)")))
	
      (native "@" "free" NIL Sockaddr)
      
      Sock))


(de listen-sock (Path
		 Nonblock
		 Backlog)

    #{
    https://gavv.github.io/articles/unix-socket-reuse/
    
    A socket file is created by bind(2) call.
    If the file already exists, EADDRINUSE is returned.
    
    Unlike Internet sockets (AF_INET), Unix domain sockets (AF_UNIX) doesn’t have
    SO_REUSEADDR, at least on Linux and BSD. The only way to reuse a socket file
    is to remove it with unlink().

    There are two bad approaches to deal with this problem:

    #1
    
    We could call unlink() just before bind().

    The problem is that if we run two instances of our process, the second one will silently remove socket used by the first one, instead of reporting a failure.

    Also, there is a race here since the socket can be created by another process between unlink() and bind().

    We could call unlink() when the process exits instead.

    The problem is that if our process crashes, unlink() will not be called and we’ll have a dangling socket.

    #2

    Using a lock file

    One option is to use a lock file in addition to the socket file.

    We’ll use a separate lock file and never call unlink() on it. When a process is going to bind a socket, it first tries to acquire a lock:

    If the lock can’t be acquired, it means that another process is holding the lock now, because kernel guarantees that the lock is released if owner process exits or crashes.

    If the lock is successfully acquired, we can safely unlink() the socket, because we’re the only owner and no race may occur.

    #3
    Using abstract namespace sockets

    Another option is to use Linux-specific abstract namespace sockets.

    To create an abstract namespace socket, set the first byte in the sun_path field of the sockaddr_un to \0. See unix(7). This socket will not be mapped to the filesystem, so it’s not possible to use filesystem permissions or remove it with unlink().

    The advantage is that such a socket is automatically removed when the process exits, so there is no problem with socket reusing.
    
    }#
    
    (unlink Path)
    
    (let (Sock (init-socket Nonblock)
	  Sockaddr (init-sockaddr 1 Path))
      
      (let (Ret (bind-sockfd Sock
		             Sockaddr
		             SOCKADDR-LEN))
	(when (= Ret -1)
	  (quit "(= bind-sockfd -1)")))
      
      (let (Ret (listen-sockfd Sock Backlog))
	(when (= Ret -1)
	  (quit "(= bind-sockfd -1)")))
  
      (native "@" "free" NIL Sockaddr)
      
      Sock))

(de accept4-sock (Sock Nonblock)
    #{
    For nonblocking, will either return a valid fd (> 0)
    or it will fail and set errno to e-again or e-wouldblock
    Any other errno indicates failure somewhere else
    
    flags=0 - same as accept
    }#
    (let (Sockaddr     (native "@" "malloc" 'N 110)    #(init-sockaddr 1 "")
          Sockaddr-len (native "@" "malloc" 'N 4)) #socklen_t
	  
	  (struct Sockaddr-len 'N (cons SOCKADDR-LEN 4))
	  
	  (let (Ret (accept-4 Sock
		      Sockaddr
		      Sockaddr-len
		      (if (= Nonblock 'nonblock)
			  SOCK-NONBLOCK
			  0)))
	    
	    (native "@" "free" NIL Sockaddr)
	    Ret)))
