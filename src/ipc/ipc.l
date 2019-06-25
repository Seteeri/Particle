#{

IPC is a pair of sockets
- Client
- Server

}#

(symbols 'ipc 'pico)


# C library paths


# Set math precision to 6 decimals
(scl 6)


# Wrapper libraries
(load "/home/user/quicklisp/local-projects/protoform/src/c/epoll.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/socket.l")


(class +IPC)
(dm T (Path-listener   Nonblock-listener
       Path-connection Nonblock-connection)

    #Setup socket data

    #Only need 1 pair for all sockets
    #Since each buffer can be simultaneously used in an instance
    (=: sz-buf-rd (socket~read-rmem-default))
    (=: buf-rd    (native "@" "malloc" 'N *buf-sz))
    (=: sz-buf-wr (socket~read-wmem-default))
    (=: buf-wr    (native "@" "malloc" 'N *buf-sz))

    (=: nonblock-listener Nonblock-listener)
    (=: listener          (socket~listen-sock Path-listener Nonblock-listener))
    
    (=: nonblock-connection Nonblock-connection)
    (=: connection          (when Path-connection (socket~connect-sock Path-connection Nonblock-connection))))


(dm end-ipc> ()

    (when (: buf-rd)     (native "@" "free" NIL (: buf-rd)))
    (when (: buf-wr)     (native "@" "free" NIL (: buf-wr)))  

    (when (: listener)   (socket~end (: listener)))
    (when (: connection) (socket~end (: listener))))


(de handle-sock-conn (Events Fd Ep-fd)

    #(when (> (recv Fd *buf-rd *buf-sz 0) 0)
    #  #(println "RECV: " (struct *buf-rd '(B B B B B B B B)))
    #  (println "RECV: " (struct *buf-rd 'S)))
    #(let (bytes-read (native "@" "read" Fd *buf-rd *buf-sz)))
    
    (let (bytes-read (socket~recv Fd *buf-rd *buf-sz 0))
      
      (if (= bytes-read 0)
	  
	  #If epoll saying data to read, however recv is 0
	  #that indicates socket hung-up or errored out
	  (when (or (= (& Events epoll~ERR) epoll~ERR)
		    (= (& Events epoll~HUP) epoll~HUP)
		    (= (& Events epoll~RDHUP) epoll~RDHUP))
	    
	    (epoll~ctl-epfd Ep-fd
			    epoll~DEL
			    Sock
			    0)
	    (socket~shutdown Fd socket~SHUT-RDWR)
	    #Use builtin close
	    (close Fd)
	    (prinl "Disconnected sock: " Fd))
	  
	  (prog
	      (prinl "Read sock: " bytes-read " bytes" " = " (struct *buf-rd 'S))))))

    
(de handle-sock-listen (Events Fd Ep-fd)
    (let (Sock (socket~accept-4 Fd 'block)
          Msg "Welcome User")
      
      #Add sock to epoll
      (epoll~ctl-epfd Ep-fd
		      epoll~ADD
		      Sock
		      (| epoll~IN epoll~ERR epoll~HUP epoll~RDHUP))

      (prinl "Connect sock: " Sock " with " (| epoll~IN epoll~ERR epoll~HUP epoll~RDHUP))
      
      T))
