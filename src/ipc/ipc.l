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


#{

Pass in incomming handler functions:
* In
* Err

Pass in outgoing handler functions:
* In
* Err
* Poss specify address to address specific types

}#


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
    (=: listener          (when Path-listener (socket~listen-sock Path-listener Nonblock-listener)))
    (=: clients           ())
    
    #Poss to have multiple connections to different server
    #Diff between incoming connections and outgoing connections

    (=: nonblock-connection Nonblock-connection)
    (=: connection NIL)
    (when Path-connection
      (while (not (: connection))
	(=: connection          (socket~connect-sock Path-connection Nonblock-connection))
	#~1/144 seconds
	(wait 7))))


(dm end-ipc> ()

    (when (: buf-rd)     (native "@" "free" NIL (: buf-rd)))
    (when (: buf-wr)     (native "@" "free" NIL (: buf-wr)))  
    
    (when (: listener)   (socket~end (: listener)))
    (for Client (: clients)
	 (socket~end Client))
    
    (when (: connection) (socket~end (: listener))))

    
(dm handle-sock-listen> (Events Fd Ep-fd)
    (let (Sock (socket~accept-4 Fd 'block))

      (push (:: clients) Sock)
      
      (epoll~ctl-epfd Ep-fd
		      epoll~ADD
		      Sock
		      (| epoll~IN epoll~ERR epoll~HUP epoll~RDHUP))

      (prinl "Connect sock: " Sock " with " (| epoll~IN epoll~ERR epoll~HUP epoll~RDHUP))
      
      T))


(dm handle-sock-conn> (Events Fd Ep-fd)

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

	    (del Fd (:: clients))
	    
	    (epoll~ctl-epfd Ep-fd
			    epoll~DEL
			    Sock
			    0)

	    (socket~shutdown Fd socket~SHUT-RDWR)
	    (close Fd)
	    
	    (prinl "Disconnected sock: " Fd))
	  
	  (prog
	      (prinl "Read sock: " bytes-read " bytes" " = " (struct *buf-rd 'S))))))
