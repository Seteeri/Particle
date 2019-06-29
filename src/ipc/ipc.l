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

    (=: debug-ipc T)

    #Setup socket data

    #Only need 1 pair for all sockets
    #Since each buffer can be simultaneously used in an instance
    (=: sz-buf-rd (socket~read-rmem-default))
    (=: buf-rd    (native "@" "malloc" 'N (: sz-buf-rd)))
    (=: sz-buf-wr (socket~read-wmem-default))
    (=: buf-wr    (native "@" "malloc" 'N (: sz-buf-rd)))

    (=: nonblock-listener Nonblock-listener)
    (=: listener          (when Path-listener (socket~listen-sock Path-listener Nonblock-listener)))
    (=: clients           NIL)
    
    #Poss to have multiple connections to different server
    #Diff between incoming connections and outgoing connections

    (=: nonblock-connection Nonblock-connection)
    (=: connection NIL)
    (when Path-connection
      (while (not (: connection))
	(=: connection          (socket~connect-sock Path-connection Nonblock-connection))
	#~1/144 seconds
	(wait 7))
      (when (: debug-ipc)
	(prinl (usec) " | T | Connected sock: " (: connection)))))


(dm end-ipc> ()

    (when (: buf-rd)     (native "@" "free" NIL (: buf-rd)))
    (when (: buf-wr)     (native "@" "free" NIL (: buf-wr)))  
    
    (when (: listener)   (socket~end (: listener)))
    (for Client (: clients)
	 (socket~end Client))
    
    (when (: connection) (socket~end (: listener))))


(dm send-msg> (Msg)
    #Pass length?
    
    (let (Length (length Msg))
      
      (when (> Length 4096)
	(println "WARNING! Message will be truncated by " (- Length 4096) " bytes")
	(setq Length 4096))
    
      (struct (: buf-wr) 'N (cons Msg Length))
      
      (socket~send-fd (: connection)
	       (: buf-wr)
	       Length
	       0)))


(dm handle-sock-listen> (Events Fd Ep-fd)
    (let (Client (socket~accept-4 Fd 'block))

      #Read for ID
      (when (> (handle-sock-conn> This Events Client NIL) 0)

	#How to handle name conflicts?
	#-> Disconnect client
	
	(let (ID (struct (: buf-rd) 'S))
	  
	  #(println "ID" ID)
	  #(println (getl (: clients)))

	  #Map fd to name
	  (put (: clients) Client ID)

	  (epoll~ctl-epfd Ep-fd
			  epoll~ADD
			  Client
			  (| epoll~IN epoll~ERR epoll~HUP epoll~RDHUP))

	  (when (: debug-ipc)
	    (prinl (usec) " | handle-sock-listen | Accepted Client=" Sock ", ID=" ID))))))


(dm handle-sock-conn> (Events Fd Ep-fd)

    #(when (> (recv Fd *buf-rd *buf-sz 0) 0)
    #  #(println "RECV: " (struct *buf-rd '(B B B B B B B B)))
    #  (println "RECV: " (struct *buf-rd 'S)))
    #(let (bytes-read (native "@" "read" Fd *buf-rd *buf-sz)))
    
    (let (bytes-read (socket~recv Fd (: buf-rd) (: buf-sz) 0))
      
      #If blocking and recv called, it will block
      #If not blocking and recv called, will return 0
      #Default is blocking...

      (if (= bytes-read 0)
	  
	  #If epoll saying data to read, however recv is 0
	  #that indicates socket hung-up or errored out
	  (when (or (= (& Events epoll~ERR) epoll~ERR)
		    (= (& Events epoll~HUP) epoll~HUP)
		    (= (& Events epoll~RDHUP) epoll~RDHUP))

	    (del Fd (:: clients))
	    
            (when Ep-fd
	      (epoll~ctl-epfd Ep-fd
	                      epoll~DEL
			      Sock
			      0))

	    (socket~shutdown Fd socket~SHUT-RDWR)
	    (close Fd)

            (when (: debug-ipc)	    
	      (prinl (usec) " | handle-sock-conn | Disconnected sock=" Fd))

            bytes-read)
	  
          (prog
            (when (: debug-ipc)
	      (prinl (usec) " | handle-sock-conn | Client=" Fd ", " "Bytes-Read=:" bytes-read))
            bytes-read))))
