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
(dm T (Fd-Epoll
       Path-listener   Nonblock-listener
       Path-connection Nonblock-connection)

    (=: debug-ipc T)

    (=: fd-epoll Fd-Epoll)
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


#Rename to accept
(dm accept-sock> (Events Fd)
    (let Client (socket~accept-4 Fd 'block)

	 (when (: debug-ipc)
	   (prinl (usec) " | accept-sock> | Accepted Client=" Client))
	 
	 #Read for ID - this will block
	 (let ID (recv-msg> This Events Client)

	      (prinl (usec) " | accept-sock> | ID=" ID)
	      
	      #Disconnect client if name conflict
	      (if (assoc Client (: clients))

		  (prog
		      (prinl "WARNING! ID=" ID " exists; disconnecting client=" Client)
		     (disconnect-sock Client))
		  
		  (prog
		      (push1 (:: clients) (cons Client ID))

		     #Poss store ID into epoll-data so need not look up
		     (epoll~ctl-epfd (: fd-epoll)
				     epoll~ADD
				     Client
				     epoll~BAD)

		     (when (: debug-ipc)
		       (prinl (usec) " | accept-sock> | Registered Client=" Client ", ID=" ID)))))

    Client))


(dm recv-sock> (Events Fd Buf-Sz)
    #{
    * This is designed for blocking sockets...
    * If no-data and blocking     -> recv will block
    * If no-data and non-blocking -> recv will return -1 and set errno to EAGAIN or EWOULDBLOCK
    * EOF will return 0 on shutdown
    * ERR/HUP will return 0 also
    }#

    #For Buf-Sz: pass T to recv full length, else pass length
    (let Bytes-Read (socket~recv Fd
				 (: buf-rd)
				 (if (=T Buf-Sz)
				     (: sz-buf-rd)
				     Buf-Sz)
				 0)

	 #(println "recv-sock" (struct (: buf-rd) '(B B B B B B B B)))
	 
	 (if (=0 Bytes-Read)
	     
	     (when (or (= (& Events epoll~ERR)   epoll~ERR)
		       (= (& Events epoll~HUP)   epoll~HUP)
		       (= (& Events epoll~RDHUP) epoll~RDHUP))

	       #Make clean up fns for these or this entire block incl. above
	       (del (assoc Fd (: clients)) (:: clients))
	       (println (: clients))
	       
	       (epoll~ctl-epfd (: fd-ep)
			       epoll~DEL
			       Sock
			       0)
	       (disconnect-sock Fd)

	       (prinl (usec) " | recv-sock | Disconnected sock=" Fd)

	       Bytes-Read)
	     
	     (prog
		 (when NIL #(: debug-ipc)
		       (prinl (usec) " | recv-sock | Client=" Fd ", " "Bytes-Read=:" bytes-read))
	      Bytes-Read))))


(dm send-msg> (Msg)

    #Send length as int
    #Send payload

    #Handle errors here like recv-msg/sock
    #Pass Fd in
    
    (let Length (length Msg)
	 
	 (when (> Length (: sz-buf-rd))
	   (println "WARNING! Message will be truncated by " (- Length (: sz-buf-rd)) " bytes")
	   (setq Length (: sz-buf-rd)))

	 (struct (: buf-wr) 'N (cons Length 4))

	 #(println "send-msg" (struct (: buf-wr) '(B B B B B B B B)))
	 
	 (socket~send-fd (: connection)
			 (: buf-wr)
			 4
			 0)

	 (struct (: buf-wr) 'N (cons Msg Length))
	 (struct (+ (: buf-wr) Length) 'N (cons 0 1))
	 
	 (socket~send-fd (: connection)
			 (: buf-wr)
			 Length
			 0)))


(de wr-str-to-ptr (Str Ptr)
    (struct Ptr 'N (cons Msg Length))
    (struct (+ Ptr Length) 'N (cons 0 1)))


(dm recv-msg> (Events Fd)

    #Recv length as int
    #Recv payload
    
    (let Bytes-Read-Len (recv-sock> This Events Fd 4)

	 #(prinl (usec) " | recv-msg> | Bytes-Read-Len=" Bytes-Read-Len)
	 
	 (when (= Bytes-Read-Len 4)
	   
	   (let Length (car (struct (: buf-rd) '(I)))

		#(prinl (usec) " | recv-msg> |   Length-Msg=" Length)
		
		(let Bytes-Read-Msg (recv-sock> This Events Fd Length)

		     #Validate Length?
		     
		     (when (= Bytes-Read-Msg Length)

		       #(prinl (usec) " | recv-msg> | Msg=" Msg)
		       (pack (struct (: buf-rd) (cons 'C Length)))))))))


(de disconnect-sock (Sock)
    (socket~shutdown Sock socket~SHUT-RDWR)
    (close Sock))
