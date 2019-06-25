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


(de get-rmem-default ()
    (pipe
     (call "cat" "/proc/sys/net/core/rmem_default")
     (read)))


(de get-wmem-default ()
    (pipe
     (call "cat" "/proc/sys/net/core/wmem_default")
     (read)))


(class +IPC)
(dm T (Path-listener   Nonblock-listener
       Path-connection Nonblock-connection)

    #Setup socket data

    #Only need 1 pair for all sockets
    #Since each buffer can be simultaneously used in an instance
    (=: sz-buf-rd (get-rmem-default))
    (=: buf-rd    (native "@" "malloc" 'N *buf-sz))
    (=: sz-buf-wr (get-wmem-default))
    (=: buf-wr    (native "@" "malloc" 'N *buf-sz))

    (=: nonblock-listener Nonblock-listener)
    (=: listener          (socket~listen-sock Path-listener Nonblock-listener))
    
    (=: nonblock-connection Nonblock-connection)
    (=: connection          (when Path-connection (socket~connect-sock Path-connection Nonblock-connection))))


(dm end-ipc> ()

    (when (: buf-rd)     (native "@" "free" NIL (: buf-rd)))
    (when (: buf-wr)     (native "@" "free" NIL (: buf-wr)))  

    (when (: listener)   (socket~end (: listener) (: ep-fd)))
    (when (: connection) (socket~end (: listener) (: ep-fd))))


(de handle-sock-conn (Events Fd Ep-fd)

    #(when (> (recv Fd *buf-rd *buf-sz 0) 0)
    #  #(println "RECV: " (struct *buf-rd '(B B B B B B B B)))
    #  (println "RECV: " (struct *buf-rd 'S)))
    #(let (bytes-read (native "@" "read" Fd *buf-rd *buf-sz)))
    
    (let (bytes-read (socket~recv Fd *buf-rd *buf-sz 0))
      
      (if (= bytes-read 0)
	  
	  #If epoll saying data to read, however recv is 0
	  #that indicates socket hung-up or errored out
	  (when (or (= (& Events ERR) ERR)
		    (= (& Events HUP) HUP)
		    (= (& Events RDHUP) RDHUP))
	    
	    (ctl-epoll Ep-fd
		       DEL
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
      (ctl-epoll Ep-fd
		 ADD
		 Sock
		 (| IN ERR HUP RDHUP))

      (prinl "Connect sock: " Sock " with " (| IN ERR HUP RDHUP))
      
      T))
