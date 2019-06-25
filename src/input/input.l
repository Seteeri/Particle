#{
    Global variables start with an asterisk "*"
    Global constants may be written all-uppercase
    Functions and other global symbols start with a lower case letter
    Locally bound symbols start with an upper case letter
    Local functions start with an underscore "_"
    Classes start with a plus-sign "+", where the first letter
        is in lower case for abstract classes
        and in upper case for normal classes 
    Methods end with a right arrow ">"
    Class variables may be indicated by an upper case letter 
}#

#Create and change to namespace model
(symbols 'input 'pico)


# C library paths

# Paths
(def 'PATH-UDS-MODEL "/tmp/protoform-model.socket")
(def 'PATH-UDS-INPUT "/tmp/protoform-input.socket")


# Set math precision to 6 decimals
(scl 6)


# Wrapper libraries
(load "/home/user/quicklisp/local-projects/protoform/src/c/epoll.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/li.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/socket.l")
# Lisp libraries
(load "/home/user/quicklisp/local-projects/protoform/src/ipc/ipc.l")


#Globals
(def '*ep-events)
(def '*ep-fd)
(def '*buf-sz)
(def '*buf-rd)
(def '*input)
(def '*ipc)
(def '*sock-listen)
(def '*sock-connections)


(de get-rmem-default ()
    (pipe
     (call "cat" "/proc/sys/net/core/rmem_default")
     (read)))


(de init-input ()
    
    (when T
      (setq *ep-events (native "@" "malloc" 'N (* 12 2)) #1 event
	    *ep-fd     (epoll~create-size 1)
	    *buf-sz    (socket~read-rmem-default)
	    *buf-rd    (native "@" "malloc" 'N *buf-sz)))
      
    
    (when NIL
      (setq *input (new '(li~+Li)))
      (epoll~ctl-epfd *ep-fd
		      epoll~ADD
		      (get *input 'li~fd-context)
		      epoll~IN))

    
    (when T
      (setq *ipc (new '(ipc~+IPC) PATH-UDS-INPUT 'block
		                  ""            'block))
      (epoll~ctl-epfd *ep-fd 
		      epoll~ADD
		      (get *ipc 'ipc~listener)
		      epoll~IN)))


(de run-input (Timeout)
    #{
    Poll events ASAP and enqueue for processing by controller thread
    
    epoll-wait:
    * ep-events size should correspond to maxevents
    * -1 = timeout = block/infinite
    0 = return if nothing
    * returns event count
    }#

    (prinl "Polling...")
    
    (let (Events NIL
	  Fd     NIL)
      (loop
	 (setq num-events (epoll~wait-epfd *ep-fd *ep-events 2 Timeout))
	 
	 (when (> num-events 0)

	   (let (Ep-events (struct *ep-events '(I I)))
	     (setq Events (get Ep-events 1))
	     (setq Fd     (get Ep-events 2))
	     
	     (prinl (usec) " FD: " Fd ", Events: " Events)

	     (cond 
	       ((= Fd (get *input 'li~fd-context))
		(handle-context))
	       ((= Fd (get *ipc 'ipc~listener))
		(ipc~handle-sock-listen Events Fd *ep-fd))
	       (T
		(ipc~handle-sock-conn Events Fd *ep-fd))))))))


(de handle-context ()

    (with *input

    (li~dispatch (: li~context))

    (loop
       (setq Event (li~get-event (: li~context)))
       (T (= Event 0))
       
       #Copy event info then destroy
       (let (Type (li~event-get-type Event))
	 (prinl "Event: " (li~get-event-name Type)))
       
       (li~event-destroy Event)
       (li~dispatch (: li~context)))))


(de end-input ()
    (prinl "...Called end-input")

    #Clean up sockets
    (epoll~ctl-epfd *ep-fd epoll~DEL (get *ipc 'ipc~listener) 0)
    (epoll~ctl-epfd *ep-fd epoll~DEL (get *ipc 'ipc~connection) 0)
    (ipc~end-ipc *ipc)

    #Remove from epoll before end
    (epoll~ctl-epfd *ep-fd epoll~DEL (get *input 'fd-context) 0)
    #(println +Input li-end-input> *input)
    (li-end-input> *input)
    
    #Clean up epoll
    (when *ep-events
      (native "@" "free" NIL *ep-events))
    (close *ep-fd)
    #create in ipc
    #(for Sock *sock-connections
    #(end-sock Sock *ep-fd))
    (when *buf-rd
      (native "@" "free" NIL *buf-rd)))   


(de main ()
    #(push '*Bye '(end-input))
    (init-input)
    (run-input -1)
    (bye))
