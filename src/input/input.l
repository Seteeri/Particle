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
(load "/home/user/quicklisp/local-projects/protoform/src/c/input.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/sock.l")
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
	    *ep-fd     (epoll-create 1)
	    *buf-sz    (get-rmem-default)
	    *buf-rd    (native "@" "malloc" 'N *buf-sz)))
      
    
    (when T
      (setq *input (new '(+Input)))
      (ctl-epoll *ep-fd 
		 ADD
		 (get *input 'fd-context)
		 IN))

    
    (when T
      (setq *ipc (new '(ipc~+IPC) PATH-UDS-INPUT 'block
		                  ""            'block))
      (ctl-epoll *ep-fd 
		 ADD
		 (get *ipc 'ipc~listener)
		 IN)))


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
	 (setq num-events (epoll-wait *ep-fd *ep-events 2 Timeout))
	 
	 (when (> num-events 0)

	   (let (Ep-events (struct *ep-events '(I I)))
	     (setq Events (get Ep-events 1))
	     (setq Fd     (get Ep-events 2))
	     
	     #(prinl (usec) " FD: " Fd ", Events: " Events)

	     (cond 
	       ((= Fd (get *input 'fd-context))
		(handle-context))
	       ((= Fd (get *ipc 'ipc~listener))
		(ipc~handle-sock-listen Events Fd))
	       (T
		(ipc~handle-sock-conn Events Fd))))))))


(de handle-context ()

    (with *input

    (li-dispatch (: context))

    (loop
       (setq Event (li-get-event (: context)))
       (T (= Event 0))
       
       #Copy event info then destroy
       (let (Type (li-event-get-type Event))
	 (prinl "Event: " (li-get-event-name Type)))
       
       (li-event-destroy Event)
       (li-dispatch (: context)))))


(de end-input ()
    (prinl "...Called end-input")

    #Clean up sockets
    (end-sock *sock-listen *ep-fd)
    (for Sock *sock-connections
	 (end-sock Sock *ep-fd))
    (when *buf-rd
      (native "@" "free" NIL *buf-rd))

    #Remove from epoll before end
    (ctl-epoll *ep-fd DEL (get *input 'fd-context) 0)
    #(println +Input li-end-input> *input)
    (li-end-input> *input)
    
    #Clean up epoll
    (when *ep-events
      (native "@" "free" NIL *ep-events))
    (close-filedes *ep-fd))


(de main ()
    #(push '*Bye '(end-input))
    (init-input)
    (run-input -1)
    (bye))
