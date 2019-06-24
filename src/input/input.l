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

# UDS paths
(def 'PATH-UDS-MODEL "/tmp/protoform-model.socket")
(def 'PATH-UDS-INPUT "/tmp/protoform-input.socket")


# Set math precision to 6 decimals
(scl 6)


# Wrapper libraries
(load "/home/user/quicklisp/local-projects/protoform/src/c/epoll.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/input.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/sock.l")
# Lisp libraries


(def 'DEVICES-PATH "/dev/input")


(de init-devices (Context)
    (for File (dir DEVICES-PATH)
	 (when (and (not (= File "by-id"))
		    (not (= File "by-path")))
	   (let (Filepath (pack DEVICES-PATH "/" File)
			  Device (li-path-add-device Context Filepath))
	     (unless (= Device 0)
	       (if (not (or (li-device-has-capability Device
						      DEVICE-CAP-KEYBOARD)
			    (li-device-has-capability Device
						      DEVICE-CAP-POINTER)))
		   (li-path-remove-device Device)))))))


(de init-input ()   
    (setq *interface   (li-make-interface)
	  *context     (li-path-create-context *interface NIL)
	  *fd-li       (li-get-fd *context)
	  *ep-events   (native "@" "malloc" 'N 12) #1 event
	  *ep-fd       (epoll-create 1))

    (init-devices *context)
    
    #Add FDs to epoll - watch for incomming data
    (when NIL
      (ctl-epoll *ep-fd 
		 ADD
		 *fd-li
		 IN))

    (when T
      (setq *sock-listen (listen-sock PATH-UDS-INPUT 'block)))
      
      (ctl-epoll *ep-fd 
		 ADD
		 *sock-listen
		 IN))
      

(de run-input ()
    #{
    Poll events ASAP and enqueue for processing by controller thread
    
    epoll-wait:
    * ep-events size should correspond to maxevents
    * -1 = timeout = block/infinite
    0 = return if nothing
    * returns event count
    }#
    
    (println "Running")
    (loop
       (when (> (epoll-wait *ep-fd *ep-events 1 -1) 0)
	 (setq Fd (get (struct *ep-events '(I I)) 2))
	 (cond 
	   ((= Fd *sock-listen)
	    (handle-sock-listen)
	    T)
	   ((= Fd *fd-li)
	    (handle-context)
	    T)))))


(de handle-sock-listen ()
    (let (Sock (accept4-sock *sock-listen 'nonblock)
	       Msg "Hello User")
      (println "Sock" Sock)
      (native "@" "write" 'I Sock Msg (+ (length Msg) 0))))


(de handle-context ()
    (li-dispatch *context)
    (loop
       (setq Event (li-get-event *context))
       (T (= Event 0))
       
       #Copy event info then destroy
       (let (Type (li-event-get-type Event))
	 (prinl "Event: " (li-get-event-name Type)))
       
       (li-event-destroy Event)
       (li-dispatch *context)))


(symbols 'pico)

(de main ()
    (input~init-input)
    (input~run-input)
    (bye))
