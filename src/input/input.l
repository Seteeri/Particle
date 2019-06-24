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


(de get-rmem-default ()
    (pipe
     (call "cat" "/proc/sys/net/core/rmem_default")
     (read)))

(de init-input ()   
    (setq *interface   (li-make-interface)
	  *context     (li-path-create-context *interface NIL)
	  *fd-li       (li-get-fd *context)
	  *ep-events   (native "@" "malloc" 'N 12) #1 event
	  *ep-fd       (epoll-create 1)
	  *buf-sz      (get-rmem-default)
	  *buf-rd      (native "@" "malloc" 'N *buf-sz))

    
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

    (let (Events NIL
	  Fd     NIL)
      (loop
	 
	 (setq num-events (epoll-wait *ep-fd *ep-events 1 500))
	 
	 (when (> num-events 0)

	   (setq Events (get (struct *ep-events '(I I)) 1))
	   (setq Fd (get (struct *ep-events '(I I)) 2))
	   
	   #(prinl (usec) " FD: " Fd ", Events: " Events)
	   
	   (cond 
	     ((= Fd *fd-li)
	      (handle-context))
	     ((= Fd *sock-listen)
	      (handle-sock-listen))
	     (T
	      (handle-sock-conn Events Fd)))))))


(de handle-sock-conn (Events Fd)

    #(when (> (recv Fd *buf-rd *buf-sz 0) 0)
    #  #(println "RECV: " (struct *buf-rd '(B B B B B B B B)))
    #  (println "RECV: " (struct *buf-rd 'S)))
    #(let (bytes-read (native "@" "read" Fd *buf-rd *buf-sz)))
    
    (let (bytes-read (recv Fd *buf-rd *buf-sz 0))
      (if (= bytes-read 0)
	  #If epoll saying data to read, however recv is 0
	  #that indicates socket hung-up or errored out
	  (when (or (= (& Events ERR) ERR) (= (& Events HUP) HUP) (= (& Events RDHUP) RDHUP))
	    (ctl-epoll *ep-fd 
		       DEL
		       Sock
		       0)    
	    (shutdown Fd SHUT-RDWR)
	    (close-filedes Fd)
	    (prinl "Disconnected sock: " Fd))
	  (prog
	      (prinl "Read sock: " bytes-read " bytes" " = " (struct *buf-rd 'S))))))

    
(de handle-sock-listen ()
    (let (Sock (accept4-sock *sock-listen 'block)
          Msg "Welcome User")
      
      #Add sock to epoll
      (ctl-epoll *ep-fd 
		 ADD
		 Sock
		 (| IN ERR HUP RDHUP))

      #(native "@" "write" 'I Sock Msg (+ (length Msg) 0))
      
      T))


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
