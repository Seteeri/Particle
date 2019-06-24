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


# Set math precision to 6 decimals
(scl 6)


# Wrapper libraries
(load "/home/user/quicklisp/local-projects/protoform/src/c/os.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/input.l")
# Lisp libraries


(def 'DEVICES-PATH "/dev/input")


(de ctl-epoll (Epfd Op Fd Flags)
    #Possibly use automatic variables, allocated on the stack
    (let Event (native "@" "malloc" 'N 12)
	 (struct Event 'N Flags Fd)
	 (epoll-ctl Epfd
		    Op
		    Fd
		    Event)))


(de init-epoll (Fd)
    #Add libinput Fd to epoll list of FDs
    (let Ep-fd (epoll-create 1)
	 (ctl-epoll Ep-fd
		    ADD
		    Fd
		    IN) #x0001
	 Ep-fd))


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
    (setq *interface (make-interface-li)
	  *context   (li-path-create-context *interface NIL)
	  *ep-events (native "@" "malloc" 'N (+ 4 8 4 4 8))
	  *ep-fd     (init-epoll (li-get-fd *context)))
    (init-devices *context))


(de make-interface-li ()
    (let (Interface (native "@" "malloc" 'N 16) #libinput-interface struct
          OR (lisp 'handle-open-restricted
	       '((Path Flags User-data)
		 (native "@" "open" 'I (struct Path 'S) (struct Flags 'I))))
	  CR (lisp 'handle-close-restricted
	       '((Fd User-data)
		 (native "@" "close" 'I (struct Fd 'I)))))
      (struct Interface 'N
	      (cons OR 8)
	      (cons CR 8))))


(de run-input ()
    #Poll events ASAP and enqueue for processing by controller thread
    (let Event Nil
	 (println "Running")
	 (loop
	    #-1 = timeout = block/infinite
	    #0 = return if nothing
	    (when (> (epoll-wait *ep-fd *ep-events 1 -1) 0)
	      (li-dispatch *context)
	      (loop
		 (setq Event (li-get-event *context))
		 
		 (T (= Event 0)
		    (println "Exit loop"))

		 (println "Send message with event info" Event)

		 #Copy event info then destroy
		 (when nil
		   (let (Type (li-event-get-type Event))
		     (prinl "Event: " (li-get-event-name Type))))
		 
		 #(li-event-destroy Event)
		 #(li-dispatch *context)
		 T)))))


(symbols 'pico)


(de main ()
    (input~init-input)
    (input~run-input)
    (println 'Exiting...)
    (bye))
