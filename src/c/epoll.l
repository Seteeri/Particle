(symbols 'epoll 'pico)

#enum events
(def 'IN (hex "001"))
(def 'PRI (hex "002"))
(def 'OUT (hex "004"))
(def 'RDNORM (hex "040"))
(def 'RDBAND (hex "080"))
(def 'WRNORM (hex "100"))
(def 'WRBAND (hex "200"))
(def 'MSG (hex "400"))
(def 'ERR (hex "008"))
(def 'HUP (hex "010"))
(def 'RDHUP (hex "2000"))
(def 'EXCLUSIVE 268435456)
(def 'WAKEUP 536870912)   
(def 'ONESHOT 1073741824)
(def 'ET 2147483648)

#enum ctl
(def 'ADD 1) #Add a file descriptor to the interface.
(def 'DEL 2) #Remove a file descriptor from the interface.
(def 'MOD 3) #Change file descriptor epoll_event structure.

#{
(defcunion data)
(ptr :pointer)
(fd :int32)
(u32 :uint32)
(u64 :uint64)
}#

#{
(defcstruct epoll_event)
(events :uint32)
(data (:union data))
#4+8
}#

#For convenience
(def 'EPOLL-BAD (| IN ERR HUP RDHUP))


(de create-size (Size)
    (native "@" "epoll_create" 'I Size))


(de create-1 (Flags)
    (native "@" "epoll_create1" 'I Flags))


(de ctl-epfd-op (Epfd Op Fd Event)
    (native "@" "epoll_ctl" 'I Epfd Op Fd Event))


(de wait-epfd (Epfd Event Maxevents Timeout)
    (native "@" "epoll_wait" 'I Epfd Event Maxevents Timeout))


(de pwait (Epfd Event Maxevents Timeout Ss)
    (native "@" "epoll_pwait" 'I Epfd Event Maxevents Timeout Ss))


(de ctl-epfd (Epfd Op Fd Flags)
    #Possibly use automatic variables, allocated on the stack
    (let Event (native "@" "malloc" 'N 12)
	 (struct Event 'N (cons Flags 4) (cons Fd 4))
	 (ctl-epfd-op Epfd
		      Op
		      Fd
		      Event)
	 (native "@" "free" NIL Event)))

