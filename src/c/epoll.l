(symbols 'epoll 'pico)

(local) (create ctl wait fd)

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
(def 'BAD (| IN ERR HUP RDHUP))

(def 'SZ-EVENT 12)


(de make-event (Cnt)
  (native "@" "malloc" 'N (* 12 Cnt)))


(de create (Size)
  (native "@" "epoll_create" 'I Size))


(de create-1 (Flags)
  (native "@" "epoll_create1" 'I Flags))


(de ctl (Epfd Op Fd Flags)
  #Possibly use automatic variables, allocated on the stack
  (let Event (native "@" "calloc" 'N 1 12)
    (struct Event 'N (cons Flags 4) (cons Fd 4))
    (let Ret (native "@" "epoll_ctl" 'I Epfd Op Fd Event)
      (native "@" "free" NIL Event)
      Ret)))


(de wait (Epfd Events Maxevents Timeout)
  (native "@" "epoll_wait" 'I Epfd Events Maxevents Timeout))


(de pwait (Epfd Event Maxevents Timeout Ss)
  (native "@" "epoll_pwait" 'I Epfd Event Maxevents Timeout Ss))


(de get-event (Events Cnt)
  (struct (+ Events (* (- Cnt 1) SZ-EVENT)) '(I I)))


(class +Epoll)
(dm T (Evs-Max)
  (=: fd (create 1))
  (=: evs-max Evs-Max)
  (=: evs (make-event (: evs-max))))

(dm end> ()
  (native "@" "free" NIL (: evs)))

(dm ctl> (Op Fd Flags)
  (ctl (: fd) Op Fd Flags))

(dm wait> (Timeout)
  (wait (: fd) (: evs) (: evs-max) Timeout))

(dm get-events> (Cnt)
  (let Evs ()
    (for I Cnt
      (push 'Evs (get (get-event (: evs) I) 2)))
    (flip Evs)))