#{
Poss replace entire class with nng

Pass in/out + err handler functions:

For dispatch, poss store function pointer in struct event data or pointer to
custom struct

Handle bad accept sock
}#

(symbols 'ipc 'pico)


# Set math precision to 6 decimals
(scl 6)


(def '*ms-retry 7) # 7 ms or 144 FPS


# Wrapper libraries
(load "/home/user/Git/particle/src/c/epoll.l")
(load "/home/user/Git/particle/src/c/socket.l")


# Must define class before loading file with methods
(class +IPC)


(dm T (Path-listener Nonblock-listener Handler-Listener Handler-Accept
       Conns)

  (=: debug-ipc T)

  # Ideally, ep-cnt = number of connections to avoid excessive epoll-wait calls
  (=: fd-ep  (epoll~create 1))
  (=: cnt-ep  16)
  (=: evs-ep (epoll~make-event (: cnt-ep)))

  # Start simple - 1 pair for all sockets
  # Later, could do per connection and use as cache...
  (=: sz-buf-rd (socket~read-rmem-default))
  (=: buf-rd    (native "@" "malloc" 'N (: sz-buf-rd)))
  (=: sz-buf-wr (socket~read-wmem-default))
  (=: buf-wr    (native "@" "malloc" 'N (: sz-buf-rd)))

  # REFACTOR/TODO:
  # * Integrate server sockets into single list for FD/ID lookup
  # * Maintain list of connections for each server so listener : clients
  #   so poss to do send all clients
  #   * Create listener class: id path block handler clients
  # * Maintain list of initiated connections
  # * Create method for loop - returns conns/conns-id
  
  # Listener stuff
  (=: listener NIL)
  (=: nonblock-listener Nonblock-listener)
  (=: handler-listener Handler-Listener)
  (=: handler-accept Handler-Accept)
  (=: clients NIL)
  
  (when Path-listener
    (init-listener> This))
  
  (=: conns    NIL)
  (=: conns-id NIL)
  
  (init-conns> This Conns))


(dm init-listener> ()
  (=: listener (socket~listen-sock Path-listener 
                                   Nonblock-listener 
                                   64))
  (epoll~ctl (: fd-ep)
                  epoll~ADD
                  (: listener)
                  epoll~BAD))


(dm init-conns> (Conns)
  (for Conn Conns
      
      (let (Path     (get Conn 1)
            Nonblock (get Conn 2)
            Handler  (get Conn 3)
            ID-S     (get Conn 4)
            Sock     (socket~connect-sock Path Nonblock *ms-retry))
        
        # Only outgoing sockets send ID
        #(send-msg This ID-C Sock)
                        
        (add-conn> This Sock ID-S Handler)
                
        (when (: debug-ipc)
          (prinl (usec) " | T | Connected sock: " Sock " to " Path)))))


(dm add-conn> (Sock ID-S Handler Flags)
  #{
  # Make class for this? -> Connection
  }#
  
  (epoll~ctl (: fd-ep)
                  epoll~ADD
                  Sock
                  (if Flags Flags epoll~BAD))
  
  #SIMPLIFY:
  #(push1 (:: conns)    (cons Sock (list ID-S Handler)))
  #(push1 (:: conns-id) (cons ID-S Sock))

  (push1 (:: conns)    (cons Sock (list ID-S Handler)))
  (push1 (:: conns-id) (cons ID-S (list Sock Handler))))


(dm end> ()

  (native "@" "free" NIL (: buf-rd))
  (native "@" "free" NIL (: buf-wr))
  (native "@" "free" NIL (: evs-ep))
  
  (when (: listener)   (socket~end (: listener)))
  (for Client (: clients)
    (socket~end (car Client)))

  (when (: conns)
    (for Conn (: conns)
      (socket~end (car Conn)))))


(de disconnect-sock (Sock)
  (socket~end Sock))


(dm disconnect-client> (Sock)

  (prinl (usec) " | disconnect-client> | Sock=" Sock)
  (del (assoc Sock (: clients)) (:: clients))
  (epoll~ctl (: fd-ep)
                  epoll~DEL
                  Sock
                  0)
  (disconnect-sock Sock))


(dm accept-sock> (Sock)

  (let Client (socket~accept-4 Sock 'block)

     #(when (: debug-ipc)
     #  (prinl (usec) " | accept-sock> | Accepted Client=" Client))

    (when (= -1 Client)
      (prinl "(= -1 Client)"))

    ((: handler-accept) This Sock Client)))


(dm send-bytes> (Sock Buf Sz)
    #{
    * With blocking sockets, will block until space available (all data is buffered)
    * If msg too long, send will fail and no data will be buffered
    }#

    (let Bytes-Sent (socket~send-fd Sock
                                    (if (num? Buf) Buf (: buf-wr))
                                    (if (num? Sz)  Sz  (: sz-buf-wr))
                                    0)

       (if (= -1 Bytes-Sent)
       
           # Poss check for (= (& Events epoll~ERR) epoll~ERR) and so on...
           # For now, assume error and disconnect
           (disconnect-client> This Sock)

           (when NIL #(: debug-ipc)
             (prinl (usec) " | send-bytes> | Sock=" Sock ", " "Bytes-Sent=:" Bytes-Sent)))
                 
       Bytes-Sent))


(dm recv-bytes> (Sock Buf Sz)
  #{
  * This is designed for blocking sockets...
  * If no-data and blocking     -> recv will block
  * If no-data and non-blocking -> recv will return -1 and set errno to EAGAIN or EWOULDBLOCK
  * EOF will return 0 on shutdown
  * ERR/HUP will return 0 also
  }#

  #For Buf-Sz: pass T to recv full length, else pass length
  (let Bytes-Read (socket~recv Sock
                               (if (num? Buf) Buf (: buf-rd))
                               (if (num? Sz)  Sz  (: sz-buf-rd))
                               0)

     #(println "recv-sock" (struct (: buf-rd) '(B B B B B B B B)))

     # Handle -1
     (if (=0 Bytes-Read)
     
         # Poss check for (= (& Events epoll~ERR) epoll~ERR) and so on...
         # For now, assume error and disconnect
         (disconnect-client> This Sock)

         (when NIL #(: debug-ipc)
               (prinl (usec) " | recv-bytes> | Sock=" Sock ", " "Bytes-Read=:" Bytes-Read)))

     Bytes-Read))
         

(dm dispatch-handler-conn> (Sock)

  (let (Value   (cdr (assoc Sock (: conns)))
        Handler (get Value 2))

    (Handler Sock)))

    
(dm get-fd> (ID-S)
  (get (cdr (assoc ID-S (: conns-id))) 1))
  
  
(dm poll> (Timeout)
  #{
  epoll-wait:
  * ep-events size should correspond to maxevents
  * -1 = timeout = block
     0 = return if nothing
  * returns event count
  }#
           
  (let Cnt (epoll~wait (: fd-ep)
                            (: evs-ep)
                            (: cnt-ep)
                            Timeout)
    (for I Cnt
                            
      (let (Ep-Events (epoll~get-event (: evs-ep) I)
            Fd        (get Ep-Events 2)) # Evs = 1

        #(prinl *Pid "ipc~poll> " Fd)

        (if (= Fd (: listener))
            (accept-sock> This Fd)
            (dispatch-handler-conn> This Fd))))
           
    Cnt))