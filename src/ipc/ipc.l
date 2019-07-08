(symbols 'ipc 'pico)


# Set math precision to 6 decimals
(scl 6)


# Wrapper libraries
(load "/home/user/quicklisp/local-projects/protoform/src/c/epoll.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/socket.l")


#{
Poss replace entire class with nng

Pass in/out + err handler functions:

For dispatch, poss store function pointer in struct event data or pointer to
custom struct

Handle bad accept sock
}#


(def '*ms-retry 7) # 7 ms or 144 FPS


(class +IPC)
(dm T (Path-listener Nonblock-listener Handler-Listener
       Conns)

  (=: debug-ipc T)

  # Ideally, ep-cnt = number of connections to avoid excessive epoll-wait calls
  (=: fd-ep  (epoll~create-size 1))
  (=: cnt-ep  4)
  (=: evs-ep (epoll~make-event (: ep-cnt)))

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
  
  (=: conns    NIL)
  (=: conns-id NIL)

  # Listener stuff
  (=: listener NIL)
  (=: nonblock-listener Nonblock-listener)
  (=: handler-listener Handler-Listener)
  (=: clients NIL)
  
  (when Path-listener
    (init-listener> This))
  
  (init-conns> This Conns))


(dm init-listener> ()
  (=: listener (socket~listen-sock Path-listener 
                                   Nonblock-listener 
                                   64))
  (epoll~ctl-epfd (: fd-ep)
                  epoll~ADD
                  (: listener)
                  epoll~BAD))


(dm init-conns> (Conns)
  (for Conn Conns
      
      (let (Path     (get Conn 1)
            Nonblock (get Conn 2)
            Handler  (get Conn 3)
            ID-S     (get Conn 4)
            ID-C     (get Conn 5)
            Sock     (socket~connect-sock Path Nonblock *ms-retry))
        
        (send-msg> This ID-C Sock)
                        
        (add-conn> This Sock ID-S Handler)
                
        (when (: debug-ipc)
          (prinl (usec) " | T | Connected sock: " Sock " to " Path)))))


(dm add-conn> (Sock ID-S Handler)
  #{
  # Make class for this? -> Connection
  #
  # Poss simplify this?
  # FD -> Name
  # Name -> FD
  #
  # Choose one to map handler to instead of doubling up
  # FD : epoll data
  }#
  
  (epoll~ctl-epfd (: fd-ep)
                  epoll~ADD
                  Sock
                  epoll~BAD)        
  
  (push1 (:: conns)    (cons Sock (list ID-S Handler)))
  (push1 (:: conns-id) (cons ID-S (list Sock Handler))))


(dm end-ipc> ()

  (when (: buf-rd)     (native "@" "free" NIL (: buf-rd)))
  (when (: buf-wr)     (native "@" "free" NIL (: buf-wr)))

  (when (: listener)   (socket~end (: listener)))
  (for Client (: clients)
    (socket~end (car Client)))

  (when (: conns) 
    (for Conn (: conns)
      (socket~end (car Conn)))))


(de disconnect-sock (Sock)

  (socket~shutdown Sock socket~SHUT-RDWR)
  (close Sock))


(dm disconnect-client> (Sock)

  (prinl (usec) " | disconnect-client> | Sock=" Sock)
  (del (assoc Sock (: clients)) (:: clients))
  (epoll~ctl-epfd (: fd-ep)
                  epoll~DEL
                  Sock
                  0)
  (disconnect-sock Sock))


(dm accept-sock> (Sock Events)

  (let Client (socket~accept-4 Sock 'block)

     #(when (: debug-ipc)
     #  (prinl (usec) " | accept-sock> | Accepted Client=" Client))

    (when (= -1 Client)
      (prinl "(= -1 Client)"))

     #Read for ID - this will block
     (let ID (recv-msg> This Client Events)

          #(prinl (usec) " | accept-sock> | ID=" ID)

          #Disconnect client if name conflict
          (if (assoc Client (: clients))

              (prog
                 
                 (prinl "WARNING! ID=" ID " exists; disconnecting client=" Client)
                 
                 (disconnect-sock Client))

              (prog
              
                 # Note, client IDs use {ID}-{PID} and outgoing conns use ID-S
                 (add-conn> This Client ID (: handler-listener))
                                                  
                 # Add to client list also
                 (push1 (:: clients) (cons Client (list ID (: handler-listener))))

                 (when (: debug-ipc)
                   (prinl (usec) " | accept-sock> | Registered Client=" Client ", ID=" ID)))))

  Client))


(dm send-bytes> (Sock Events Buf Sz)
    #{
    * With blocking sockets, will block until space available (all data is buffered)
    * If msg too long, send will fail and no data will be buffered
    }#

    (let Bytes-Sent (socket~send-fd Sock
                                    (if (num? Buf) Buf (: buf-wr))
                                    (if (num? Sz)  Sz  (: sz-buf-wr))
                                    0)

       (if (= -1 Bytes-Sent)
       
           (when (or (= (& Events epoll~ERR)   epoll~ERR)
                     (= (& Events epoll~HUP)   epoll~HUP)
                     (= (& Events epoll~RDHUP) epoll~RDHUP))
             (disconnect-client> This Sock))

           (when NIL #(: debug-ipc)
                 (prinl (usec) " | send-bytes> | Sock=" Sock ", " "Bytes-Sent=:" Bytes-Sent)))
                 
       Bytes-Sent))


(dm send-msg> (Msg Sock Debug)
  #{
  * Send length as int, then send payload right after
  }#
  
  (let Length (length Msg)

     (when Debug (prinl (usec) " | send-msg> | Msg=" Msg))

     (when (> Length (: sz-buf-rd))
       (println "WARNING! Message will be truncated by " (- Length (: sz-buf-rd)) " bytes")
       (setq Length (: sz-buf-rd)))

     (struct (: buf-wr) 'N (cons Length 4))

     #(println "send-msg" (struct (: buf-wr) '(B B B B B B B B)))

     (socket~send-fd Sock
                     (: buf-wr)
                     4
                     0)

     (struct    (: buf-wr)         'N (cons Msg Length))
     (struct (+ (: buf-wr) Length) 'N (cons 0 1))

     (socket~send-fd Sock
                     (: buf-wr)
                     Length
                     0)))


(dm recv-bytes> (Sock Events Buf Sz)
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
     
         (when (or (= (& Events epoll~ERR)   epoll~ERR)
                   (= (& Events epoll~HUP)   epoll~HUP)
                   (= (& Events epoll~RDHUP) epoll~RDHUP))
           (disconnect-client> This Sock))

         (when NIL #(: debug-ipc)
               (prinl (usec) " | recv-bytes> | Sock=" Sock ", " "Bytes-Read=:" Bytes-Read)))

     Bytes-Read))
         
         
(dm recv-msg> (Sock Events Debug)
  #{
  * Recv length as int, then recv payload
  }#

  (let Bytes-Read-Len (recv-bytes> This Sock Events NIL 4)

     (when Debug (prinl (usec) " | recv-msg> | Bytes-Read-Len=" Bytes-Read-Len))

     # TODO: Need to handle for other value, i.e. print message
     (when (= Bytes-Read-Len 4)

       (let Length (car (struct (: buf-rd) '(I)))

          (when Debug (prinl (usec) " | recv-msg> |   Length-Msg=" Length))

          (let Bytes-Read-Msg (recv-bytes> This Sock Events NIL Length)

             #Validate Length?

             (when (= Bytes-Read-Msg Length)
             
               (when Debug (prinl (usec) " | recv-msg> | Bytes-Read-Msg=" Bytes-Read-Msg))
               (pack (struct (: buf-rd) (cons 'C Length)))))))))


(dm send-clients-msg> (Msg)
  #For now, meant for servers but poss pass in conns instead of clients
  
  (for Client (: clients)
    (send-msg> This Msg (car Client))))


(dm dispatch-handler-conn> (Sock Events)

  (let (Value   (cdr (assoc Sock (: conns)))
        Handler (get Value 2))

    (Handler Sock Events)))

    
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
            
 (for I (epoll~wait-epfd (: fd-ep)
                         (: evs-ep)
                         (: cnt-ep)
                         Timeout)
                         
   (let (Ep-Events (epoll~get-event (: evs-ep) I)
         Evs       (get Ep-Events 1)
         Fd        (get Ep-Events 2))

     (if (= Fd (: listener))
         (accept-sock> This Fd Evs)
         (dispatch-handler-conn> This Fd Evs)))))