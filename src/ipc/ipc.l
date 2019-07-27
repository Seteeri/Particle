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
(load "/home/user/Git/particle/src/misc.l")
(load "/home/user/Git/particle/src/c/epoll.l")
(load "/home/user/Git/particle/src/c/socket.l")


# Must define class before loading file with methods
(class +IPC)


(dm T (Path-listener Nonblock-listener Handler-Listener Handler-Accept
       Conns)

  (=: debug-ipc T)

  # Ideally, cnt = number of connections to avoid excessive epoll-wait calls
  (=: ep (new '(epoll~+Epoll) 16))

  # Start simple - 1 pair for all sockets
  # Later, could do per connection and use as cache...
  (=: sz-buf-rd (sock~read-rmem-default))
  (=: buf-rd    (native "@" "malloc" 'N (: sz-buf-rd)))
  (=: sz-buf-wr (sock~read-wmem-default))
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

  (=: listener (sock~listen Path-listener 
                            Nonblock-listener 
                            64))
  (epoll~ctl> (: ep)
              epoll~ADD
              (: listener)
              epoll~BAD))


(dm init-conns> (Conns)

  (for Conn Conns
      
      (let ((Path Nonblock Handler ID-S) Conn
            Sock                         (sock~connect-sock Path Nonblock *ms-retry))
                                
        (add-conn> This Sock ID-S Handler)

        (print-info "init-conns>" (pack "Sock=" Sock " connected to " Path)))))


(dm add-conn> (Sock ID-S Handler Flags)
  #{
  # Make class for this? -> Connection
  }#
  
  (epoll~ctl> (: ep)
              epoll~ADD
              Sock
              (if Flags Flags epoll~BAD))
  
  # Push into same list?
  (push1 (:: conns)    (cons Sock (list ID-S Handler)))
  (push1 (:: conns-id) (cons ID-S (list Sock Handler))))


(dm end> ()

  (native "@" "free" NIL (: buf-rd))
  (native "@" "free" NIL (: buf-wr))

  (epoll~end> (: ep))
  
  (when (: listener)   (sock~end (: listener)))
  (for Client (: clients)
    (sock~end (car Client)))

  (when (: conns)
    (for Conn (: conns)
      (sock~end (car Conn)))))


(de disconnect-sock (Sock)
  (sock~end Sock))


(dm disconnect-client> (Sock)

  (print-info "disconnect-client" (pack "Sock=" Sock))
  (del (assoc Sock (: clients)) (:: clients))
  (epoll~ctl> (: ep)
              epoll~DEL
              Sock
              0)
  (sock~end Sock))


(dm accept-sock> (Sock)

  (let Client (sock~accept Sock 'block)

    (when (<= Client 0)
      (print-info "ipc~accept-sock" (pack "Client=" Client)))

    ((: handler-accept) This Sock Client)))


(dm send-bytes> (Sock Buf Sz Flags)
  #{
  * With blocking sockets, will block until space available (all data is buffered)
  * If msg too long, send will fail and no data will be buffered
  }#

  (let Bytes-Sent (sock~send Sock
                             (if (num? Buf) Buf (: buf-wr))
                             (if (num? Sz)  Sz  (: sz-buf-wr))
                             (if (num? Flags) Flags 0))
     (case Bytes-Sent
     
        # Orderly shutdown
        (0  (nil (print-info "send-bytes>" (pack "ERRNO: " (errno) " " Sock)) (disconnect-client> This Sock)))
        
        # Error
        (-1 (nil (print-info "send-bytes>" (pack "ERRNO: " (errno) " " Sock)) (disconnect-client> This Sock)))
        
        (T  Bytes-Sent))))


(dm recv-bytes> (Sock Buf Sz Flags)
  #{
  * This is designed for blocking sockets...
  * If no-data and blocking     -> recv will block
  * If no-data and non-blocking -> recv will return -1 and set errno to EAGAIN or EWOULDBLOCK
  * EOF will return 0 on shutdown
  * ERR/HUP will return 0 also
  
  * Return num or NIL
  }#

  (let Bytes-Read (sock~recv Sock
                             (if (num? Buf)   Buf (: buf-rd))
                             (if (num? Sz)    Sz  (: sz-buf-rd))
                             (if (num? Flags) Flags 0))
                               
     (case Bytes-Read
     
        # Orderly shutdown
        (0  (nil (print-info "recv-bytes>" (pack "ERRNO: " (errno) " " Sock)) (disconnect-client> This Sock)))
        
        # Error
        (-1 (nil (print-info "recv-bytes>" (pack "ERRNO: " (errno) " " Sock)) (disconnect-client> This Sock)))
        
        (T  Bytes-Read))))
         

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
           
  (let Cnt (epoll~wait> (: ep) Timeout)

    (for Fd (epoll~get-events> (: ep) Cnt)

      (if (= Fd (: listener))
          (accept-sock> This Fd)
          (dispatch-handler-conn> This Fd)))

    Cnt))