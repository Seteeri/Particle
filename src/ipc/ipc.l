#{

IPC is a pair of sockets
- Client
- Server

}#

(symbols 'ipc 'pico)


# C library paths


# Set math precision to 6 decimals
(scl 6)


# Wrapper libraries
(load "/home/user/quicklisp/local-projects/protoform/src/c/epoll.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/socket.l")


#{

Pass in incomming handler functions:
* In
* Err

Pass in outgoing handler functions:
* In
* Err
* Poss specify address to address specific types

Pass in lists:
* Server: (list (path, nonblock) ...)
* Clients: (list (path, nonblock) ...)

For dispatch, get function pointer from struct event data

Handle bad accept sock
}#


(class +IPC)
(dm T (Fd-Epoll
       Path-listener   Nonblock-listener
       Conns)

    (=: debug-ipc T)

    (=: fd-epoll Fd-Epoll)
    #Setup socket data

    #Only need 1 pair for all sockets
    #Since each buffer can be simultaneously used in an instance
    (=: sz-buf-rd (socket~read-rmem-default))
    (=: buf-rd    (native "@" "malloc" 'N (: sz-buf-rd)))
    (=: sz-buf-wr (socket~read-wmem-default))
    (=: buf-wr    (native "@" "malloc" 'N (: sz-buf-rd)))

    (=: nonblock-listener Nonblock-listener)
    (=: listener NIL)
    (when Path-listener
      (=: listener          (socket~listen-sock Path-listener 
                                                Nonblock-listener 
                                                64))
      (epoll~ctl-epfd Fd-Epoll 
                      epoll~ADD
                      (: listener)
                      epoll~BAD))
    (=: clients           NIL)

    #Poss to have multiple connections to different server
    #Diff between incoming connections and outgoing connections

    (=: conns    NIL)
    (=: conns-id NIL)
    
    # TODO: Add retry
    (for Conn Conns
        (let (Path     (get Conn 1)
              Nonblock (get Conn 2)
              Handler  (get Conn 3)
              ID-S     (get Conn 4)
              ID-C     (get Conn 5)
              Sock     (socket~connect-sock Path Nonblock))
          (send-msg> This ID-C Sock)
          (epoll~ctl-epfd Fd-Epoll
                          epoll~ADD
                          Sock
                          epoll~BAD)
          # Make class for this?
          # Better way to do this?
          (push1 (:: conns)    (cons Sock (list ID-S Handler)))
          (push1 (:: conns-id) (cons ID-S (list Sock Handler)))
          (when (: debug-ipc)
            (prinl (usec) " | T | Connected sock: " Sock " to " Path))))
    T)


(dm end-ipc> ()

    (when (: buf-rd)     (native "@" "free" NIL (: buf-rd)))
    (when (: buf-wr)     (native "@" "free" NIL (: buf-wr)))

    (when (: listener)   (socket~end (: listener)))
    (for Client (: clients)
         (socket~end Client))

    (when (: connection) (socket~end (: listener))))


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


(dm accept-sock> (Events Sock)
    (let Client (socket~accept-4 Sock 'block)

         #(when (: debug-ipc)
         #  (prinl (usec) " | accept-sock> | Accepted Client=" Client))

        (when (= -1 Client)
          (prinl "(= -1 Client)"))

         #Read for ID - this will block
         (let ID (recv-msg> This Events Client)

              #(prinl (usec) " | accept-sock> | ID=" ID)

              #Disconnect client if name conflict
              (if (assoc Client (: clients))

                  (prog
                      (prinl "WARNING! ID=" ID " exists; disconnecting client=" Client)
                     (disconnect-sock Client))

                  (prog
                      (push1 (:: clients) (cons Client ID))

                     #Poss store ID into epoll-data so need not look up
                     (epoll~ctl-epfd (: fd-epoll)
                                     epoll~ADD
                                     Client
                                     epoll~BAD)

                     (when (: debug-ipc)
                       (prinl (usec) " | accept-sock> | Registered Client=" Client ", ID=" ID)))))

    Client))


(dm send-bytes> (Events Sock Buf Sz)
    #{
    * With blocking sockets, will block until space available (all data is buffered)
    * If msg too long, send will fail and no data will be buffered
    }#

    (let Bytes-Sent (socket~send-fd Sock
                                    (if (num? Buf) #ptr
                                        Buf
                                        (: buf-wr))
                                    (if (num? Sz)
                                        Sz
                                        (: sz-buf-wr))
                                    0)

         (if (= -1 Bytes-Sent)
             (when (or (= (& Events epoll~ERR)   epoll~ERR)
                       (= (& Events epoll~HUP)   epoll~HUP)
                       (= (& Events epoll~RDHUP) epoll~RDHUP)
                       (= Events T))
               (disconnect-client> This Sock))

             (when NIL #(: debug-ipc)
                   (prinl (usec) " | send-bytes> | Sock=" Sock ", " "Bytes-Sent=:" Bytes-Sent)))
         Bytes-Sent))


(dm recv-bytes> (Events Sock Buf Sz)
    #{
    * This is designed for blocking sockets...
    * If no-data and blocking     -> recv will block
    * If no-data and non-blocking -> recv will return -1 and set errno to EAGAIN or EWOULDBLOCK
    * EOF will return 0 on shutdown
    * ERR/HUP will return 0 also
    }#

    #For Buf-Sz: pass T to recv full length, else pass length
    (let Bytes-Read (socket~recv Sock
                                 (if (num? Buf) #ptr
                                     Buf
                                     (: buf-rd))
                                 (if (num? Sz)
                                     Sz
                                     (: sz-buf-rd))
                                 0)

         #(println "recv-sock" (struct (: buf-rd) '(B B B B B B B B)))

         (if (=0 Bytes-Read)
             (when (or (= (& Events epoll~ERR)   epoll~ERR)
                       (= (& Events epoll~HUP)   epoll~HUP)
                       (= (& Events epoll~RDHUP) epoll~RDHUP)
                       (= Events T))
               (disconnect-client> This Sock))

             (when NIL #(: debug-ipc)
                   (prinl (usec) " | recv-bytes> | Sock=" Sock ", " "Bytes-Read=:" Bytes-Read)))

         Bytes-Read))


(dm send-msg> (Msg Sock)

    #Send length as int
    #Send payload

    #Handle errors here like recv-msg/sock
    #Pass Sock in

    (let Length (length Msg)

         (when (> Length (: sz-buf-rd))
           (println "WARNING! Message will be truncated by " (- Length (: sz-buf-rd)) " bytes")
           (setq Length (: sz-buf-rd)))

         (struct (: buf-wr) 'N (cons Length 4))

         #(println "send-msg" (struct (: buf-wr) '(B B B B B B B B)))

         (socket~send-fd Sock
                         (: buf-wr)
                         4
                         0)

         (struct (: buf-wr) 'N (cons Msg Length))
         (struct (+ (: buf-wr) Length) 'N (cons 0 1))

         (socket~send-fd Sock
                         (: buf-wr)
                         Length
                         0)))


(dm recv-msg> (Events Sock)

    #Recv length as int
    #Recv payload

    (let Bytes-Read-Len (recv-bytes> This Events Sock NIL 4)

         #(prinl (usec) " | recv-msg> | Bytes-Read-Len=" Bytes-Read-Len)

         (when (= Bytes-Read-Len 4)

           (let Length (car (struct (: buf-rd) '(I)))

                #(prinl (usec) " | recv-msg> |   Length-Msg=" Length)

                (let Bytes-Read-Msg (recv-bytes> This Events Sock NIL Length)

                     #Validate Length?

                     (when (= Bytes-Read-Msg Length)

                       #(prinl (usec) " | recv-msg> | Msg=" Msg)
                       (pack (struct (: buf-rd) (cons 'C Length)))))))))


(dm send-clients-msg> (Msg)
  #For now, meant for servers but poss pass in conns instead of clients
  (for Client (: clients)
    (send-msg> This Msg (car Client))))


(dm dispatch-handler-conn> (Sock Events)
  (let (Value   (cdr (assoc Sock (: conns)))
        ID      (get Value 1)
        Handler (get Value 2))
    (Handler Sock Events)))

    
(dm get-fd> (ID-S)
  (get (cdr (assoc ID-S (: conns-id))) 1))