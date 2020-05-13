#{
  Poss replace entire class with nng

  Pass in/out + err hnd functions:

  For dispatch, poss store function pointer in struct event data or pointer to
  custom struct

  Handle bad acpt sock
}#

(symbols 'ipc 'pico)


# Set math precision to 6 decimals
# (scl 6)


(def '*ms-retry 7) # 7 ms or 144 FPS


(load (pack (pwd) "/src/misc.l"))
(load (pack (pwd) "/src/c/epoll.l"))
(load (pack (pwd) "/src/c/socket.l"))


(class +IPC)


(dm T (Path-Lstr Nonb-Lstr Hnd-Lstr Hnd-Acpt
       Conns)

  # Ideally, cnt = number of connections to avoid excessive epoll-wait calls
  (=: ep (new '(epoll~+Epoll) 16))

  # Start simple - 1 pair for all sockets
  # Later, could do per connection and use as cache...
  (=: sz-buf-rd (sock~get-rmem-def))
  (=: buf-rd    (native "@" "malloc" 'N (: sz-buf-rd)))
  (=: sz-buf-wr (sock~get-wmem-def))
  (=: buf-wr    (native "@" "malloc" 'N (: sz-buf-wr)))

  # REFACTOR/TODO:
  # * Integrate server sockets into single list for FD/ID lookup
  # * Maintain list of connections for each server so lstr : clients
  #   so poss to do send all clients
  #   * Create lstr class: id path block hnd clients
  # * Maintain list of initiated connections
  # * Create method for loop - returns conns/conns-id
  
  (=: lstr)
  (=: hnd-lstr Hnd-Lstr)
  (=: hnd-acpt Hnd-Acpt)
  (=: clients)
  (when Path-Lstr
    (init-lstr> This @ Nonb-Lstr))
    
  (=: conns)
  (=: conns-id)
  
  (add-conns> This Conns))

  
(dm end> ()

  (native "@" "free" NIL (: buf-rd))
  (native "@" "free" NIL (: buf-wr))

  (epoll~end> (: ep))
  
  (when (: lstr) (sock~end (: lstr)))
  
  (for Cli (: clients)
    (sock~end (car Cli)))

  (for Conn (: conns)
    (sock~end (car Conn))))
    


(dm init-lstr> (Path Nonb)

  (=: lstr (sock~listen Path
                        Nonb
                        16))
  (epoll~ctl> (: ep)
              epoll~ADD
              (: lstr)
              epoll~BAD))


(dm add-conns> (Conns)

  (for Conn Conns

    # Pass retry value to conn
    (let ((Path Nonb Hnd ID-S) Conn)

      (when (sock~connect-sock Path Nonb *ms-retry)
                              
        (add-conn> This @ ID-S Hnd)

        (prinl-info "add-conns>" (pack "Sock=" @ " connected to " Path))))))


(dm add-conn> (Sock ID-S Hnd Flags)

  (epoll~ctl> (: ep)
              epoll~ADD
              Sock
              epoll~BAD)
  
  # Push into same list?
  (push1 (:: conns)    (cons Sock (list ID-S Hnd)))
  (push1 (:: conns-id) (cons ID-S Sock)))


(de disc-sock (Sock) (sock~end Sock))
(dm acpt-sock> (Sock) ((: hnd-acpt) This Sock (sock~acpt Sock 'block)))


(dm send-sock> (Sock Buf Sz Flags)
  #{
  * With blocking sockets, will block until space available (all data is buffered)
  * If msg too long, send will fail and no data will be buffered
  }#
                             
  (case (sock~send Sock
                  (if (num? Buf)   @ (: buf-wr))
                  (if (num? Sz)    @ (: sz-buf-wr))
                  (if (num? Flags) @ 0))
    (0  NIL) # Orderly shutdown
    (-1 NIL) # Error
    (T  @))) # Poss != Sz


(dm recv-sock> (Sock Buf Sz Flags)
  #{
  * This is designed for blocking sockets...
  * If no-data and blocking     -> recv will block
  * If no-data and non-blocking -> recv will return -1 and set errno to EAGAIN or EWOULDBLOCK
  * EOF will return 0 on shutdown
  * ERR/HUP will return 0 also
  }#

  (case (sock~recv Sock
                   (if (num? Buf)   @ (: buf-rd))
                   (if (num? Sz)    @ (: sz-buf-rd))
                   (if (num? Flags) @ 0))
    (0  NIL) # Orderly shutdown
    (-1 NIL) # Error
    (T  @)))


(dm call-hnd-conn> (Sock)
  (let Val (cdr (assoc Sock (: conns)))
    ((cadr Val) Sock)))

    
(dm fd> (ID-S)
  (cdr (assoc ID-S (: conns-id))))
  
  
(dm poll> (To)
  #{
    epoll-wait:
    * ep-events size should correspond to maxevents
    * -1 = timeout = block
      0 = return if nothing
    * returns event count
  }#
           
  (let Cnt (epoll~wait> (: ep) To)

    (for Fd (epoll~get-events> (: ep) Cnt)

      (if (= Fd (: lstr))
          (acpt-sock> This Fd)
          (call-hnd-conn> This Fd)))

    Cnt))
