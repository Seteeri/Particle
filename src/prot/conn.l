# Default namespace

#{
Poss create +Prot class to hold ipc instead of passing it around constantly

Protocol should only call methods on Ipc and Objects passed to it

(mc dst src sz off)
# src = NIL -> read bytes after msg
# dst = NIL -> send back on socket
# sz  = NIL -> read all
}#
#{
(memcpy dst src sz off)
If src NIL: read bytes after msg
If dst NIL: send bytes back
}#


(de acpt-cli (Ipc Sock Cli)

  #(prinl-info "acpt-client" (pack "Wait for ID from " Cli))

  # Read for ID - this will block
  (let? ID (recv-msg Ipc Cli)

    (prinl-info "acpt-client" (pack "Sock=" Cli ", ID= " ID))
    
    # Disconnect client if name conflict
    (if (assoc Cli (get Ipc 'ipc~clients))
        (ipc~disc-sock Cli ID)
        (add-cli Ipc Cli ID))))


(de add-cli (Ipc Cli ID)
  (with Ipc
    # Note, client IDs use {ID}-{PID} and outgoing conns use ID-S
    (ipc~add-conn> Ipc Cli ID (: ipc~hnd-lstr))

    # Add to client list also
    (push1 (:: ipc~clients) (cons Cli (list ID (: ipc~hnd-lstr))))

    (prinl-info "add-client" (pack "Sock=" Sock ", ID= " ID))))


(de disc-cli (Ipc Sock)

  (prinl-info "disc-cli" (pack "Sock=" Sock))

  (with Ipc
    (del (assoc Sock (: clients)) (:: clients))
    (let ID (get (cdr (assoc Sock (: conns))) 1)
      (del (assoc ID (: conns-id)) (:: conns-id)))
    (del (assoc Sock (: conns)) (:: conns))
  
    # Why is this NIL???
    (when (: ep)
      (epoll~ctl> (: ep)
                  epoll~DEL
                  Sock
                  0)))
              
  (ipc~disc-sock Sock)
  
  # Exit for now
  (bye))  


(de disconn-conn (Ipc Sock)

  (prinl-info "disconn-conn" (pack "Sock=" Sock))

  (with Ipc
    (let ID (get (cdr (assoc Sock (: conns))) 1)
      (del (assoc ID (: conns-id)) (:: conns-id)))
    (del (assoc Sock (: conns)) (:: conns))
  
    # Why is this NIL???
    (when (: ep)
      (epoll~ctl> (: ep)
                  epoll~DEL
                  Sock
                  0)))
              
  (ipc~disc-sock Sock)
  
  # Exit for now
  (bye))


(de reg-conns (Ipc ID-C)

  (with Ipc
  
    (for Conn (: ipc~conns)
    
      (prinl-info "reg-conns" (pack ID-C " sent to " (car Conn)))
      
      (send-msg Ipc (car Conn) ID-C))))
