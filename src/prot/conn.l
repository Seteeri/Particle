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


(de accept-client (Ipc Sock Client)

  #(prinl-info "accept-client" (pack "Wait for ID from " Client))

  #Read for ID - this will block
  (let ID (recv-msg Ipc Client)

    (prinl-info "accept-client" (pack "Sock=" Client ", ID= " ID))

    #Disconnect client if name conflict
    (if (assoc Client (get Ipc 'ipc~clients))
        (ipc~disconn-sock Client ID)
        (add-client Ipc Client ID))))


(de add-client (Ipc Client ID)
  (with Ipc
    # Note, client IDs use {ID}-{PID} and outgoing conns use ID-S
    (ipc~add-conn> Ipc Client ID (: ipc~handler-listener))

    # Add to client list also
    (push1 (:: ipc~clients) (cons Client (list ID (: ipc~handler-listener))))

    (prinl-info "add-client" (pack "Sock=" Sock ", ID= " ID))))


(de disconn-client (Ipc Sock)

  (prinl-info "disconn-client" (pack "Sock=" Sock))

  (with Ipc
    (del (assoc Sock (: clients)) (:: clients))
    (let ID (get 1 (cdr (assoc Sock (: conns))))
      (del (assoc ID (: conns-id)) (:: conns-id)))
    (del (assoc Sock (: conns)) (:: conns)))
  
  (epoll~ctl> (: ep)
              epoll~DEL
              Sock
              0)
              
  (ipc~disconn-sock Sock))


(de disconn-conn (Ipc Sock)

  (prinl-info "disconn-conn" (pack "Sock=" Sock))

  (with Ipc
    (let ID (get 1 (cdr (assoc Sock (: conns))))
      (del (assoc ID (: conns-id)) (:: conns-id)))
    (del (assoc Sock (: conns)) (:: conns)))
  
  (epoll~ctl> (: ep)
              epoll~DEL
              Sock
              0)
              
  (ipc~disconn-sock Sock))


(de reg-conns (Ipc ID-C)

  (with Ipc
  
    (for Conn (: ipc~conns)
    
      (prinl-info "reg-conns" (pack ID-C " sent to " (car Conn)))
      
      (send-msg Ipc (car Conn) ID-C))))