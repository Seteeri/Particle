# Default namespace

#{
Poss create +Prot class to hold ipc instead of passing it around constantly

Protocol should only call methods on Ipc and Objects passed to it

(mc dst src sz off)
# src = NIL -> read bytes after msg
# dst = NIL -> send back on socket
# sz  = NIL -> read all
}#


(de accept-client (Ipc Sock Client)

  #(print-info "accept-client" (pack "Wait for ID from " Client))

  #Read for ID - this will block
  (let ID (recv-msg Ipc Client)

    (print-info "accept-client" (pack "Sock= " Client ", ID= " ID))

    #Disconnect client if name conflict
    (if (assoc Client (get Ipc 'ipc~clients))
        (disconnect-sock Client ID)
        (add-client Ipc Client ID))))


(de disconnect-sock (Client ID)
  (prinl "WARNING! ID=" ID " exists; disconnecting client=" Client)
  (ipc~disconnect-sock Client))


(de add-client (Ipc Client ID)
  (with Ipc
    # Note, client IDs use {ID}-{PID} and outgoing conns use ID-S
    (ipc~add-conn> Ipc Client ID (: ipc~handler-listener))

    # Add to client list also
    (push1 (:: ipc~clients) (cons Client (list ID (: ipc~handler-listener))))

    (print-info "add-client" (pack "Sock= " Sock ", ID= " ID))))


(de reg-conns (Ipc ID-C)

  (with Ipc
  
    (for Conn (: ipc~conns)
    
      (print-info "reg-conns" (pack ID-C " sent to " (car Conn)))
      
      (send-msg Ipc (car Conn) ID-C))))