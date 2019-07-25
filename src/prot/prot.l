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

  #Read for ID - this will block
  (let ID (recv-msg Ipc Client)

    #(prinl (usec) " | accept-client | ID=" ID)

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

    (when (or (: debug-ipc) T)
      (prinl (usec) " | accept-client | Registered Client=" Client ", ID=" ID))))


(de reg-conns (Ipc ID-C)
  (with Ipc
    (for Conn (: ipc~conns)
      #(println (usec) " | reg-conns | " (car Conn))
      (send-msg Ipc ID-C (car Conn)))))


(de recv-msg (Ipc Sock)
  #{
  * Recv length as int, then recv payload
  }#

  (let Len (recv-msg-len Ipc Sock)

    (when Len

      (recv-msg-body Ipc Sock Len))))


(de recv-msg-len (Ipc Sock)

  (with Ipc

    # Always returns 4 or NIL
    (let Bytes-Read (ipc~recv-bytes> Ipc Sock T 4 'socket~MSG-WAITALL)

      (if Bytes-Read

        Bytes-Read

        (print-info "recv-msg-len" (pack "(not (= Bytes-Read=" Bytes-Read " 4))"))))))


(de recv-msg-body (Ipc Sock Len)

  (with Ipc
  
    (let (Len (car (struct (: ipc~buf-rd) '(I)))
          Bytes-Read (ipc~recv-bytes> Ipc Sock T Len 'socket~MSG-WAITALL))

      (if Bytes-Read

        (pack (struct (: ipc~buf-rd) (cons 'C Len)))

        (print-info "recv-msg-body" (pack "(not (= Bytes-Read=" Bytes-Read " " Len "))"))))))


# Make Sock before Msg
(de send-msg (Ipc Msg Sock Debug)
  #{
  * Send length as int, then send body after
  }#

  (let Len (length Msg)

    # Check Len does not exceed size of int?

    # Make fn of Ipc?
    (with Ipc
      (when (> Len (: ipc~sz-buf-rd))
        (print-info "send-msg" (pack "Msg length exceeds " (: ipc~sz-buf-rd) " by " (- Len Len-Max) " bytes"))))

    (send-msg-len Ipc Sock Len)

    (send-msg-body Ipc Sock Len Msg)))
          

(de send-msg-len (Ipc Sock Len)

  (with Ipc

    (struct (: ipc~buf-wr) 'N (cons Len 4))

    (let Bytes-Sent (ipc~send-bytes> Ipc Sock (: ipc~buf-wr) 4)

      (unless Bytes-Sent

        (print-info "send-msg-len" "Failed to send")))))


(de send-msg-body (Ipc Sock Len Body)

  (with Ipc
  
    (struct (: ipc~buf-wr) 'N (cons Body Len) (cons 0 1))
    
    (let Bytes-Sent (ipc~send-bytes> Ipc Sock (: ipc~buf-wr) Len)

      (unless Bytes-Sent

        (print-info "send-msg-body" "Failed to send")))))


(de send-msg-clients (Ipc Msg)
  (for Client (get Ipc 'ipc~clients)
    (send-msg Ipc Msg (car Client))))


(de send-msg-conns (Ipc Msg)
  (for Conn (get Ipc 'ipc~conns)
    (send-msg Ipc Msg (car Conn))))


(de flush-msgs (Ipc Sock Fn-Proc)

  # Either use yield or pass fn

  (for Msg (read-msgs Ipc Sock)
    (Fn-Proc Ipc Sock Msg)))


(de read-msgs (Ipc Sock)

  # Recv buffer limit on sock

  (let Bytes-Read-Len (ipc~recv-bytes> Ipc Sock T T)

    #(prinl (usec) " | read-msgs | Bytes-Read-Len=" Bytes-Read-Len)

    # Should always be at least 1 byte since this is called when data available
    (when (< Bytes-Read-Len 4)
      (prinl (usec) " | read-msgs | (< 4 Bytes-Read-Len=" Bytes-Read-Len ")"))

     (let (Ptr (get Ipc 'ipc~buf-rd)
           Bytes-Proc 0
           Msgs ())

       (while (< Bytes-Proc Bytes-Read-Len)

          # Return msg and entire msg length (offset to next msg)
          (let Data (parse-msg Ptr)
            (push 'Msgs (get Data 1))
            (inc 'Bytes-Proc (get Data 2))
            (inc 'Ptr (get Data 2))))

        Msgs)))


(de parse-msg (Ptr)
  # TODO: Refactor this with recv-msg into single function which takes a ptr

  (let Len (car (struct Ptr '(I)))

    (let (Msg (pack (struct (+ Ptr 4) (cons 'C Len)))
          Data (any Msg)
          Name-Fn (get Data 1)
          Ret NIL)

      #(prinl (usec) " | parse-msg | Msg=" Msg)

      # This is render specific -> move to render/conn
      (if (= Name-Fn "mc")

          (setq Ret (list (list Data (+ Ptr 4 Len))
                                (+ 4 Len (get Data 4))))

          (prinl (usec) " | parse-msg | UNKNOWN REQUEST! " Msg))

      Ret)))


################################################################################


(de recv-obj (Ipc Sock Obj Sz-Obj Off-Obj)
  # In Model/Render: recv object on socket, and deserialize to obj or cpy ptr

  (ipc~recv-bytes> Ipc
                   Sock
                   T  # buf-rd
                   Sz-Obj)

  # Rename to paste
  (cpy-to-obj> Obj (get Ipc 'ipc~buf-rd))

  Obj)


(de send-obj (Ipc Sock Obj Sz-Obj Off-Obj)
  # In Model/Render: serialize object or copy ptr, and send on socket

  (let Len (cpy-to-ptr> Obj (get Ipc 'ipc~buf-wr))

    (ipc~send-bytes> Ipc
                     Sock
                     T # buf-wr
                     Len)))
