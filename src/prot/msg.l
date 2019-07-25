# SEND


# Make Sock before Msg
(de send-msg (Ipc Sock Body Bin Sz-Bin)
  #{
  * Send len-msg, len-bin, msg, bin
  * Later could use single call by writing all data to ptr then call send
  }#

  # Check Len does not exceed size of int?

  (let Len (length Body)

    (when (send-msg-len Ipc Sock Len)

      (if Bin

        (send-msg-len Ipc Sock Sz-Bin)

        (send-msg-len Ipc Sock 0)

      (send-msg-body Ipc Sock Len Body)

      (when Bin

          (send-msg-bin Ipc Sock Sz-Bin Bin))))))
          

(de send-msg-len (Ipc Sock Len)

  (with Ipc

    (struct (: ipc~buf-wr) 'N (cons Len 4))

    (let Bytes-Sent (ipc~send-bytes> Ipc Sock (: ipc~buf-wr) 4)

      (if Bytes-Sent

        Bytes-Sent

        (print-info "send-msg-len" "Failed to send")))))


(de send-msg-body (Ipc Sock Len Body)

  (with Ipc
  
    (struct (: ipc~buf-wr) 'N (cons Body Len) (cons 0 1))
    
    (let Bytes-Sent (ipc~send-bytes> Ipc Sock (: ipc~buf-wr) Len)

      (if Bytes-Sent

        Bytes-Sent

        (print-info "send-msg-body" "Failed to send")))))


(de send-msg-clients (Ipc Msg)
  (for Client (get Ipc 'ipc~clients)
    (send-msg Ipc (car Client) Msg)))


(de send-msg-conns (Ipc Msg)
  (for Conn (get Ipc 'ipc~conns)
    (send-msg Ipc (car Conn) Msg)))


# FLUSH


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


# RECV


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
    (let Bytes-Recv (ipc~recv-bytes> Ipc Sock T 4 'socket~MSG-WAITALL)

      (if Bytes-Recv

        Bytes-Recv

        (print-info "recv-msg-len" (pack "(not (= Bytes-Recv=" Bytes-Recv " 4))"))))))


(de recv-msg-body (Ipc Sock Len)

  (with Ipc
  
    (let (Len (car (struct (: ipc~buf-rd) '(I)))
          Bytes-Recv (ipc~recv-bytes> Ipc Sock T Len 'socket~MSG-WAITALL))

      (if Bytes-Recv

        (pack (struct (: ipc~buf-rd) (cons 'C Len)))

        (print-info "recv-msg-body" (pack "(not (= Bytes-Recv=" Bytes-Recv " " Len "))"))))))      