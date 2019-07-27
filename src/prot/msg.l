# SEND


# Make Sock before Msg
(de send-msg (Ipc Sock Body Bin Sz-Bin)
  #{
  * Send len-msg, len-bin, msg, bin
  * Later could use single call by writing all data to ptr then call send
  }#

  # Check Len does not exceed size of int?

  (let Len (+ (length Body) 1)

    (when (send-msg-len Ipc Sock Len)

      (if Bin

        (send-msg-len Ipc Sock Sz-Bin)

        (send-msg-len Ipc Sock 0))

      (send-msg-body Ipc Sock Len Body)
      
      (when Bin

        (send-msg-bin Ipc Sock Sz-Bin Bin)))))
          

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


(de send-msg-bin (Ipc Sock Sz Bin)

  (with Ipc

    (native "@" "memcpy" NIL (: ipc~buf-wr) Bin Sz)
    
    (let Bytes-Sent (ipc~send-bytes> Ipc Sock (: ipc~buf-wr) Sz)

      (if Bytes-Sent

        Bytes-Sent

        (print-info "send-msg-bin" "Failed to send")))))


(de send-msg-clients (Ipc Msg)
  (for Client (get Ipc 'ipc~clients)
    (send-msg Ipc (car Client) Msg)))


(de send-msg-conns (Ipc Msg)
  (for Conn (get Ipc 'ipc~conns)
    (send-msg Ipc (car Conn) Msg)))


# FLUSH


(de flush-msgs (Ipc Sock Fn-Proc)

  # Either use yield or pass fn

  # Recv all data on sock (buffer limit)
  
  (let Bytes-Read (ipc~recv-bytes> Ipc Sock T T 0)

    (if (< Bytes-Read 4)

      (print-info "read-msgs" (pack "Bytes-Read-Len=" Bytes-Read-Len))

      (let (Ptr (get Ipc 'ipc~buf-rd)
            End (+ Ptr Bytes-Read))
        (proc-msgs Ipc Sock Ptr End Fn-Proc)))))


(de proc-msgs (Ipc Sock Ptr End Fn)

  # Pass Msg, Sz-Bin, Bin(ptr)

  (while (< Ptr End)

    (let ((Len-Msg Sz-Bin) (struct Ptr '(I I)))
    
      (inc 'Ptr (+ 4 4))

      (let (Msg  (pack (struct Ptr (cons 'C Len-Msg)))
            Data (any Msg))

        (inc 'Ptr Len-Msg)

        (Fn Ipc Sock Data Sz-Bin Ptr))

      (inc 'Ptr Sz-Bin))))


# RECV


(de recv-msg (Ipc Sock Bin)
  #{
  * Recv length as int, then recv payload
  }#

  (let (Len-Msg (recv-msg-len Ipc Sock)
        Sz-Bin  (recv-msg-len Ipc Sock))

    (when (and Len-Msg Sz-Bin)

      (let Body (recv-msg-body Ipc Sock Len-Msg)

        (if Bin

          (cons Body 
                (when (and Bin Sz-Bin)
                  (recv-msg-bin Ipc Sock Sz-Bin)))

          Body)))))


(de recv-msg-len (Ipc Sock)

  (with Ipc

    (let Bytes-Recv (ipc~recv-bytes> Ipc Sock T 4 sock~MSG-WAITALL)

      (if Bytes-Recv

        (car (struct (: ipc~buf-rd) '(I)))

        (print-info "recv-msg-len" (pack "Bytes-Recv=" Bytes-Recv))))))


(de recv-msg-body (Ipc Sock Len)

  (with Ipc
  
    (let Bytes-Recv (ipc~recv-bytes> Ipc Sock T Len sock~MSG-WAITALL)

      (if Bytes-Recv

        (pack (struct (: ipc~buf-rd) (cons 'C Len)))

        (print-info "recv-msg-body" (pack "(not (= Bytes-Recv=" Bytes-Recv " " Len "))"))))))


(de recv-msg-bin (Ipc Sock Sz Bin)

  (with Ipc
  
    (let (Bytes-Recv (ipc~recv-bytes> Ipc Sock Bin Sz sock~MSG-WAITALL))

      (unless Bytes-Recv

        (print-info "recv-msg-body" (pack "(not (= Bytes-Recv=" Bytes-Recv " " Len "))"))))))