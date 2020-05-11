(de send-msgs (Ipc Sock Msgs)

  #{
    Msgs are a list
    
    ( (Sexpr1 Bin1)
      (Sexpr2 Bin2)
      (Sexpr3 Bin3)
      ...)

    However, they are written back to back (and the list not directly written)

    Try to format all into the pointer, and then return unsent msgs or do
    send all
  }#

  (let Len-Sock 0

    # Ensure it will fit
    # Conservative estimate could be half
  
    (for Msg Msgs
      (let ((Sexpr Bin Sz-Bin) Msg)
        (inc Len-Sock (wr-msg Ipc Sexpr Bin Sz-Bin))))

    (let Len-Sock (wr-msg Ipc Sexpr Bin Sz-Bin)
      
      (ipc~send-bytes> Ipc Sock T Len-Sock))))


(de wr-msg (Ipc Sexpr Bin Sz-Bin)
  (let (Bin    (if Bin @ 0)
        Sz-Bin (if Sz-Bin @ 0)
        Len-Sexpr (+ (length Sexpr) 1)
        Len-Body  (+ Len-Sexpr Sz-Bin)
        Len-Msg   (+ 4 4 Len-Body)
        Len-Sock  (+ 4 4 4 Len-Body))

    (with Ipc

      (struct (: ipc~buf-wr) 'N
              (cons Len-Msg 4)
              (cons Len-Sexpr 4)
              (cons Sz-Bin 4)
              (cons Sexpr Len-Sexpr) 0)

      (when Bin
        (native "@" "memcpy" NIL
                (+ (: ipc~buf-wr) 4 4 4 Len-Sexpr) Bin Sz-Bin)))

    Len-Sock))


# SEND-MSG
# Send single msg


(de send-msg (Ipc Sock Sexpr Bin Sz-Bin)

  # Could do this unblocked; if delayed, then server is lagging and could warn

  (let Len-Sock (wr-msg Ipc Sexpr Bin Sz-Bin)
    (ipc~send-bytes> Ipc Sock T Len-Sock)))


(de send-msg-clients (Ipc Msg)
  (for Client (get Ipc 'ipc~clients)
    #(prinl-info "send-msg-clients" (str Client))
    (send-msg Ipc (car Client) Msg)))


(de send-msg-conns (Ipc Msg)
  (for Conn (get Ipc 'ipc~conns)
    (send-msg Ipc (car Conn) Msg)))


# RECV
# Recv single msg - wait on all data

(de recv-msg (Ipc Sock)

  (let ((Len-Msg Len-Sexpr Sz-Bin) (recv-msg-len Ipc Sock))

    #(prinl-info "recv-msg" (pack "RECV " Len-Msg " " Len-Sexpr " " Sz-Bin))

    (when (and Len-Msg
               (recv-msg-bin Ipc Sock (+ Len-Sexpr Sz-Bin)))

        (with Ipc
          (pack (struct (: ipc~buf-rd) (cons 'C Len-Sexpr)))))))


(de recv-msg-with-bin (Ipc Sock)

  (let ((Len-Msg Len-Sexpr Sz-Bin) (recv-msg-len Ipc Sock))

    #(prinl-info "recv-msg" (pack "RECV " Len-Msg " " Len-Sexpr " " Sz-Bin))

    (when (and Len-Msg
               (recv-msg-bin Ipc Sock (+ Len-Sexpr Sz-Bin)))

      (with Ipc
        (cons (pack (struct (: ipc~buf-rd) (cons 'C Len-Sexpr)))
              Sz-Bin)))))


(de recv-msg-len (Ipc Sock)

  (with Ipc
    
    (if (ipc~recv-bytes> Ipc Sock T 12 sock~MSG-WAITALL)

      (struct (: ipc~buf-rd) '(I . 3))

      (nil (prinl-info "recv-msg-len" (pack "Bytes-Recv=" @))))))


(de recv-msg-bin (Ipc Sock Sz)

  (with Ipc

    (if (ipc~recv-bytes> Ipc Sock T Sz sock~MSG-WAITALL)

      @

      (nil (prinl-info "recv-msg-bin" (pack "(not (= Bytes-Recv=" @ " " Len "))"))))))


# FLUSH


(de flush-part (Ipc Sock Thresh)
  # Fill buffer until threshold
  (while (< (inc '*buf-flush-fill
                 (ipc~recv-bytes> Ipc Sock
                                  (+ *buf-flush *buf-flush-fill)
                                  (- *buf-flush-sz *buf-flush-fill)
                                  0))
            Thresh)
    (yield)))


(def '*dlst (new '(+Dlst)))
# Sock should be loaded
(def '*buf-flush (native "@" "malloc" 'N (sock~get-rmem-def)))


(de write-int-to-bytes (N)
  (list (& (>> 24 N) (hex "FF"))
        (& (>> 16 N) (hex "FF"))
        (& (>> 8  N) (hex "FF"))
        (&  N        (hex "FF"))))


(de read-int-from-bytes (B)
  (| (>> -24 (get B 1))
     (>> -16 (get B 2))
     (>> -8  (get B 3))
     (get B 4)))


(de flush-msgs (Ipc Sock)

  #(prinl-info "flush-msgs-2" (pack "*dlst.len=" (get *dlst 'len)))

  (let? Bytes-Read (ipc~recv-bytes> Ipc Sock T T 0)
    (for B (struct (get Ipc 'ipc~buf-rd) (cons 'B Bytes-Read))
      (let Node (new '(+Node-Dlst) B)
        (ins-las *dlst Node)))
    Bytes-Read))


(de parse-msgs (Ipc Sock Fn-Proc)

  #(prinl-info "flush-msgs-2" (pack "*dlst.len=" (get *dlst 'len)))

  # 2 things
  # 1. Check if list has sufficient data
  # 2. Handle lists

  (let Cont T
  
    (while Cont

      # First check if sufficient length otherwise get head
      (if (not (>= (get *dlst 'len) 12))

        (off Cont)

        (let Head (rem-frs-n-d *dlst 12)

          #(prinl-info "flush-msgs-2" (pack "Head= " (str Head)))

          # Parse Head
          (let (Sz-Bin    (read-int-from-bytes Head)
                Len-Sexpr (read-int-from-bytes (nth Head 5))
                Len-Msg   (read-int-from-bytes (nth Head 9)))

            #(prinl-info "flush-msgs-2" (pack "Sz-Bin=" Sz-Bin ", Len-Sexpr=" Len-Sexpr))

            # Parse Msg
            (if (not (>= (get *dlst 'len) (+ Len-Sexpr Sz-Bin)))

              (prog
                # Push head back
                (for B Head (ins-frs *dlst (new '(+Node-Dlst) B)))
                (off Cont))

              (let (Str  (rem-frs-n-d-c *dlst Len-Sexpr)
                    Msg  (pack (flip (cdr Str)))
                    Data (car (str Msg)))

                    # Parse Bin
                    (when (not (=0 Sz-Bin))
                      (for I Sz-Bin
                        (byte (+ *buf-flush (- I 1))
                              (get (rem-frs *dlst) 'dat))))
                    
                    (Fn-Proc Ipc Sock Data Sz-Bin *buf-flush))))))))

  #(prinl-info "flush-msgs-2" (pack "*dlst.len=" (get *dlst 'len)))
  #(prinl-info "flush-msgs-2" "#### END FLUSH ####")
  #(prinl)
  )
