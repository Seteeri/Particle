# SEND-MSG
# Send single msg


(de send-msg (Ipc Sock Sexpr Bin Sz-Bin)

  # Write, in order:
  # - Len-Msg   (Int)
  # - Len-Sexpr (Int)
  # - Sz-Bin   (Int)
  # - Sexpr     (Utf-8)
  # - Bin       (Bytes)

  (let (Bin    (if Bin Bin 0)
        Sz-Bin (if Sz-Bin Sz-Bin 0)
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

    #(prinl-info "send-msg" "SEND")
    #(println Len-Sock Len-Sexpr Sz-Bin Sexpr Bin)
    #(println (struct (: ipc~buf-wr) (cons 'B Len-Sock))))

    (let Bytes-Sent (ipc~send-bytes> Ipc Sock T Len-Sock)

      (if Bytes-Sent

        Bytes-Sent

        (nil (prinl-info "send-msg-len" (pack "Failed to send" " " (if Bytes-Sent Bytes-Sent NIL))))))))


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

    (let Bytes-Recv (ipc~recv-bytes> Ipc Sock T 12 sock~MSG-WAITALL)

      (if Bytes-Recv

        (struct (: ipc~buf-rd) '(I . 3))

        (nil (prinl-info "recv-msg-len" (pack "Bytes-Recv=" Bytes-Recv)))))))


(de recv-msg-bin (Ipc Sock Sz)

  (with Ipc

    (let (Bytes-Recv (ipc~recv-bytes> Ipc Sock T Sz sock~MSG-WAITALL))

      (if Bytes-Recv

        Bytes-Recv

        (nil (prinl-info "recv-msg-body" (pack "(not (= Bytes-Recv=" Bytes-Recv " " Len "))")))))))


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
(def '*buf-flush (native "@" "malloc" 'N
                  (pipe
                    (call "cat" "/proc/sys/net/core/rmem_default")
                    (read))))


(de write-int-to-bytes (N)
  (list (& (>> 24 N) (hex "FF"))
        (& (>> 16 N) (hex "FF"))
        (& (>> 8  N) (hex "FF"))
        (&  N        (hex "FF"))))


(de read-int-from-bytes (Bytes)
  (| (>> -24 (get Bytes 1))
     (>> -16 (get Bytes 2))
     (>> -8  (get Bytes 3))
     (get Bytes 4)))


(de flush-msgs-2 (Ipc Sock Fn-Proc)

  # Alternative is two have two buffers that we swap
  # But length is limited
  # Doubly linked list is more dynamic/simpler?

  #(prinl)
  #(prinl-info "flush-msgs-2" "#### START FLUSH ####")
  #(prinl-info "flush-msgs-2" (pack "*dlst.len=" (get *dlst 'len)))

  (let Bytes-Read (ipc~recv-bytes> Ipc Sock T T 0)
    (for B (struct (get Ipc 'ipc~buf-rd) (cons 'B Bytes-Read))
      (let Node (new '(+Node-Dlst) B)
        (ins-las *dlst Node))))

  #(prinl-info "flush-msgs-2" (pack "*dlst.len=" (get *dlst 'len)))

  # 2 things
  # 1. Check if list has sufficient data
  # 2. Handle lists

  (let Cont T
    (while Cont

      # First check if sufficient length otherwise get head
      (if (not (>= (get *dlst 'len) 12))

        (setq Cont NIL)

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
                (setq Cont NIL))

              (let (Str  (rem-frs-n-d-c *dlst Len-Sexpr)
                    Msg  (pack (flip (cdr Str)))
                    Data (any Msg))

                    # Parse Bin
                    (if (=0 Sz-Bin)

                      (Fn-Proc Ipc Sock Data Sz-Bin *buf-flush)

                      (prog
                        (for I Sz-Bin
                          (byte (+ *buf-flush (- I 1))
                                (get (rem-frs *dlst) 'dat)))
                        (Fn-Proc Ipc Sock Data Sz-Bin *buf-flush))))))))

    #(prinl-info "flush-msgs-2" (pack "End loop with *dlst.len=" (get *dlst 'len)))
    ))

  #(prinl-info "flush-msgs-2" (pack "*dlst.len=" (get *dlst 'len)))
  #(prinl-info "flush-msgs-2" "#### END FLUSH ####")
  #(prinl)
  )
