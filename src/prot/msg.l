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

        (prinl-info "send-msg-len" "Failed to send")))))


(de send-msg-body (Ipc Sock Len Body)

  (with Ipc

    (struct (: ipc~buf-wr) 'N (cons Body Len) (cons 0 1))

    (let Bytes-Sent (ipc~send-bytes> Ipc Sock (: ipc~buf-wr) Len)

      (if Bytes-Sent

        Bytes-Sent

        (prinl-info "send-msg-body" "Failed to send")))))


(de send-msg-bin (Ipc Sock Sz Bin)

  (with Ipc

    (native "@" "memcpy" NIL (: ipc~buf-wr) Bin Sz)

    (let Bytes-Sent (ipc~send-bytes> Ipc Sock (: ipc~buf-wr) Sz)

      (if Bytes-Sent

        Bytes-Sent

        (prinl-info "send-msg-bin" "Failed to send")))))


(de send-msg-clients (Ipc Msg)
  (for Client (get Ipc 'ipc~clients)
    #(prinl-info "send-msg-clients" (str Client))
    (send-msg Ipc (car Client) Msg)))


(de send-msg-conns (Ipc Msg)
  (for Conn (get Ipc 'ipc~conns)
    (send-msg Ipc (car Conn) Msg)))


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

      # What if < 4?
      (if Bytes-Recv

        (car (struct (: ipc~buf-rd) '(I)))

        (prinl-info "recv-msg-len" (pack "Bytes-Recv=" Bytes-Recv))))))


(de recv-msg-body (Ipc Sock Len)

  (with Ipc

    (let Bytes-Recv (ipc~recv-bytes> Ipc Sock T Len sock~MSG-WAITALL)

      (if Bytes-Recv

        (pack (struct (: ipc~buf-rd) (cons 'C Len)))

        (prinl-info "recv-msg-body" (pack "(not (= Bytes-Recv=" Bytes-Recv " " Len "))"))))))


(de recv-msg-bin (Ipc Sock Sz Bin)

  (with Ipc

    (let (Bytes-Recv (ipc~recv-bytes> Ipc Sock Bin Sz sock~MSG-WAITALL))

      (unless Bytes-Recv

        (prinl-info "recv-msg-body" (pack "(not (= Bytes-Recv=" Bytes-Recv " " Len "))"))))))


# FLUSH


(de flush-msgs (Ipc Sock Fn-Proc)

  (let Ptr (get Ipc 'ipc~buf-rd)

    #(while (< (ipc~recv-bytes> Ipc Sock T T (| sock~MSG-PEEK sock~MSG-DONTWAIT)) 8)
    #  (yield))

    (let Bytes-Read (ipc~recv-bytes> Ipc Sock T 8 sock~MSG-WAITALL)

      # Get body lengths
      (let ((Len-Msg Sz-Bin) (struct Ptr '(I I)))

        #(prinl-info "proc-msgs" (pack Len-Msg " " Sz-Bin))

        # Debug
        #(when (or (< Len-Msg 0) (< Sz-Bin 0))
        #  (prinl-info "proc-msgs" "Len-Msg or Sz-Bin invalid!"))

        #(while (< (ipc~recv-bytes> Ipc Sock T T (| sock~MSG-PEEK sock~MSG-DONTWAIT)) (+ Len-Msg Sz-Bin))
        #  (yield))

        (let Bytes-Read (ipc~recv-bytes> Ipc Sock T (+ Len-Msg Sz-Bin) sock~MSG-WAITALL)

          # Process msg
          (let (Msg  (pack (struct Ptr (cons 'C Len-Msg)))
                Data (any Msg))

            (Fn-Proc Ipc Sock Data Sz-Bin (+ Ptr Len-Msg))))))))


(de flush-part (Ipc Sock Thresh)
  # Fill buffer until threshold
  (while (< (inc '*buf-flush-fill
                 (ipc~recv-bytes> Ipc Sock
                                  (+ *buf-flush *buf-flush-fill)
                                  (- *buf-flush-sz *buf-flush-fill)
                                  0))
            Thresh)
    (prinl-info "flush-part" (pack "YIELD! *buf-flush-fill=" *buf-flush-fill " Thresh=" Thresh))
    (yield))
  (prinl-info "flush-part" (pack "*buf-flush-fill=" *buf-flush-fill " Thresh=" Thresh)))

################################################################################

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
    #(prinl-info "flush-msgs-2" (pack "Bytes-Read=" Bytes-Read)))

  #(prinl-info "flush-msgs-2" (pack "*dlst.len=" (get *dlst 'len)))

  # 2 things
  # 1. Check if list has sufficient data
  # 2. Handle lists

  (let Cont T
    (while Cont

      # First check if sufficient length otherwise get head
      (if (not (>= (get *dlst 'len) 8))

        (setq Cont NIL)
      
        (let Head (rem-frs-n-d *dlst 8)

          #(prinl-info "flush-msgs-2" (pack "Head= " (str Head)))
          
          # Parse Head
          (let (Sz-Bin  (read-int-from-bytes Head)          #cut 4 'Head
                Len-Msg (read-int-from-bytes (nth Head 5)))

            #(prinl-info "flush-msgs-2" (pack "Sz-Bin=" Sz-Bin ", Len-Msg=" Len-Msg))

            #(when (or (> Len-Msg 212992) (< Len-Msg 0))
            #  (prinl-info "flush-msgs-2" "LEN-MSG FAILURE" (pack Len-Msg))
            #  (wait 1000000))
            #(when (or (> Sz-Bin 212992) (< Sz-Bin 0))
            #  (prinl-info "flush-msgs-2" "SZ-BIN FAILURE" (pack Sz-Bin))
            #  (wait 1000000))

            # Parse Msg

            (if (not (>= (get *dlst 'len) (+ Len-Msg Sz-Bin)))

              (prog
                #(println "Body len short, try after next EPOLLIN" (get *dlst 'len) (+ Len-Msg Sz-Bin))
                # Push head back
                (for B Head (ins-frs *dlst (new '(+Node-Dlst) B)))
                (setq Cont NIL))
            
              (let (Str  (rem-frs-n-d-c *dlst Len-Msg)
                    Msg  (pack (flip (cdr Str)))
                    Data (any Msg))

                    # Parse Bin
                    (if (=0 Sz-Bin)
                      (Fn-Proc Ipc Sock Data Sz-Bin *buf-flush)
                      (prog
                        # Bin is backwards?
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
