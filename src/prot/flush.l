#{

  For coroutine:
  
    while read data sock
      If complete msg, push to queue
      else yield
  
  Caller will then process the msgs
  
  Msg body header needs flag
  * Ensure all msgs processed on same frame/batch
  
}#

# UNUSED
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
(def '*buf-flush (native "@" "malloc" 'N (sock~get-rmem-def)))

(de wr-int (N)
  # (hex "FF")
  (list (& (>> 24 N) 255)
        (& (>> 16 N) 255)
        (& (>> 8  N) 255)
        (&        N  255)))


(de rd-int (B)
  (| (>> -24 (car   B))
     (>> -16 (cadr  B))
     (>> -8  (caddr B))
             (cadddr B)))


(de flush-msgs (Ipc Sock)

  #(when (= NAME-PID "RENDER")
  #  (prinl-info "flush-msgs" (pack "Queued Msgs: " (get *dlst 'len))))

  (let? Bytes-Read (ipc~recv-bytes> Ipc Sock T T 0)
    (mapcar '((B) (ins-las *dlst (new '(+Node-Dlst) B)))
            (struct (get Ipc 'ipc~buf-rd) (cons 'B Bytes-Read)))
    #{
    (for B (struct (get Ipc 'ipc~buf-rd) (cons 'B Bytes-Read))
      (let Node (new '(+Node-Dlst) B)
        (ins-las *dlst Node)))
    }#
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

        (prog
          #(prinl-info "parse-msgs" "Incomplete head; try again later")
          (off Cont))

        (let Hd (rem-frs-n-d *dlst 12)

          #(prinl-info "flush-msgs-2" (pack "Hd= " (str Hd)))

          # Parse Hd
          (let (Sz-Bin    (rd-int Hd)
                Len-Sexpr (rd-int (nth Hd 5))
                Len-Msg   (rd-int (nth Hd 9)))

            #(prinl-info "flush-msgs-2" (pack "Sz-Bin=" Sz-Bin ", Len-Sexpr=" Len-Sexpr))

            # Parse Msg
            (if (not (>= (get *dlst 'len) (+ Len-Sexpr Sz-Bin)))

              (prog
                # Push Hd back
                (for B Hd (ins-frs *dlst (new '(+Node-Dlst) B)))
                (off Cont))

              (let (Str  (rem-frs-n-d-c *dlst Len-Sexpr)
                    Msg  (pack (flip (cdr Str)))
                    Data (car (str Msg)))

                # Parse Bin
                (unless (=0 Sz-Bin)
                  (for I Sz-Bin
                    (byte (+ *buf-flush (- I 1))
                          (get (rem-frs *dlst) 'dat))))
                
                (Fn-Proc Ipc Sock Data Sz-Bin *buf-flush)))))))))
