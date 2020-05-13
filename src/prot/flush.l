#{

  For coroutine:
  
    while read data sock
      If complete msg, push to queue
      else yield
  
  Caller will then process the msgs
  
  Msg body header needs flag
  * Ensure all msgs processed on same frame/batch
  
}#

(def '*buf-flush (native "@" "malloc" 'N (sock~get-rmem-def)))

(de wr-int (N)
  # (hex "FF")
  (list (& (>> 24 N) 255)
        (& (>> 16 N) 255)
        (& (>> 8  N) 255)
        (&        N  255)))


(de rd-int (B)
  (| (>> -24 (cadddr   B))
     (>> -16 (caddr  B))
     (>> -8  (cadr B))
             (car B)))
                
#{

  Poss faster method is to maintain pointer to back
  Push front, pop back
  
  (con que-end data)
  (setq que-end (nth data *len-buf))

}#

(def '*len-buf 0)
(off *que-msg)
(def '*que-end NIL)

(de flush-sock (Ipc Sock)

  (let? Sz-Rd (ipc~recv-sock> Ipc Sock T T 0)

    (let D (struct (get Ipc 'ipc~buf-rd) (cons 'B Sz-Rd))      
      (if *que-end
          (con *que-end D)
          (setq *que-msg D))
      (setq *que-end (nth D Sz-Rd)))
            
    (inc '*len-buf Sz-Rd)
        
    Sz-Rd))


(de parse-msgs (Ipc Sock Fn-Proc)

  (co 'co-parse-sock
    
    (loop
      
      (let (Hd NIL
            (Sz-Bin Len-Sexpr Len-Msg) NIL)
            
        # Wait for head
        (until Hd
          (if (>= *len-buf 12)
              (setq Hd (pop-fifo 12)
                    P (parse-hd Hd)
                    Len-Msg   (car P)
                    Len-Sexpr (cadr P)
                    Sz-Bin    (caddr P))
              (yield)))
        
        #(println Sz-Bin Len-Sexpr Len-Msg)        
        
        # Wait for body
        (while (< *len-buf (+ Len-Sexpr Sz-Bin))
          (yield))
                
        # Read sexpr
        # Proc msg
        (let Msg (parse-sexpr Len-Sexpr)
        
          #(println Msg)
              
          # Copy bin to ptr
          # Let fn-proc decide what to do
          # Render does a straight memcpy
          # How to avoid extra cpying?
          # Buffer (kernel) -> List -> Buffer (user)
          # Or pass list?
          #
          # Faster way with struct and single fn call
          (unless (=0 Sz-Bin)
            (dec '*buf-flush)
            (for (I . X) (pop-fifo Sz-Bin)
              (byte (+ *buf-flush I) X))
            (inc '*buf-flush))
                    
          (Fn-Proc Ipc Sock Msg Sz-Bin *buf-flush))))))


(de pop-fifo (Cnt)
  (let L (cut Cnt '*que-msg)
    (dec '*len-buf Cnt)
    (unless *que-msg (off *que-end))
    L))
          
    
# Move to msg?
(de parse-hd (Hd)
  (list (rd-int Hd)
        (rd-int (nth Hd 5))
        (rd-int (nth Hd 9))))


(de parse-sexpr (Cnt)
  (car (str (pack (mapcar '((B) (char B)) (pop-fifo Cnt))))))
