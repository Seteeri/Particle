#{
(mc dst src sz off)
# src = NIL -> read bytes after msg
# dst = NIL -> send back on socket
# sz  = NIL -> read all
}#

(dm handle-memcpy> (Sock Data)

  #(mc Dst Src Sz) + data
  (let (Dst  (get Data 2)
        Src  (get Data 3)
        Sz   (get Data 4)
        Off  (get Data 5))
    
    (cond ((and Dst (not Src)) # Write bytes to ..., recv from socket
           ((: fn-mc-wr) Dst Sock Sz Off))

          ((and (not Dst) Src) # Read bytes from ..., send on socket
           ((: fn-mc-rd) Src Sock Sz Off))
           
          ((and Dst Src)
           (prinl (usec) " | handle-memcpy | Ptr to ptr memcpy not implemented!"))
           
          (T
           (prinl (usec) " | handle-memcpy | Invalid Dst Src parameters!")))))
  
  
# Projview

  
(dm read-pvm> (Sock)

  (send-msg> This "(mc NIL projview 128 0)" Sock)

  (let Bytes-Recv (recv-bytes> This Sock T 128)

    (if (= Bytes-Recv 128)
      (struct (: buf-rd) '(-1.0 . 32)) # Use structure
      (prinl (usec) " | read-pvm | Bytes-Recv=" Bytes-Recv " errno=" (errno)))))


(dm write-pvm> (Pvm Sock)

  # Serialize pvm
  (send-msg> This "(mc projview NIL 128 0)" Sock)

  (struct (: buf-wr) 'N (cons -1.0 Pvm))

  #(println "write-pvm" (struct (: buf-wr) '(B . 128)))

  (send-bytes> This
               Sock
               T
               128))


# Vert


(dm read-vert> (Sock Cnt)

  # Get raw bytes -> Reconstruct object
  (send-msg> This (pack "(mc NIL nodes 208 " (* (- Cnt 1) 208) ")") Sock)
  
  (let Bytes-Recv (recv-bytes> This Sock T 208)
  
    (if (= Bytes-Recv 208)
      (struct (: buf-rd) '((-1.0 . 16) (-1.0 . 16) (-1.0 . 16) (I . 4)))
      (prinl (usec) " | read-vert | Bytes-Recv=" Bytes-Recv " errno=" (errno)))))


(dm write-vert> (Vert Cnt Sock)

  # Serialize pvm
  (send-msg> This (pack "(mc nodes NIL 208 " (* (- Cnt 1) 208) ")") Sock)
  
  (let P (: buf-wr)
  
    (struct P           'N (cons -1.0 (get Vert 1)))
    (struct (inc 'P 64) 'N (cons -1.0 (get Vert 2)))
    (struct (inc 'P 64) 'N (cons -1.0 (get Vert 3)))
    (struct (inc 'P 64) 'N (cons (get (get Vert 4) 1) 4)
                           (cons (get (get Vert 4) 2) 4)
                           (cons (get (get Vert 4) 3) 4)
                           (cons (get (get Vert 4) 4) 4)))
  (send-bytes> This
               Sock
               T
               208))