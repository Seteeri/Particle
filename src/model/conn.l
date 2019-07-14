(de handle-memcpy (Sock Data)

  #(mc Dst Src Sz) + data
  (let (Dst  (get Data 2)
        Src  (get Data 3)
        Sz   (get Data 4)
        Off  (get Data 5))
    
    (cond ((and Dst (not Src)) # Recv sock; cpy to ptr
           (write-dst Dst Sock Sz Off))

          ((and (not Dst) Src) # Cpy ptr; send on sock
           (read-src Src Sock Sz Off))
           
          ((and Dst Src)
           (prinl (usec) " | handle-memcpy | Ptr to ptr memcpy not implemented!"))
           
          (T
           (prinl (usec) " | handle-memcpy | Invalid Dst Src parameters!")))))


(de write-dst (Dst Sock Sz Off)

  # Call recv-obj

  (ipc~recv-bytes> *ipc Sock NIL Sz)

  (cond ((= Dst "projview")
         (prinl (usec) " | write-dst | projview not implemented!"))
   
        ((= Dst "nodes")
         (prinl (usec) " | write-dst | nodes not implemented!"))))


(de read-src (Src Sock Sz Off)

  # Return serialized bytes on socket
  (cond ((= Src "projview")
         (send-obj *ipc Sock *projview 128))

        ((= Src "nodes")
         (send-obj *ipc Sock (get *verts (+ (/ Off 208) 1)) 208))))
    
