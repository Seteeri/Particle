(de process-msg (Ipc Sock Data Sz-Bin Bin)

  #(prinl-info "process-msg/MODEL" (str Data))

  (let Name-Fn (get Data 1)

    # For now, assume memcpy
    (cond ((= Name-Fn "mc")
           (handle-memcpy Sock Data Bin))

          ((= Data "bye")
           (ipc~disconn-sock Sock)
           (ipc~end> *ipc)
           (println "Model says bye!")
           (bye))

          (T
           (prinl-info "MODEL/process-msg" (pack "UNKNOWN REQUEST! " Data))))))


(de handle-memcpy (Sock Data Ptr)

  #(println "handle-memcpy" Data)

  #(mc Dst Src Sz) + data
  (let ((Dst Src Sz Off) (cdr Data))

    (cond ((and Dst (not Src)) # Write bytes to ..., recv from socket
           (write-dst Ptr Dst Sock Sz Off))

          ((and (not Dst) Src) # Read bytes from ..., send on socket
           (read-src Src Sock Sz Off))

          ((and Dst Src)
           (prinl-info "handle-memcpy" "Ptr to ptr memcpy not implemented!"))

          (T
           (prinl-info "handle-memcpy" "Invalid Dst Src parameters!")))))


(de write-dst (Ptr Dst Sock Sz Off)

  #(println "write-dst" Ptr Dst Sock Sz Off)

  (cond ((= Dst "projview")
         (cpy-to-obj> *projview Ptr))
   
        ((= Dst "nodes")
         (cpy-to-obj> (get *verts (get-vert-ix Off)) Ptr))))


(de read-src (Src Sock Sz Off)

  #(println "read-src" Src Sock Sz Off)
  
  (cond ((= Src "projview")  
         (send-obj *ipc Sock *projview SZ-PV))

        ((= Src "nodes")
         (send-obj *ipc Sock (get *verts (get-vert-ix Off)) SZ-VERT))))