(de process-msg (Ipc Sock Data Sz-Bin Bin)

  (let Name (name (get Data 1))
  
    # mcr
    # Copy a range to the socket
          
    (cond ((= Name "mc")
           (handle-memcpy Sock Data Bin))

          ((= Name "rdy")
           (let Msg (pop '*q-msg)
             (if Msg
               (send-msg *ipc Sock Msg)
               (queue '*q-worker Sock))))
                         
          ((= Name "bye")
           # Send to all conns
           (send-msg-conns *ipc "bye")
           (ipc~disconn-sock Sock)
           (ipc~end> *ipc)
           (println "Ctrl says bye!")
           (bye))

          (T
           (prinl-info "CONTROL/process-msg" (pack "UNKNOWN REQUEST! " Data))))))


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