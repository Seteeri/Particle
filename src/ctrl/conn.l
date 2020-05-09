(de proc-msg (Ipc Sock Data Sz-Bin Bin)
  
  # mcr
  # Copy a range to the socket
        
  (case (name (car Data))
  
    ("mc"
      (handle-memcpy Sock Data Bin))

    ("rdy"
      (let Msg (pop '*q-msg)
        (if Msg
          (send-msg *ipc Sock Msg)
          (queue '*q-worker Sock))))
                    
    ("bye"
      # Send to all conns
      (send-msg-conns *ipc "(bye)")
      (ipc~disconn-sock Sock)
      (ipc~end> *ipc)
      (println "Ctrl says bye!")
      (bye))

    (T
      (prinl-info "CONTROL/proc-msg" (pack "UNKNOWN REQUEST! " Data)))))


(de handle-memcpy (Sock Data Ptr)

  #(println "handle-memcpy" Data)

  #(mc Dst Src Sz) + data
  (let ((Dst Src Sz Off) (cdr Data))

    (cond ((and Dst (not Src)) # Write bytes to ..., recv from socket
           (wr-dst Ptr Dst Sock Sz Off))

          ((and (not Dst) Src) # Read bytes from ..., send on socket
           (rd-src Src Sock Sz Off))

          ((and Dst Src)
           (prinl-info "handle-memcpy" "Ptr to ptr memcpy not implemented!"))

          (T
           (prinl-info "handle-memcpy" "Invalid Dst Src parameters!")))))


(de wr-dst (Ptr Dst Sock Sz Off)

  (println "wr-dst" Ptr Dst Sock Sz Off)

  (case Dst
  
    ("projview"
      (cpy-to-obj> *projview Ptr))
   
    ("nodes"
      (cpy-to-obj> (get *verts (get-vert-ix Off)) Ptr))))


(de rd-src (Src Sock Sz Off)

  #(println "rd-src" Src Sock Sz Off)
  
  (case Src
  
    ("projview"
      (send-obj *ipc Sock *projview (meta '(+Projview) 'sz-pl)))

    ("nodes"
      (send-obj *ipc Sock (get *verts (get-vert-ix Off)) (meta '(+Vertex) 'sz-pl)))))
