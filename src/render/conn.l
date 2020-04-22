(de proc-msg (Ipc Sock Data Sz-Bin Bin)

  #(prinl-info "proc-msg/RENDER" (str Data))

  (let (Name-Fn (get Data 1))

    (cond ((= Name-Fn "mc")
           (handle-memcpy Sock Data Bin))

          ((= Name-Fn "zv")
           (handle-memset Sock Data))
           
          (T
           (prinl-info "proc-msg/RENDER" (pack "UNKNOWN REQUEST! " Msg))))))


(de handle-memset (Sock Data)
  #(println 'handle-memset Data)
  (let (Off     (get Data 2)
        Ptr-Gl  (+ (get '*buffer-objects 'nodes 'gl~ptr) Off))
    (native "@" "memset" NIL Ptr-Gl 0 208)))


(de handle-memcpy (Sock Data Ptr)

  #(mc Dst Src Sz) + data
  (let ((Dst Src Sz Off) (cdr Data))

    (cond ((and Dst (not Src)) # Write bytes to ..., recv from socket
           (write-dst Ptr Dst Sock Sz Off))

          ((and (not Dst) Src) # Read bytes from ..., send on socket
           (read-src Src Sock Sz Off))

          ((and Dst Src)
           (prinl (usec) " | handle-memcpy | Ptr to ptr memcpy not implemented!"))

          (T
           (prinl (usec) " | handle-memcpy | Invalid Dst Src parameters!")))))


(de write-dst (Ptr Dst Sock Sz Off)

  (cond ((= Dst "projview")
         #(println (usec) " | write-dst projview | " (struct (get *ipc 'ipc~buf) '((B . 64) (B . 64))))
         (let (Ptr-Gl  (get '*buffer-objects 'projview 'gl~ptr)
               Ptr-Buf Ptr)
           (memcpy Ptr-Gl (+ Ptr-Buf 9) 64)
           (memcpy (+ Ptr-Gl 64) (+ Ptr-Buf 9 64 24) 64)))

        ((= Dst "nodes")
         #(println (usec) " | write-dst nodes | " (struct (get *ipc 'ipc~buf) '(B . 241)))
         #(prinl-info "write-dst" (pack "Off=" Off))

         (let (Ptr-Gl  (+ (get '*buffer-objects 'nodes 'gl~ptr) Off)
               Ptr-Buf Ptr)
                                   
           (when NIL
             (println (struct Ptr-Buf
                              '((N . 1)
                                (C . 1)
                                (-1.0 . 3)
                                (-1.0 . 3)
                                (-1.0 . 3)
                                (-1.0 . 16)
                                (-1.0 . 16)
                                (-1.0 . 16)
                                (I . 1))))
             (println (usec) " | write-dst nodes | " (struct (+ Ptr-Buf 1) '((-1.0 . 3) (-1.0 . 3) (-1.0 . 3)))))
            
           (memcpy Ptr-Gl (+ Ptr-Buf 45) (+ 64 64 64 4))
           
           T))))


(de read-src (Src Sock Sz Off)

  # Unused; retrieve data from GL buffers

  (cond ((= Src "projview")
         (prinl (usec) " | read-src | projview not implemented!"))

        ((= Src "nodes")
         (prinl (usec) " | read-src | nodes not implemented!"))))
         
         
