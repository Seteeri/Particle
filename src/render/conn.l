(de proc-msg (Ipc Sock Data Sz-Bin Bin)

  (case (car Data)

    ("mc"
      (handle-memcpy Sock Data Bin))

    ("zv"
      (handle-memset Sock Data))
      
    (T
      (prinl-info "proc-msg/RENDER" (pack "UNKNOWN REQUEST! " Msg)))))


(de handle-memset (Sock Data)
  # Render only
  (let (Off     (get Data 2)
        Ptr-Gl  (+ (get '*buffer-objects 'nodes 'gl~ptr) Off))
    (native "@" "memset" NIL Ptr-Gl 0 (meta '(+Vertex) 'sz-gl))))


(de handle-memcpy (Sock Data Ptr)

  #(mc Dst Src Sz) + data
  (let ((Dst Src Sz Off) (cdr Data))

    (cond ((and Dst (not Src)) # Write bytes to ..., recv from socket
           (wr-dst Ptr Dst Sock Sz Off))

          ((and (not Dst) Src) # Read bytes from ..., send on socket
           (rd-src Src Sock Sz Off))

          ((and Dst Src)
           (prinl (usec) " | handle-memcpy | Ptr to ptr memcpy not implemented!"))

          (T
           (prinl (usec) " | handle-memcpy | Invalid Dst Src parameters!")))))


(de wr-dst (Ptr Dst Sock Sz Off)

  (cond ((= Dst "projview")
         (let (Ptr-Gl  (get '*buffer-objects 'projview 'gl~ptr)
               Ptr-Buf Ptr)
           #(println (usec) " | wr-dst projview | " (struct Ptr '((B . 64) (B . 64))))
           (memcpy Ptr-Gl (+ Ptr-Buf 9) 64)
           (memcpy (+ Ptr-Gl 64) (+ Ptr-Buf 9 64 24) 64)))

        ((= Dst "nodes")
         #(println (usec) " | wr-dst nodes | " (struct (get *ipc 'ipc~buf) '(B . 241)))
         #(prinl-info "wr-dst" (pack "Off=" Off))

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
             (println (usec) " | wr-dst nodes | " (struct (+ Ptr-Buf 1) '((-1.0 . 3) (-1.0 . 3) (-1.0 . 3)))))
          
           # 45 = off @ mm
           # should calc auto
           (memcpy Ptr-Gl (+ Ptr-Buf 45) (+ 64 64 64 4))
           
           T))))


(de rd-src (Src Sock Sz Off)

  # Unused; retrieve data from GL buffers

  (cond Src
    ("projview"
      (prinl (usec) " | rd-src | projview not implemented!"))

    ("nodes"
      (prinl (usec) " | rd-src | nodes not implemented!"))))
