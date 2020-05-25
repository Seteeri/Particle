(de proc-msg (Ipc Sock Data Sz-Bin Bin)

  (case (car Data)

    ("mc"
      (serv-memcpy Sock Data Bin))

    ("zv"
      (serv-memset Sock Data))
      
    (T
      T)))


(de serv-memset (Sock Data)
  # Render only
  (let (Off     (get Data 2)
        Ptr-Gl  (+ (get '*buffer-objects 'nodes 'gl~ptr) Off))
    (native "@" "memset" NIL Ptr-Gl 0 (meta '(+Vertex) 'sz-gl))))


(de serv-memcpy (Sock Data Ptr)

  #(mc Dst Src Sz) + data
  (let ((Dst Src Sz Off) (cdr Data))

    (cond ((and Dst (not Src)) # Write bytes to ..., recv from socket
           (wr-dst Ptr Dst Sock Sz Off))

          ((and (not Dst) Src) # Read bytes from ..., send on socket
           (rd-src Src Sock Sz Off))

          ((and Dst Src)
           (prinl (usec) " | serv-memcpy | Ptr to ptr memcpy not implemented!"))

          (T
           (prinl (usec) " | serv-memcpy | Invalid Dst Src parameters!")))))


(de wr-dst (Ptr Dst Sock Sz Off)

  (cond ((= Dst "projview")
         (let (Ptr-Gl  (get '*buffer-objects 'projview 'gl~ptr)
               Ptr-Buf Ptr)
           (memcpy Ptr-Gl (+ Ptr-Buf 9) 64)
           (memcpy (+ Ptr-Gl 64) (+ Ptr-Buf 9 64 24) 64)))

        ((= Dst "nodes")
         (let (Ptr-Gl  (+ (get '*buffer-objects 'nodes 'gl~ptr) Off)
               Ptr-Buf Ptr)
                                                 
           # 45 = off @ mm
           # should calc auto
           (memcpy Ptr-Gl (+ Ptr-Buf 45) (+ 64 64 64 4))))))


(de rd-src (Src Sock Sz Off)

  # Unused; retrieve data from GL buffers

  (cond Src
    ("projview"
      (prinl (usec) " | rd-src | projview not implemented!"))

    ("nodes"
      (prinl (usec) " | rd-src | nodes not implemented!"))))
