(de write-dst (Dst Sock Sz Off)

  (ipc~recv-bytes> *ipc Sock NIL Sz)

  (cond ((= Dst "projview")
         T)
   
        ((= Dst "nodes")
         T)))


(de read-src (Src Sock Sz Off)

  (cond ((= Src "projview")
         (with *projview
           (serialize-mat-to-ptr (: mat-proj) (: ptr))
           (serialize-mat-to-ptr (: mat-view) (+ (: ptr) 64))
           (ipc~send-bytes> *ipc
                            Sock
                            (: ptr)
                            128)))
   
        ((= Src "nodes")
         (ipc~send-bytes> *ipc
                          Sock
                          (serialize-to-ptr> (get *verts (+ (/ Off 208) 1)))
                          208))))