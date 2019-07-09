(de write-dst (Dst Sock Sz Off)

  (ipc~recv-bytes> *ipc Sock NIL Sz)

  (cond ((= Dst "projview")
         # Update model matrix
         T)
   
        ((= Dst "nodes")
         T))
         
  # After update must broadcast data to all (except sender)
  # For each client:
  #   (send-msg>   *ipc "(update-cache Src/Dst Sz Off")
  #   (send-bytes> *ipc (: buf-rd))
  
  # Alternative is to have P2P workers/ctrl network
  # On update, peer sends to everyone...
  # What if two workers editing same data and broadcast, then data in each peer
  # will be arbitrary, possibly different and not all the same
  
  T)


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