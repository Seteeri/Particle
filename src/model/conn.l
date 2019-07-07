(de send-pvm (Sock)
  (with *projview 

    #(print-mat4-3 (: mat-proj))
    #(prinl)
    #(print-mat4-3 (: mat-view))

    (serialize-mat-to-ptr (: mat-proj) (: ptr))
    (serialize-mat-to-ptr (: mat-view) (+ (: ptr) 64))
    
    #(println "recv-conn-ctrl" (struct (: ptr) '(B . 128)))
    
    (ipc~send-bytes> *ipc
                     Sock
                     epoll~BAD
                     (: ptr)
                     128)))


(de send-vert (Sock Cnt)
  (ipc~send-bytes> *ipc
                   Sock
                   epoll~BAD
                   (serialize-to-ptr> (get *verts Cnt))
                   208))