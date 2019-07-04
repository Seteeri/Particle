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
                     T # Handle any error
                     (: ptr)
                     128)))
                     
                     
#{
    (let Vert (get *verts 1)

      # TODO: Pass raw bytes -> faster
      # Returns list of bytes/nums -> encode as string
      (let Data (serialize-to-ptr> Vert)

        (when NIL
          (ipc~send-msg> *ipc Data Fd))))
}#