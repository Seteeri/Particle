# These functions read/write objs to socket

(de recv-obj (Ipc Sock Obj Sz-Obj Off-Obj)
  # In Model/Render: recv object on socket, and deserialize to obj or cpy ptr

  (when (ipc~recv-bytes> Ipc Sock T Sz-Obj)

    # Rename to paste
    (cpy-to-obj> Obj (get Ipc 'ipc~buf-rd)))

    Obj)


(de send-obj (Ipc Sock Obj Sz-Obj Off-Obj)
  # In Model/Render: serialize object or copy ptr, and send on socket

  (let Len (cpy-to-ptr> Obj (get Ipc 'ipc~buf-wr))

    (ipc~send-bytes> Ipc
                     Sock
                     T # buf-wr
                     Len)))
