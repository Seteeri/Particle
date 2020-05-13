(de recv-obj (Ipc Sock Obj Sz-Obj)
  # In Model/Render: recv object on socket, and deserialize to obj or cpy ptr

  (when (ipc~recv-sock> Ipc Sock T Sz-Obj)

    (cpy-to-obj> Obj (get Ipc 'ipc~buf-rd)))

    Obj)


(de send-obj (Ipc Sock Obj Sz-Obj)
  # In Model/Render: serialize object or copy ptr, and send on socket

  (let Len (cpy-to-ptr> Obj (get Ipc 'ipc~buf-wr))

    (ipc~send-sock> Ipc
                     Sock
                     T # buf-wr
                     Len)))
