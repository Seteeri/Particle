(de req-send-obj (Ipc Sock Obj Sz-Obj Off-Obj)
  # Better to write all strings/bytes to buf-wr, then send in one go

  (send-msg Ipc
            (fmt-msg-send> Obj Off-Obj)
            Sock)

  (send-obj Ipc Sock Obj Sz-Obj Off-Obj))


(de req-recv-obj (Ipc Sock Obj Sz-Obj Off-Obj)
  # Either pass in object or instance new one

  (send-msg Ipc
            (fmt-msg-recv> Obj Off-Obj)
            Sock)

  (recv-obj Ipc Sock Obj Sz-Obj Off-Obj))


(de update-vert (Ipc Vert Off)
  (req-send-obj Ipc
                (ipc~get-fd> Ipc "RENDER") # Pass string and auto look up?
                Vert
                SZ-VERT
                Off))


(de update-pvm (Ipc Projview)
  (req-send-obj Ipc
                (ipc~get-fd> Ipc "RENDER")
                Projview
                SZ-PV
                0))