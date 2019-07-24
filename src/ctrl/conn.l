# Split into handlers and requests

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


(de req-draw (Ipc Sock)
  # Async
  
  (send-msg Ipc
            "(draw)"
            Sock))
  
  
################################################################################


(de render-vert (Ipc Vert Off)
  (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
        Sock-Render (ipc~get-fd> *ipc "RENDER"))
    (req-send-obj Ipc
                  Sock-Render
                  Vert
                  SZ-VERT
                  Off)
    (req-send-obj Ipc
                  Sock-Model
                  Vert
                  SZ-VERT
                  Off)))             


(de render-pvm (Ipc Projview Width Height)
  # Update and send
  (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
        Sock-Render (ipc~get-fd> *ipc "RENDER"))

    (with *projview
      (=: width  Width)
      (=: height Height)
      (update-mat-proj> *projview))

    # Either send relevant data or send entire data
    # -> Minimize render processing time

    # Pass list of socks so need only serialize once
    
    (req-send-obj Ipc
                  Sock-Render
                  Projview
                  SZ-PV
                  0)

    (req-send-obj Ipc
                  Sock-Model
                  Projview
                  SZ-PV
                  0)))