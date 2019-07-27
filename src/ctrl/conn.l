# Split into handlers and requests

(de req-send-obj (Ipc Sock Obj Sz-Obj Off-Obj)

  (send-msg Ipc
            Sock
            (fmt-msg-send> Obj Off-Obj)
            *ptr-vert
            (serialize-to-ptr> Obj *ptr-vert))

  #(println (usec) " | req-send-obj | " (struct *ptr-vert '(B . 233)))

  T)


(de req-recv-obj (Ipc Sock Obj Sz-Obj Off-Obj)
  # Either pass in object or instance new one

  (send-msg Ipc
            Sock
            (fmt-msg-recv> Obj Off-Obj))

  (recv-obj Ipc Sock Obj Sz-Obj Off-Obj))


################################################################################


(de render-vert (Ipc Vert Off)
  (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
        Sock-Render (ipc~get-fd> *ipc "RENDER"))

    (print-info "render-vert" (pack "Sending vert @ " Off))

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

    (print-info "render-vert" "Sending pvm...")

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