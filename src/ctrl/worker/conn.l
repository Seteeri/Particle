# Requests are sent
# Cmds are recieved

##############################################
  

(de req-send-obj (Ipc Sock Obj Sz-Obj Off-Obj)

  (send-msg Ipc
            Sock
            (fmt-msg-send> Obj Off-Obj)
            *ptr-vert
            (serialize-to-ptr> Obj *ptr-vert)))


(de req-recv-obj (Ipc Sock Obj Sz-Obj Off-Obj)
  # Either pass in object or instance new one

  (send-msg Ipc
            Sock
            (fmt-msg-recv> Obj Off-Obj))

  (recv-obj Ipc Sock Obj Sz-Obj Off-Obj))


################################################################################


(de req-send-vert (Ipc Sock Obj)
  (req-send-obj Ipc
                Sock
                Obj
                SZ-VERT
                (get Obj 'off-gl)))


(de req-recv-vert (Ipc Sock Obj Cnt)
  (req-recv-obj Ipc
                Sock
                (if Obj Obj (new '(+Vertex)))
                SZ-VERT
                (get-vert-off Cnt)))


(de req-send-pv (Ipc Sock Obj)
  (req-send-obj Ipc
                Sock
                Obj
                SZ-PV
                0))


(de req-recv-pv (Ipc Sock Obj)
  (req-recv-obj Ipc
                Sock
                (if Obj Obj (new '(+Projview)))
                SZ-PV
                0))


################################################################################


(de cmd-render-vert (Ipc Vert)
  (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
        Sock-Render (ipc~get-fd> *ipc "RENDER"))

    (print-info "render-vert" (pack "Sending vert @ " (get Vert 'off-gl)))

    (req-send-vert Ipc
                   Sock-Render
                   Vert)

    (req-send-vert Ipc
                   Sock-Model
                   Vert)))


(de cmd-render-pv (Ipc Width Height)
  (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
        Sock-Render (ipc~get-fd> *ipc "RENDER"))

    # Pass list of socks so need only serialize once

    #(print-info "render-vert" "Sending pvm...")

    (req-send-pv Ipc
                 Sock-Render
                 *projview)

    (req-send-pv Ipc
                 Sock-Model
                 *projview)))



(de update-vert (Ipc Vert)
  (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
        Sock-Render (ipc~get-fd> *ipc "RENDER"))
        
    (req-send-vert Ipc
                   Sock-Render
                   Vert)

    (req-send-vert Ipc
                   Sock-Model
                   Vert)))


(de cmd-calc-pv (Width Height)
  (with *projview
    (=: width  Width)
    (=: height Height)
    (update-mat-proj> *projview)))


######################


(de update-pv (Ipc Pv)
  (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
        Sock-Render (ipc~get-fd> *ipc "RENDER"))
        
    (req-send-pv Ipc
                 Sock-Render
                 Pv)

    (req-send-pv Ipc
                 Sock-Model
                 Pv)))    