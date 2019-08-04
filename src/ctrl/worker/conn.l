# Requests are sent
# Cmds are recieved

##############################################
  
(de req-send-obj (Ipc Sock Obj Sz-Obj Off-Obj)

  (let Msg (fmt-msg-send> Obj Off-Obj)
    
    (send-msg Ipc
              Sock
              Msg
              *ptr-vert
              (serialize-to-ptr> Obj *ptr-vert))))


(de req-recv-obj (Ipc Sock Obj Sz-Obj Off-Obj)
  # Either pass in object or instance new one

  (let Msg (fmt-msg-recv> Obj Off-Obj)

    #(prinl-info "req-recv-obj" Msg)

    (send-msg Ipc
              Sock
              Msg))

  (recv-obj Ipc
            Sock
            Obj
            Sz-Obj))


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
  (let (Sock-Render (ipc~get-fd> *ipc "RENDER"))
    #(prinl-info "render-vert" (pack "Sending vert @ " (get Vert 'off-gl)))
    (req-send-vert Ipc
                   Sock-Render
                   Vert)))


(de cmd-render-pv (Ipc)
  (let (Sock-Render (ipc~get-fd> *ipc "RENDER"))
    (req-send-pv Ipc
                 Sock-Render
                 *projview)))


(de cmd-update-vert (Ipc Vert)
  (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
        Sock-Render (ipc~get-fd> *ipc "RENDER"))

    (req-send-vert Ipc
                   Sock-Render
                   Vert)

    (req-send-vert Ipc
                   Sock-Model
                   Vert)))


(de cmd-update-pv (Ipc)
  (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
        Sock-Render (ipc~get-fd> *ipc "RENDER"))
        
    (req-send-pv Ipc
                 Sock-Render
                 *projview)

    (req-send-pv Ipc
                 Sock-Model
                 *projview)))
                 

(de cmd-calc-pv (Width Height)
  (with *projview
    (=: width  Width)
    (=: height Height)
    (update-mat-proj> *projview)))