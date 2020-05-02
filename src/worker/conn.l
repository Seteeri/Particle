# Requests are sent
# Cmds are recieved

##############################################
  
(de req-send-obj (Ipc Sock Obj Sz-Obj Off-Obj)

  (let Msg (fmt-msg-send> Obj Off-Obj)

    #(prinl-info "req-send-obj" Msg)
    
    (send-msg Ipc
              Sock
              Msg
              *ptr-vert
              (wr-ptr> Obj *ptr-vert))))


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
                (if Obj @ (new '(+Vertex)))
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
                (if Obj @ (new '(+Projview)))
                SZ-PV
                0))


################################################################################


# Rename from cmd-render to rpc-render

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


(de cmd-upd-vert (Ipc Vert)
  (let Sock-Render (ipc~get-fd> *ipc "RENDER")

    (req-send-vert Ipc
                   Sock-Render
                   Vert)))


(de cmd-upd-pv (Ipc)
  (let Sock-Render (ipc~get-fd> *ipc "RENDER")
        
    (req-send-pv Ipc
                 Sock-Render
                 *projview)))
                 

(de cmd-calc-pv (Width Height)
  (with *projview
    (=: width  Width)
    (=: height Height))
  (upd-mat-proj> *projview))
