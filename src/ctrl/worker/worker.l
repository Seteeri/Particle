#{
    Global variables start with an asterisk "*"
    Global constants may be written all-uppercase
    Functions and other global symbols start with a lower case letter
    Locally bound symbols start with an upper case letter
    Local functions start with an underscore "_"
    Classes start with a plus-sign "+", where the first letter
        is in lower case for abstract classes
        and in upper case for normal classes
    Methods end with a right arrow ">"
    Class variables may be indicated by an upper case letter
}#


(symbols 'worker 'pico)

# Set math precision to 6 decimals
(scl 6)


(def 'NAME-PID "WORKER")

# Paths
(def 'PATH-UDS-MODEL   "/tmp/particle-model.socket")
(def 'PATH-UDS-CONTROL "/tmp/particle-control.socket")
(def 'PATH-UDS-RENDER  "/tmp/particle-render.socket")
(def 'PATH-GLYPHS      "/home/user/Git/particle/src/glyphs")

# Constants
(def '*sca-vert  (*/ 1.0 1.0 96.0))
(def '*sca-glyph 5.8239365)   # MSDF/tex parameter
(def '*adv-glyph 9.375)       # MSDF/tex parameter
(def '*adv-vert  (*/ (*/ *adv-glyph *sca-glyph 1.0) *sca-vert 1.0))

# Globals
(def '*metrics)
(def '*verts)
(def '*xkb)
(def '*timeline)
(def '*vert-ptr)


# Wrapper libraries
(load "/home/user/Git/particle/src/c/mathc.l")
(load "/home/user/Git/particle/src/c/epoll.l")
(load "/home/user/Git/particle/src/c/li.l")
(load "/home/user/Git/particle/src/c/socket.l")
(load "/home/user/Git/particle/src/c/ksd.l")
(load "/home/user/Git/particle/src/c/xkbcommon.l")
# Lisp libraries
(load "/home/user/Git/particle/src/misc.l")
(load "/home/user/Git/particle/src/ipc/ipc.l")
(load "/home/user/Git/particle/src/prot/conn.l")
(load "/home/user/Git/particle/src/prot/msg.l")
(load "/home/user/Git/particle/src/prot/obj.l")
(load "/home/user/Git/particle/src/metrics/metrics.l")
(load "/home/user/Git/particle/src/model/particle.l")
(load "/home/user/Git/particle/src/model/projview.l")
(load "/home/user/Git/particle/src/model/vertex.l")
(load "/home/user/Git/particle/src/ctrl/xkb.l")
(load "/home/user/Git/particle/src/ctrl/worker/conn.l")
(load "/home/user/Git/particle/src/ctrl/worker/cmd/kb.l")
(load "/home/user/Git/particle/src/ctrl/worker/cmd/vert.l")
(load "/home/user/Git/particle/src/ctrl/worker/cmd/pen.l")
(load "/home/user/Git/particle/src/ctrl/worker/cmd/cam.l")


(de init-worker ()

  (let Argv (argv)

    (setq *width     (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "width invalid")))
          *height    (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "height invalid")))
          *verts-max (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "verts invalid")))
          *i-proc    (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "i invalid")))))

  (setq *metrics (load-metrics *sca-glyph))

  (let ID-C (pack "WORKER-" *i-proc)

    (setq *ipc (new '(ipc~+IPC)
                     (pack "/tmp/particle-worker-" *i-proc ".socket") 'ipc~block handle-client accept-client
                     (list (list PATH-UDS-MODEL   T handle-model  "MODEL")
                           (list PATH-UDS-CONTROL T handle-ctrl   "CONTROL")
                           (list PATH-UDS-RENDER  T handle-render "RENDER")
                           )))

    (reg-conns *ipc ID-C))

  (send-msg *ipc (ipc~get-fd> *ipc "CONTROL") "rdy")

  (setq *projview (req-recv-pv *ipc
                               (ipc~get-fd> *ipc "MODEL")
                               NIL))
  (cmd-render-pv *ipc)

  (setq *xkb (new '(+Xkb)))

  # Most efficient way to pull this much data is to batch it or send rx will
  # minimize the IPC bottleneck

  (gc (* (+ 104 2) 4))
  (prinl-info "init-worker" (pack "Pulling " *verts-max " verts..."))
  
  (let (Sock-Model (ipc~get-fd> *ipc "MODEL")
        Cnt-X 0
        Pos-X 0.0
        Pos-Y 0.0)

    (seed (in "/dev/urandom" (rd 64)))
        
    (for I *verts-max
      
      (let Vert (req-recv-vert *ipc
                               Sock-Model
                               NIL
                               I)

        (with Vert
          
          (=: pos  (list Pos-X Pos-Y 0.0))
          (when NIL
            (=: rota (list 0.0   0.0   (rand 0.0 6.283185307179586)))
            (=: rgba (list (rand 0.0 1.0) (rand 0.0 1.0) (rand 0.0 1.0) 1.0 
                           (rand 0.0 1.0) (rand 0.0 1.0) (rand 0.0 1.0) 1.0 
                           (rand 0.0 1.0) (rand 0.0 1.0) (rand 0.0 1.0) 1.0 
                           (rand 0.0 1.0) (rand 0.0 1.0) (rand 0.0 1.0) 1.0))))

        #(update-glyph> Vert (char (rand 33 126)))
        
        (update-model-matrix> Vert)

        # FIX - use update...
        (cmd-render-vert *ipc Vert)

        (inc 'Pos-X *adv-vert)
        (when (= (inc 'Cnt-X) 1024)
          #(prin-info "init-worker" (pack "Request/Render vertex " I " of " *verts-max "\r"))
          (setq Cnt-X 0)
          (setq Pos-X 0.0)
          (dec 'Pos-Y (+ *adv-vert *adv-vert))
          #(println "POS-Y: " Pos-Y)
          T)

        #(prinl-info "init-worker" (pack "Pulled " I))

        (push '*verts Vert))))

  (prinl-info "init-worker" "Done pulling verts")

  # Designate vertex 1 as ptr
  (setq *vert-ptr (pop '*verts)))


# Unused currentyl
(de handle-client (Sock))


(de run-worker (Timeout)

  (prinl-info "run-worker" "Starting epoll...")
  (loop (ipc~poll> *ipc Timeout)))


(de handle-ctrl (Sock)

  (let (Msg (recv-msg *ipc Sock)
        Data (any Msg)
        Name (get Data 1))

    # recv-msg will return data or NIL
    # This means sending NIL will disconnect the client, like EOF
    (unless Msg
      (disconn-conn *ipc Sock))

    (cond ((= Name 'KEYBOARD-KEY)
           (cmd-kb Data))

          ((= Name "update-vert")
           (cmd-update-vert *ipc *vert-ptr))

          ((= Name "calc-pv")
           (cmd-calc-pv (get Data 2) (get Data 3)))

          ((= Name "update-pv")
           (cmd-update-pv *ipc))

          ((= Data "bye")
           # Send bye to ctrl which will send to all workers/render/model
           (send-msg *ipc Sock "bye")
           (ipc~disconn-sock Sock)
           (ipc~end> *ipc)
           (println "Worker says bye!")
           (bye))

          (T
           T))

    # Send ready when complete
    (send-msg *ipc Sock "rdy")

    #(prinl (usec) " | WORKER/" *Pid " | Worker ready")

    T))


(de handle-model (Sock)

  (let Msg (recv-msg *ipc Sock)

    # Handle recv updates from model

    # recv-msg will return data or NIL
    # This means sending NIL will disconnect the client, like EOF
    (unless Msg
      (disconn-conn *ipc Sock))

    (when NIL
      (prinl Msg))))


(de handle-render (Sock)

  (let Msg (recv-msg *ipc Sock)

    # recv-msg will return data or NIL
    # This means sending NIL will disconnect the client, like EOF
    (unless Msg
      (disconn-conn *ipc Sock))

   (when NIL

     (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
           Sock-Render (ipc~get-fd> *ipc "RENDER")
           Vert        *vert-ptr)

        (with Vert
          (when NIL
            (=: pos (list (+ (get (: pos) 1) 0.05)
                          (+ (get (: pos) 2) 0.0)
                          0.0)))
          (=: rota (list 0.0
                         0.0
                         (+ (get (: rota) 3) 0.017)))
          (update-model-matrix> Vert))

        # FIX - use update...
        (cmd-render-vert *ipc Vert)))))


(de main ()

  (init-worker)
  (run-worker -1)
  (bye))
