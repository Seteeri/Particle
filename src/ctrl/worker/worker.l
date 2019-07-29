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

  (setq *xkb (new '(+Xkb)))

  (let Sock-Model (ipc~get-fd> *ipc "MODEL")
    (for N *verts-max
      (let Vert (req-recv-vert *ipc
                               Sock-Model
                               NIL
                               N)
        (push '*verts Vert))))

  # Designate vertex 1 as ptr
  (setq *vert-ptr (pop '*verts)))


(de handle-client (Sock))


(de run-worker (Timeout)

  (print-info "run-worker" "Starting epoll...")
  (loop (ipc~poll> *ipc Timeout)))


(de handle-ctrl (Sock)

  (let (Msg (recv-msg *ipc Sock)
        Data (any Msg)
        Name (get Data 1))

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
           (ipc~disconnect-sock Sock)
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

    (when NIL
      (prinl Msg))))


(de handle-render (Sock)

  (let Msg (recv-msg *ipc Sock)

   (when T

     # Pull data from model
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

        (cmd-update-vert *ipc Vert)))))

(de main ()

  (init-worker)
  (run-worker -1)
  (bye))
