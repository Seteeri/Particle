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


(scl 6)

(load (pack (pwd) "/src/misc.l"))
(mapc 'load-cwd
      (list "/src/c/mathc.l"
            "/src/c/posix/mman.l"
            "/src/c/posix/stdio.l"
            "/src/c/posix/string.l"
            "/src/c/posix/unistd.l"
            "/src/c/epoll.l"
            "/src/c/socket.l"
            "/src/misc.l"
            "/src/dlst.l"
            "/src/ipc/ipc.l"
            "/src/prot/conn.l"
            "/src/prot/msg.l"
            "/src/prot/obj.l"
            "/src/model/projview.l"
            "/src/metrics/metrics.l"
            "/src/model/conn.l"
            "/src/model/vertex.l"))

(def 'NAME-PID "MODEL")

# C library paths
(def 'PATH-GLYPHS "/home/user/Particle/particle/src/glyphs")
# UDS paths
(def 'PATH-UDS-MODEL  "/tmp/particle-model.socket")
(def 'PATH-UDS-INPUT  "/tmp/particle-input.socket")
(def 'PATH-UDS-RENDER "/tmp/particle-render.socket")

# Globals
(def '*sca-vert (*/ 1.0 1.0 96.0))
(def '*sca-glyph 5.8239365) # MSDF/tex parameter
(def '*adv-glyph 9.375)       # MSDF/tex parameter


(de init-verts ()

  (prinl-info "init-verts" (pack "Making " *verts-max " verts..."))

  (let Verts ()
    (for I *verts-max
      (let Vert (new '(+Vertex)
                     (get-vert-off I)
                     "@"
                     *color-vert-default
                     (list 0.0 0.0 0.0) # pos
                     (list 0.0 0.0 0.0) # rot=0, sca=global scale
                     (list *sca-vert *sca-vert *sca-vert))
        (push 'Verts Vert)))

    (setq Verts (flip Verts))

    (prinl-info "init-verts" "Finish making verts")

    (unless (info "bin/verts.bin")
      (prinl-info "init-verts" "Writing ./bin/verts.bin")
      (out "bin/verts.bin"
        (for Vert Verts
          (pr (val Vert) (getl Vert))))
      (prinl-info "init-verts" "Wrote ./bin/verts.bin"))

     Verts))


(de init-model ()

  (init-ipc)

  #Setup projview
  (setq *projview (new '(+Projview) *width *height 'o))
  (update-mat-proj> *projview)
  (update-mat-view> *projview)

  #(serialize-to-ptr> *projview (get *projview 'ptr))
  #(deserialize-fm-ptr> *projview (get *projview 'ptr))
  #(show *projview)
  #(view (getl *projview))

  #Load here, copy to shm, msg view, view copies to OpenGL buffers
  (when T
    (setq *metrics (load-metrics *sca-glyph)))
  #(let Metrics (get *metrics 42)
  #  (println (view (getl Metrics))))

  #{
  GC beforehand since static data is at the beginning,
  and allocations afterwards will go right after
  Nodes won't be deleted so they are considered static data
  which improves locality

  Increase heap size to node size
  208 bytes per node roughly
  ->Do test run to determine size

  2**19 = 524288

   - Graphs = namespaces?
   - Generate nodes for builtin symbols?
  }#

  (gc 256)
  (setq *verts (init-verts)))


(de init-ipc ()
  (setq *ipc (new '(ipc~+IPC)
                  PATH-UDS-MODEL T recv-conn accept-client
                  NIL)))


(de run-model (Timeout)

  (prinl-info "run-model" "Starting epoll...")
  (loop
    (ipc~poll> *ipc Timeout)))


(de recv-conn (Sock)
  (unless (flush-msgs *ipc Sock)
    (disconn-client *ipc Sock))
  (parse-msgs *ipc Sock process-msg))


(de main ()
  (bind-args)
  (init-model)
  (run-model -1)
  (println 'Exiting...)
  (bye))
