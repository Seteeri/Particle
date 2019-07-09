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


(symbols 'model 'pico)


# Set math precision to 6 decimals
(scl 6)


# C library paths
(def 'PATH-MATHC "/home/user/quicklisp/local-projects/protoform/ext/mathc/libmathc.so")
(def 'PATH-GLYPHS "/home/user/quicklisp/local-projects/protoform/src/glyphs")
# UDS paths
(def 'PATH-UDS-MODEL  "/tmp/protoform-model.socket")
(def 'PATH-UDS-INPUT  "/tmp/protoform-input.socket")
(def 'PATH-UDS-RENDER "/tmp/protoform-render.socket")


# Wrapper libraries
(load "/home/user/quicklisp/local-projects/protoform/src/c/mathc.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/posix/mman.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/posix/stdio.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/posix/string.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/posix/unistd.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/epoll.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/socket.l")
# Lisp libraries
(load "/home/user/quicklisp/local-projects/protoform/src/ipc/ipc.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/projview.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/metrics.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/conn.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/graph/vertex.l")


# Globals
(def '*sca-vert 0.008)
(def '*sca-glyph 5.8239365) # MSDF/tex parameter
(def '*adv-glyph 9.375)       # MSDF/tex parameter  


(de init-verts ()

  (let Verts ()
  
       (for I *verts-max
       
            (let Vert (new '(+Vertex)
                           42 # *
                           *color-vert-default
                           (list 0.0 0.0 0.0) # pos
                           (list 0.0 0.0 0.0) # rot=0, sca=global scale
                           (list *sca-vert *sca-vert *sca-vert))
                           
                 #(prinl "Created vertex " I " "  Vert)
                 (push 'Verts Vert)))
                 
       Verts))


(de init-verts-default ()
  #{
  Traverse all internal/protected symbols
  - PicoLisp symbols = 300+?
  - Pointer = cons pair

  - Track transient symbols
  }#
  T)


(de init-model ()

  #(*/ 1.0 2560.0 2.0)
  #1080.0
  #(** 2 19)

  #Get number of nodes and window size from args instead of file
  (let (Argv (argv))
  
    (setq *width     (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "width invalid")))
          *height    (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "height invalid")))
          *verts-max (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "verts invalid")))))

  (init-ipc)

  #Setup projview
  (setq *projview (new '(+Projview) *width *height 'orthographic))
  #Simplify these - single function?
  (update-mat-proj> *projview)
  (update-mat-view> *projview)

  #Debug
  #(show *projview)
  #(view (getl *projview))

  #Load here, copy to shm, msg view, view copies to OpenGL buffers
  (when T
    (setq *metrics (load-metrics)))
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
  }#

  (gc (+ 104 2))
  (prinl (usec) " | MODEL | init-model | Heap resized to 106 MB for " *verts-max " nodes")

  #{
   - Graphs = namespaces?
   - Generate nodes for builtin symbols?
  }#
  (setq *verts (init-verts)))


(de init-ipc ()
  (setq *ipc (new '(ipc~+IPC) 
                  PATH-UDS-MODEL 'ipc~block recv-conn
                  NIL
                  read-src
                  write-dst)))


(de run-model (Timeout)

  (prinl (usec) " | run-model | Starting epoll...")
  (loop (ipc~poll> *ipc Timeout)))


(de recv-conn (Sock)
  #{
  
  If model recv'd memcpy, write data and broadcast update
  
  (memcpy dst src sz off)
  
  If src NIL: read bytes after msg
  If dst NIL: send bytes back
  
  }#
  
  (let (Msg (ipc~recv-msg> *ipc Sock)
        Data (any Msg)
        Name-Fn (get Data 1))

    (prinl (usec) " | MODEL  | recv-conn | " Msg)

    # For now, assume memcpy
    (cond ((= Name-Fn "mc")
           (ipc~handle-memcpy> *ipc Sock Data))

          (T
           (prinl (usec) " | MODEL  | recv-conn-ctrl | UNKNOWN REQUEST! " Msg)))))


(de main ()

  (init-model)
  (run-model -1)
  (println 'Exiting...)
  (bye))
