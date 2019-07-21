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

# Paths
(def 'PATH-UDS-MODEL  "/tmp/protoform-model.socket")
(def 'PATH-UDS-CONTROL  "/tmp/protoform-control.socket")
(def 'PATH-UDS-RENDER "/tmp/protoform-render.socket")
(def 'PATH-GLYPHS "/home/user/quicklisp/local-projects/protoform/src/glyphs")

# Constants
(def '*sca-vert (*/ 1.0 1.0 96.0))
(def '*sca-glyph 5.8239365)   # MSDF/tex parameter
(def '*adv-glyph 9.375)       # MSDF/tex parameter
(def '*adv-vert (*/ (*/ *adv-glyph *sca-glyph 1.0) *sca-vert 1.0))

# Globals
(def '*vert)
(def '*verts)


# Wrapper libraries
(load "/home/user/quicklisp/local-projects/protoform/src/c/mathc.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/epoll.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/li.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/socket.l")
# Lisp libraries
(load "/home/user/quicklisp/local-projects/protoform/src/ipc/ipc.l")
(load "/home/user/quicklisp/local-projects/protoform/src/ctrl/conn.l")
(load "/home/user/quicklisp/local-projects/protoform/src/prot/prot.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/projview.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/vertex.l")
(load "/home/user/quicklisp/local-projects/protoform/src/metrics/metrics.l")
(load "/home/user/quicklisp/local-projects/protoform/src/ctrl/kb.l")


(de init-worker ()

  (let (Argv (argv))

    (setq *width     (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "width invalid")))
          *height    (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "height invalid")))
          *verts-max (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "verts invalid")))
          *i-proc    (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "i invalid")))))

  (setq *metrics (load-metrics *sca-glyph))
  
  (let ID-C (pack "WORKER-" *Pid)

    (setq *ipc (new '(ipc~+IPC)
                     (pack "/tmp/protoform-worker-" *i-proc ".socket") 'ipc~block handle-client accept-client
                     (list (list PATH-UDS-MODEL   'ipc~block handle-model  "MODEL")
                           (list PATH-UDS-CONTROL 'ipc~block handle-ctrl   "CONTROL")
                           (list PATH-UDS-RENDER  'ipc~block handle-render "RENDER")
                           )))
                     
    (reg-conns *ipc ID-C))                     
                      
  (send-msg *ipc "RDY" (ipc~get-fd> *ipc "CONTROL"))

  (setq *off-vert (get-vert-off *i-proc))
  (setq *vert (req-recv-obj *ipc
                            (ipc~get-fd> *ipc "MODEL")
                            (new '(+Vertex))
                            SZ-VERT
                            *off-vert))

  (for N *verts-max
    (let Off (get-vert-off N)
      (push '*verts (cons Off
                          (req-recv-obj *ipc
                                        (ipc~get-fd> *ipc "MODEL")
                                        (new '(+Vertex))
                                        SZ-VERT
                                        Off)))))
  (setq '*verts (reverse *verts))

  #(println "*vert: " (getl *vert))

  (setq *projview (req-recv-obj *ipc 
                                (ipc~get-fd> *ipc "MODEL")
                                (new '(+Projview))
                                SZ-PV
                                0))
  
  T)
    

(de handle-client (Sock))


(de run-worker (Timeout)

  (prinl (usec) " | run-worker | Starting epoll...")
  (loop (ipc~poll> *ipc Timeout)))


(de handle-ctrl (Sock)

  (let (Msg (recv-msg *ipc Sock)
        Data (any Msg)
        Name (get Data 1))

    (cond ((= Name 'KEYBOARD-KEY)
           (handle-keyboard Sock Data)
           T)
           
          ((= Name "update-vert")
           (prinl (usec) " | WORKER/" *Pid " | " Msg)
           (update-vert *ipc *vert *off-vert)
           T)
           
          ((= Name "update-pvm")
           (prinl (usec) " | WORKER/" *Pid " | " Msg)
           (update-pvm *ipc *projview)
           T)
          
          # Handle recv updates from model
           
          (T
           T))
               
    # Send ready when complete
    (send-msg *ipc "RDY" Sock)
    
    #(prinl (usec) " | WORKER/" *Pid " | Worker RDY")
    
    T))
      

(de handle-model (Sock)

  (let Msg (recv-msg *ipc Sock)

    (when NIL
      (prinl Msg))))


(de handle-render (Sock)

  (let Msg (recv-msg *ipc Sock)

   (when NIL
   
     # Pull data from model
     (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
           Sock-Render (ipc~get-fd> *ipc "RENDER"))

       (with *vert

        (=: pos (list (+ (get (: pos) 1) 0.05)
                      (+ (get (: pos) 2) 0.0)
                      0.0))
        (update-model-matrix> *vert)
                
        # Below calls are reserializing
                
        (req-send-obj *ipc
                      Sock-Render
                      *vert
                      SZ-VERT
                      *off-vert)

        # Push to model also
        (when NIL
          (req-send-obj *ipc
                        Sock-Model
                        *vert
                        SZ-VERT
                        *off-vert)))))))


(de main ()

  (init-worker)
  (run-worker -1)
  (bye))
