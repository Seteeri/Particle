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


(symbols 'ctrl 'pico)


# C library paths
(def 'PATH-MATHC "/home/user/quicklisp/local-projects/protoform/ext/mathc/libmathc.so")
# Paths
(def 'PATH-UDS-MODEL  "/tmp/protoform-model.socket")
(def 'PATH-UDS-INPUT  "/tmp/protoform-input.socket")
(def 'PATH-UDS-RENDER "/tmp/protoform-render.socket")
(def 'PATH-UDS-CONTROL "/tmp/protoform-control.socket")


# Set math precision to 6 decimals
(scl 6)


# Wrapper libraries
(load "/home/user/quicklisp/local-projects/protoform/src/c/mathc.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/epoll.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/li.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/socket.l")
# Lisp libraries
(load "/home/user/quicklisp/local-projects/protoform/src/ipc/ipc.l")
(load "/home/user/quicklisp/local-projects/protoform/src/ctrl/conn.l")


#Globals
(def '*q-worker)
(def '*q-msg)


(de init-ctrl ()

  (let (Argv (argv))
  
    (setq *width     (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "width invalid")))
          *height    (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "height invalid")))
          *verts-max (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "verts invalid")))))

  # REFACTOR:
  # (ID-S PATH BLOCK HANDLE)
  # (ID-S PATH BLOCK HANDLE ID-C)
  (let ID-C (pack "CTRL-" *Pid)
  
    (setq *ipc (new '(ipc~+IPC)
               PATH-UDS-CONTROL            'ipc~block handle-worker
               (list (list PATH-UDS-MODEL  'ipc~block handle-model  "MODEL"  ID-C)
                     (list PATH-UDS-INPUT  'ipc~block handle-input  "INPUT"  ID-C)
                     (list PATH-UDS-RENDER 'ipc~block handle-render "RENDER" ID-C)))))

  # Create queue
  # Push below msgs into queue for workers to resolve
  #
  # Pull PVM from Model -> Send PVM and nodes to Render
  (queue '*q-msg "(update-vert 1)")
  (queue '*q-msg "(update-vert 2)")
  (queue '*q-msg "(update-pvm)")
  
    
  (when NIL
    (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
          Sock-Render (ipc~get-fd> *ipc "RENDER")
          Cam         (ipc~read-pvm> *ipc Sock-Model))

      (when NIL # Testing
        (ipc~write-vert> *ipc (ipc~read-vert> *ipc Sock-Model 1)
                    1
                    Sock-Render))
                  
      (ipc~write-pvm> *ipc Cam Sock-Render))))


(de handle-worker (Sock)

  (let (Msg (ipc~recv-msg> *ipc Sock)
        Data (any Msg))
    
    # Poss handle finish messages also
    
    (let Msg (pop '*q-msg)
      (if Msg
        (ipc~send-msg> *ipc Msg Sock)
        (queue '*q-worker Sock)))
        
    #(prinl (usec) " | CTRL   | handle-ctrl | " Msg)
    
    T))
    

(de run-ctrl (Timeout)

  (prinl (usec) " | run-ctrl | Starting epoll...")
  (loop (ipc~poll> *ipc Timeout)))


(de handle-input (Sock)

  (let (Msg (ipc~recv-msg> *ipc Sock)
        Data (any Msg))
    #(prinl (usec) " | CTRL   | handle-input | ")
    
    # If worker available
    #    If event-queue: send event-queue
    #    Else event-new: send event-new
    
    (when T
      (let Worker (pop '*q-worker)
        (if Worker
          (let Msg-Q (pop '*q-msg)
            (if Msg-Q
              (ipc~send-msg> *ipc Msg-Q Worker)
              (ipc~send-msg> *ipc Msg   Worker)))
          (queue '*q-msg Msg))))))


(de handle-model (Sock)

  (let Msg (ipc~recv-msg> *ipc Sock)
    (when NIL
      (prinl Msg))))


(de handle-render (Sock)

  (let Msg (ipc~recv-msg> *ipc Sock)
    (when NIL
      (prinl Msg))))


(de main ()
  (init-ctrl)
  (run-ctrl -1)
  (bye))
