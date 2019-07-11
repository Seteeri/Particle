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


# C library paths

# Paths
(def 'PATH-UDS-MODEL  "/tmp/protoform-model.socket")
(def 'PATH-UDS-CONTROL  "/tmp/protoform-control.socket")
(def 'PATH-UDS-RENDER "/tmp/protoform-render.socket")


# Set math precision to 6 decimals
(scl 6)


# Wrapper libraries
(load "/home/user/quicklisp/local-projects/protoform/src/c/epoll.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/li.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/socket.l")
# Lisp libraries
(load "/home/user/quicklisp/local-projects/protoform/src/ipc/ipc.l")
(load "/home/user/quicklisp/local-projects/protoform/src/ctrl/conn.l")


(def '*vert)


(de init-worker ()

  (let (Argv (argv))

    (setq *width     (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "width invalid")))
          *height    (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "height invalid")))
          *verts-max (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "verts invalid")))
          *i-proc    (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "i invalid")))))

  (let ID-C (pack "WORKER-" *Pid)

    (setq *ipc (new '(ipc~+IPC)
               (pack "/tmp/protoform-worker-" *i-proc ".socket") 'ipc~block handle-client
               (list (list PATH-UDS-MODEL   'ipc~block handle-model  "MODEL"   ID-C)
                     (list PATH-UDS-CONTROL 'ipc~block handle-ctrl   "CONTROL" ID-C)
                     (list PATH-UDS-RENDER  'ipc~block handle-render "RENDER"  ID-C)))))
                     
  (ipc~send-msg> *ipc "RDY" (ipc~get-fd> *ipc "CONTROL"))
      
  (setq *vert (ipc~read-vert> *ipc (ipc~get-fd> *ipc "MODEL") *i-proc)))
    

(de handle-client (Sock))


(de run-worker (Timeout)

  (prinl (usec) " | run-worker | Starting epoll...")
  (loop (ipc~poll> *ipc Timeout)))


(de handle-ctrl (Sock)

  (let (Msg (ipc~recv-msg> *ipc Sock)
        Data (any Msg)
        Name (get Data 1))

    (cond ((= Name 'KEYBOARD-KEY)
           (handle-keyboard Sock Data))
           
          ((= Name "update-vert")
           #(prinl (usec) " | " *Pid " | " Msg)
           (ipc~update-vert> *ipc *i-proc)
           T)
           
          ((= Name "update-pvm")
           #(prinl (usec) " | " *Pid " | " Msg)
           (ipc~update-pvm> *ipc)
           T)
           
          (T
           T))
               
    # Send ready when complete
    (ipc~send-msg> *ipc "RDY" Sock)
    
    #(prinl (usec) " | " *Pid " | " Msg)
    
    T))
      

(de handle-model (Sock)

  (let Msg (ipc~recv-msg> *ipc Sock)

    (when NIL
      (prinl Msg))))


(de handle-render (Sock)

  (let Msg (ipc~recv-msg> *ipc Sock)

   (when T
   
   # Modify position
   (let (Cnt   (case *i-proc
                  (1 13)
                  (2 13)
                  (3 14)
                  (4 14))
         Delta     (case *i-proc
                    (1 -0.01)
                    (2 0.01)
                    (3 -0.01)
                    (4 0.01))
         Mm (get *vert 1)
         Mm-New (place Cnt Mm (+ (get Mm Cnt) Delta))
         Vert-New (place 1 *vert Mm-New))
               
     # Push update to render
     (ipc~write-vert> *ipc Vert-New *i-proc Sock)
     
     # Push update to model
     
     (setq *vert Vert-New))

    )

    (when NIL
      (prinl Msg))))


(de main ()

  (init-worker)
  (run-worker -1)
  (bye))
