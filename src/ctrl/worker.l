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

# Paths
(def 'PATH-UDS-MODEL  "/tmp/protoform-model.socket")
(def 'PATH-UDS-INPUT  "/tmp/protoform-input.socket")
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


#Globals
(def '*ep-events)
(def '*cam) # TEMP
(def '*vert) # TEMP


(de init-worker ()

    (let (Argv (argv))

      (setq *width     (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "width invalid")))
            *height    (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "height invalid")))
            *verts-max (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "verts invalid")))
            *i-proc    (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "i invalid")))))

    (let ID-C (pack "WORKER-" *Pid)

      (setq *ipc (new '(ipc~+IPC)
                 ""             'ipc~block handle-client
                 (list (list PATH-UDS-MODEL   'ipc~block handle-model   "MODEL"   ID-C)
                       (list PATH-UDS-CONTROL 'ipc~block handle-control "CONTROL" ID-C)
                       (list PATH-UDS-RENDER  'ipc~block handle-render  "RENDER"  ID-C))))))


(de handle-client (Sock Events))


(de run-worker (Timeout)

  (prinl (usec) " | run-ctrl | Starting epoll...")
  (loop (ipc~poll> *ipc Timeout)))


(de handle-control (Sock Events)

  (let (Msg (ipc~recv-msg> *ipc Sock Events)
        Data (any Msg))

    (when NIL
      (prinl Msg))))
      

(de handle-model (Sock Events)

  (let Msg (ipc~recv-msg> *ipc Sock Events)

    (when NIL
      (prinl Msg))))


(de handle-render (Sock Events)

  (let Msg (ipc~recv-msg> *ipc Sock Events)

    (when NIL
      (prinl Msg))))


(de main ()

  (init-worker)
  (run-worker -1)
  (bye))
