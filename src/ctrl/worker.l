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

#Create and change to namespace model
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


(de init-ctrl ()

    (let (Argv (argv))
      (setq *width     (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "width invalid")))
            *height    (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "height invalid")))
            *verts-max (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "verts invalid")))
            *i-proc    (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "i invalid")))))

    (setq *ep-cnt    3
          *ep-events (epoll~make-event *ep-cnt)
          *ep-fd     (epoll~create-size 1))

    (let ID-C (pack "CTRL-" *Pid)
      (setq *ipc (new '(ipc~+IPC)
                 *ep-fd
                 ""             'ipc~block
                 (list (list PATH-UDS-MODEL  'ipc~block handle-model  "MODEL"  ID-C)
                       (list PATH-UDS-INPUT  'ipc~block handle-input  "INPUT"  ID-C)
                       (list PATH-UDS-RENDER 'ipc~block handle-render "RENDER" ID-C)))))
                       
    # Pull PVM from Model -> Send PVM and nodes to Render
    (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
          Sock-Render (ipc~get-fd> *ipc "RENDER")
          Cam         (read-pvm  Sock-Model epoll~BAD)
          Vert        (read-vert Sock-Model epoll~BAD 1))
      (write-pvm  Cam  Sock-Render epoll~BAD)
      (println Vert)
      
      #{
      For default ptr vert see:
      (setf *node-ptr-main* (init-node-ptr-shm *digraph-main*
                                               *vertices-main*
                                               (vec3 0 0 0))      
      }#
      
      #(write-vert Vert Sock-Render)
      T))


(de run-ctrl (Timeout)
    #{
    Poll events ASAP and enqueue for processing by controller thread

    epoll-wait:
    * ep-events size should correspond to maxevents
    * -1 = timeout = block
    0 = return if nothing
    * returns event count
    }#

    (prinl (usec) " | run-ctrl | Starting epoll...")

    (let (Events     NIL
          Fd         NIL)
      (loop
         (for I (epoll~wait-epfd *ep-fd 
                                 *ep-events 
                                 *ep-cnt
                                 Timeout)
           (let Ep-Events (epoll~get-event *ep-events I)
             (setq Events (get Ep-Events 1))
             (setq Fd     (get Ep-Events 2))
             (ipc~dispatch-handler-conn> *ipc Fd Events))))))


(de handle-input (Sock Events)
  (let (Msg (ipc~recv-msg> *ipc Sock Events)
        Data (any Msg)
        Name (get Data 1))
    (when (= Name 'KEYBOARD-KEY)
      (prin (usec) " | CTRL   | handle-input | ")
      #(print Data)
      #(prinl)
      (handle-keyboard Sock Events Data))))
      

(de handle-model (Sock Events)
  (let Msg (ipc~recv-msg> *ipc Sock Events)
    (when NIL
      (prinl Msg))))


(de handle-render (Sock Events)
  (let Msg (ipc~recv-msg> *ipc Sock Events)
    (when NIL
      (prinl Msg))))


(de main ()
    (init-ctrl)
    (run-ctrl -1)
    (bye))
