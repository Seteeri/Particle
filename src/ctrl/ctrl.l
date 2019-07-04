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
(load "/home/user/quicklisp/local-projects/protoform/src/ctrl/ipc.l")


#Globals
(def '*ep-events)


(de init-ctrl ()

    (let (Argv (argv))
      (setq *width     (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "width invalid")))
            *height    (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "height invalid")))
            *verts-max (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "verts invalid")))))

    (setq *ep-events (native "@" "malloc" 'N (* 12 2)) #1 event
          *ep-fd     (epoll~create-size 1)
          *buf-sz    (socket~read-rmem-default)
          *buf-rd    (native "@" "malloc" 'N *buf-sz))

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
          Cam         (read-pvm Sock-Model))
      (write-pvm Cam Sock-Render))

    T)


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
          Fd         NIL
          Num-Events 0)
      (loop
        (setq Num-Events (epoll~wait-epfd *ep-fd *ep-events 2 Timeout))
        (for I Num-Events
           (let (Ep-Events (struct *ep-events '(I I)))
             (setq Events (get Ep-Events 1))
             (setq Fd     (get Ep-Events 2))
             (ipc~dispatch-handler-conn> *ipc Fd Events))))))


(def '*cam)
(de handle-input (Sock Events)
  (let (Msg (ipc~recv-msg> *ipc Events Sock)
        Data (any Msg)
        Name (get Data 1))
    (when (= Name 'KEYBOARD-KEY)
      (prinl (usec) " | CTRL  | handle-input | " Msg)
      (handle-keyboard Sock Data))))
      

(de handle-model (Sock Events)
  (let Msg (ipc~recv-msg> *ipc Events Sock)
    (when NIL
      (prinl Msg))))


(de handle-render (Sock Events)
  (let Msg (ipc~recv-msg> *ipc Events Sock)
    (when NIL
      (prinl Msg))))


(de main ()
    (init-ctrl)
    (run-ctrl -1)
    (bye))


################################################################################


(de handle-keyboard (Sock Data)
  (let (Key   (get Data 4)
        State (get Data 5))
    (cond ((and (= Key 106) (= State 1)) # Move camera right
           (move-cam Sock 29 -1.0))
          ((and (= Key 105) (= State 1)) # Move camera left
           (move-cam Sock 29 1.0))
          ((and (= Key 103) (= State 1)) # Move camera up
           (move-cam Sock 30 -1.0))
          ((and (= Key 108) (= State 1)) # Move camera dn
           (move-cam Sock 30 1.0)))))

                         
(de move-cam (Sock Cnt Delta)
   # Pull data from model
   (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
         Sock-Render (ipc~get-fd> *ipc "RENDER")
         Cam         (if *cam *cam (read-pvm Sock-Model)))
      #(println Cam)
      # Modify position
     (let Cam-New (place Cnt Cam (+ (get Cam Cnt) Delta))
       # Push data to model
       #(write-pvm Cam Sock-Model)
       # Push update to render
       (write-pvm Cam-New Sock-Render)
       (prinl (usec))
       (setq *cam Cam-New))))