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

# Set math precision to 6 decimals
(scl 6)


(def 'NAME-PID "CTRL")

# Paths
(def 'PATH-UDS-MODEL  "/tmp/particle-model.socket")
(def 'PATH-UDS-INPUT  "/tmp/particle-input.socket")
(def 'PATH-UDS-RENDER "/tmp/particle-render.socket")
(def 'PATH-UDS-CONTROL "/tmp/particle-control.socket")


#Globals
(def '*q-worker)
(def '*q-msg)
(def '*xkb)
(def '*timerfd)
(def '*delay-rep 400) # ms
(def '*delay-int 40)  # ms


# Wrapper libraries
(load "/home/user/Git/particle/src/c/mathc.l")
(load "/home/user/Git/particle/src/c/epoll.l")
(load "/home/user/Git/particle/src/c/li.l")
(load "/home/user/Git/particle/src/c/socket.l")
(load "/home/user/Git/particle/src/c/xkbcommon.l")
(load "/home/user/Git/particle/src/c/timerfd.l")
(load "/home/user/Git/particle/src/c/ksd.l")
# Lisp libraries
(load "/home/user/Git/particle/src/misc.l")
(load "/home/user/Git/particle/src/ipc/ipc.l")
(load "/home/user/Git/particle/src/prot/conn.l")
(load "/home/user/Git/particle/src/prot/msg.l")
(load "/home/user/Git/particle/src/prot/obj.l")
#(load "/home/user/Git/particle/src/ctrl/conn.l")
(load "/home/user/Git/particle/src/ctrl/xkb.l")



(de init-ctrl ()

  (let Argv (argv)

    (setq *width     (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "width invalid")))
          *height    (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "height invalid")))
          *verts-max (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "verts invalid")))))

  (let ID-C (pack "CTRL")

    (setq *ipc (new '(ipc~+IPC)
               PATH-UDS-CONTROL            T handle-worker accept-client
               (list (list PATH-UDS-INPUT  T handle-input  "INPUT")
                     (list PATH-UDS-MODEL  T '((Sock) (recv-msg *ipc Sock))  "MODEL")
                     (list PATH-UDS-RENDER T handle-render "RENDER"))))

    (reg-conns *ipc ID-C))

  (queue '*q-msg "(update-vert 1)")
  
  (queue '*q-msg (pack "(calc-pv " *width " " *height ")"))
  (queue '*q-msg (pack "(update-pv)"))

  (setq *xkb (new '(+Xkb)))

  (setq *timerfd (new '(timerfd~+Timerfd)))
  (ipc~add-conn> *ipc (get *timerfd 'timerfd~fd) "TI" handle-timer epoll~IN))


(de handle-render (Sock)
  # Currently, render sends frame time which is not used...

  # Render will send window resize events
  # Ctrl will send window resize event to workers
  (let (Msg (recv-msg *ipc Sock)
        Data (any Msg))

      (if (lst? Data)

        (when (= (get Data 1) "window-size")
          (dispatch-msg (pack "(calc-pv " (get Data 2) " " (get Data 3) ")"))
          (dispatch-msg (pack "(update-pv)")))
          
        (when (= Data "bye")
          # Send to all conns
          (send-msg-conns *ipc "bye")
          (ipc~disconnect-sock Sock)
          (ipc~end> *ipc)
          (println "Ctrl says bye!")
          (bye)))))


(de handle-worker (Sock)

  (let (Msg  (recv-msg *ipc Sock)
        Data (any Msg)
        Name (name Data))

    # Handle other messages
    (cond ((= Name "rdy")
           (let Msg (pop '*q-msg)
             (if Msg
               (send-msg *ipc Sock Msg)
               (queue '*q-worker Sock))))

          ((= Name "bye")
           # Send to all conns
           (send-msg-conns *ipc "bye")
           (ipc~disconnect-sock Sock)
           (ipc~end> *ipc)
           (println "Ctrl says bye!")
           (bye))

          (T
           (prinl-info "handle-worker" (pack "UNKNOWN REQUEST: " (str Msg)))))

    #(prinl (usec) " | CTRL   | handle-worker | " Msg)

    T))


(de run-ctrl (Timeout)

  (prinl-info "run-ctrl" "Starting epoll...")
  (loop (ipc~poll> *ipc Timeout)))


(de handle-input (Sock)

  # Currently assuming kb events only

  (let (Msg  (recv-msg *ipc Sock)
        Data (any Msg)
        Name (get Data 1))
    #(prinl (usec) " | CTRL   | handle-input | " Data)

    (when (= Name 'KEYBOARD-KEY)

      (update-xkb Data)

      (dispatch-msg (format-msg-ev Data)))))


(de format-msg-ev (Data)
  (pack "("
        (glue " "
              (list (name (get Data 1))
                    (format (get Data 2))
                    (format (get Data 3))
                    (format (get Data 4))
                    (format (get Data 5))
                    (format (get Data 6))))
        ")"))


(de dispatch-msg (Msg)

  # If worker available
  #    If event-queue: send event-queue
  #    Else event-new: send event-new

  (let Worker (pop '*q-worker)
    (if Worker
      (let Msg-Q (pop '*q-msg)
        (if Msg-Q
          (send-msg *ipc Worker Msg-Q)
          (send-msg *ipc Worker Msg)))
      (queue '*q-msg Msg))))


(de handle-timer (Sock)
  # Returns an unsigned 8-byte integer (uint64_t)
  # containing the number of expirations that have occurred.

  # Must call read, not recv
  (with *ipc
    (when (n0 (native "@" "read" 'I Sock (: ipc~buf-rd) 8))
      (let (Keycode (get *timerfd 'timerfd~data)
            Msg (pack "(KEYBOARD-KEY 0 0 " (format Keycode) " 2 2)"))
        (dispatch-msg Msg))
        #(prinl-info "handle-timer" (pack "Keycode=" Keycode " Events=" (struct (: ipc~buf-rd) '(N . 1))))
        T)))


(de update-xkb (Data)

  #{
  https://www.x.org/releases/X11R7.7/doc/xproto/x11protocol.html#Keyboards

  A KEYCODE represents a physical (or logical) key.
  Keycodes lie in the inclusive range [8,255].
  }#

  # https://gitlab.freedesktop.org/wayland/weston/blob/master/libweston/input.c#L1993

  (let (Keyed   (get Data 4)
        Keycode (+ Keyed 8)
        State   (get Data 5)
        Keysym  (get-one-sym> *xkb Keycode)
        Name    (xkb~keysym-get-name Keysym)
        Utf-8   (get-utf8> *xkb Keycode))

    (update-key> *xkb Keycode State)
    
    (when (= (key-repeats> *xkb Keycode) xkb~KEY-DOWN)
      # Pass key evdev since worker will +8 to match passthrough events from
      # libinput
      (update-timer Keyed State))))


(de update-timer (Keycode State)

  (if (= State xkb~KEY-DOWN)

    # interval, delay/expiration-initial
    (let Spec (list 0 (* *delay-int 1000000) 0 (* *delay-rep 1000000))
      #(println "Arm timer!" Keycode Spec)
      (put *timerfd 'timerfd~data Keycode)
      (when (= (timerfd~set-time> *timerfd Spec) -1)
        (println "errno: " (errno))
        (quit "timerfd~set-time failed")))

    (when (= Keycode (get *timerfd 'timerfd~data))
      #(println "Disarm timer!" Keycode)
      (timerfd~disarm> *timerfd))))


(de main ()
  (init-ctrl)
  (run-ctrl -1)
  (bye))
