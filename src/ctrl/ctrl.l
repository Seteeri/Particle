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
            "/src/c/epoll.l"
            "/src/c/li.l"
            "/src/c/socket.l"
            "/src/c/xkbcommon.l"
            "/src/c/timerfd.l"
            "/src/c/ksd.l"
            "/src/misc.l"
            "/src/ipc/ipc.l"
            "/src/prot/conn.l"
            "/src/prot/msg.l"
            "/src/prot/obj.l"
            "/src/ctrl/xkb.l"))

(def 'NAME-PID "CTRL")

#Globals
(def '*q-worker)
(def '*q-msg)
(def '*xkb)
(def '*timerfd)
(def '*delay-rep 400) # ms
(def '*delay-int 40)  # ms


(de init-ctrl ()

  (let ID-C (pack "CTRL")

    (setq *ipc (new '(ipc~+IPC)
               PATH-UDS-CONTROL            T handle-worker accept-client
               (list (list PATH-UDS-INPUT  T handle-input  "INPUT")
                     (list PATH-UDS-MODEL  T '((Sock) (recv-msg *ipc Sock))  "MODEL")
                     (list PATH-UDS-RENDER T handle-render "RENDER"))))

    (reg-conns *ipc ID-C))

  (queue '*q-msg (pack "(calc-pv " *width " " *height ")"))
  (queue '*q-msg (pack "(update-pv)"))

  (setq *xkb (new '(+Xkb)))

  (setq *timerfd (new '(timerfd~+Timerfd)))
  (ipc~add-conn> *ipc (get *timerfd 'timerfd~fd) "TI" handle-timer epoll~IN))


(de run-ctrl (Timeout)

  (prinl-info "run-ctrl" "Starting epoll...")
  (loop (ipc~poll> *ipc Timeout)))


(de handle-worker (Sock)

  (let (Msg  (recv-msg *ipc Sock)
        Data (any Msg)
        Name (name Data))

    # recv-msg will return data or NIL
    # This means sending NIL will disconnect the client, like EOF
    (unless Msg
      (disconn-client *ipc Sock))

    # Handle other messages
    (cond ((= Name "rdy")
           (let Msg (pop '*q-msg)
             (if Msg
               (send-msg *ipc Sock Msg)
               (queue '*q-worker Sock))))

          ((= Name "bye")
           # Send to all conns
           (send-msg-conns *ipc "bye")
           (ipc~disconn-sock Sock)
           (ipc~end> *ipc)
           (println "Ctrl says bye!")
           (bye))

          (T
           (prinl-info "handle-worker" (pack "UNKNOWN REQUEST: " (str Msg)))))

    #(prinl (usec) " | CTRL   | handle-worker | " Msg)

    T))


(de handle-render (Sock)
  # Currently, render sends frame time which is not used...

  # Render will send window resize events
  # Ctrl will send window resize event to workers
  (let (Msg (recv-msg *ipc Sock)
        Data (any Msg))

    # recv-msg will return data or NIL
    # This means sending NIL will disconnect the client, like EOF
    (unless Msg
      (disconn-conn *ipc Sock))

    (if (lst? Data)

      (when (= (get Data 1) "window-size")
        (dispatch-msg (pack "(calc-pv " (get Data 2) " " (get Data 3) ")"))
        (dispatch-msg (pack "(update-pv)")))
        
      (when (= Data "bye")
        # Send to all conns
        (send-msg-conns *ipc "bye")
        (ipc~disconn-sock Sock)
        (ipc~end> *ipc)
        (println "Ctrl says bye!")
        (bye)))))


(de handle-input (Sock)

  # Currently assuming kb events only

  (let (Msg  (recv-msg *ipc Sock)
        Data (any Msg)
        Name (get Data 1))

    # recv-msg will return data or NIL
    # This means sending NIL will disconnect the client, like EOF
    (unless Msg
      (disconn-client *ipc Sock))

    (cond ((= Name 'KEYBOARD-KEY)
            (update-xkb Data)))
            
    (dispatch-msg (format-msg-ev Data))))


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
           Sent NIL
        (if Msg-Q
          (setq Sent (send-msg *ipc Worker Msg-Q))
          (setq Sent (send-msg *ipc Worker Msg)))
        (unless Sent (disconn-conn *ipc Worker)))
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
  (bind-args)
  (init-ctrl)
  (run-ctrl -1)
  (bye))
