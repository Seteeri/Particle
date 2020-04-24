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
            "/src/c/li.l"
            "/src/c/socket.l"
            "/src/c/xkbcommon.l"
            "/src/c/timerfd.l"
            "/src/c/ksd.l"
            "/src/dlst.l"            
            "/src/ipc/ipc.l"
            "/src/metrics/metrics.l"            
            "/src/prot/conn.l"
            "/src/prot/msg.l"
            "/src/prot/obj.l"
            "/src/ctrl/xkb.l"
            "/src/ctrl/conn.l"
            "/src/worker/projview.l"
            "/src/worker/particle/vertex.l"))

(def 'NAME-PID "CTRL")

# Constants
(def '*glyph-dims '(96.0 96.0))
(def '*sca-vert  (*/ 1.0 1.0 96.0)) # vertex scale = 1/96.0
(def '*sca-glyph 5.8239365) # msdf param, mul by shape units -> px units, used by metrics
(def '*adv-glyph 9.375) # msdf param, mul by sca-glyph to get proper adv
# assumes all verts are scaled the same
(def '*adv-vert  (*/ (*/ *adv-glyph *sca-glyph 1.0) *sca-vert 1.0))
(def '*adv-vert-2  (+ *adv-vert *adv-vert))
(def '*adv-vert-3  (+ *adv-vert-2 *adv-vert))

#Globals
(def '*q-worker)
(def '*q-msg)
(def '*xkb)
(def '*timerfd)
(def '*delay-rep 400) # ms
(def '*delay-int 40)  # ms


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
                     (need 3 *sca-vert))
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


(de init-ctrl ()

  #Setup projview
  (setq *projview (new '(+Projview) *width *height 'o))
  (upd-mat-proj> *projview)
  (upd-mat-view> *projview)
  
  #(wr-ptr> *projview (get *projview 'ptr))
  #(rd-ptr> *projview (get *projview 'ptr))
  #(show *projview)
  #(view (getl *projview))
  
  (setq *ipc (new '(ipc~+IPC)
              *uds-ctrl             T handle-worker accept-client
              (list (list *uds-in   T handle-input  "INPUT")
                    (list *uds-rend T handle-render "RENDER"))))

  (reg-conns *ipc NAME-PID)

  (queue '*q-msg (pack "(calc-pv " *width " " *height ")"))
  (queue '*q-msg (pack "(update-pv)"))

  (setq *xkb (new '(+Xkb)))

  (setq *timerfd (new '(timerfd~+Timerfd)))
  (ipc~add-conn> *ipc (get *timerfd 'timerfd~fd) "TI" handle-timer epoll~IN))


(de run-ctrl (Timeout)

  (prinl-info "run-ctrl" "Starting epoll...")
  (loop (ipc~poll> *ipc Timeout)))


(de handle-worker (Sock)

  (unless (flush-msgs *ipc Sock)
    (disconn-client *ipc Sock))
    
  (parse-msgs *ipc Sock proc-msg))


(de handle-render (Sock)
  # Currently, render sends frame time which is not used...
  # Render will send window resize events
  # Ctrl will send window resize event to workers
    
  (let (Msg (recv-msg *ipc Sock)
        Data (any Msg)
        Name (get Data 1))
    
    # recv-msg will return data or NIL
    # This means sending NIL will disconnect the client, like EOF
    (if Msg
        (case Name
          ("fbsize"
            (dispatch-msg (pack "(calc-pv " (get Data 2) " " (get Data 3) ")"))
            (dispatch-msg (pack "(update-pv)")))

          ("focus"
            )
            
          ("bye"
            # Send to all conns
            (send-msg-conns *ipc "bye")
            (ipc~disconn-sock Sock)
            (ipc~end> *ipc)
            (println "Ctrl says bye!")
            (bye)))
        (disconn-conn *ipc Sock))))


(de handle-input (Sock)

  # Currently assuming kb events only

  #(println 'handle-input)
  
  (let (Msg  (recv-msg *ipc Sock)
        Data (any Msg)
        Name (car Data))
        
    # recv-msg will return data or NIL
    # This means sending NIL will disconnect the client, like EOF
    (if Msg
        (prog
          (cond ((= Name 'KEYBOARD-KEY)
                  (upd-xkb Data)))
          (dispatch-msg (format-msg-ev Data)))
        (disconn-client *ipc Sock))))


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


(de upd-xkb (Data)

  #{
  https://www.x.org/releases/X11R7.7/doc/xproto/x11protocol.html#Keyboards

  A KEYCODE represents a physical (or logical) key.
  Keycodes lie in the inclusive range [8,255].
  }#

  # https://gitlab.freedesktop.org/wayland/weston/blob/master/libweston/input.c#L1993

  (let (Keyed   (get Data 4)
        Keycode (+ Keyed 8)
        State   (get Data 5))

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
