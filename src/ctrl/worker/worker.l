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


# Set math precision to 6 decimals
(scl 6)


(def 'NAME-PID "WORKER")

# Paths
(def 'PATH-UDS-MODEL   "/tmp/particle-model.socket")
(def 'PATH-UDS-CONTROL "/tmp/particle-control.socket")
(def 'PATH-UDS-RENDER  "/tmp/particle-render.socket")
(def 'PATH-GLYPHS      "/home/user/Git/particle/src/glyphs")

# Constants
(def '*sca-vert  (*/ 1.0 1.0 96.0))
(def '*sca-glyph 5.8239365)   # MSDF/tex parameter
(def '*adv-glyph 9.375)       # MSDF/tex parameter
(def '*adv-vert  (*/ (*/ *adv-glyph *sca-glyph 1.0) *sca-vert 1.0))
(def 'KEY-REPEAT 2)

# Globals
(def '*metrics)
(def '*verts)
(def '*xkb)
(def '*binds)
(def '*timeline)
(def '*vert-ptr)
(def '*cnt-v)


# Wrapper libraries
(load "/home/user/Git/particle/src/c/mathc.l")
(load "/home/user/Git/particle/src/c/epoll.l")
(load "/home/user/Git/particle/src/c/li.l")
(load "/home/user/Git/particle/src/c/socket.l")
(load "/home/user/Git/particle/src/c/ksd.l")
(load "/home/user/Git/particle/src/c/xkbcommon.l")
# Lisp libraries
(load "/home/user/Git/particle/src/misc.l")
(load "/home/user/Git/particle/src/ipc/ipc.l")
(load "/home/user/Git/particle/src/prot/conn.l")
(load "/home/user/Git/particle/src/prot/msg.l")
(load "/home/user/Git/particle/src/prot/obj.l")
(load "/home/user/Git/particle/src/metrics/metrics.l")
(load "/home/user/Git/particle/src/model/particle.l")
(load "/home/user/Git/particle/src/model/projview.l")
(load "/home/user/Git/particle/src/model/vertex.l")
(load "/home/user/Git/particle/src/ctrl/xkb.l")
(load "/home/user/Git/particle/src/ctrl/worker/conn.l")
(load "/home/user/Git/particle/src/ctrl/worker/ptr.l")
(load "/home/user/Git/particle/src/ctrl/worker/draw.l")
(load "/home/user/Git/particle/src/ctrl/worker/timeline.l")
(load "/home/user/Git/particle/src/ctrl/worker/cmd/vert.l")
(load "/home/user/Git/particle/src/ctrl/worker/cmd/cam.l")
(load "/home/user/Git/particle/src/ctrl/worker/cmd/ptr.l")


(de init-worker ()

  (let Argv (argv)

    (setq *width     (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "width invalid")))
          *height    (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "height invalid")))
          *verts-max (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "verts invalid")))
          *i-proc    (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "i invalid")))))

  (setq *metrics (load-metrics *sca-glyph))

  (let ID-C (pack "WORKER-" *i-proc)

    (setq *ipc (new '(ipc~+IPC)
                     (pack "/tmp/particle-worker-" *i-proc ".socket") 'ipc~block handle-client accept-client
                     (list (list PATH-UDS-MODEL   T handle-model  "MODEL")
                           (list PATH-UDS-CONTROL T handle-ctrl   "CONTROL")
                           (list PATH-UDS-RENDER  T handle-render "RENDER")
                           )))

    (reg-conns *ipc ID-C))

  (send-msg *ipc (ipc~get-fd> *ipc "CONTROL") "rdy")

  (setq *projview (req-recv-pv *ipc
                               (ipc~get-fd> *ipc "MODEL")
                               NIL))
  (cmd-render-pv *ipc)

  (setq *xkb (new '(+Xkb)))

  (add-key-bindings)
  
  (gc 256)

  (in "verts.bin"
    (let (A NIL
          B NIL
          C 0
          Vert NIL)
      (while (and (setq Cls (rd)) (setq Propls (rd)) (< C *verts-max))
        (putl (setq Vert (box Cls)) Propls)
        (inc 'C)
        (push '*verts Vert))))

  (prinl-info "init-worker" (pack "Loaded " (length *verts) " verts"))

  # How to manage default internal symbols?
  # -> Must create particles
  # -> But where to place them?
  #    -> Timeline
  #
  # Create defs/ptrs
  # Create particles for defs
  #   Push particles into timeline
  #
  # On command:
  # - Add keybinding to timeline (input)
  # - Add command to timeline
  # - Add particle/output to timeline
  #   - One list of verts for the timeline and one list of verts for main
  #   - Same data
  #
  # Load all code into timeline
  # Execute timeline

  # Designate vertex 1 as ptr
  # Actually a cons cell
  (setq *vert-ptr (pop '*verts))

  (nl-ptr)
  
  (setq *cnt-v 2)

  # Render timeline
  (when NIL
    (push '*timeline (new '(+Particle)
                          *timeline
                          (draw-str "*timeline"
                                    (get-color-type *timeline))))
    (adv-ptr 1.0))
  

  (prinl-info "init-worker" "Starting epoll...")
  (loop (ipc~poll> *ipc -1)))


(de add-key-bindings ()

  # CMDS:
  #
  # Unused:
  # CTRL/ALT/SHIFT+TAB, CTRL/ALT/SHIFT+ESC, CAPS
  #
  # Identity:
  # Make-Char (aka trans sym) = ASCII+PACK
  # Make-Num = CTRL+NUM
  # Make-Int-Sym = ASCII+CMD(PACK+INTERN)
  # Make-Cons = SHIFT+ENTER
  #
  # Combos:
  # Pack+Intern: cmd-intern-2
  # Pack+Any+Eval
  #
  # Identity:
  # Del/Rem = DEL/BACKSPACE
  # Eval = ALT+ENTER
  # Cut:   move ptr->buffer(Stack/LIFO)
  # Copy:  dup ptr->buffer(Stack/LIFO)
  # Paste: move buf->ptr(Stack/LIFO)
  #
  # Position
  # Cam: Ctrl+HJKL or CTRL+Arrow
  # Ptr: ALT+HJKL  or Arrow
  #
  # Transformers:
  # Intern:        T-Sym->I-Sym
  # Sym+Quote:     I-Sym->T-Sym
  # Pack/Cut/Chop: T-Sym->T-Sym
  # Format:        Str<->Num
  #
  #
  # MOLECULAR:
  # (Make-Cons)
  # Push
  # Pop
  # List - by default, everything is already in a list

  # Enter = Linefeed
  (bind-keys (list ksd~Return              ) T cmd-make-char)
  (bind-keys (list ksd~Alt-R     ksd~Return) T cmd-make-cons)
  (bind-keys (list ksd~Shift-R   ksd~Return) T cmd-intern)
  (bind-keys (list ksd~Control-R ksd~Return) T cmd-eval)

  (for (K 32 (<= K 255) (inc K))
    (bind-keys (list K)             T cmd-make-char)
    (bind-keys (list ksd~Shift-L K) T cmd-make-char)
    (bind-keys (list ksd~Shift-R K) T cmd-make-char))

  # ALT+NUM
  (for (K ksd~d0 (<= K ksd~d9) (inc K))
    (bind-keys (list ksd~Control-L K) T cmd-make-num)
    (bind-keys (list ksd~Control-R K) T cmd-make-num))

  (bind-keys (list ksd~End      ) T cmd-pack)
  (bind-keys (list ksd~BackSpace) T cmd-del)
  (bind-keys (list ksd~Delete   ) T cmd-del)

  (bind-keys (list ksd~Control-L ksd~x) T '((Keysym) (println "Ctrl-L-x: CUT")))
  (bind-keys (list ksd~Control-L ksd~c) T '((Keysym) (println "Ctrl-L-c: COPY")))
  (bind-keys (list ksd~Control-L ksd~v) T '((Keysym) (println "Ctrl-L-v: PASTE")))
  (bind-keys (list ksd~Alt-L     ksd~z) T '((Keysym) (println "Alt-L-z: REDO")))
  (bind-keys (list ksd~Control-L ksd~z) T '((Keysym) (println "Ctrl-L-z: UNDO")))

  ##########

  (bind-keys (list ksd~Right) T cmd-move-ptr-px)
  (bind-keys (list ksd~Left)  T cmd-move-ptr-nx)
  (bind-keys (list ksd~Up)    T cmd-move-ptr-py)
  (bind-keys (list ksd~Down)  T cmd-move-ptr-ny)

  (bind-keys (list ksd~Control-L ksd~Right) T cmd-move-cam-px)
  (bind-keys (list ksd~Control-L ksd~Left)  T cmd-move-cam-nx)
  (bind-keys (list ksd~Control-L ksd~Up)    T cmd-move-cam-py)
  (bind-keys (list ksd~Control-L ksd~Down)  T cmd-move-cam-ny)

  (bind-keys (list ksd~Prior    ) T cmd-zoom-cam-in)
  (bind-keys (list ksd~Next     ) T cmd-zoom-cam-out))


# Unused
(de handle-client (Sock))


(de handle-ctrl (Sock)

  (let (Msg (recv-msg *ipc Sock)
        Data (any Msg)
        Name (get Data 1))

    # recv-msg will return data or NIL
    # This means sending NIL will disconnect the client, like EOF
    (unless Msg
      (disconn-conn *ipc Sock))

    (cond ((= Name 'KEYBOARD-KEY)
           (dispatch-kb Data))

          ((= Name "update-vert")
           (cmd-update-vert *ipc *vert-ptr))

          ((= Name "calc-pv")
           (cmd-calc-pv (get Data 2) (get Data 3)))

          ((= Name "update-pv")
           (cmd-update-pv *ipc))

          ((= Data "bye")
           # Send bye to ctrl which will send to all workers/render/model
           (send-msg *ipc Sock "bye")
           (ipc~disconn-sock Sock)
           (ipc~end> *ipc)
           (println "Worker says bye!")
           (bye))

          (T
           T))

    # Send ready when complete
    (send-msg *ipc Sock "rdy")

    #(prinl (usec) " | WORKER/" *Pid " | Worker ready")

    T))


(de handle-model (Sock)

  (let Msg (recv-msg *ipc Sock)

    # Handle recv updates from model

    # recv-msg will return data or NIL
    # This means sending NIL will disconnect the client, like EOF
    (unless Msg
      (disconn-conn *ipc Sock))

    (when NIL
      (prinl Msg))))


(de handle-render (Sock)

  (let Msg (recv-msg *ipc Sock)

    # recv-msg will return data or NIL
    # This means sending NIL will disconnect the client, like EOF
    (unless Msg
      (disconn-conn *ipc Sock))

   (when NIL

     (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
           Sock-Render (ipc~get-fd> *ipc "RENDER")
           Vert        *vert-ptr)

        (with Vert
          (when NIL
            (=: pos (list (+ (get (: pos) 1) 0.05)
                          (+ (get (: pos) 2) 0.0)
                          0.0)))
          (=: rota (list 0.0
                         0.0
                         (+ (get (: rota) 3) 0.017)))
          (update-model-matrix> Vert))

        # FIX - use update...
        (cmd-render-vert *ipc Vert)))))


(de dispatch-kb (Data)

  (let (Keycode (+ (get Data 4) 8)
        State   (get Data 5)
        Keysym  (get-one-sym> *xkb Keycode)  # or Data 7
        Name    (xkb~keysym-get-name Keysym)
        Utf-8   (get-utf8> *xkb Keycode))

    (prinl-info "cmd-kb" (pack (str Data) " " Keysym))

    # Update states
    (update-key> *xkb Keycode State)

    # Note this returns the actual cell
    (with *xkb
      (let State-Xkb (assoc Keysym (: states))
        (when State-Xkb
          (del State-Xkb (:: states)))
        (push (:: states) (cons Keysym State))))

    # Check bindings and dispatch handlers

    # Poss user remaps kb so another key produces a diff keysym so this would
    # be invalid

    # num and caps
    #if (xkb_state_led_name_is_active(state, XKB_LED_NAME_NUM) > 0)
    #    <The Num Lock LED is active>

    (when (or (= State xkb~KEY-DOWN)
              (= State KEY-REPEAT))

      (let (Mods NIL)

        (with *xkb

          (when (= (is-shift-active> *xkb) 1)
            (when (= (cdr (assoc ksd~Shift-L (: states))) xkb~KEY-DOWN)
              (push 'Mods ksd~Shift-L))
            (when (= (cdr (assoc ksd~Shift-R (: states))) xkb~KEY-DOWN)
              (push 'Mods ksd~Shift-R)))

          (when (= (is-ctrl-active> *xkb) 1)
            (when (= (cdr (assoc ksd~Control-L (: states))) xkb~KEY-DOWN)
              (push 'Mods ksd~Control-L))
            (when (= (cdr (assoc ksd~Control-R (: states))) xkb~KEY-DOWN)
              (push 'Mods ksd~Control-R)))

          (when (= (is-alt-active> *xkb) 1)
            (when (= (cdr (assoc ksd~Alt-L (: states))) xkb~KEY-DOWN)
              (push 'Mods ksd~Alt-L))
            (when (= (cdr (assoc ksd~Alt-R (: states))) xkb~KEY-DOWN)
              (push 'Mods ksd~Alt-R)))

          (when (= (is-logo-active> *xkb) 1)
            (when (= (cdr (assoc ksd~Super-L (: states))) xkb~KEY-DOWN)
              (push 'Mods ksd~Super-L))
            (when (= (cdr (assoc ksd~Super-R (: states))) xkb~KEY-DOWN)
              (push 'Mods ksd~Super-R))))

        # TODO: Sort key times

        (push 'Mods Keysym)
        (setq Mods (flip Mods))

        # Find map
        (let Map (assoc Mods *binds)

          (when Map

            (let (Rep (car (cdr Map))
                  Fn  (cdr (cdr Map)))

              (unless (and (= State KEY-REPEAT) (not Rep))
                (Fn Keysym)))))))))


(de bind-keys (Keys Rep Fn)

  (push '*binds (cons Keys (cons Rep Fn))))


(de main () (init-worker))


#################


(de pull-verts ()

  (prinl-info "init-worker" (pack "Pulling " *verts-max " verts..."))

  (let (Sock-Model (ipc~get-fd> *ipc "MODEL")
        Cnt-X 0
        Pos-X 0.0
        Pos-Y 0.0)

    (seed (in "/dev/urandom" (rd 64)))

    (for I *verts-max

      (let Vert (req-recv-vert *ipc
                               Sock-Model
                               NIL
                               I)

        (when NIL

          (with Vert

            (=: pos  (list Pos-X Pos-Y 0.0))
            (when NIL
              (=: rota (list 0.0   0.0   (rand 0.0 6.283185307179586)))
              (=: rgba (list (rand 0.0 1.0) (rand 0.0 1.0) (rand 0.0 1.0) 1.0
                             (rand 0.0 1.0) (rand 0.0 1.0) (rand 0.0 1.0) 1.0
                             (rand 0.0 1.0) (rand 0.0 1.0) (rand 0.0 1.0) 1.0
                             (rand 0.0 1.0) (rand 0.0 1.0) (rand 0.0 1.0) 1.0))))

          #(update-glyph> Vert (char (rand 33 126)))

          (update-model-matrix> Vert)

          # FIX - use update...
          (cmd-render-vert *ipc Vert)

          (inc 'Pos-X *adv-vert)
          (when (= (inc 'Cnt-X) 256)
            #(prin-info "init-worker" (pack "Request/Render vertex " I " of " *verts-max "\r"))
            (setq Cnt-X 0)
            (setq Pos-X 0.0)
            (dec 'Pos-Y (+ *adv-vert *adv-vert))
            #(println "POS-Y: " Pos-Y)
            T))

        (push '*verts Vert))))

  (prinl-info "init-worker" "Done pulling verts"))