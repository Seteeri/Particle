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
# Pass from main
(def 'PATH-GLYPHS      "/home/user/Particle/particle/src/glyphs")

# Constants
(def '*sca-vert  (*/ 1.0 1.0 96.0))
(def '*sca-glyph 5.8239365)   # MSDF/tex parameter
(def '*adv-glyph 9.375)       # MSDF/tex parameter
(def '*adv-vert  (*/ (*/ *adv-glyph *sca-glyph 1.0) *sca-vert 1.0))
(def 'KEY-REPEAT 2)

# Global Symbols
(def '*metrics)
(def '*xkb)
(def '*binds)

(def '*verts)
(def '*cnt-v 1)

# Vertices (particle?) for pointer
(def '*vert-ptr)

# Not actually a pointer but the data structure itself
(def '*part-ptr)
(def '*lst-ptr)
(def '*lists-ptr)


# Wrapper libraries
(load "/home/user/Particle/particle/src/c/mathc.l")
(load "/home/user/Particle/particle/src/c/epoll.l")
(load "/home/user/Particle/particle/src/c/li.l")
(load "/home/user/Particle/particle/src/c/socket.l")
(load "/home/user/Particle/particle/src/c/ksd.l")
(load "/home/user/Particle/particle/src/c/xkbcommon.l")
# Lisp libraries
(load "/home/user/Particle/particle/src/misc.l")
(load "/home/user/Particle/particle/src/ipc/ipc.l")
(load "/home/user/Particle/particle/src/prot/conn.l")
(load "/home/user/Particle/particle/src/prot/msg.l")
(load "/home/user/Particle/particle/src/prot/obj.l")
(load "/home/user/Particle/particle/src/metrics/metrics.l")
(load "/home/user/Particle/particle/src/model/particle.l")
(load "/home/user/Particle/particle/src/model/projview.l")
(load "/home/user/Particle/particle/src/model/vertex.l")
(load "/home/user/Particle/particle/src/ctrl/xkb.l")
(load "/home/user/Particle/particle/src/worker/conn.l")
(load "/home/user/Particle/particle/src/worker/ptr.l")
(load "/home/user/Particle/particle/src/worker/draw/draw.l")
(load "/home/user/Particle/particle/src/worker/draw/structs.l")
(load "/home/user/Particle/particle/src/worker/draw/forms.l")
(load "/home/user/Particle/particle/src/worker/cmd/vert.l")
(load "/home/user/Particle/particle/src/worker/cmd/cam.l")
(load "/home/user/Particle/particle/src/worker/cmd/ptr.l")


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

  # Render default symbols?
  # User can then use builtin copy to produce a copy of the symbol

  # Allocate pointer - use same char as PicoLisp ":"
  # Make pointer a particle also
  (setq *vert-ptr (alloc-vert *col-red ":"))
    
  (let ((X Y) (get *vert-ptr 'pos))
    (setq *part-ptr 
          (gen-part (list 'PARTICLE '(a b) '(c d))
                    0.0
                    X)))
    
  (setq *lst-ptr *part-ptr)
  
  (adv-ptr)

  (prinl-info "init-worker" "Starting epoll...")
  
  (loop (ipc~poll> *ipc -1)))


(de alloc-vert (Col Glyph)
  (let Vert (pop '*verts)
    (with Vert
      (=: rgba (list (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0)))
    (when Glyph (update-glyph> Vert Glyph))
    (update-model-matrix> Vert)
    (cmd-update-vert *ipc Vert)

    (inc '*cnt-v)

    Vert))


(de add-key-bindings ()

  # Singular Ops (new/del)

  ## List

  (bind-keys (list ksd~Control-L     ksd~Return) T cmd-make-list)
  (bind-keys (list ksd~Control-R     ksd~Return) T cmd-make-list)

  (bind-keys (list ksd~BackSpace) T cmd-del)
  (bind-keys (list ksd~Delete   ) T cmd-del)

  ## Number

  (for (K ksd~d0 (<= K ksd~d9) (inc K))

    (bind-keys (list ksd~Control-L K) T cmd-make-num)
    (bind-keys (list ksd~Control-R K) T cmd-make-num))

  ## String/Symbol

  (for (K 33 (<= K 255) (inc K))
    (bind-keys (list K)                     T cmd-make-char)
    (bind-keys (list ksd~Shift-L K)         T cmd-make-char)
    (bind-keys (list ksd~Shift-R K)         T cmd-make-char))
  ### Special cases: Space (32), Return, Tab
  (bind-keys (list ksd~space)               T cmd-make-char)
  (bind-keys (list ksd~Return)              T cmd-make-char)
  (bind-keys (list ksd~Tab)                 T cmd-make-char)

  ## Std Ops

  (bind-keys (list ksd~Control-L ksd~x) T '((Keysym) (println "Ctrl-L-x: CUT")))
  (bind-keys (list ksd~Control-L ksd~c) T '((Keysym) (println "Ctrl-L-c: COPY")))
  (bind-keys (list ksd~Control-L ksd~v) T '((Keysym) (println "Ctrl-L-v: PASTE")))
  (bind-keys (list ksd~Control-L ksd~z) T '((Keysym) (println "Ctrl-L-z: UNDO")))
  (bind-keys (list ksd~Alt-L     ksd~z) T '((Keysym) (println "Alt-L-z: REDO")))

  # Binary Ops
  (bind-keys (list ksd~Control-L ksd~q) T cmd-eval)

  (bind-keys (list ksd~Control-L ksd~e) T cmd-pack)
  (bind-keys (list ksd~Control-L ksd~r) T cmd-chop)
    
  (bind-keys (list ksd~Control-L ksd~f) T cmd-any)
  (bind-keys (list ksd~Control-L ksd~g) T cmd-sym)
  
  (bind-keys (list ksd~Control-L ksd~t) T cmd-format)
  (bind-keys (list ksd~Control-L ksd~h) T cmd-intern)


  (bind-keys (list ksd~Control-L ksd~y) T '((Keysym) (println "cmd-reverse")))
  (bind-keys (list ksd~Alt-L ksd~y) T '((Keysym) (println "cmd-flip")))
  
  # Pointer

  # Selection

  (bind-keys (list ksd~Control-R ksd~w) T cmd-select-up) #'((Keysym) (println "Control-R + W: Select Up")))
  (bind-keys (list ksd~Control-R ksd~a) T cmd-select-le) #'((Keysym) (println "Control-R + A: Select Le")))
  (bind-keys (list ksd~Control-R ksd~s) T cmd-select-dn) #'((Keysym) (println "Control-R + S: Select Dn")))
  (bind-keys (list ksd~Control-R ksd~d) T cmd-select-ri) #'((Keysym) (println "Control-R + D: Select Ri")))

  # Placement

  (bind-keys (list ksd~Alt-R ksd~w) T cmd-place-up) #'((Keysym) (println "Alt-R + W: Place Up")))
  (bind-keys (list ksd~Alt-R ksd~a) T cmd-place-le) #'((Keysym) (println "Alt-R + A: Place Le")))
  (bind-keys (list ksd~Alt-R ksd~s) T cmd-place-dn) #'((Keysym) (println "Alt-R + S: Place Dn")))
  (bind-keys (list ksd~Alt-R ksd~d) T cmd-place-ri) #'((Keysym) (println "Alt-R + D: Place Ri")))

  # Euclidean Space

  (bind-keys (list ksd~Right)       T cmd-translate-ptr-px)
  (bind-keys (list ksd~Left)        T cmd-translate-ptr-nx)
  (bind-keys (list ksd~Up)          T cmd-translate-ptr-py)
  (bind-keys (list ksd~Down)        T cmd-translate-ptr-ny)

  # Camera

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
