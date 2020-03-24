#{
  Global variables start with an asterisk "*"
  Global constants may be written all-uppercase
  Functions and other global symbols start with a lower case letter
  Locally bound symbols start with an upper case letter
  Local functions start with an underscore "_"
  Classes start with a plus-sign "+", where the first letter
    is in lower case for abstract classes and in upper case for normal classes
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
(def '*verts) # Verts loaded
(def '*cnt-v 1)

# List works for small amount...
# assoc/list -> binary trees -> database

# Master index of all particles in the "world" - store sym:particle
# Stores pointer particles and main list particle
(def '*particles)
(def '*part-lsts) # Traversal history list
(def '*part-main) # Master list
(def '*part-cmds) # List of commands (undo/history/timeline)
(def '*part-ptrs) # List of pointers

(def '*cur (list 0.0 0.0 0.0)) # Used when drawing; not literally drawn

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
(load "/home/user/Particle/particle/src/model/projview.l")
(load "/home/user/Particle/particle/src/model/vertex.l")
(load "/home/user/Particle/particle/src/ctrl/xkb.l")
(load "/home/user/Particle/particle/src/worker/conn.l")
(load "/home/user/Particle/particle/src/worker/cursor.l")
(load "/home/user/Particle/particle/src/worker/draw/draw.l")
#(load "/home/user/Particle/particle/src/worker/draw/forms.l")
(load "/home/user/Particle/particle/src/worker/cmd/part.l")
(load "/home/user/Particle/particle/src/worker/cmd/cam.l")
(load "/home/user/Particle/particle/src/worker/cmd/ptr.l")
(load "/home/user/Particle/particle/src/worker/particle/particle.l")
(load "/home/user/Particle/particle/src/worker/particle/pair.l")
(load "/home/user/Particle/particle/src/worker/particle/sym.l")
(load "/home/user/Particle/particle/src/worker/particle/num.l")
(load "/home/user/Particle/particle/src/worker/particle/layout.l")
(load "/home/user/Particle/particle/src/worker/particle/gen.l")
(load "/home/user/Particle/particle/src/worker/particle/ops.l")

(def '*bindings-key
      ## Number
      '(~(let List NIL
          (for (K ksd~d0 (<= K ksd~d9) (inc K))
            (push 'List (list (list ksd~Control-L K) T cmd-make-num))
            (push 'List (list (list ksd~Control-R K) T cmd-make-num)))
          List)
      ## Char Strings
      ~(let List NIL
        (for (K 33 (<= K 255) (inc K))
          (push 'List (list (list K)                     T cmd-make-char))
          (push 'List (list (list ksd~Shift-L K)         T cmd-make-char))
          (push 'List (list (list ksd~Shift-R K)         T cmd-make-char)))
        List)
        
      ((`ksd~BackSpace) T `cmd-del)
      ((`ksd~Delete   ) T `cmd-del)

      ### Special cases: Space (32), Return, Tab
      ((`ksd~space)               T `cmd-make-char)
      ((`ksd~Return)              T `cmd-make-char)
      ((`ksd~Tab)                 T `cmd-make-char)

      ## CUA Ops
      ((`ksd~Control-L `ksd~x) T ((Keysym) (println "Ctrl-L-x: CUT")))
      ((`ksd~Control-L `ksd~c) T ((Keysym) (println "Ctrl-L-c: COPY")))
      ((`ksd~Control-L `ksd~v) T ((Keysym) (println "Ctrl-L-v: PASTE")))
      ((`ksd~Control-L `ksd~z) T ((Keysym) (println "Ctrl-L-z: UNDO")))
      ((`ksd~Alt-L     `ksd~z) T ((Keysym) (println "Alt-L-z: REDO")))

      # Lisp Ops
      ((`ksd~Control-L `ksd~q) T `cmd-eval)
      ((`ksd~Control-L `ksd~u) T `cmd-pack)
      ((`ksd~Control-L `ksd~r) T `cmd-chop)
      ((`ksd~Control-L `ksd~f) T `cmd-any)
      ((`ksd~Control-L `ksd~g) T `cmd-sym)
      ((`ksd~Control-L `ksd~t) T `cmd-format)
      ((`ksd~Control-L `ksd~h) T `cmd-intern)
      ((`ksd~Control-L `ksd~y) T '((Keysym) (println "cmd-reverse")))
      ((`ksd~Alt-L     `ksd~y) T '((Keysym) (println "cmd-flip")))

      # Create NIL
      ((`ksd~Control-L `ksd~e)    T `cmd-make-nil)
      ((`ksd~Control-R `ksd~e)    T `cmd-make-nil)

      ((`ksd~Control-R `ksd~Return) T ((Keysym)
                                        (let Bnds (calc-bounds> (val> p0) NIL T)
                                          (mov> p0 Bnds))
                                        #(println (get (val> p0) 'c))
                                        #(view (getl (get (val> p0) 'c))) (prinl) (prinl)
                                        #(println (val> p0))
                                        #(view (getl (val> p0))) (prinl) (prinl)
                                        #(println (get (val> p0) 'b))
                                        #(view (getl (get (val> p0) 'b))) (prinl) (prinl)
                                        ))

      # Pointer Translation
      ((`ksd~Control-L `ksd~w) T `cmd-point-ptr-out)
      ((`ksd~Control-R `ksd~w) T `cmd-point-ptr-out)      

      ((`ksd~Control-L `ksd~a) T `cmd-point-ptr-pre)      
      ((`ksd~Control-R `ksd~a) T `cmd-point-ptr-pre)      

      ((`ksd~Control-L `ksd~s) T `cmd-point-ptr-in)
      ((`ksd~Control-R `ksd~s) T `cmd-point-ptr-in)

      ((`ksd~Control-L `ksd~d) T `cmd-point-ptr-nxt)
      ((`ksd~Control-R `ksd~d) T `cmd-point-ptr-nxt)

      # Camera Translation
      # Make dir
      ((`ksd~Right) T `cmd-trans-ptr-px)
      ((`ksd~Left)  T `cmd-trans-ptr-nx)
      ((`ksd~Up)    T `cmd-trans-ptr-py)
      ((`ksd~Down)  T `cmd-trans-ptr-ny)

      ((`ksd~Control-L `ksd~Right) T `cmd-move-cam-px)
      ((`ksd~Control-L `ksd~Left)  T `cmd-move-cam-nx)
      ((`ksd~Control-L `ksd~Up)    T `cmd-move-cam-py)
      ((`ksd~Control-L `ksd~Down)  T `cmd-move-cam-ny)
      ((`ksd~Prior    )            T `cmd-zoom-cam-in)
      ((`ksd~Next     )            T `cmd-zoom-cam-out)))


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

  (apply-key-bindings *bindings-key)

  (gc 256)

  (let (A NIL
        B NIL
        C 0
        Vert NIL)
    (in "verts.bin"          
      (while (and (setq Cls (rd))
                  (setq Propls (rd))
                  (< C *verts-max))
        (putl (setq Vert (box Cls)) Propls)
        (inc 'C)
        (push '*verts Vert)))

    (prinl-info "init-worker" (pack "Loaded " C " verts of " *verts-max)))

  # TODO: Render default symbols

  # However, to draw the Ptr requires Ptr...
  # -> Manually make Pointer particle

  (when NIL (run-tests))

  (gen-def-parts)

  (prinl-info "init-worker" "Starting epoll...")

  (loop (ipc~poll> *ipc -1)))


(de gen-def-parts ()
  # Draw default global symbols/lists

  # Root particles are stored in *particles
  # *particles itself is not drawn

  (let ((X Y) *cur)

    (idx '*particles
          (def '*part-cmds
               (gen-part '*part-cmds 'x))
          T)
    (set-cur-x X)
    (lay-part *part-cmds *cur)


    (nl-cur 3.0)
    (set-cur-x X)


    #############################
    # Generate main particle list
    (idx '*particles
          (def '*part-main
                (gen-part '*part-main 'x))
          T)
    (set-cur-x X)
    (lay-part *part-main *cur)


    (nl-cur 3.0)
    (set-cur-x X)

    
    #######################
    # TODO: Draw *part-ptrs
    # Will need to ignore drawing CDR for particle
    #(push '*part-ptrs (gen-part 'p0 'x))
    (idx '*particles
         (def 'p0
              (gen-part 'p0 'x))
         T)

    # Connect pointer to main
    # Remove NIL first
    (del> (get p0 'b) T T)
    (put p0 'b *part-main)

    (set-cur-x X)
    (put p0 'lay 'y)
    (lay-part p0 *cur)

    #(let Bounds (calc-bounds> *part-main)
    #  (mov> p0 Bounds))


    ############
    
    (when NIL

      (nl-cur 3.0)
      (set-cur-x X)

      (nl-cur)
      (set-cur-x X)
      (let Part (gen-part NIL 'y)
        
        (nl-cur 2.0)
        (set-cur-x X)
        
        # This will override layout in Particles
        (lay-part Part *cur)))


    (when NIL
    
      (nl-cur)
      (set-cur-x X)
      (let Part (gen-part '("let" ("a" 1 "b" 2) ("println" "hello world")) 'y)
        
        (nl-cur 2.0)
        (set-cur-x X)
        (lay-part Part *cur)))

    (when NIL
    
      (nl-cur)
      (set-cur-x X)
      (let Part (gen-part '(("1" "Content"
                             ("1.1" "Content"
                              ("1.1.1" "Content")
                              ("1.1.1" "Content"
                               ("1.1.1.1" "Content")
                               ("1.1.1.1" "Content")))))
                          'y)
        
        (nl-cur 2.0)
        (set-cur-x X)
        (lay-part Part *cur)))))
      

(de run-tests ())


(de alloc-vert (Pos Char Col)
  (let Vert (pop '*verts)
    (with Vert
      (=: pos Pos)
      (=: rgba (list (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0)))
    (when Char (update-glyph> Vert Char))
    (upd-mdl-mtrx> Vert)
    (cmd-upd-vert *ipc Vert)

    (inc '*cnt-v)

    Vert))


(de apply-key-bindings (Binds)
  (for Bind Binds (apply bind-keys Bind)))


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

          ((or (= Name 'TOUCH-UP)
               (= Name 'TOUCH-DOWN)
               (= Name 'TOUCH-FRAME)
               (= Name 'TOUCH-MOTION))
           (prinl-info "handle-ctrl" (glue " " Data)))

          ((= Name "update-vert")
           (cmd-upd-vert *ipc *vert-ptr))

          ((= Name "calc-pv")
           (cmd-calc-pv (get Data 2) (get Data 3)))

          ((= Name "update-pv")
           (cmd-upd-pv *ipc))

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
          (upd-mdl-mtrx> Vert))

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

      (let Mods ()

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
          (upd-mdl-mtrx> Vert)

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
