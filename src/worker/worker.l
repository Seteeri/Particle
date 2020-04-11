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


(scl 6)

(load (pack (pwd) "/src/misc.l"))
(mapc 'load-cwd
      (list "/src/c/mathc.l"
            "/src/c/epoll.l"
            "/src/c/li.l"
            "/src/c/socket.l"
            "/src/c/ksd.l"
            "/src/c/xkbcommon.l"
            "/src/misc.l"
            "/src/ipc/ipc.l"
            "/src/prot/conn.l"
            "/src/prot/msg.l"
            "/src/prot/obj.l"
            "/src/metrics/metrics.l"
            "/src/ctrl/projview.l"
            "/src/ctrl/vertex.l"
            "/src/ctrl/xkb.l"
            "/src/worker/conn.l"
            "/src/worker/cursor.l"
            "/src/worker/debug.l"
            "/src/worker/draw.l"
            "/src/worker/cmd/part.l"
            "/src/worker/cmd/cam.l"
            "/src/worker/cmd/ptr.l"
            "/src/worker/particle/particle.l"
            "/src/worker/particle/pair.l"
            "/src/worker/particle/sym.l"
            "/src/worker/particle/num.l"
            "/src/worker/particle/ops/align.l"
            "/src/worker/particle/ops/gen.l"
            "/src/worker/particle/ops/layout.l"
            "/src/worker/particle/ops/ops.l"
            "/src/worker/particle/ops/point.l"))


(def 'NAME-PID "WORKER")

# Pass from main
(def 'PATH-GLYPHS      "/home/user/Particle/particle/src/glyphs")

# Constants
(def '*sca-vert  (*/ 1.0 1.0 96.0))
(def '*sca-glyph 5.8239365)   # MSDF/tex parameter
(def '*adv-glyph 9.375)       # MSDF/tex parameter
(def '*adv-vert  (*/ (*/ *adv-glyph *sca-glyph 1.0) *sca-vert 1.0))
(def '*adv-vert-2  (+ *adv-vert *adv-vert))
(def '*adv-vert-3  (+ *adv-vert-2 *adv-vert))
(def 'KEY-REPEAT 2)
(def '*sublist-space-y 2.0)

# Global Symbols
(def '*metrics)
(def '*xkb)
(def '*binds)
(def '*verts) # Verts loaded
(def '*cnt-v 1)

# List works for small amount...
# assoc/list -> binary trees -> database

# Master index of all particles in the "world" - store sym:particle
# Eventually this will be moved to external symbols
(def '*particles)
(def '*lsts) # Traversal history list
(def '*main) # Current list
(def '*line) # Current line 
(def '*cmds) # List of commands (undo/history/timeline)
(def '*ptrs) # List of pointers

(def '*cur (list 0.0 0.0 0.0)) # Used when drawing; not literally drawn


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

      # Does not work...
      # ((`ksd~Shift-L `ksd~Tab) T ((Keysym) (println "Shift-L-Tab: TOGGLE LAYOUT REF")))
      ((`ksd~Alt-L   `ksd~Tab) T `cmd-swap-layout)
      ((`ksd~Alt-R   `ksd~Return) T `cmd-mov-nl)
      ((`ksd~Alt-R   `ksd~BackSpace) T `cmd-mov-bl)
      
      ## CUA Ops
      ((`ksd~Control-L `ksd~x) T ((Keysym) (println "Ctrl-L-x: CUT")))
      ((`ksd~Control-L `ksd~c) T ((Keysym) (println "Ctrl-L-c: COPY")))
      ((`ksd~Control-L `ksd~v) T ((Keysym) (println "Ctrl-L-v: PASTE")))
      ((`ksd~Control-L `ksd~z) T ((Keysym) (println "Ctrl-L-z: UNDO")))
      ((`ksd~Alt-L     `ksd~z) T ((Keysym) (println "Alt-L-z: REDO")))

      # Str Ops - work on current list
      # encourages users to create a list when editing
      ((`ksd~Control-L `ksd~u) T `cmd-pack)
      # split-space (word) : glue " ", 
      # chop (letter) : glue "", pack
      # : (mapcar pack (split (chop "The quick brown fox") " ")) -> ("The" "quick" "brown" "fox")

      ((`ksd~Control-L `ksd~t) T `cmd-format)
      ((`ksd~Control-L `ksd~y) T '((Keysym) (println "cmd-reverse")))
      ((`ksd~Alt-L     `ksd~y) T '((Keysym) (println "cmd-flip")))      
      
      # List/Sym/Atom Ops
      ((`ksd~Control-L `ksd~q) T `cmd-eval)
      ((`ksd~Control-L `ksd~r) T `cmd-reset)
      ((`ksd~Control-L `ksd~f) T `cmd-any)
      ((`ksd~Control-L `ksd~g) T `cmd-sym)
      ((`ksd~Control-L `ksd~h) T `cmd-intern)

      # Create NIL
      ((`ksd~Control-L `ksd~e)    T `cmd-make-nil)
      ((`ksd~Control-R `ksd~e)    T `cmd-make-nil)

      #(view (getl (get *0 'b)))
      # If we lay relative to origin should stay in same place
      ((`ksd~Control-R `ksd~Return) T ((Keysym)

                                        # Print ptr
                                        (let Ref (get *0 'b)
                                          (println Ref (get Ref 'data)))

                                        (println "Master Line: " *master (get *master 'b 'data))

                                        # Print lines out
                                        (let (Line *line
                                              Ref (get *0 'b))
                                        
                                          # Print current line
                                          (println "Ref: " Ref (get Ref 'data))
                                          (println "*on-car: " *on-car)
                                          (println "Line:")
                                          (println Line (get Line 'data))
                                          (view (getl (get Line 'line)))

                                          (mov-*1 Line))))

      # Pointer Translation
      ((`ksd~Control-L `ksd~w) T `cmd-ptr-up)
      ((`ksd~Control-L `ksd~a) T `cmd-ptr-left)      
      ((`ksd~Control-L `ksd~s) T `cmd-ptr-down)
      ((`ksd~Control-L `ksd~d) T `cmd-ptr-right)

      ((`ksd~Right) T `cmd-trans-ptr-px)
      ((`ksd~Left)  T `cmd-trans-ptr-nx)
      ((`ksd~Up)    T `cmd-trans-ptr-py)
      ((`ksd~Down)  T `cmd-trans-ptr-ny)

      # Camera Translation
      # Prior/Next
      ((`ksd~Alt-R `ksd~Right) T `cmd-move-cam-px)
      ((`ksd~Alt-R `ksd~Left)  T `cmd-move-cam-nx)
      ((`ksd~Alt-R `ksd~Up)    T `cmd-move-cam-py)
      ((`ksd~Alt-R `ksd~Down)  T `cmd-move-cam-ny)
      ((`ksd~Prior)            T `cmd-zoom-cam-in)
      ((`ksd~Next)             T `cmd-zoom-cam-out)))

      
(de init-worker ()
  
  (setq *i-proc 1)

  (setq *metrics (load-metrics *sca-glyph)) 
  
  (setq *ipc (new '(ipc~+IPC)
                    (pack "/tmp/part-work-" *i-proc ".sock") 'ipc~block handle-client accept-client
                    (list (list *uds-ctrl T handle-ctrl   "CONTROL")
                          (list *uds-rend T handle-render "RENDER")
                          )))
                          
  (reg-conns *ipc (pack NAME-PID "-" *i-proc))
  
  ##################################
  # Control should send this on init
  (setq *projview (req-recv-pv *ipc
                               (ipc~get-fd> *ipc "CONTROL")
                               NIL))
  (cmd-render-pv *ipc)
  ####################
  
  
  #####################
  # Move to control
  (setq *xkb (new '(+Xkb)))

  (apply-key-bindings *bindings-key)
  ##################################
  
  (gc 512)

  (let (A NIL
        B NIL
        C 0
        Vert NIL)
    (in "bin/verts.bin"          
      (while (and (setq Cls (rd))
                  (setq Propls (rd))
                  (< C *verts-max))
        (putl (setq Vert (box Cls)) Propls)
        (inc 'C)
        (push '*verts Vert)))

    (prinl-info "init-worker" (pack "Loaded " C " verts of " *verts-max)))

  (when NIL (run-tests))

  (gen-def-parts)
  
  (send-msg *ipc (ipc~get-fd> *ipc "CONTROL") "(rdy)")
  
  (prinl-info "init-worker" "Starting epoll...")

  (loop (ipc~poll> *ipc -1)))


(de gen-def-parts ()
  # Draw default global symbols/lists

  # Root particles are stored in *particles
  # *particles itself is not drawn

  (let ((X Y) *cur)

    (idx '*particles
          (def '*cmds
               (gen-part '*cmds 'x))
          T)
    (set-cur-x X)
    (put *cmds 'lay 'y)
    (lay-part-start *cmds *cmds)

    #############################
    # Generate main particle list

    (set-cur-x X)
    (set-cur-y Y)
    (adv-cur 48.0)

    (def '*main (gen-part '*main 'x))

    # DO this properly - in gen?
    (with (get *main 'b)
      (=: line (new '(+Line) NIL NIL NIL (get *main 'a 'dims))))
      
    (setq *pos-main (copy *cur))
    (setq *line (get *main 'b))
    (setq *master *main)
      
    (lay-part-start *main *line)
    
    #(inc-line-by-part-x> (get *main 'b) (get *main 'b))
    
    #######################
    # TODO: Draw *ptrs
    (idx '*particles
         (def '*0
              (gen-part '*0 'x))
         T)

    # Remove NIL
    (del> (get *0 'b) T T)
    # Replace with main 'b = NIL
    (put *0 'b (get *main 'b))
    # Set layout
    (put *0 'lay 'y)
    
    (mov-cur-abv (get *main 'b))
    (lay-part-start *0 *line 'skip-cdr)
        
    # Skip cdr since ptr should be rel to cdr
    #(lay-part-start *0 *cur 'skip-cdr)
    #(mov-part-abv> *0 (get *main 'b) (cons T))
    
    # TEMP
    (setq p1 *cmds)

    (idx '*particles (def '*1 (gen-part '*1 'y)) T)
    (del> (get *1 'b) T T)
    (nl-cur -3.0)
    (lay-part-start *1 *line)

    ############
    
    (nl-cur 6.0)    
    
    (when NIL

      (let Part (gen-part '("a" "b" ("c" "d" "e" ("f"))) 'x)
      
        # (put (get Part 'b 'b) 'lay 'y)
        # (put (get Part 'b 'b 'a 'b 'b) 'lay 'y)
        
        (set-cur-x X)
        
        # Must calc bnds for lists
        (lay-part-start Part *line)))


    (when NIL
    
      (nl-cur)
      (set-cur-x X)
      (let Part (gen-part '("let" ("a" 1 "b" 2) ("println" "hello world")) 'y)
        
        (nl-cur 2.0)
        (set-cur-x X)
        (lay-part-start Part *line)))

    (when NIL
    
      (nl-cur)
      (set-cur-x X)
      (let Part (gen-part '(("1" "Content"
                             ("1.1" "Content"
                              ("1.1.1" "Content")
                              ("1.1.1" "Content"
                               ("1.1.1.1" "Content")
                               ("1.1.1.2" "Content")))))
                          'y)
        
        (nl-cur 2.0)
        (set-cur-x X)
        (lay-part-start Part *line)))))
      

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
    (send-msg *ipc Sock "(rdy)")

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

  #(println 'handle-render)

  (let Msg (recv-msg *ipc Sock)

    # recv-msg will return data or NIL
    # This means sending NIL will disconnect the client, like EOF
    (unless Msg
      (disconn-conn *ipc Sock))

   (when NIL

     (let (Sock-Render (ipc~get-fd> *ipc "RENDER")
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

    #(prinl-info "cmd-kb" (pack (str Data) " " Keysym))

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


(de main ()
  (bind-args)
  (init-worker))
