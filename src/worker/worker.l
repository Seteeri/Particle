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
            "/src/worker/cmd/str.l"
            "/src/worker/cmd/cam.l"
            "/src/worker/cmd/ptr.l"
            "/src/worker/binds.l"            
            "/src/worker/particle/particle.l"
            "/src/worker/particle/pair.l"
            "/src/worker/particle/sym.l"
            "/src/worker/particle/num.l"
            "/src/worker/particle/ops/align.l"
            "/src/worker/particle/ops/gen.l"
            "/src/worker/particle/ops/layout.l"
            "/src/worker/particle/ops/pair.l"
            "/src/worker/particle/ops/sym.l"            
            "/src/worker/particle/ops/point.l"))


(def 'NAME-PID "WORKER")

# Pass from main
(def 'PATH-GLYPHS      "/home/user/Particle/particle/src/glyphs")

# Constants
(def '*glyph-dims '(96.0 96.0))
(def '*sca-vert  (*/ 1.0 1.0 96.0)) # vertex scale = 1/96.0
(def '*sca-glyph 5.8239365) # msdf param, mul by shape units -> px units, used by metrics
(def '*adv-glyph 9.375) # msdf param, mul by sca-glyph to get proper adv

# assumes all verts are scaled the same
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
(def '*start) # Current list
(def '*line) # Current line 
(def '*cmds) # List of commands (undo/history/timeline)
(def '*ptrs) # List of pointers

(def '*cur (list 0.0 0.0 0.0)) # for drawing. not literally drawn


(de init-worker ()
  
  (setq *i-proc 1)

  (setq *metrics (load-metrics)) 
  
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
  # Why is this here again?
  (setq *xkb (new '(+Xkb)))

  (for Bind *bindings-key
    (apply '((Keys Rep Fn)
             (let B (cons Keys (cons Rep Fn))
              (push '*binds B)))
           Bind))
  
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

  (let ((X Y) *cur)

    # Root particles are stored in *particles
    # *particles itself is not drawn
    #
    # Won't need this with external syms  
  
    (def '*cmds (gen-part '*cmds 'x))
    
    (set-cur-x X)
    (put *cmds 'lay 'y)
    
    # Must make lines
    (put *cmds 'line (new '(+Line) NIL (get *cmds 'b) NIL))
    (put *cmds 'b 'line (new '(+Line) *cmds NIL *cmds))
    
    # (mov-2> *cmds *cur)
    (lay-part-start *cmds *cmds)

    #################
    # Gen/Draw *start

    (set-cur-x X)
    (set-cur-y Y)
    (adv-cur 48.0)

    (def '*start (gen-part '*start 'x))

    # DO this properly - in gen?
    (with (get *start 'b)
      (=: line (new '(+Line) NIL NIL NIL (get *start 'a 'dims))))
      
    (setq *pos-main (copy *cur)
          *line (get *start 'b)
          *master *start)
    
    # Force draw for first particle since ori=cur
    (mov-2> *start *cur)
    (lay-part-start *start *line)
        
    ##################
    # TODO: Draw *ptrs
    
    (idx '*particles (def '*0 (gen-part '*0 'x)) T)

    # Remove NIL
    (del> (get *0 'b) T T)
    # Replace with main 'b = NIL
    (put *0 'b (get *start 'b))
    # Set layout
    (put *0 'lay 'y)
    
    (mov-cur-abv (get *start 'b))
    (lay-part-start *0 *line 'skip-cdr)
        
    # Skip cdr since ptr should be rel to cdr
    #(lay-part-start *0 *cur 'skip-cdr)
    #(mov-part-abv> *0 (get *start 'b) (cons T))
    
    ######
    # TEMP
    
    (setq p1 *cmds)

    (idx '*particles (def '*1 (gen-part '*1 'y)) T)
    (del> (get *1 'b) T T)
    #(nl-cur -3.0)
    #(lay-part-start *1 *line)

    ############
    
    (nl-cur 6.0)    
    
    (when NIL

      (let Part (gen-part '("a" "b" ("c" "d" "e" ("f"))) 'y)
      
        # (put (get Part 'b 'b) 'lay 'y)
        # (put (get Part 'b 'b 'a 'b 'b) 'lay 'y)
        
        (set-cur-x X)
        
        # Must calc bnds for lists
        (lay-part-start Part *line)))
        
    (when NIL
    
      (nl-cur)
      (set-cur-x X)
      (let Part (gen-part '*binds 'y)
        
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
          (upd-mod-mat> Vert))

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

            (let (Rep (cadr Map)
                  Fn  (cddr Map))

              (unless (and (= State KEY-REPEAT) (not Rep))
                (Fn Keysym)))))))))


(de main ()
  (bind-args)
  (init-worker))
