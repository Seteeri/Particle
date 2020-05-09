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


# (scl 6)

(load (pack (pwd) "/src/misc.l"))
(mapc 'ld-cwd
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
            "/src/font/metrics.l"
            "/src/ctrl/xkb.l"
            "/src/worker/debug.l"            
            "/src/worker/conn.l"
            "/src/worker/cursor.l"   
            "/src/worker/projview.l"
            "/src/worker/vertex.l"            
            "/src/worker/gen-vert.l"
            "/src/worker/cmd/pair.l"
            "/src/worker/cmd/str.l"
            "/src/worker/cmd/cam.l"
            "/src/worker/cmd/ptr.l"
            "/src/worker/binds.l"            
            "/src/worker/particle/particle.l"
            "/src/worker/particle/pair.l"
            "/src/worker/particle/sym.l"
            "/src/worker/particle/num.l"
            "/src/worker/particle/point.l"            
            "/src/worker/particle/ops/align.l"
            "/src/worker/particle/ops/gen.l"
            "/src/worker/particle/ops/lay.l"
            "/src/worker/particle/ops/pair.l"
            "/src/worker/particle/ops/line.l"
            "/src/worker/particle/ops/sym.l"))


(def 'NAME-PID "WORKER")

(def 'KEY-REPEAT 2)
(def '*sublist-space-y 1.0)

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
(def '*log) # List of history for undo/redo
(def '*ptrs) # List of pointers

(def '*cur (list 0.0 0.0 0.0)) # for drawing. not literally drawn


(de init-worker ()
    
  (setq *i-proc 1
        *idx-uni-met (ld-idx-uni-met (pack *path-gly "DejaVuSansMono.met.idx.bin"))
        *idx-uni-tex (in (pack *path-gly "DejaVuSansMono.rgba8.idx.bin") (rd))
        *ipc (new '(ipc~+IPC)
                   (pack "/tmp/part-work-" *i-proc ".sock") 'ipc~block handle-client accept-client
                   (list (list *uds-ctrl T handle-ctrl   "CONTROL")
                         (list *uds-rend T handle-render "RENDER"))))
                         
  (reg-conns *ipc (pack NAME-PID "-" *i-proc))
  
  ##################################
  
  (setq *projview (req-recv-pv *ipc
                               (ipc~get-fd> *ipc "CONTROL")
                               NIL
                               0))
                               
  (req-send *ipc *projview)
  
  ####################
  
    
  #####################

  (setq *xkb (new '(+Xkb)))

  # User can modify '*binds-* and 
  # then call func to upd
  (let L (sort 
           (append
              (list-binds-ascii *binds-latin-1)
              (list-binds-num *binds-num)
              (list-binds *binds-ops)))
    (balance '*binds L))
    
  ##################################
    
  (for I *verts-max (push '*verts (get-vert-off I)))
  (setq *verts (flip *verts))  
  
  (gen-def-parts)
  
  (send-msg *ipc (ipc~get-fd> *ipc "CONTROL") "(rdy)")
  
  (prinl-info "init-worker" "Starting epoll...")

  (loop (ipc~poll> *ipc -1)))

  
(de ld-metrics (Path)
  #*metrics (ld-metrics (pack *path-gly "m/"))
  (let (Metrics ()
        Path-M-Def (pack Path "32-m.v.l"))
    (for Code 255 
      (push 'Metrics
            (let (Met (new '(+Metrics))
                  Path-M (pack Path Code "-m.v.l"))
              (unless (info Path-M) (setq Path-M Path-M-Def))
              (let Pro (car (str (in Path-M
                                      (till NIL T))))
                (putl Met Pro)
                (calc-sca-f> Met))
              Met)))
    (flip Metrics)))

    
(de ld-idx-uni-met (File)

  # Must update scale
  # unless scale is done during gen
  # for now, keep dynamic

  # HEAP:
  # B-Tree
  # Objs
  # 
  # Should interleave by looping and creating simul.
  # This would require building b-tree here
  
  (let B (in File (rd))
    (for M (idx 'B)
      (let O (new '(+Metrics))
        (putl O (cdr M))
        (calc-sca-f> O)
        (con M O)))
    B))

    
(de gen-def-parts ()

  (let ((X Y) *cur)

    # Root particles are stored in *particles
    # *particles itself is not drawn
    #
    # Won't need this with external syms  
  
    #######
    # Binds
  
    (setq *bind-ops-p (gen-sym-pair '*binds-ops 'y))
    
    #{
    # Make all pairs Y and create lines
    (let P *bind-ops-p
      (while P
        (when (isa '(+Pair) (get P 'a))
          (put P 'lay 'y)
          (put P 'line (new '(+Line)))
          (put (get P 'a) 'line (new '(+Line))))))
    }#
    
    (lay-part-start *bind-ops-p *bind-ops-p)  
      
        
    #####
    # Log
       
    (setq *log (gen-sym-pair '*log 'y))
        
    # Must make lines
    #(put *log 'lay 'y)    
    #(put *log 'line (new '(+Line) NIL (get *log 'b)))
    #(put *log 'b 'line (new '(+Line) *log NIL *log))
    
    (set-cur-x X) (set-cur-y Y)
    (adv-cur 24.0)
    (lay-part-start *log *log)
        
    ########
    # *start

    (setq *start (gen-sym-pair '*start 'x))

    # DO this properly - in gen?
    (with (get *start 'b)
      (=: line (new '(+Line) NIL NIL NIL (get *start 'a 'dims))))
      
    (setq *pos-main (copy *cur)
          *line (get *start 'b)
          *master *start)
    
    (set-cur-x X) (set-cur-y Y)
    (adv-cur 48.0)    
    # Force draw for first particle since ori=cur
    (mov-verts> *start *cur)
    (lay-part-start *start *line)
        
    ##################
    # TODO: Draw *ptrs
    
    (idx '*particles (def '*0 (gen-sym-pair '*0 'x)) T)

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

    (idx '*particles (def '*1 (gen-sym-pair '*1 'y)) T)
    (del> (get *1 'b) T T)

    ############
        
    (when NIL

      (let Part (gen-part *binds-ops
                          'y)
                          
        (set-cur-x X)
        
        # Must calc bnds for lists
        (lay-part-start Part Part)))
        
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
        Data (car (str Msg))
        Name (car Data))
        
    # recv-msg will return data or NIL
    # This means sending NIL will disconnect the client, like EOF
    (if Msg
        
        (prog
      
          (case Name
          
            (KEYBOARD-KEY
              (dispatch-kb Data))

            ((TOUCH-UP
              TOUCH-DOWN
              TOUCH-FRAME
              TOUCH-MOTION)
              (prinl-info "handle-ctrl" (glue " " Data)))

            ("update-vert"
              (req-send *ipc *vert-ptr))

            ("calc-pv"
              # method?
              (with *projview
                (=: width  (get Data 2))
                (=: height (get Data 3)))
              (upd-mat-proj> *projview))

            ("upd-pv"
              (req-send *ipc *projview))

            ("bye"
              # Send bye to ctrl which will send to all workers/render/model
              (send-msg *ipc Sock "(bye)")
              (ipc~disconn-sock Sock)
              (ipc~end> *ipc)
              (println "Worker says bye!")
              (bye)))

          # Send ready when complete
          (send-msg *ipc Sock "(rdy)"))
          
        (disconn-conn *ipc Sock))))


(de handle-render (Sock)

  #(println 'handle-render)

  (let Msg (recv-msg *ipc Sock)
  
    #(test-rot-p0)
  
    (if Msg
        (disconn-conn *ipc Sock))))


(de send-msg-rend (M) (send-msg *ipc (ipc~get-fd> *ipc "RENDER") M))


(de dispatch-kb (Data)

  (let (Keycode (+ (get Data 4) 8)
        State   (get Data 5)
        Ks  (get-one-sym> *xkb Keycode)  # or Data 7
        Name    (xkb~keysym-get-name Ks)
        Utf-8   (get-utf8> *xkb Keycode))

    #(prinl-info "cmd-kb" (pack (str Data) " " Ks))

    # Update states
    (upd-key> *xkb Keycode State)

    # Note this returns the actual cell
    (with *xkb
      (let State-Xkb (assoc Ks (: states))
        (when State-Xkb
          (del State-Xkb (:: states)))
        (push (:: states) (cons Ks State))))

    # Check bindings and dispatch handlers

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

        (push 'Mods Ks)
        (setq Mods (flip Mods))
        
        (let? V (cdr (lup *binds Mods))
          (let (Rep (car V)
                Fn (cadr V))
            (unless (and (= State KEY-REPEAT) (not Rep))
              (Fn Ks))))))))


(def 'PI 3.141592)
(def 'PI2 (+ PI PI))
(def '*rot-z 0.0)
(de test-rot-p0 ()

  (let? Part (get *0 'a)
    
    (with Part
    
      (let Vert (car (: verts))
        
        (with Vert

          (let (Ce (with (: met)
                    (let (Dims (: dims))
                      (list (*/ (*/ (car Dims) (: sca-f) 1.0) 0.5 1.0)
                            (*/ (*/ (cadr Dims) (: sca-f) 1.0) 0.5 1.0)
                            0.0)))
                Tr (mat4-translation
                     (list (- (car Ce))
                           (- (cadr Ce))
                           0.0)))
                    
          (inc '*rot-z 0.03)
          (when (> *rot-z PI2) (setq *rot-z 0.0))
          
          # Scale from origin (left bottom)
          # Translate origin to center
          # Rotate rel to center
          # Translate back to origin
          # Translate to final pos          
          
          (=: mm
              (mat4-mul
                (mat4-translation (: pos))
                (mat4-mul
                  (mat4-inverse Tr)
                  (mat4-mul
                    (mat4-rotation-z *rot-z)
                    (mat4-mul
                      Tr
                      (mat4-mul
                        (mat4-inverse (mat4-translation (: pos)))
                        (mat4-mul
                          (mat4-translation (: pos))
                          (mat4-mul
                            (mat4-rotation-z (caddr (: rota)))
                            (mat4-mul
                              (mat4-rotation-y (cadr (: rota)))
                              (mat4-mul
                                (mat4-rotation-x (car (: rota)))
                                (mat4-scaling (: sca))))))))))))
                                
          ))
                
        (req-send *ipc Vert)))))              
              
              
(de main ()
  (bind-args)
  (init-worker))
