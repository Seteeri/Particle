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
      (list "/src/glm.l"
            "/src/c/epoll.l"
            "/src/c/li.l"
            "/src/c/socket.l"
            "/src/c/ksd.l"
            "/src/c/xkbcommon.l"
            "/src/misc.l"
            "/src/ipc/ipc.l"
            "/src/prot/conn.l"
            "/src/prot/send.l"
            "/src/prot/recv.l"
            "/src/prot/obj.l"
            "/src/font/metrics.l"
            "/src/font/font.l"
            "/src/ctrl/xkb.l"
            "/src/worker/debug.l"            
            "/src/worker/conn.l"
            "/src/worker/cursor.l"   
            "/src/worker/projview.l"
            "/src/worker/vertex.l"            
            "/src/worker/gen-vert.l"
            "/src/worker/binds.l"       
            "/src/worker/ptr.l"            
            "/src/worker/cmd/pair.l"
            "/src/worker/cmd/str.l"
            "/src/worker/cmd/cam.l"
            "/src/worker/cmd/ptr.l"            
            "/src/worker/particle/particle.l"
            "/src/worker/particle/pair.l"
            "/src/worker/particle/sym.l"
            "/src/worker/particle/num.l"
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
(def '*font)
(def '*xkb)
(def '*binds)
(def '*vert-offs)
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
        *font (new '(+Font)
                   "DejaVuSansMono"
                   (pack *path-gly "DejaVuSansMono.met.idx.bin")
                   (pack *path-gly "DejaVuSansMono.rgba8.idx.bin")
                   19.265625)
        *ipc (new '(ipc~+IPC)
                   (pack "/tmp/part-work-" *i-proc ".sock") 'ipc~block serv-cli acpt-cli
                   (list (list *uds-ctrl T serv-ctrl   "CONTROL")
                         (list *uds-rend T serv-render "RENDER"))))
                         
  (reg-conns *ipc (pack NAME-PID "-" *i-proc))
  
  ##################################
  
  (setq *projview (req-recv-pv *ipc
                               (ipc~fd> *ipc "CONTROL")
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
    
  (for I *verts-max (push '*vert-offs (vert-off-gl I)))
  (setq *vert-offs (flip *vert-offs))  
  
  (gen-def-parts)
  
  (send-msg *ipc (ipc~fd> *ipc "CONTROL") "(rdy)")
  
  (prinl-info "init-worker" "Starting epoll...")
    
  (loop (ipc~poll> *ipc -1)))


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
        
    (def '*cdr (gen-sym-pair '*cdr 'x NIL T "↓" *sol-yellow))
    (def '*car (gen-sym-pair '*car 'x NIL T "↑" *sol-yellow))
    
    # 2191
    # ↑ ↓
    
    (set-ptr *cdr (get *start 'b))
    (put *cdr 'lay 'y)
    (mov-cur-abv (get *start 'b))
    (lay-ptr *cdr)

    (set-ptr *car (get *start 'b))
    (put *car 'lay 'y)
    (nl-cur -1.0) (adv-cur -3.0)
    (lay-ptr *car)    
    
    ######
    # TEMP
    
    (def '*1 (gen-sym-pair '*1 'y NIL T))

    #########
    
    (let L NIL
      (for X '(*car
               *cdr
               *1
               *start
               *log
               *bind-ops-p)
        (push 'L (cons X (val X))))
      (balance '*particles (sort L)))
    
    #########
        
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
(de serv-cli (Sock))


(de serv-ctrl (Sock)

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

            (POINTER-MOTION
              (when NIL
                (let U (unproj (list (num-to-fl (/ *width 2)) (num-to-fl (/ *height 2)) 0.0)
                              (get *projview 'mat-view)
                              (get *projview 'mat-proj)
                              (list 0.0 0.0 (num-to-fl *width) (num-to-fl *height)))
                  (println "unproj"
                          (format (car U) *Scl)
                          (format (cadr U) *Scl)
                          (format (caddr U) *Scl)))))
            
            ((TOUCH-UP
              TOUCH-DOWN
              TOUCH-FRAME
              TOUCH-MOTION)
              (prinl-info "serv-ctrl" (glue " " Data)))

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
              (ipc~disc-sock Sock)
              (ipc~end> *ipc)
              (println "Worker says bye!")
              (bye)))

          # Send ready when complete
          (send-msg *ipc Sock "(rdy)"))
          
        (disconn-conn *ipc Sock))))


(de serv-render (Sock)

  #(println 'serv-render)

  (let Msg (recv-msg *ipc Sock)
  
    #(test-rot-p0)
  
    (unless Msg
      (disconn-conn *ipc Sock))))


(de send-msg-rend (M) (send-msg *ipc (ipc~fd> *ipc "RENDER") M))


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

    # Check bindings and dispatch hnds

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


#(def 'PI 3.141592)
#(def 'PI2 (+ PI PI))
(def '*rot-z 0.0)
(de test-rot-p0 ()

  (let? Part (get *cdr 'a)
    
    (with Part
    
      (let Vert (car (: verts))
        
        (with Vert

          (let (Ce (with (: met)
                    (let (Dims (: dims))
                      (list (*/ (*/ (car Dims) (: sca-f) 1.0) 0.5 1.0)
                            (*/ (*/ (cadr Dims) (: sca-f) 1.0) 0.5 1.0)
                            0.0)))
                Tr (mat4-tra
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
          
          #{
          (=: mm
              (mat4-mul
                (mat4-tra (: pos))
                (mat4-mul
                  (mat4-inv Tr)
                  (mat4-mul
                    (mat4-rz *rot-z)
                    (mat4-mul
                      Tr
                      (mat4-mul
                        (mat4-inv (mat4-tra (: pos)))
                        (mat4-mul
                          (mat4-tra (: pos))
                          (mat4-mul
                            (mat4-rz (caddr (: rota)))
                            (mat4-mul
                              (mat4-ry (cadr (: rota)))
                              (mat4-mul
                                (mat4-rx (car (: rota)))
                                (mat4-sca (: sca))))))))))))
          }#
          
          (=: mm
              (mat4-mul@
                (mat4-tra (: pos))
                (mat4-inv Tr)
                (mat4-rz *rot-z)
                Tr
                (mat4-inv (mat4-tra (: pos)))
                (mat4-tra (: pos))
                (mat4-rz (caddr (: rota)))
                (mat4-ry (cadr (: rota)))
                (mat4-rx (car (: rota)))
                (mat4-sca (: sca))))
                                
          ))
                
        (req-send *ipc Vert)))))              
              
              
(de main ()
  (bind-args)
  (init-worker))
