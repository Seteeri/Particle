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
            "/src/worker/binds.l"       
            "/src/worker/ptr.l"            
            #"/src/worker/gen.l"
            "/src/worker/gen2.l"
            "/src/worker/cmd/pair.l"
            "/src/worker/cmd/str.l"
            "/src/worker/cmd/cam.l"
            "/src/worker/cmd/ptr.l"            
            "/src/worker/particle/particle.l"
            "/src/worker/particle/pair.l"
            "/src/worker/particle/sym.l"
            "/src/worker/particle/num.l"
            "/src/worker/particle/ops/align.l"
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

(def '*cur (list 0.0 0.0 0.0))
(def '*fwd-in)


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
                         (list *uds-rend T serv-rend "RENDER"))))
                         
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
    
  (setq *vert-offs
        (make
          (for I *verts-max 
            (link (vert-off-gl I)))))
  
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
  
    (when NIL
    
      (setq *bind-ops-p (gen2-sym-full '*binds-ops 'y))
      
      (lay-part-start *bind-ops-p *bind-ops-p))
            
    #####
    # Log
    
    (mov-cur X Y)
    (adj-cur-x 24.0)    
    
    (when NIL
    
      (setq *log (gen2-sym-full '*log 'y))
                
      (mov-cur X Y)
      (adj-cur-x 24.0)
      (lay-part-start *log *log))
                
    ########
    # *start

    (setq *start (gen2-sym-full '*start 'x)
          *pos-main (copy *cur)
          *line (get *start 'b)
          *master *start)
    
    (mov-cur X Y)
    (adj-cur-x 48.0)    
    # Force draw for first particle since ori=cur
    (mov-verts> *start *cur)
    (lay-part-start *start *line)
        
    ##################
    # TODO: Draw *ptrs
        
    (def '*cdr (gen2-sym-full '*cdr 'x (cons NIL T) "↓" *sol-yellow))
    (def '*car (gen2-sym-full '*car 'x (cons NIL T) "↑"))
    
    #{
      : (char "→")
      -> 8594
      : (char "←")
      -> 8592
      : (char "↑")
      -> 8593
      : (char "↓")
      -> 8595
    }#
    
    (set-ptr *cdr (get *start 'b))
    (put *cdr 'lay 'y)
    (mov-cur-abv (get *start 'b))
    (lay-ptr *cdr)

    (set-ptr *car (get *start 'b))
    (put *car 'lay 'y)
    (adj-cur -5.0 3.0)
    (lay-ptr *car)    
    
    # Set input to Cdr
    (def '*fwd-in *cdr)
    
    ######
    # TEMP
    
    # ⇲ -> ⇲
    (def '⇲ (gen2-sym-full '⇲ 'y (cons NIL T) "⇲" *sol-yellow))

    #########
    
    (let L NIL
      (for X '(*car
               *cdr
               ⇲
               *start
               *log
               *bind-ops-p)
        (push 'L (cons X (val X))))
      (balance '*particles (sort L)))
    
    #########

    (when T
    
      (adj-cur-y)
      (mov-cur-x X)
      
      #{
      
        # Test gen2-part
        
        '(("1" "Content"
            ("1.1" "Content"
            ("1.1.1" "Content")
            ("1.1.1" "Content"
              ("1.1.1.1" "Content")
              ("1.1.1.2" "Content")))))
              
        '(((((NIL))))))
        
        '(((1) 2) 3))
        
        '(((1 2) 3 4) 5 6))
        
        '((1 2) (3 4) (5 6)))      
        
        '(1 (2))
        
      }#
       
      (let Part (gen2-part-x '(("*binds" "...")
                               ("*registers" "...")
                               ("*log" "...")
                               ("*start" "...")
                               ("*pointers" "...")))
 
        (adj-cur-y 2.0)
        (mov-cur-x X)
        (adj-cur-x -40.0)
        (lay-part-start Part Part)))))


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

        
(de send-msg-rend (M) (send-msg *ipc (ipc~fd> *ipc "RENDER") M))        


(de serv-rend (Sock)

  #(println 'serv-rend)

  (let Msg (recv-msg *ipc Sock)
  
    (anim-rot-z-cdr)
    (send-msg-rend "T")
  
    (unless Msg
      (disconn-conn *ipc Sock))))


(de dispatch-kb (Data)

  (let (Keycode (+ (get Data 4) 8)
        State   (get Data 5)
        Ks  (get-one-sym> *xkb Keycode)  # or Data 7
        Name    (xkb~keysym-get-name Ks)
        Utf-8   (get-utf8> *xkb Keycode))

    (upd-key> *xkb Keycode State)

    (with *xkb
      (let State-Xkb (assoc Ks (: states))
        (when State-Xkb
          (del State-Xkb (:: states)))
        (push (:: states) (cons Ks State))))

    # Check bindings and dispatch hnds

    (when (or (= State xkb~KEY-DOWN)
              (= State KEY-REPEAT))
      (let (Mods (make-xkb-mods Ks)
            Cmd (cdr (lup *binds Mods)))
        (when Cmd
          (let Fn (car Cmd)
            (Fn Ks (= State KEY-REPEAT))))))))


(de make-xkb-mods (Ks)
  (make
    (with *xkb
      (when (= (is-shift-active> *xkb) 1)
        (when (= (cdr (assoc ksd~Shift-L (: states))) xkb~KEY-DOWN)
          (link ksd~Shift-L))
        (when (= (cdr (assoc ksd~Shift-R (: states))) xkb~KEY-DOWN)
          (link ksd~Shift-R)))
      (when (= (is-ctrl-active> *xkb) 1)
        (when (= (cdr (assoc ksd~Control-L (: states))) xkb~KEY-DOWN)
          (link ksd~Control-L))
        (when (= (cdr (assoc ksd~Control-R (: states))) xkb~KEY-DOWN)
          (link ksd~Control-R)))
      (when (= (is-alt-active> *xkb) 1)
        (when (= (cdr (assoc ksd~Alt-L (: states))) xkb~KEY-DOWN)
          (link ksd~Alt-L))
        (when (= (cdr (assoc ksd~Alt-R (: states))) xkb~KEY-DOWN)
          (link ksd~Alt-R)))
      (when (= (is-logo-active> *xkb) 1)
        (when (= (cdr (assoc ksd~Super-L (: states))) xkb~KEY-DOWN)
          (link ksd~Super-L))
        (when (= (cdr (assoc ksd~Super-R (: states))) xkb~KEY-DOWN)
          (link ksd~Super-R))))
    (link Ks)))       
              

(def 'PI2 (+ PI PI))
(def '*rot-z 0.0)
(de anim-rot-z-cdr ()

  (let? Part (get *fwd-in 'a)
    
    (with Part
    
      (let Vert (car (: verts))
        
        (with Vert
                   
          (inc '*rot-z 0.03)
          (when (> *rot-z PI2) (setq *rot-z 0.0))
          
          # Scale from origin (left bottom)
          # Translate origin to center
          # Rotate rel to center
          # Translate back to origin
          # Translate to final pos          
          
          (let Tra (mat4-tra (: pos))
          
            (=: mm
                (mat4-mul@
                  Tra
                  (: mat4-cen-i)
                  (mat4-rz *rot-z)
                  (: mat4-cen)
                  (mat4-inv Tra)
                  Tra
                  # Skip since all identity
                  #(mat4-rz (caddr (: rota)))
                  #(mat4-ry (cadr (: rota)))
                  #(mat4-rx (car (: rota)))
                  (mat4-sca (: sca))))))
                
        (req-send *ipc Vert)))))
              
              
(de main ()
  (bind-args)
  (init-worker))
