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


(load (pack (pwd) "/particle/misc.l"))
(mapc 'ld-cwd
      (list "/particle/glm.l"
            "/particle/c/epoll.l"
            "/particle/c/li.l"
            "/particle/c/socket.l"
            "/particle/c/ksd.l"
            "/particle/c/xkbcommon.l"
            "/particle/misc.l"
            "/particle/kvmap.l"
            "/particle/theme/selenized.l"
            "/particle/ipc/ipc.l"
            "/particle/prot/conn.l"
            "/particle/prot/send.l"
            "/particle/prot/recv.l"
            "/particle/prot/obj.l"
            "/particle/font/metrics.l"
            "/particle/font/font.l"
            "/particle/ctrl/xkb.l"
            
            
            "/particle/worker/debug.l"            
            "/particle/worker/conn.l"
            "/particle/worker/cursor.l"   
            "/particle/worker/projview.l"
            
            "/particle/worker/binds/an.l"                   
            "/particle/worker/binds/ops.l"                               
            
            "/particle/worker/vertex/make.l"
            "/particle/worker/vertex/vertex.l"
            
            "/particle/worker/cmd/pair.l"
            "/particle/worker/cmd/line.l"
            "/particle/worker/cmd/eval.l"
            "/particle/worker/cmd/str.l"
            "/particle/worker/cmd/cam.l"
            "/particle/worker/cmd/ptr.l"                    
            
            "/particle/worker/point/point.l"
            "/particle/worker/point/pair.l"
            "/particle/worker/point/atom.l"
            "/particle/worker/point/line.l"
            "/particle/worker/point/sym.l"
            "/particle/worker/point/num.l"
            "/particle/worker/point/nil.l"
            "/particle/worker/point/gen.l"
            
            "/particle/worker/ops/ptr.l"                        
            "/particle/worker/ops/align.l"            
            "/particle/worker/ops/lay.l"
            "/particle/worker/ops/del.l"
            "/particle/worker/ops/line.l"
            "/particle/worker/ops/pair-car.l"
            "/particle/worker/ops/pair-cdr.l"            
            "/particle/worker/ops/pair-sym.l"))


(def 'NAME-PID "WORKER")

(def 'KEY-REPEAT 2)
(def '*cdr-space-x 2.0)
(def '*sublist-space-y 1.0)

# Global Symbols
(def '*xkb)
(def '*vert-offs)
(def '*cnt-v 1)
(def '*font) # Make this visible also?

(def '*points)
(def '*cnt-p 0)
(def '*binds)
(def '*start)  # Current list
(def '*log)    # List of history for undo/redo
(def '*ptrs)   # List of pointers
# ...

(def '*line)                   # Cur line
(def '*cur (list 0.0 0.0 0.0)) # 3D cur
(def '*ptr)                 # Cur ptr


(def 'PATH-H64S "/home/user/Particle/particle/ext/libh64s.so")


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
              (list-binds-ops *binds-ops)))
    (balance '*binds L))
    
  ##################################
  
  (prinl-info "init-worker" (pack "Heap: " (heap)))
  (gc 48)
  (setq *vert-offs
        (make
          (for I *verts-max 
            (link (vert-off-gl I)))))
  (prinl-info "init-worker" (pack "Heap: " (heap)))
  
  (gen-def-pts)
  
  (send-msg *ipc (ipc~fd> *ipc "CONTROL") "(rdy)")
  
  (prinl-info "init-worker" "Starting epoll...")
        
  (loop (ipc~poll> *ipc -1)))


(de gen-def-pts ()
  
  #{
    
    Proccess number (Worker)
    - Namespace list
      - Particles list
        - *...
    
    Create one for the other procs?
    - They would all have to draw also
    - Then share res between procs in database
    
  }#
  
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

  (let ((X Y) *cur)
    
    (let (Po-Start (gen-sym-full '*start 'x T T)
          Po-Cdr   (gen-sym-full '*cdr 'y T T "↓" *selb-yellow)
          #Po-Car  (gen-sym-full '*car 'y T NIL "↑")
          #Po-D    (gen-sym-full '⇲    'y T NIL "⇲" *selb-yellow)
          )
    
    (mov-cur 0.0 0.0)
    (lay-pt-start-2 '*start '*start T T)
        
    (top> Po-Start)
    (lay-pt-start-2 '*cdr '*cdr)
        
    (setq *cdr '*start)    
    
    (def '*ptr '*cdr)
        
    T)))


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

  (let? Po (get *ptr 'a)
    
    (with Po
    
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
