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
            "/particle/worker/idx.l"
                        
            "/particle/worker/pointer/pointer.l"
            "/particle/worker/pointer/p.l"
            "/particle/worker/pointer/ln.l"
            "/particle/worker/pointer/pt.l"
            "/particle/worker/pointer/ref.l"
                        
            "/particle/worker/mode/txt/an.l"
            "/particle/worker/mode/txt/op.l"

            "/particle/worker/mode/cmd/op.l"
            
            "/particle/worker/vertex/make.l"
            "/particle/worker/vertex/vertex.l"
            
            "/particle/worker/cmd/atom.l"
            "/particle/worker/cmd/ptr.l"                                
            "/particle/worker/cmd/pair.l"
            "/particle/worker/cmd/repl.l"            
            "/particle/worker/cmd/line.l"
            "/particle/worker/cmd/conv.l"
            "/particle/worker/cmd/cam.l"
            
            "/particle/worker/point/gen.l"
            "/particle/worker/point/point.l"
            "/particle/worker/point/line.l"            
            "/particle/worker/point/pair.l"
            "/particle/worker/point/atom.l"
            "/particle/worker/point/num.l"
            "/particle/worker/point/nil.l"
            "/particle/worker/point/sym/sym.l"
            "/particle/worker/point/sym/box.l"
            "/particle/worker/point/sym/int.l"
            "/particle/worker/point/sym/str.l"
            "/particle/worker/point/sym/ext.l"
                                
            "/particle/worker/ops/align.l"            
            "/particle/worker/ops/lay.l"
            "/particle/worker/ops/bsp.l"
            "/particle/worker/ops/del.l"
            "/particle/worker/ops/str.l"
            "/particle/worker/ops/line.l"
            "/particle/worker/ops/alt-lay.l"       
            "/particle/worker/ops/alt-reg.l"
            "/particle/worker/ops/conv.l"
            
            "/particle/worker/ops/pair/pair.l"
            "/particle/worker/ops/pair/atom.l"
            
            # MOV -> TEST.L
            
            "/particle/test/atom/lst.l"
            "/particle/test/atom/str.l"
            "/particle/test/atom/alt.l"
            "/particle/test/atom/del.l"
            "/particle/test/atom/bsp.l"
            #"/particle/test/atom/nl.l"
            
            "/particle/test/pair/lst.l"
            "/particle/test/pair/str.l"
            "/particle/test/pair/alt.l"
            "/particle/test/pair/del.l"
            "/particle/test/pair/bsp.l"
            #"/particle/test/pair/nl.l"
            
            "/particle/test/test.l"))


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
(def '*modes)
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
  
  ############################################
    
  (setq *projview (req-recv-pv *ipc
                               (ipc~fd> *ipc "CONTROL")
                               NIL
                               0))
                               
  (req-send *ipc *projview)
  
  (setq *xkb (new '(+Xkb)))  
  
  (prinl-info "init-worker" (pack "Heap: " (heap)))
  (gc 48)
  (setq *vert-offs
        (make
          (for I *verts-max 
            (link (vert-off-gl I)))))
  (prinl-info "init-worker" (pack "Heap: " (heap)))  
  
  #########################
  
  (ld-modes)
    
  (def-pts)
  
  (send-msg *ipc (ipc~fd> *ipc "CONTROL") "(rdy)")
  
  (prinl-info "init-worker" "Starting epoll...")
        
  (loop (ipc~poll> *ipc -1)))

  
(de ld-modes ()  
  
  # dispatch-kb will lup key in binds
  # so to set mode, change value of binds
  
  # TODO:
  # - Rename *binds -> *modes  
  # - Make binds a list which contains all the modes
  # - Dispatch-kb will use car of *binds
  #   - If not found, then try static binds

  # Mov loader functions to modes
  
  # Load txt
  (let L (sort 
           (append
              (list-binds-txt-ascii *binds-txt-latin-1)
              (list-binds-txt-num *binds-txt-num)
              (list-binds-txt-ops *binds-txt-ops)))
    (balance '*mode-txt L))
    
  # Load cmd
  (balance '*mode-cmd 
    (sort (list-binds-cmd *binds-cmd))))
  

(de def-pts ()
  
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
    
    : (char (hex "2607"))
    
  }#  

  (let ((X Y) *cur)
    
    (setq *ptr (new '(+Pointer)))
    (put *ptr 'ref NIL)
        
    (let (Pt-Sta (gen-sym-exp '*start 'x T T)
          Pt-Ptr (gen-ptr     '*ptr "↓" *selb-yellow)
          Pt-Txt (gen-ptr     '*txt "↓" *selb-yellow)
          Pt-Cmd (gen-ptr     '*cmd "↓" *selb-yellow)
          Pt-D   (gen-point   '⇲      'x))
      
      (setq *po-ptr-txt Pt-Txt
            *po-ptr-cmd Pt-Cmd)
      
      (put (get Pt-Sta 'b) 'line (new '(+Line)))
      (mov-cur 0.0 0.0)
      (lay-pt-start '*start '*start T T)

      (del> (get Pt-Ptr 'b)) (put Pt-Ptr 'b T)
      (del> (get Pt-Txt 'b)) (put Pt-Txt 'b T)
      (del> (get Pt-Cmd 'b)) (put Pt-Cmd 'b T)
      
      (setq *modes *mode-cmd)
      (put *ptr 'pt Pt-Cmd)
      
      # Upd +Point on change
      (set-ln-n> *ptr Pt-Sta)
      (pt> *ptr '*start '*start T)
      
      (mov-cur 0.0 1.0)     
      # Call this through *ptr method      
      (top> Pt-Sta)
      (draw> *ptr)
    
      (mov-cur 0.0 0.0)
      (adj-cur-y 3.0)
      (col-verts> Pt-D *selb-yellow 1.0)
      (lay-pt-start '⇲ '⇲ T T)
      
      (when NIL
        (run-tests)
        #(quit "Finished tests")
        )
                
      T)))


(de del-start-val (Pt-Sta Draw)
    
  ~(assert (not (get Pt-Sta 'line 'n)))  
    
  (del> (if (get Pt-Sta 'b)
            @ 
            (lup-pt *start))
        T T)
  (con> Pt-Sta NIL (gen-point NIL 'x))

  (put (get Pt-Sta 'b) 'line (new '(+Line)))
  
  (pt> *ptr '*start 'b NIL)
  (put *ptr 'lns (list Pt-Sta))
    
  (when Draw
    (setq *cur (list 0.0 0.0 0.0))
    (lay-pt-start '*start '*start T T)
    (top> Pt-Sta)
    (draw> *ptr)))


# Unused
(de serv-cli (Sock))


(de serv-ctrl (Sock)

  (let (Msg (recv-msg *ipc Sock)
        Dat (car (str Msg))
        Name (car Dat))
        
    # recv-msg will return data or NIL
    # This means sending NIL will disconnect the client, like EOF
    (if Msg
        
        (prog
      
          (case Name
          
            (KEYBOARD-KEY
              (dispatch-kb Dat))

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
              (prinl-info "serv-ctrl" (glue " " Dat)))

            ("update-vert"
              (req-send *ipc *vert-ptr))

            ("calc-pv"
              # method?
              (with *projview
                (=: width  (get Dat 2))
                (=: height (get Dat 3)))
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


(de dispatch-kb (Dat)

  (let (Keycode (+ (get Dat 4) 8)
        State   (get Dat 5)
        Ks  (get-one-sym> *xkb Keycode)  # or Dat 7
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
            Cmd (cdr (lup *modes Mods)))
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

  (let? Po (get *ptr 'pt 'a)
    
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
