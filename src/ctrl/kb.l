(def '*pen-v (list 0.0 (- (+ *adv-vert *adv-vert)) 0.0))
(def '*cnt-v 2)


(de handle-keyboard (Sock Data)
  
  #{
  https://www.x.org/releases/X11R7.7/doc/xproto/x11protocol.html#Keyboards
  
  A KEYCODE represents a physical (or logical) key. 
  Keycodes lie in the inclusive range [8,255]. 
  }#
  
  # https://gitlab.freedesktop.org/wayland/weston/blob/master/libweston/input.c#L1993  
  
  (let (Keycode (+ (get Data 4) 8)
        State   (get Data 5)
        Keysym  (get-one-sym> *xkb Keycode)
        Name    (xkb~keysym-get-name Keysym)
        Utf-8   (get-utf8> *xkb Keycode))
      
    
    #(prinl "Code/Sym/Name/Utf-8/State: " Keycode " " Keysym " " Name " " Utf-8 " " State)

    # Returns update mask
    (update-key> *xkb Keycode State)

    # Handle repeat keys
    (when (= (key-repeats> *xkb Keycode) 1)
      (update-timer Keysym State))

    (dispatch-handlers Keycode State Keysym)))
    
    
(de handle-timer (Sock)
  # Returns an unsigned 8-byte integer (uint64_t) 
  # containing the number of expirations that have occurred.
  
  # Must call read, not recv
  (with *ipc
    (when (n0 (native "@" "read" 'I Sock (: ipc~buf-rd) 8))
      (let Data (get *timerfd 'timerfd~data)
        (handle-key-rep (car Data) (cdr Data)))
      (prinl (usec) " | Expired timer! Events=" (struct (: ipc~buf-rd) '(N . 1))))))


(de handle-key-rep (Keysym Keycode)
  (dispatch-handlers Keycode 1 Keysym))


(de update-timer (Keysym State)

  # :repeat-delay :initform 400
  # :repeat-interval :initform 40
  # 50 ms = 50,000,000 ns

  (if (= State 1)
        
    # interval, delay/expiration-initial
    (let (Spec (list 0 (* 40 1000000) 0 (* 400 1000000)))
      (println "Arm timer!" Spec)
      (put *timerfd 'timerfd~data (cons Keysym Keycode))
      (when (= (timerfd~set-time> *timerfd Spec) -1)
        (println "errno: " (errno))
        (quit "timerfd~set-time failed")))
  
    (prog
      (when (= Keysym (car (get *timerfd 'timerfd~data)))
        (println "Disarm timer!" Keysym)
        (timerfd~disarm> *timerfd)))))

    
(de dispatch-handlers (Keycode State Keysym)

  (when (= State xkb~KEY-DOWN)

    (cond ((= Keysym ksd~Right) (move-cam 1 '(1.0 0.0)))
          ((= Keysym ksd~Left)  (move-cam 1 '(-1.0 0.0)))
          ((= Keysym ksd~Up)    (move-cam 2 '(0.0 1.0)))
          ((= Keysym ksd~Down)  (move-cam 2 '(0.0 -1.0)))

          ((= Keysym ksd~Prior) (zoom-cam -3.0))
          ((= Keysym ksd~Next)  (zoom-cam 3.0))

          (T                    (handle-ascii Keysym Keycode)))))


(de adv-pen (Cnt)

  (setq *pen-v (list (+ (get *pen-v 1) (*/ *adv-vert Cnt 1.0))
                     (get *pen-v 2)
                     0.0)))

                     
(de nl-pen ()

  (setq *pen-v (list 0.0
                     (- (get *pen-v 2) (+ *adv-vert *adv-vert))
                     0.0)))


(de handle-ascii (Keysym Keycode)

  # Next try to draw a symbol or class

  # To draw any cons cell:
  # Check CAR/CDR
  # - If cons cell, call draw fn or put in list (which will create another particle)
  # - If encoded num or str, add verts to current particle
  
  # Kb handlers will call this function
  # However, the result of eval is drawn
  # ...REDL read, eval, draw/render/rasterize, loop
  # So pressing ASCII equivalent to executing eval

  (when (and (>= Keysym 32) (<= Keysym 255))

    (let (Data (cons)
          Prtcl (new '(+Particle) Data ()))

      #(make-verts "SYM-T")
      #(draw-verts Verts)

      (when NIL
        (print (>> -4 (adr Data)))
        (prinl)
        (prin "  ")
        (print (struct (>> -4 (adr Data)) '((B . 8) (B . 8))))
        (prin " | ")
        (print (struct (>> -4 (adr Data)) '((N . 1) (N . 1))))
        (prinl))
                
      (draw-str (if (= Keysym ksd~space) " " (char Keysym)))

      # Produce verts+edges for rep
      # Push verts into particle
      # Push prtcl into timeline
      # - Timeline is in namespace
        
      T))
      
  (cond ((= Keysym ksd~Return) (nl-pen))
        ((= Keysym ksd~Tab)    (adv-pen 2.0))))


(de draw-str (Str)

  (let (Sock-Model (ipc~get-fd> *ipc "MODEL"))

    (for C (chop Str)

      (let (Off-Vert (get *verts *cnt-v)
            Off      (car Off-Vert)
            Vert     (cdr Off-Vert))

        # Set baseline
        (with Vert
          (=: pos (list (get *pen-v 1) (get *pen-v 2) 0.0)))

        (update-glyph> Vert C)

        (adv-pen 1.0)

        (update-model-matrix> Vert)

        (render-vert *ipc Vert Off)

        (req-send-obj *ipc
                      Sock-Model
                      Vert
                      SZ-VERT
                      Off)

        (inc '*cnt-v 1)))))


(de draw-bytes (Bytes)

  (for Byte Bytes
  
    (draw-num Byte)))


(de draw-num (Num)

  (let (Sock-Model (ipc~get-fd> *ipc "MODEL")
        Str (format Num))

    (for N (chop Str)

      (let (Off-Vert (get *verts *cnt-v)
            Off (car Off-Vert)
            Vert (cdr Off-Vert))

        # Reset vertex
        # Pos = baseline = X,0,0
        (with Vert
          (=: pos (list (get *pen-v 1) (get *pen-v 2) 0.0)))

        (update-glyph> Vert N)

        (adv-pen 1.0)

        (update-model-matrix> Vert)

        (render-vert *ipc Vert Off)

        (req-send-obj *ipc
                      Sock-Model
                      Vert
                      SZ-VERT
                      Off)

        (inc '*cnt-v 1)))

      # Inc byte spacing depending on length
      (when T
        (setq *pen-v (list (+ (get *pen-v 1) (* *adv-vert (- 4 (length Str))))
                           (get *pen-v 2)
                           0.0)))

                         T))


################################################################################


# Handlers
# Do not belong in projview - put in projview in kb subfolder

(de move-cam (Cnt Move)

   (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
         Sock-Render (ipc~get-fd> *ipc "RENDER"))

     (with *projview

      (=: pos (list (+ (get (: pos) 1) (get Move 1))
                    (+ (get (: pos) 2) (get Move 2))
                    10.0))

      (update-mat-view> *projview)

      # Either send relevant data or send entire data
      # -> Minimize render processing time

      # Pass list of socks so need only serialize once

      (req-send-obj *ipc
                    Sock-Render
                    *projview
                    SZ-PV
                    0)

      # Push to model also
      (req-send-obj *ipc
                    Sock-Model
                    *projview
                    SZ-PV
                    0))))


(de zoom-cam (Delta)

   (let (Sock-Model  (ipc~get-fd> *ipc "MODEL")
         Sock-Render (ipc~get-fd> *ipc "RENDER"))

     (with *projview

      (inc (:: scale-ortho) Delta)

      (update-mat-proj> *projview)

      (req-send-obj *ipc
                    Sock-Render
                    *projview
                    SZ-PV
                    0)

      # Push to model also
      (req-send-obj *ipc
                    Sock-Model
                    *projview
                    SZ-PV
                    0))))