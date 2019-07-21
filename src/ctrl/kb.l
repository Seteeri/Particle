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
      
    
    (prinl "Code/Sym/Name/Utf-8/State: " Keycode " " Keysym " " Name " " Utf-8 " " State)

    # Returns update mask
    (update-key> *xkb Keycode State)

    # Handle repeat keys
    # (not (xkb~key-repeats> *xkb Keycode))

    # Normally, dispatched handler would be called

    (when (= State xkb~KEY-DOWN)

      (cond ((= Keysym ksd~Right) (move-cam Sock 1 '(1.0 0.0)))
            ((= Keysym ksd~Left)  (move-cam Sock 1 '(-1.0 0.0)))
            ((= Keysym ksd~Up)    (move-cam Sock 2 '(0.0 1.0)))
            ((= Keysym ksd~Down)  (move-cam Sock 2 '(0.0 -1.0)))

            ((= Keysym ksd~Prior) (zoom-cam Sock -3.0))
            ((= Keysym ksd~Next)  (zoom-cam Sock 3.0))

            (T                    (handle-ascii Keysym))))))


(de adv-pen ()

  (setq *pen-v (list (+ (get *pen-v 1) *adv-vert)
                     (get *pen-v 2)
                     0.0)))

                     
(de nl-pen ()

  (setq *pen-v (list 0.0
                     (- (get *pen-v 2) (+ *adv-vert *adv-vert))
                     0.0)))


(de handle-ascii (Keysym)

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
                
      (draw-str (if (= Keysym ksd~space) " " (xkb~keysym-get-name Keysym)))

      # Produce verts+edges for rep
      # Push verts into particle
      # Push prtcl into timeline
      # - Timeline is in namespace

      # Selecting any vertex will return the particle data
      # Quoting the vertex will return the vertex itself...
      # MAKE PARTICLE CONS CELL WHERE CAR=VERTS CDR=DATA

      #(nl-pen)
      
      )))


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

        (adv-pen)

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

        (adv-pen)

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

(de move-cam (Sock Cnt Move)

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


(de zoom-cam (Sock Delta)

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