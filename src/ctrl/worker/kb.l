(def '*pen-v (list 0.0 (- (+ *adv-vert *adv-vert)) 0.0))
(def '*cnt-v 2)

   
(de handle-kb (Data)

  # Worker should not have xkb dependency
  # Need to get keysym and keycode

  (let (Keycode (get Data 4)
        State   (get Data 5)
        Keysym  (get Data 7))

    # 2 = repeat
    (when (or (= State xkb~KEY-DOWN) (= State 2))

      (cond ((= Keysym ksd~Right) (move-cam 1 '(1.0 0.0)))
            ((= Keysym ksd~Left)  (move-cam 1 '(-1.0 0.0)))
            ((= Keysym ksd~Up)    (move-cam 2 '(0.0 1.0)))
            ((= Keysym ksd~Down)  (move-cam 2 '(0.0 -1.0)))

            ((= Keysym ksd~Prior) (zoom-cam -0.5))
            ((= Keysym ksd~Next)  (zoom-cam 0.5))

            (T                    (handle-ascii Keysym))))))


(de adv-pen (Cnt)

  (setq *pen-v (list (+ (get *pen-v 1) (*/ *adv-vert Cnt 1.0))
                     (get *pen-v 2)
                     0.0)))

                     
(de nl-pen ()

  (setq *pen-v (list 0.0
                     (- (get *pen-v 2) (+ *adv-vert *adv-vert))
                     0.0)))


(de handle-ascii (Keysym)

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

  (let Sock-Model (ipc~get-fd> *ipc "MODEL")

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