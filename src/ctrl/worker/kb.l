(def '*pen-v (list 0.0 (- (+ *adv-vert *adv-vert)) 0.0))
(def '*cnt-v 2)

(def 'KEY-REPEAT 2)

(de handle-kb (Data)

  (let (Keycode (+ (get Data 4) 8)
        State   (get Data 5)
        Keysym  (get-one-sym> *xkb Keycode)  # or Data 7
        Name    (xkb~keysym-get-name Keysym)
        Utf-8   (get-utf8> *xkb Keycode))

    (update-key> *xkb Keycode State)

    # Dispatch bindings

    (print-info "handle-kb" (pack (str Data) " " Keysym))

    (when (or (= State xkb~KEY-DOWN)
              (= State KEY-REPEAT))

      (cond ((= Keysym ksd~End)       (cmd-pack))
            ((= Keysym ksd~Home)      (cmd-eval))
            ((= Keysym ksd~BackSpace) (cmd-del))
            ((= Keysym ksd~Delete)    (cmd-del))

            # HJKL

            ((= Keysym ksd~Right) (cmd-move-cam 1 '(1.0   0.0)))
            ((= Keysym ksd~Left)  (cmd-move-cam 1 '(-1.0  0.0)))
            ((= Keysym ksd~Up)    (cmd-move-cam 2 '(0.0   1.0)))
            ((= Keysym ksd~Down)  (cmd-move-cam 2 '(0.0  -1.0)))

            ((= Keysym ksd~Prior) (cmd-zoom-cam -0.5))
            ((= Keysym ksd~Next)  (cmd-zoom-cam 0.5))

            (T                    (handle-ascii Keysym))))))


(de cmd-pack ()

  (print-info "cmd-pack")

  # Build list of chars
  # Add double-quotes

  # Also set marker before and after

  (let (           #Quote-Start (pop '*verts)
        Verts  ()  #(list Quote-Start)
        Chars  ()) #(list "\""))

    # Pop particles off timeline - ignore ptr (ptr not in timeline)
    (let Prt NIL
      (while (setq Prt (pop '*timeline))
        (push 'Chars (get Prt 'data))
        # Assumes Verts is list of 1 item
        (for Vert (get Prt 'verts)
          (push 'Verts Vert))))

    # Reposition
    # 2 possible methods:
    # 1. Set position for each
    # 2. Set chr for each
    #(for Vert Verts)

    # Create particle
    (let (Data (pack Chars)
          Prt (new '(+Particle) Data Verts))

      (push '*timeline Prt)

      (println Data))))


(de cmd-del (Dir)

  (print-info "cmd-del")

  # Currently, render will attempt to render all verts (*max-verts)
  # -> Send cmd to render to adjust max
  # - instanceCount is number of instances from beginning of array
  # *verts is LIFO, thus will always keep instances pack from beginning of array
  # -> Maintain vert cnt, instanceCount = size - used
  #
  # However, if del from arbitrary point then must zero it and instanceCount
  # remains the same
  # After del, vert returns to queue, and next vert will reuse it
  #
  # Makes most sense to simply zero vert, and not worry about instanceCount as
  # frag will be discarded

  # Process:
  # 1. Pop Prt from timeline/ptr
  # 2. For Verts: send zero-vert cmd to render
  # 3. For Verts: push Verts

  (let (Sock-Render (ipc~get-fd> *ipc "RENDER")
        Prt         (pop '*timeline)
        Verts       (get Prt 'verts))

    (println (get Prt 'data) (length Verts))

    (for Vert Verts
      (let Msg (pack "(zv " (get Vert 'off-gl) '")")
        (send-msg *ipc Sock-Render Msg))
      (push '*verts Vert)
      # For now, push pen back
      (adv-pen -1.0))))


(de cmd-eval ()

  # Build string from ptr-prev to ptr

  (println "CMD-EVAL"))


################################################################################


(de adv-pen (Cnt)
  (let Pos (list (+ (get *pen-v 1) (*/ *adv-vert Cnt 1.0))
                (get *pen-v 2)
                0.0)
    (setq *pen-v Pos)
    (print-info "adv-pen" (pack (format (get Pos 1) *Scl) " " (format (get Pos 2) *Scl)))))


(de nl-pen ()

  (setq *pen-v (list 0.0
                     (- (get *pen-v 2) (+ *adv-vert *adv-vert))
                     0.0)))


################################################################################


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

  (when (and (>= Keysym 32)
             (<= Keysym 255))

    # For strings either use quotations or put additional spacing between symbols
    # Problem is spaces are invisible
    # (draw-str (pack "\"" Data "\""))

    (let (Data  (if (= Keysym ksd~space) " " (char Keysym))
          Verts (draw-str Data)
          Prt   (new '(+Particle) Data Verts))

      (push '*timeline Prt)

      (when NIL
        (print (>> -4 (adr Data)))
        (prinl)
        (prin "  ")
        (print (struct (>> -4 (adr Data)) '((B . 8) (B . 8))))
        (prin " | ")
        (print (struct (>> -4 (adr Data)) '((N . 1) (N . 1))))
        (prinl))
        
      T))

  (cond ((= Keysym ksd~Return) (nl-pen))
        ((= Keysym ksd~Tab)    (adv-pen 2.0))))


(de draw-str (Str)

  (let (Sock-Model (ipc~get-fd> *ipc "MODEL")
        Verts ())

    (for C (chop Str)

      (let (Vert     (pop '*verts)
            Off      (get Vert 'off-gl))

        # Set baseline
        (with Vert
          (=: pos (list (get *pen-v 1) (get *pen-v 2) 0.0)))

        (update-glyph> Vert C)

        (update-model-matrix> Vert)

        (render-vert *ipc Vert Off)

        (req-send-obj *ipc
                      Sock-Model
                      Vert
                      SZ-VERT
                      Off)

        (adv-pen 1.0)

        (inc '*cnt-v 1)

        (push 'Verts Vert)))

      (flip Verts)))


(de draw-bytes (Bytes)

  (for Byte Bytes

    (draw-num Byte)))


# Refactor this into draw-char
(de draw-num (Num)

  (let (Sock-Model (ipc~get-fd> *ipc "MODEL")
        Str (format Num))

    (for N (chop Str)

      (let (Vert     (pop '*verts)
            Off      (get Vert 'off-gl))

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

(de cmd-move-cam (Cnt Move)

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


(de cmd-zoom-cam (Delta)

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