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


(de cmd-char (Keysym)

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


##################

# Are these commands also?

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
