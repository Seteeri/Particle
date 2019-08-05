(de read-string ()
  (let (Verts  ()
        Data  ())
    # Pop particles off timeline - ignore ptr (ptr not in timeline)
    (for Prt *timeline
      (push 'Data (get Prt 'data))
      (for Vert (get Prt 'verts)
        (push 'Verts Vert)))

    (list Verts Data)))


(de pop-string ()
  (let (Verts  ()
        Data  ())
    # Pop particles off timeline - ignore ptr (ptr not in timeline)
    (let Prt NIL
      (while (setq Prt (pop '*timeline))
        (push 'Data (get Prt 'data))
        (for Vert (get Prt 'verts)
          (push 'Verts Vert))))

    (list Verts Data)))


(de draw-str (Str)

  (let Verts ()

    (for C (chop Str)

      (let Vert (pop '*verts)

        # Set baseline
        (with Vert
          (=: pos (list (get *pen-v 1) (get *pen-v 2) 0.0)))

        (update-glyph> Vert C)

        (update-model-matrix> Vert)

        (cmd-update-vert *ipc Vert)

        (adv-pen 1.0)

        (inc '*cnt-v 1)

        (push 'Verts Vert)))

      (flip Verts)))


(de draw-bytes (Bytes)

  (for Byte Bytes

    (draw-num Byte)))


# Refactor this into draw-char
(de draw-num (Num)

  (let Str (format Num)

    (for N (chop Str)

      (let Vert (pop '*verts)

        # Reset vertex
        # Pos = baseline = X,0,0
        (with Vert
          (=: pos (list (get *pen-v 1) (get *pen-v 2) 0.0)))

        (update-glyph> Vert N)

        (adv-pen 1.0)

        (update-model-matrix> Vert)

        (cmd-update-vert *ipc Vert)

        (inc '*cnt-v 1)))

      # Inc byte spacing depending on length
      (when T
        (setq *pen-v (list (+ (get *pen-v 1) (* *adv-vert (- 4 (length Str))))
                           (get *pen-v 2)
                           0.0)))

                         T))
