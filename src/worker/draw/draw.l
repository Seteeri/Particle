(de draw-chars (Chars Col Adv)
  # Similar to str without quotes

  (let Verts ()

    (for C (chop Chars)
      (push 'Verts (draw-glyph C Col))
      (when Adv (adv-ptr)))

    (flip Verts)))


(de draw-glyph (Char Col)

  (let Vert (pop '*verts)

    # Reset vertex
    # Pos = baseline = X,0,0
    (with Vert
      (=: pos (get *vert-ptr 'pos))
      (=: rgba (list (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0)))

    (update-glyph> Vert Char)

    (update-model-matrix> Vert)

    (cmd-update-vert *ipc Vert)

    (inc '*cnt-v 1)

    Vert))
