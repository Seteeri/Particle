# Batch these

(de draw-str (Str Col Adv)

  (let Verts ()

    (for C (chop (pack "\"" Str "\""))
      (push 'Verts (draw-glyph C Col))
      (when Adv (adv-ptr)))

    (flip Verts)))


(de draw-num (Num Col Adv)

  (let Verts ()

    (for C (chop (format Num))
      (push 'Verts (draw-glyph C Col))
      (when Adv (adv-ptr)))

    # Inc byte spacing depending on length

    (flip Verts)))


(de draw-sym (Sym Col Adv)

  (let Verts ()

    (for C (chop (sym Sym))
      (push 'Verts (draw-glyph C Col))
      (when Adv (adv-ptr)))

    # Inc byte spacing depending on length

    (flip Verts)))
