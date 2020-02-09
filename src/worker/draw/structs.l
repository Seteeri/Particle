#{
  TODO: 
  * Batch updates
}#


(de draw-repr (Repr Col Adv)

  (let Verts ()

    (for C Repr
      (push 'Verts (draw-glyph C Col))
      (when Adv (adv-cursor)))

    (flip Verts)))
  
(de draw-str (Str Col Adv) (draw-repr (chop (pack "\"" Str "\"")) Col Adv))
(de draw-num (Num Col Adv) (draw-repr (chop (format Num)) Col Adv))
(de draw-sym (Sym Col Adv) (draw-repr (chop (sym Sym)) Col Adv))

(de draw-atom (Atom Col Adv) 
  (let Col-2 (if Col Col (get-color-type Atom))
    (cond ((num? Atom) (draw-num Atom Col-2 Adv))
          ((str? Atom) (draw-str Atom Col-2 Adv))
          (T           (draw-sym Atom Col-2 Adv)))))
