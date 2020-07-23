(class +Pointer)

(dm set-ln-p> (Pt-Ref Pt-Tgt)    

  # If new has line, set it
  # else if old has line but new doesn't = mov prv line

  (if (get Pt-Tgt 'line)
      (unless (= (c-lns> This) Pt-Tgt)
        (lns> This))
      (when (get Pt-Ref 'line)
        (lns> This))))


(dm set-ln-n> (Pt)
  # Tgt has line
  (when (and (get Pt 'line)
             (not (= (c-lns> This) Pt)))
    (lns> This Pt)))
