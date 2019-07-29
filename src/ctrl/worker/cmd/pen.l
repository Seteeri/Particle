# Are these commands also?

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
