(de print-cur ()
  (println (format (get *cur 1) *Scl) (format (get *cur 2) *Scl)))

(de mov-cur-part (Part Pos)
  (cond ((= Pos 'start) (mov-cur-vert (first-vert> Part)))
        ((= Pos 'end)   (mov-cur-vert (last-vert> Part)))))


(de mov-cur-vert (Vert)
  (with Vert
    (setq *cur (get-origin> (get *metrics (: chr))
                            (: pos)
                            *sca-vert))))


(de adv-cur (Cnt)

  (unless Cnt (setq Cnt 1.0))

  (setq *cur (place 1
                    *cur
                    (+ (car *cur)
                       (*/ *adv-vert Cnt 1.0)))))


(de nl-cur (Cnt)

  (unless Cnt (setq Cnt 1.0))

  (setq *cur (place 2
                    *cur
                    (- (cadr *cur)
                       (*/ (+ *adv-vert *adv-vert) Cnt 1.0)))))


(de set-cur-x (X)
  (setq *cur (place 1 *cur X)))


(de set-cur-y (Y)
  (setq *cur (place 2 *cur Y)))


(de align-cur-to-bnds (Ref) # Rename to align
  # x: CDR is to the right; use X bounds
  # y: CDR is below; use Y bounds
  (let Bnds (bounds> Ref NIL T) # Skip CDR
    (if (= (get Ref 'lay) 'x)
        (prog
          # Move cur to x base of Ref and y origin of Ref
          (set-cur-x (get Bnds 1))
          (set-cur-y (get (get-origin> Ref) 2))
          (adv-cur 2.0))
        (prog
          # Move cur to x origin of Ref and y base of Ref
          (set-cur-x (get (get-origin> Ref) 1))
          (set-cur-y (get Bnds 2))
          (nl-cur 2.0)))))
