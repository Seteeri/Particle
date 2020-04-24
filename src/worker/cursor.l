(de print-cur ()
  (println (format (get *cur 1) *Scl) (format (get *cur 2) *Scl)))


(de mov-cur-abv (Part)
  # Add start/end?
  (setq *cur (get Part 'ori))
  (nl-cur -1.0))
  
  
(de mov-cur-ori (Part)
  (setq *cur (get Part 'ori)))
  

(de mov-cur-part (Part Pos)
  (cond ((= Pos 'start) (mov-cur-vert (first-vert> Part)))
        ((= Pos 'end)   (mov-cur-vert (last-vert> Part)))))


(de mov-cur-vert (Vert)
  (with Vert
    (setq *cur (ori> (: met)
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
