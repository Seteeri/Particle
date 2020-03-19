(de mov-cur-vert (Vert)
  (with Vert
    (set-cur-pos (get-origin> (get *metrics (: chr))
                              (: pos)
                              *sca-vert))))


(de adv-cur (Cnt)

  (unless Cnt (setq Cnt 1.0))

  (setq *pos-cursor (place 1
                        *pos-cursor
                        (+ (car *pos-cursor)
                           (*/ *adv-vert Cnt 1.0)))))


(de nl-cur (Cnt)

  (unless Cnt (setq Cnt 1.0))

  (setq *pos-cursor (place 2
                        *pos-cursor
                        (- (cadr *pos-cursor)
                           (*/ (+ *adv-vert *adv-vert) Cnt 1.0)))))


(de set-cur-x (X)
  (setq *pos-cursor (place 1 *pos-cursor X)))


(de set-cur-y (Y)
  (setq *pos-cursor (place 2 *pos-cursor Y)))


(de set-cur-pos (Pos)
  (setq *pos-cursor Pos))
