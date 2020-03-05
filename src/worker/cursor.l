(de mov-cursor (Vert)
  (let ((X Y) (get-origin> (get *metrics (get Vert 'chr))
                           (get Vert 'pos)
                           *sca-vert))
    (set-cur-x X)
    (set-cur-y Y)))


(de adv-cursor (Cnt)

  (unless Cnt (setq Cnt 1.0))

  (setq *pos-cursor (place 1
                        *pos-cursor
                        (+ (car *pos-cursor)
                           (*/ *adv-vert Cnt 1.0)))))


(de nl-cursor (Cnt)

  (unless Cnt (setq Cnt 1.0))

  (setq *pos-cursor (place 2
                        *pos-cursor
                        (- (cadr *pos-cursor)
                           (*/ (+ *adv-vert *adv-vert) Cnt 1.0)))))


(de set-cur-x (Pos)
  (setq *pos-cursor (place 1 *pos-cursor Pos)))


(de set-cur-y (Pos)
  (setq *pos-cursor (place 2 *pos-cursor Pos)))


(de set-cur-pos (Pos)
  (setq *pos-cursor Pos))
