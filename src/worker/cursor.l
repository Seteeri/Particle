(de mov-cursor (Vert)
  (let ((X Y) (get-origin> (get *metrics (get Vert 'chr))
                           (get Vert 'pos)
                           *sca-vert))
    (set-cur-x X)
    (set-ptr-y Y)))


(de adv-cursor (Cnt)

  (unless Cnt (setq Cnt 1.0))

  (setq *pos-draw (place 1
                        *pos-draw
                        (+ (car *pos-draw)
                           (*/ *adv-vert Cnt 1.0)))))


(de nl-cursor (Cnt)

  (unless Cnt (setq Cnt 1.0))

  (setq *pos-draw (place 2
                        *pos-draw
                        (- (cadr *pos-draw)
                           (*/ (+ *adv-vert *adv-vert) Cnt 1.0)))))


(de set-cur-x (Pos)
  (setq *pos-draw (place 1 *pos-draw Pos)))


(de set-ptr-y (Pos)
  (setq *pos-draw (place 2 *pos-draw Pos)))


(de set-cur-pos (Pos)
  (setq *pos-draw Pos))
