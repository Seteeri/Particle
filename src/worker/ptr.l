(class +Pointer)
(dm T (Part)
  # Particle
  (=: p))

# Really for cursor, not ptr

(de mov-ptr (Vert)
  (let ((X Y) (get-origin> (get *metrics (get Vert 'chr))
                           (get Vert 'pos)
                           *sca-vert))
    (set-ptr-x X)
    (set-ptr-y Y)))


(de adv-ptr (Cnt)

  (unless Cnt (setq Cnt 1.0))

  (setq *pos-draw (place 1
                        *pos-draw
                        (+ (car *pos-draw)
                           (*/ *adv-vert Cnt 1.0)))))


(de nl-ptr (Cnt)

  (unless Cnt (setq Cnt 1.0))

  (setq *pos-draw (place 2
                        *pos-draw
                        (- (cadr *pos-draw)
                           (*/ (+ *adv-vert *adv-vert) Cnt 1.0)))))


(de set-ptr-x (Pos)
  (setq *pos-draw (place 1 *pos-draw Pos)))


(de set-ptr-y (Pos)
  (setq *pos-draw (place 2 *pos-draw Pos)))


(de set-ptr-pos (Pos)
  (setq *pos-draw Pos))
