(class +Pointer)
(dm T (Part)
  # Particle
  (=: p))

#{
(dm mov> (Vert Up))
(dm adv> (Cnt))
(dm nl> (Cnt))
}#


# These are not really specific to pointer as they can apply to any Particle


(de mov-ptr (Vert Up)
  (let Ori (get-origin> (get *metrics (get Vert 'chr))
                        (get Vert 'pos)
                        *sca-vert)
    (set-ptr-x (get Ori 1))
    (set-ptr-y (get Ori 2))
    (update-model-matrix> *vert-ptr)
    (when Up
      (cmd-upd-vert *ipc *vert-ptr))))


(de adv-ptr (Cnt)

  (unless Cnt (setq Cnt 1.0))

  (with *vert-ptr
    (=: pos (place 1
                  (: pos)
                  (+ (car (: pos))
                     (*/ *adv-vert Cnt 1.0)))))
      
  (upd-ptr))


(de nl-ptr (Cnt)

  (unless Cnt (setq Cnt 1.0))

  (with *vert-ptr
    (let Pos (list (get (: pos) 1)
                   (- (get (: pos) 2) (*/ (+ *adv-vert *adv-vert) Cnt 1.0))
                   (get (: pos) 3))
      (=: pos Pos)))
      
  (upd-ptr))


(de set-ptr-x (Pos)
  (with *vert-ptr 
    (=: pos (place 1 (: pos) Pos))))


(de set-ptr-y (Pos)
  (with *vert-ptr 
    (=: pos (place 2 (: pos) Pos))))


(de set-ptr-pos (Pos)
  (put '*vert-ptr 'pos Pos))


(de upd-ptr ()
  (update-model-matrix> *vert-ptr)
  (cmd-upd-vert *ipc *vert-ptr))
