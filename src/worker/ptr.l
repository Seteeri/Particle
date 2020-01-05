# Contains a Particle
(class +Pointer)


(de mov-ptr (Vert Up)
  (let Ori (get-origin> (get *metrics (get Vert 'chr))
                        (get Vert 'pos)
                        *sca-vert)
    (set-x-ptr-2 (get Ori 1))
    (set-y-ptr-2 (get Ori 2))
    (update-model-matrix> *vert-ptr)
    (when Up
      (cmd-update-vert *ipc *vert-ptr))))


(de adv-ptr (Cnt)

  (unless Cnt (setq Cnt 1.0))

  #(prinl-info "adv-ptr" (format Cnt *Scl))

  (with *vert-ptr

    #(prinl-info "adv-ptr" (pack "Old: " (format (get (: pos) 1) *Scl) " " (format (get (: pos) 2) *Scl)))

    (let Pos (list (+ (get (: pos) 1) (*/ *adv-vert Cnt 1.0))
                   (get (: pos) 2)
                   (get (: pos) 3))
      (=: pos Pos))

    #(prinl-info "adv-ptr" (pack "New: " (format (get (: pos) 1) *Scl) " " (format (get (: pos) 2) *Scl)))
    )
      
  (update-model-matrix> *vert-ptr)
  (cmd-update-vert *ipc *vert-ptr))


(de nl-ptr (Cnt)

  (unless Cnt (setq Cnt 1.0))

  (with *vert-ptr
    (let Pos (list (get (: pos) 1)
                   (- (get (: pos) 2) (*/ (+ *adv-vert *adv-vert) Cnt 1.0))
                   (get (: pos) 3))
      (=: pos Pos)))
      
  (update-model-matrix> *vert-ptr)
  (cmd-update-vert *ipc *vert-ptr))


(de set-pos-ptr (Pos)
  (with *vert-ptr (=: pos Pos))
  (update-model-matrix> *vert-ptr)
  (cmd-update-vert *ipc *vert-ptr))


(de set-x-ptr (Pos)
  (with *vert-ptr (=: pos (list Pos (get (: pos) 2) (get (: pos) 3))))
  (update-model-matrix> *vert-ptr)
  (cmd-update-vert *ipc *vert-ptr))


(de set-y-ptr (Pos)
  (with *vert-ptr (=: pos (list (get (: pos) 1) Pos (get (: pos) 3))))
  (update-model-matrix> *vert-ptr)
  (cmd-update-vert *ipc *vert-ptr))


(de set-x-ptr-2 (Pos)
  (with *vert-ptr (=: pos (list Pos (get (: pos) 2) (get (: pos) 3)))))


(de set-y-ptr-2 (Pos)
  (with *vert-ptr (=: pos (list (get (: pos) 1) Pos (get (: pos) 3)))))
