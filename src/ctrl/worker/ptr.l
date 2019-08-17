# Are these commands also?

(de adv-ptr (Cnt)

  (unless Cnt (setq Cnt 1.0))

  (prinl-info "adv-ptr" (format Cnt *Scl))

  (with *vert-ptr

    (prinl-info "adv-ptr" (pack "Old: " (format (get (: pos) 1) *Scl) " " (format (get (: pos) 2) *Scl)))

    (let Pos (list (+ (get (: pos) 1) (*/ *adv-vert Cnt 1.0))
                   (get (: pos) 2)
                   0.0)
      (=: pos Pos))

    (prinl-info "adv-ptr" (pack "New: " (format (get (: pos) 1) *Scl) " " (format (get (: pos) 2) *Scl))))
      
  (update-model-matrix> *vert-ptr)
  (cmd-update-vert *ipc *vert-ptr))


(de nl-ptr (Cnt)

  (with *vert-ptr
    (let Pos (list 0.0
                   (- (get (: pos) 2) (+ *adv-vert *adv-vert))
                   0.0)
      (=: pos Pos)))
      
  (update-model-matrix> *vert-ptr)
  (cmd-update-vert *ipc *vert-ptr))
