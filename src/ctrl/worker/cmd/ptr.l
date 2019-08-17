(de cmd-move-ptr (Cnt Move)

  (with *vert-ptr
      (=: pos (list (+ (get (: pos) 1) (get Move 1))
                    (+ (get (: pos) 2) (get Move 2))
                    (get (: pos) 3))))

  (update-model-matrix> *vert-ptr)

  (cmd-update-vert *ipc *vert-ptr))


(de cmd-move-ptr-px (Keysym) (cmd-move-ptr 1 (list (*/ *adv-vert 1.0 1.0)   0.0)))
(de cmd-move-ptr-nx (Keysym) (cmd-move-ptr 1 (list (*/ *adv-vert -1.0 1.0)  0.0)))

(de cmd-move-ptr-py (Keysym) (cmd-move-ptr 2 (list 0.0 (+ *adv-vert *adv-vert))))
(de cmd-move-ptr-ny (Keysym) (cmd-move-ptr 2 (list 0.0 (- (+ *adv-vert *adv-vert)))))