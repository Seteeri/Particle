(de cmd-move-cam (Move)

   (with *projview

      (=: pos (list (+ (get (: pos) 1) (get Move 1))
                    (+ (get (: pos) 2) (get Move 2))
                    10.0))

      (let ((X Y Z) (: pos))
        (prinl-info 'cmd-move-cam (glue " " (list (format X *Scl) (format Y *Scl))))))

    (update-mat-view> *projview)

    # Either send relevant data or send entire data
    # -> Minimize render processing time

    # Pass list of socks so need only serialize once

    (cmd-upd-pv *ipc))


(de cmd-move-cam-px (Keysym) (cmd-move-cam (list *adv-vert-2     0.0)))
(de cmd-move-cam-nx (Keysym) (cmd-move-cam (list (- *adv-vert-2) 0.0)))
(de cmd-move-cam-py (Keysym) (cmd-move-cam (list 0.0 *adv-vert-2)))
(de cmd-move-cam-ny (Keysym) (cmd-move-cam (list 0.0 (- *adv-vert-2))))


(de cmd-zoom-cam (Delta)

  (with *projview

    (inc (:: scale-ortho) Delta)
    
    (prinl-info 'cmd-zoom-cam (format (: scale-ortho) *Scl)))

  (update-mat-proj> *projview)
  
  (cmd-upd-pv *ipc))


(de cmd-zoom-cam-in  (Keysym) (cmd-zoom-cam -1.0))
(de cmd-zoom-cam-out (Keysym) (cmd-zoom-cam  1.0))
