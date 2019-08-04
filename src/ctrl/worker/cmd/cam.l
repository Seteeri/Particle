(de cmd-move-cam (Cnt Move)

   (with *projview

      (=: pos (list (+ (get (: pos) 1) (get Move 1))
                    (+ (get (: pos) 2) (get Move 2))
                    10.(get (: pos) 3))))

    (update-mat-view> *projview)

    # Either send relevant data or send entire data
    # -> Minimize render processing time

    # Pass list of socks so need only serialize once

    (cmd-update-pv *ipc))


(de cmd-zoom-cam (Delta)

  (with *projview

    (inc (:: scale-ortho) Delta))

  (update-mat-proj> *projview)

  (cmd-update-pv *ipc))