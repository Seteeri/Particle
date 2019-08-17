# COuld place these in ptr also?

(de intern-2 (Data)
  (let (Res (intern Data)
        Col (get-color-type Res))

    (println Res)

    (adv-ptr 1.0)

    (push '*timeline (new '(+Particle) Res (draw-str Data Col)))))
    

(de read-string-end (End)
  # Do not include N
  (let (Verts  ()
        Data  ())
    (for Prt *timeline
      (T (= Data End))
      (with Prt
        (push 'Data (: data))
        (for Vert (: verts)
          (push 'Verts Vert))))

    (list Verts Data)))


(de read-string-2 ()

  # read until nonstr
  
  (let (Verts ()
        Data  ())
        
    (for Prt *timeline
      (T (not (str? Prt)))
      (push 'Data (get Prt 'data))
      (for Vert (get Prt 'verts)
        (push 'Verts Vert)))

    (list Verts Data)))


(de pop-string ()
  (let (Verts  ()
        Data  ())
    (let Prt NIL
      (while (setq Prt (pop '*timeline))
        (push 'Data (get Prt 'data))
        (for Vert (get Prt 'verts)
          (push 'Verts Vert))))

    (list Verts Data)))


(de pop-string-2 ()

  # pop until nonchar
  
  (let (Verts ()
        Data  ()
        Prt   NIL)
        
    (loop
      (setq Prt (pop '*timeline))
      (when Prt
        (if (str? Prt)
            (prog
              (push 'Data (get Prt 'data))
              (for Vert (get Prt 'verts)
                (push 'Verts Vert)))
            (prog
              (push '*timeline Prt)
              (setq Prt NIL))))
      (T (not Prt)))

    (list Verts Data)))