# COuld place these in ptr also?

(de del-particle (Prt)
  (let (Sock-Render (ipc~get-fd> *ipc "RENDER")
        Verts (get Prt 'verts))
    (for Vert Verts
      (let Msg (pack "(zv " (get Vert 'off-gl) '")")
        (send-msg *ipc Sock-Render Msg))
      (push '*verts Vert))))


(de intern-particle (Data)
  (let (Res (intern Data)
        Col (get-color-type Res))
    (push '*timeline (new '(+Particle) Res (draw-str Data Col)))))
    

################################################################################

# Rename these to build-data?

(de build-str-end (End)

  # Do not include N

  (let (Verts  ()
        Data  ())
    (for Prt *timeline
      (T (= (get Prt 'data) End))
      (with Prt
        (push 'Data (: data))
        (for Vert (: verts)
          (push 'Verts Vert))))

    (list Verts Data)))


(de build-str ()

  # Build until non-str encountered
  
  (let (Verts ()
        Data  ())
        
    (for Prt *timeline
      # Stop on non str
      (NIL (str? (get Prt 'data)))
      (push 'Data (get Prt 'data))
      (for Vert (get Prt 'verts)
        (push 'Verts Vert)))

    (list Verts Data)))


(de build-data ()
  (let Data ()
    (for Prt *timeline
      (push 'Data (get Prt 'data)))
    Data))


################################################################################


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


################################################################################

(de print-tl ()
  (for Prt *timeline
    (with Prt
      (prin (: data) " ")))
  (prinl))

(de print-tl-2 ()
  (for Prt *timeline
    (with Prt
      (println (sym? (: data)) (str? (: data)) (num? (: data))))))