# COuld place these in ptr also?

(de del-particle (Prt)
  (let (Sock-Render (ipc~get-fd> *ipc "RENDER")
        Verts (get Prt 'verts))
    (for Vert Verts
      (let Msg (pack "(zv " (get Vert 'off-gl) '")")
        (send-msg *ipc Sock-Render Msg))
      (push '*verts Vert))))    

################################################################################

(de build-data ()

  (let Data ()

    (for Prt (car *lists-ptr)

      (push 'Data (get Prt 'data)))

    Data))


# Pack Options
# 1. Pack only strs
# 2. Pack only nums
# 3. Pack both (default)


(de build-pack ()

  # Build until non-str/num encountered
  
  (let (Verts ()
        Data  ())
        
    (for Prt (car *lists-ptr)
      # Stop on non str
      (T (and (not (str? (get Prt 'data))) (not (num? (get Prt 'data)))))
      (push 'Data (get Prt 'data))
      (for Vert (get Prt 'verts)
        (push 'Verts Vert)))

    (list Verts Data)))


(de build-str ()

  # Build until non-str encountered
  
  (let (Verts ()
        Data  ())
        
    (for Prt (car *lists-ptr)
      # Stop on non str
      (T (or (not (str? (get Prt 'data))) (= Prt 'PARTICLE)))
      (push 'Data (get Prt 'data))
      (for Vert (get Prt 'verts)
        (push 'Verts Vert)))

    (list Verts Data)))


################################################################################

# DEBUG

(de print-tl ()
  (for Prt (car *lists-ptr)
    (with Prt
      (prin (: data) " ")))
  (prinl))

(de print-tl-2 ()
  (for Prt (car *lists-ptr)
    (with Prt
      (println (sym? (: data)) (str? (: data)) (num? (: data))))))