################################################################################

(de build-data ()

  (let Data ()

    (for Part (car *lists-ptr)

      (push 'Data (get Part 'data)))

    Data))


# Pack Options
# 1. Pack only strs
# 2. Pack only nums
# 3. Pack both (default)


(de build-pack ()

  # Build until non-str/num encountered
  
  (let (Verts ()
        Data  ())
        
    (for Part (car *lists-ptr)
      # Stop on non str
      (T (and (not (str? (get Part 'data))) (not (num? (get Part 'data)))))
      (push 'Data (get Part 'data))
      (for Vert (get Part 'verts)
        (push 'Verts Vert)))

    (list Verts Data)))


(de build-str ()

  # Build until non-str encountered
  
  (let (Verts ()
        Data  ())
        
    (for Part (car *lists-ptr)
      # Stop on non str
      (T (or (not (str? (get Part 'data))) (= Part 'PARTICLE)))
      (push 'Data (get Part 'data))
      (for Vert (get Part 'verts)
        (push 'Verts Vert)))

    (list Verts Data)))


################################################################################

# DEBUG

(de print-tl ()
  (for Part (car *lists-ptr)
    (with Part
      (prin (: data) " ")))
  (prinl))

(de print-tl-2 ()
  (for Part (car *lists-ptr)
    (with Part
      (println (sym? (: data)) (str? (: data)) (num? (: data))))))