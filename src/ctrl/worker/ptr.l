# Are these commands also?

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


(de set-pos-ptr-x (Pos)
  (with *vert-ptr (=: pos (list Pos (get (: pos) 2) (get (: pos) 3))))
  (update-model-matrix> *vert-ptr)
  (cmd-update-vert *ipc *vert-ptr))


(de set-pos-ptr-y (Pos)
  (with *vert-ptr (=: pos (list (get (: pos) 1) Pos (get (: pos) 3))))
  (update-model-matrix> *vert-ptr)
  (cmd-update-vert *ipc *vert-ptr))


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