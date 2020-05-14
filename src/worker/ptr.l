(de lay-ptr (P)
  (lay-part-start P P 'skip-cdr))


# Easier to read...
(de set-ptr (P B)
  (put P 'b B))


(de point-ptr (Part)
  (set-ptr *fwd-in Part)
  #(println 'point-ptr "Mov -> " Part (get Part 'data))
  (lay-ptr *fwd-in))


(de point-prv ()
  #(println 'point-prv)
  (let? Part (prv> (get *fwd-in 'b)) (point-ptr Part)))


(de point-nxt ()
  (let? Part (nxt> (get *fwd-in 'b)) (point-ptr Part)))


(de point-sup ()
  #(println 'point-sup)
  (let? Part (sup> (get *fwd-in 'b)) (point-ptr Part)))


(de point-sub ()
  #(println 'point-sub)
  (let? Part (sub> (get *fwd-in 'b)) (point-ptr Part)))


(de point-ln-first ()
  (let (F T
        S (if *on-car  # Move up if on Car
              (get *fwd-in 'b 'c)
              (get *fwd-in 'b))
        P S)
    (while F
      (if (get P 'line)
          (off F)
          (setq F (setq P (get P 'c)))))
    (unless (= P S)
      (if *on-car
          (sub> P)
          (mov-cur-abv P))
      (point-ptr P))))

      
(de point-ln-last ()
  (let (F T
        S (if *on-car  # Move up if on Car
              (get *fwd-in 'b 'c)
              (get *fwd-in 'b))
        P S)
    # Stop before line
    (while F
      (if (or (get P 'b 'line) (not (get P 'b)))
          (off F)
          (setq F (setq P (get P 'b)))))
    (unless (= P S)
      (if *on-car
          (sub> P)
          (mov-cur-abv P))
      (point-ptr P))))
