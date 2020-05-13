(de lay-ptr ()
  (lay-part-start ↓0 ↓0 'skip-cdr))


# Easier to read...
(de set-ptr (P)
  (put ↓0 'b P))


(de point-ptr (Part)
  (set-ptr Part)
  #(println 'point-ptr "Mov -> " Part (get Part 'data))
  (lay-part-start ↓0 *line 'skip-cdr))


(de point-prv ()
  #(println 'point-prv)
  (let? Part (prv> (get ↓0 'b)) (point-ptr Part)))


(de point-nxt ()
  (let? Part (nxt> (get ↓0 'b)) (point-ptr Part)))


(de point-sup ()
  #(println 'point-sup)
  (let? Part (sup> (get ↓0 'b)) (point-ptr Part)))


(de point-sub ()
  #(println 'point-sub)
  (let? Part (sub> (get ↓0 'b)) (point-ptr Part)))


(de point-ln-first ()
  (let (F T
        S (if *on-car  # Move up if on Car
              (get ↓0 'b 'c)
              (get ↓0 'b))
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
              (get ↓0 'b 'c)
              (get ↓0 'b))
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
