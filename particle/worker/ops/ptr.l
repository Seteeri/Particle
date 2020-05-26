(de lay-ptr (P)
  (lay-pt-start P P 'skip-cdr))


(de set-ptr (P B)
  (put P 'b B))
  

(de get-ptr-ref ()
  (val *ptr))
  
(de pt-ptr (Po)
  (set-ptr *ptr Po)
  (lay-ptr *ptr)
  (println 'pt-ptr "-> " Po))


(de pt-prv ()
  (let? Po (prv> (get-ptr-ref))
    (pt-ptr Po)))


(de pt-nxt ()
  (let? Po (nxt> (get-ptr-ref))
    (pt-ptr Po)))


(de pt-sup ()
  (let? Po (sup> (get-ptr-ref))
    (pt-ptr Po)))


(de pt-sub ()
  (let? Po (sub> (get-ptr-ref))
    (pt-ptr Po)))


(de pt-ln-fir ()
  (let (F T
        S (if *on-car  # Move up if on Car
              (get *ptr 'b 'c)
              (get-ptr-ref))
        P S)
    (while F
      (if (get P 'line)
          (off F)
          (setq F (setq P (get P 'c)))))
    (unless (= P S)
      (if *on-car
          (sub> P)
          (mov-cur-abv P))
      (pt-ptr P))))

      
(de pt-ln-las ()
  (let (F T
        S (if *on-car  # Move up if on Car
              (get *ptr 'b 'c)
              (get-ptr-ref))
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
      (pt-ptr P))))
       
        
(de set-on-car (F)
  # Either color or mov to ptr list
  (if F
      (prog
        (on *on-car)
        (col> *car *selb-yellow 1.0)
        (col> (get *car 'a) *selb-yellow 1.0)
        (setq *ptr *car)
        (col> *cdr *selb-violet 1.0)
        (col> (get *cdr 'a) *selb-violet 1.0))
      (prog
        (off *on-car)
        (col> *car *selb-violet 1.0)
        (col> (get *car 'a) *selb-violet 1.0)
        (setq *ptr *cdr)
        (col> *cdr *selb-yellow 1.0)
        (col> (get *cdr 'a) *selb-yellow 1.0))))
        
        
(de tog-on-car ()
  (set-on-car (not *on-car)))
