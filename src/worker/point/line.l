(class +Line)
(dm T (P N L D)
  (=: p P)
  (=: n N)
  (=: l L)
  (=: dims (if D @ (list 0.0 0.0))))


(dm upd-dims> (Ori)
  (=: dims (list (abs (- *bnds-x (car  Ori)))
                 (abs (- *bnds-y (cadr Ori))))))
  
