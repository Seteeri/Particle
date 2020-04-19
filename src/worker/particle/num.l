#{
                      cell [PARTICLE]
                        |
            +-----------+-----------+
            |           |           |
         Number       Symbol       Pair
                        |
                        |
   +--------+-----------+-----------+
   |        |           |           |
  NIL   Internal    Transient    External
}#


(class +Num +Particle)

(dm lay-x> ()
  (setq *cur (mov> This *cur))
  (upd-cur)
  This)


(dm lay-x-debug> ()

  (let (X (get *cur 1)
        Y (get *cur 2))

    (setq *cur (mov> This *cur))
    (upd-cur)        
        
    (when (or (not (= X (get *cur 1)))
              (not (= Y (get *cur 2))))
                            
      (println 'lay-x> '+Num
              (format X *Scl) '-> (format (get *cur 1) *Scl)
              (format Y *Scl) '-> (format (get *cur 2) *Scl))))

  This)  
  
  
(dm lay-y> ()
  (setq *cur (mov> This *cur))
  (upd-cur)
  This)
