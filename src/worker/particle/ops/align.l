(de align-cur-to-bnds (Part)
  # x: CDR is to the right; use X bounds
  # y: CDR is below; use Y bounds
  (let Bnds (bounds> Part NIL T) # Skip CDR
    (if (= (get Part 'lay) 'x)
        (prog
          # Move cur to x base of Part and y origin of Part
          (set-cur-x (get Bnds 1))
          (set-cur-y (get Part 'ori 2))
          (adv-cur 2.0))
        (prog
          # Move cur to x origin of Part and y base of Part
          (set-cur-x (get Part 'ori 1))
          (set-cur-y (get Bnds 2))
          (nl-cur 2.0)))))


(de align-cur-to-prv (Car)

  # x = origin of Pair
  # y = bnds of line
  
  (set-cur-x (get Car 'c 'ori 1))
  
  (let Ori (get (if (isa '+Sym *line) 
                    (get *line 'b)
                    *line) 'ori)
    # This is not using line bnds, but rather c origin
    (set-cur-y (get Car 'c 'ori 2))
    (nl-cur 2.0)
    (nl-cur *sublist-space-y)))


(de align-cur-to-line (Car)

  # x = origin of line
  # y = bnds of line
    
  (let (Line (get Any 'line 'p)
        Ori (get (if (isa '+Sym Line)
                         (get Line 'b)
                         Line) 'ori))
        
    (set-cur-x (get Ori 1))
    (unless (isa '+Sym Line)
      (println 'align-cur-to-line "Adv-cur" Line (get Line 'data))
      (adv-cur 2.0))
    
    (set-cur-y (get Car 'c 'ori 2))
    (nl-cur 2.0)
    (nl-cur *sublist-space-y)
    
    #(prin 'align-cur-to-prv " " (get *line 'data) " " (format (get Ori 1) *Scl) " " (format (get Ori 2) *Scl) " ")
    #(print-cur)
    
    ))
