# Called by delete
# And when updating lines
# Can bnds be cache'd?
# So need not traverse tree each time
# Store car and cdr dims?

(de align-cur-to-car-bnds (Part)
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


# These are used with layout

(de align-cur-to-line (Part)

  # x = origin of line
  # y = bnds of line
  (let (Line (get Part 'line 'p)
        Ori-Line (get Line 'ori)
        Ori (if (isa '+Sym Line)
                (get Line (if (= (get Line 'lay) 'x)
                             'b
                             'a)
                     'ori)
                (get Line 'a 'ori))
        Bnds (get Line 'line 'dims))

    (println 'align-cur-to-line 'Line Line (get Line 'data))

    (set-cur-x (get Ori 1))

    (set-cur-y (- (get Ori-Line 2) (get Bnds 2)))
    (nl-cur (+ 1.0 *sublist-space-y))))


#{
(de align-cur-to-prv (Part)

  (if (= (get Part 'lay) 'x)

      (prog
      
        # x = origin of Pair
        # y = bnds of line
        (set-cur-x (get Part 'c 'ori 1))

        (let (Line (get Part 'line 'p)
              Ori (get (if (isa '+Sym Line)
                           (get Line 'b)
                           Line)
                     'ori)
              Bnds (get Line 'line 'dims))
                          
          (set-cur-y (- (get Ori 2) (get Bnds 2)))
          (nl-cur (+ 1.0 *sublist-space-y)))
        
        #(println 'align-cur-to-prv "Set *cur: " *cur)
        (setq *cur-x (get *cur 1) *cur-y (get *cur 2)))

      (prog
        (println 'align-cur-to-prv "IMPLEMENT Y LAYOUT"))))
}#
