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

(de align-cur-to-line-y (Part)

  #{
        
    Y layout:
    
    [Y]  CAR
    CDR
    
    x = line ori
    y = bnds of 'c 'a (or use cur-*?)
  }#

  (let (Ori-C (get Part 'line 'l 'ori)
        Ori-CA (get Part 'c 'a 'ori)
        Dims-CA (get Part 'c 'a 'line 'dims))

    #(println 'align-cur-to-line-y Part (get Part 'data) 'to (get Part 'line) (get Part 'line 'data))

    (set-cur-x (get Ori-C 1))

    (set-cur-y (- (get Ori-CA 2) (get Dims-CA 2)))
    (nl-cur (+ 1.0 *sublist-space-y))
    
    (print-cur)
    ))


(de align-cur-to-line-x (Part)

  #{
    
    X layout:
  
    [X]  CDR
    CAR
    
    
    [X]
    
    x = 'c 'ori
    y = bnds of 'c ..., which should be *cur-y
      
  }#

  (let Ori-C (get Part 'line 'l 'ori)
  
    #(println 'align-cur-to-line-x Part (get Part 'data) 'Line Line (get Line 'data))

    (set-cur-x (get Ori-C 1))

    (set-cur-y *cur-y)
    (nl-cur (+ 1.0 *sublist-space-y))
    
    (print-cur)
    ))
