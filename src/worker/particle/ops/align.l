# These are used with layout

# Move one more folder up

(de align-cur-to-line-y (Part)

  #{
        
    Y layout:
    
    [Y]  CAR
    CDR
    
    x = line ori
    y = line bnds of 'c 'a
  }#

  (let (Ori-C (get Part 'line 'l 'ori)
        Ori-CA (get Part 'c 'a 'ori)
        Dims-CA (get Part 'c 'a 'line 'dims))

    (when NIL
      (println 'align-cur-to-line-y 
              Part (get Part 'data) 
              'to 
              (get Part 'line) (get Part 'line 'data)))

    (set-cur-x (get Ori-C 1))

    (set-cur-y (- (get Ori-CA 2) (get Dims-CA 2)))
    (nl-cur (+ 1.0 *sublist-space-y))
    
    #(print-cur)
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

    (when NIL
      (println 'align-cur-to-line-x 
               Part (get Part 'data) 
               'to 
               (get Part 'line 'l 'data)))
    
    (set-cur-x (get Ori-C 1))

    (set-cur-y *cur-y)
    (nl-cur (+ 1.0 *sublist-space-y))
    
    #(print-cur)
    ))
