# These are used with layout

(de align-cur-to-line-y (Part)

  #{
        
    Y layout:
    
    [Y]  CAR
    CDR
    
    x = line ori
    y = line bnds of 'c 'a
  }#

  (let Ori-C (get Part 'line 'l 'ori)
        
    (when NIL
      (println 'align-cur-to-line-y 
               Part
               (get Part 'data)))

    ~(assert Ori-C)               
               
    #(mov-cur-y (- (get Ori-CA 2) (get Dims-CA 2)))               
               
    (mov-cur (get Ori-C 1) *bnds-y)
    (adj-cur-y (+ 1.0 *sublist-space-y))))


(de align-cur-to-line-x (Part)

  #{
    
    X layout:
  
    [X]  CDR
    CAR
    
    
    [X]
    
    x = 'c 'ori
    y = bnds of 'c ..., which should be *bnds-y
      
  }#

  (let Ori-C (get Part 'line 'l 'ori)
  
    (when NIL
      (println 'align-cur-to-line-x 
               Part
               'to 
               (get Part 'line 'l)))
    
    ~(assert Ori-C) 
    
    (mov-cur (get Ori-C 1) *bnds-y)
    (adj-cur-y (+ 1.0 *sublist-space-y))))
