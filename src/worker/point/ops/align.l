# These are used with layout

(de align-cur-to-line-y (Part)

  #{
        
    Y layout:
    
    [Y]  CAR
    
    Part
    
    x = line ori
    y = line bnds of 'c 'a
  }#

  (let (Ori-C (get Part 'line 'l 'ori)
        Ori-A (get Part 'c 'a 'ori)
        Dims-A (get Part 'c 'a 'dims))
                
    (when T
      (println 'align-cur-to-line-y 
               Part
               (get Part 'data)))

    ~(assert Ori-C)          

    # Get Y pos from Car bnds
    (mov-cur (get Ori-C 1)
             (- (cadr Ori-A) (cadr Dims-A)))
    (adj-cur-y (+ 1.0 *sublist-space-y))))


(de align-cur-to-line-x (Part)

  #{
    
    X layout:
  
    [X]  Part
    CAR
    
    x = 'c 'ori
    y = bnds of 'c ..., which should be *bnds-y
      
  }#

  (let (Ori-C (get Part 'line 'l 'ori)
        Ori-A (get Part 'c 'a 'ori)
        Dims-A (get Part 'c 'a 'dims))
  
    (when T
      (println 'align-cur-to-line-x 
               Part
               'to 
               (get Part 'line 'l)))
    
    ~(assert Ori-C) 
    
    (mov-cur (- (car Ori-A) (car Dims-A))
             (cadr Ori-C))
    (adj-cur-x *cdr-space-x)))
