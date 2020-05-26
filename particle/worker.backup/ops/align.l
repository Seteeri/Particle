# These are used with layout

(de align-cur-line (Po)

  #{
        
    Y layout:
    
    [Y]  CAR
    
    Po
    
    x = line ori
    y = line bnds of 'c 'a
  }#

  (let (Ori-C (get Po 'line 'l 'ori)
        Ori-A (get Po 'c 'a 'ori)
        Dims-A (get Po 'c 'a 'dims))
                
    (when NIL
      (println 'align-cur-line
               Po
               (get Po 'data)))

    ~(assert Ori-C)          

    # Get Y pos from Car bnds
    (mov-cur (get Ori-C 1)
             (- (cadr Ori-A) (cadr Dims-A)))
    (adj-cur-y (+ 1.0 *sublist-space-y))))


(de align-cur-line-x (Po)

  #{
    
    X layout:
  
    [X]  Po
    CAR
    
    x = 'c 'ori
    y = bnds of 'c ..., which should be *bnds-y
      
  }#

  (let (Ori-C (get Po 'line 'l 'ori)
        Ori-A (get Po 'c 'a 'ori)
        Dims-A (get Po 'c 'a 'dims))
  
    (when NIL
      (println 'align-cur-line-x 
               Po
               'to 
               (get Po 'line 'l)))
    
    ~(assert Ori-C) 
    
    (mov-cur (- (car Ori-A) (car Dims-A))
             (cadr Ori-C))
    (adj-cur-x *cdr-space-x)))
