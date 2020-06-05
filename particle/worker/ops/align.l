# These are used with layout

(de align-cur-line (Po)

  #{
        
    Y layout:
    
    [Y]  CAR
    
    Po
  }#

  (let (Prv (if (get Po 'line 'p) @ *line-lay)
        Car-P (car (get Prv 'data))
        Po-Car (if Car-P (lup-pt @) (get Prv 'a))
        Ori-P (get Prv 'ori)
        Dims-P (if (lay-y?> Prv)
                   (get Po-Car 'line 'dims)
                   (get Prv 'line 'dims)))
                
    (when (and (lay-y?> Prv) T)
      (println 'align-cur-line 
        Po (get Po 'data))
      (println 'align-cur-line 
        Prv (get Prv 'data) Po-Car)
      (println 'align-cur-line 
        Dims-P (lup-pt (car (get Prv 'data)))))

    ~(assert Prv)
    ~(assert Ori-P)
    ~(assert Dims-P)

    (prog # If Y Pair, get Y from Car
      (mov-cur (car Ori-P)
              (- (cadr Ori-P) (cadr Dims-P)))
      (adj-cur-y (+ 1.0 *sublist-space-y)))))
    
    
    #(if (lay-x?> Po) # If X Pair, get Y from Cdr
    #    (prog
    #      (println 'align-cur-line "*cur in place already"))



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
