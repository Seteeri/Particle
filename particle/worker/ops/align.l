# These are used with layout

(de align-cur-line (Po)

  #{
        
    Y layout:
    
    [Y]  CAR
    
    Po
  }#

  (let (Lst (get Po 'line 'l)
        Ori-L (get Lst 'ori)
        
        Prv (get Po 'line 'p)
        Ori-P (get Prv 'ori)
        Dims-P (if (lay-y?> Prv)
                   (get (lup-pt (car (get Prv 'data))) 'line 'dims)
                   (get Prv 'line 'dims)))
                
    (when T
      (println 'align-cur-line
               Po
               (get Po 'data)))

    ~(assert Lst)

    (prog # If Y Pair, get Y from Car
      (mov-cur (car Ori-L)
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
