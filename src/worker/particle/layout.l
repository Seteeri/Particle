(de lay-part (Any Base)
  
  # (= (cons T) (list T)) -> T
  
  (if (= (type Any) '(+Pair))
  
      (lay-pair Any Base)
      
      (lay-atom Any Base)))


(de lay-pair (Any Base)

  # (-Y 1)
  # 1st arg is pos CAR
  # 2nd arg is max CDR

  #{
  (if (lst? (cdr Any)) # T for NIL
        
    # Handle specific forms here
    (cond (T
           (lay-pair Any)))
    
    # Pair
    (lay-pair Any))
  }#

  (if (= (get Any 'lay) 'x)
    
    (lay-pair-x Any Base)
    
    (lay-pair-y Any Base)))
    

(de lay-pair-x (Any Base)
  #{
    Layout CAR=Y, CDR=X relative to Any
    
    Main issue when a new list layout detected,
    the extents of the following list etc. must be known
    before switching back to the original list
    
    On list layout change, push a number to the list
    Use list for nested layout changes
    pop after return, apply to next item in the list
    The last item will have the computed final bounds
    
    Instead of a list, poss pass a value and return the final one
    
    When drawing across, must keep track of Y depth in domain of CAR/CDR
    
  }#

  # RETURN MIN Y
    
  (mov> Any Base)

  (with Any
  
    #(println 'lay-pair-x (get Any 'data))
  
    (let ((X-Car Y-Car) (lay-pair-x-car Any
                                        Base
                                        (: a)
                                        (: lay)))
                                        
      # Handle if short
      (let X-End (last-vert-pos> Any 1)
        (when (< (get *cur 1) X-End)
          (setq *cur (place 1 *cur X-End))))

      # If Car layout diff, must use the max X bounds
      (unless (= (get (: a) 'lay) (: lay))
        (println "CAR Y; CDR X" 
                 (format X-Car *Scl)
                 (format (get *cur 1) *Scl))
        (set-cur-x (max X-Car (get *cur 1))))
        
      (let ((X-Cdr Y-Cdr) (lay-pair-x-cdr Base
                                        (: b)
                                        (: lay)))
          
        (list (max X-Car X-Cdr)
              (min Y-Car Y-Cdr))))))

(de lay-pair-x-car (Any Base Car Lay)

  (let ((X Y) Base)

    # Instead do move part to pos
    (mov-part-bel> Car Any)

    (mov-cur-vert (last-vert> Car))
    
    #(prin "  ") (println 'x 'a (= (get Car 'lay) Lay))
    
    # Use X as baseline since CARs are drawn Y
    
    (mov-cur-vert (last-vert> Car))
    
    # Return min/max between base and ret lay-part
    (lay-part Car
              (list X (get *cur 2))
              Lay)))

(de lay-pair-x-cdr (Base Cdr Lay)

  (let ((X Y) Base)
      
    # Cursor should be in X pos after drawing CAR recursively

    (adv-cur 2.0)

    (set-cur-y Y)
    
    (let (Cur *cur)
    
      (mov> Cdr *cur)
      
      (mov-cur-vert (last-vert> Cdr))
      
      #(prin "  ") (println 'x 'b (= (get Cdr 'lay) Lay))
                
      (lay-part Cdr
                Cur
                Lay))))
            
##############

(de lay-pair-y (Any Base Lay)
  # Draw CAR=X, CDR=Y

  # RETURN MAX X

  (mov> Any Base)
  
  (with Any
  
    #(println (get Any 'data))
    
    # If previous pair was x layout, must keep y (not modify y)
  
    (let ((X-Car Y-Car) (lay-pair-y-car Base
                                        (: a)
                                        Lay
                                        (last-vert> Any)))
        
      # If Car layout diff, must use the min Y bounds
            
      (unless (= (get (: a) 'lay) (: lay))
        (println "CAR X; CDR Y" 
                 (format Y-Car *Scl)
                 (format (get *cur 2) *Scl))
        (set-cur-y Y-Car))
            
      (let ((X-Cdr Y-Cdr) (lay-pair-y-cdr Base
                                          (: b)
                                          Lay))

        (list (max X-Car X-Cdr)
              (min Y-Car Y-Cdr))))))

(de lay-pair-y-car (Base Car Lay Last-Vert)

  (let ((X Y) Base)

    # Move cursor to end of car into pos
    (mov-cur-vert Last-Vert)
    
    # Set to 1 to remove space or inc etc
    (adv-cur 2.0)
    
    # Save this pos as base
    (let (Cur *cur)
    
      (mov> Car *cur)
      (mov-cur-vert (last-vert> Car))
    
      #(prin "  ") (println 'y 'a (= (get Car 'lay) Lay))
      
      (lay-part Car
                Cur
                Lay))))

(de lay-pair-y-cdr (Base Cdr Lay)

  (let ((X Y) Base)
          
    # Cursor should be in Y pos after drawing CAR recursively
    # Move X back and under
    (set-cur-x X)
        
    # Inc to double space etc
    (nl-cur)
            
    (mov> Cdr *cur)
    (mov-cur-vert (last-vert> Cdr))
      
    #(prin "  ") (println 'y 'b (= (get Cdr 'lay) Lay))
            
    # Use X as baseline since CDRs are drawn Y
    (lay-part Cdr
              (list X (get *cur 2))
              Lay)))


(de lay-atom (Any Base)
  # Recurse as needed for sym, num, etc.
  
  #(println "lay-atom"
  #         (get Any 'data)
  #         (format (get *cur 1) *Scl)
  #         (format (get *cur 2) *Scl))
  
  (when (and (= (type Any) '(+Sym))
             (get Any 'b)) 
    (lay-sym Any Base))
  
  # Symbols have their value drawn
  *cur)


(de lay-sym (Any Base)

  (mov> Any *cur)

  (with Any
  
    # Then check where to put CDR of symbol
    (if (= (: lay) 'x)
      
      # Right
      (mov-part-aft> (: b) Any)
      
      # Below
      (mov-part-bel> (: b) Any))
        
    (mov-cur-vert (last-vert> (: b)))))
(de lay-part (Any Base)
  
  # (= (cons T) (list T)) -> T
  
  (if (= (type Any) '(+Pair))
  
      (lay-pair Any Base)
      
      (lay-atom Any Base)))


(de lay-pair (Any Base)

  # (-Y 1)
  # 1st arg is pos CAR
  # 2nd arg is max CDR

  #{
  (if (lst? (cdr Any)) # T for NIL
        
    # Handle specific forms here
    (cond (T
           (lay-pair Any)))
    
    # Pair
    (lay-pair Any))
  }#

  (if (= (get Any 'lay) 'x)
    
    (lay-pair-x Any Base)
    
    (lay-pair-y Any Base)))
    

(de lay-pair-x (Any Base)
  #{
    Layout CAR=Y, CDR=X relative to Any
    
    Main issue when a new list layout detected,
    the extents of the following list etc. must be known
    before switching back to the original list
    
    On list layout change, push a number to the list
    Use list for nested layout changes
    pop after return, apply to next item in the list
    The last item will have the computed final bounds
    
    Instead of a list, poss pass a value and return the final one
    
    When drawing across, must keep track of Y depth in domain of CAR/CDR
    
  }#

  # RETURN MIN Y
    
  (mov> Any Base)

  (with Any
  
    #(println 'lay-pair-x (get Any 'data))
  
    (let ((X-Car Y-Car) (lay-pair-x-car Any
                                        Base
                                        (: a)
                                        (: lay)))
                                        
      # Handle if short
      (let X-End (last-vert-pos> Any 1)
        (when (< (get *cur 1) X-End)
          (setq *cur (place 1 *cur X-End))))

      # If Car layout diff, must use the max X bounds
      (unless (= (get (: a) 'lay) (: lay))
        (println "CAR Y; CDR X" 
                 (format X-Car *Scl)
                 (format (get *cur 1) *Scl))
        (set-cur-x (max X-Car (get *cur 1))))
        
      (let ((X-Cdr Y-Cdr) (lay-pair-x-cdr Base
                                        (: b)
                                        (: lay)))
          
        (list (max X-Car X-Cdr)
              (min Y-Car Y-Cdr))))))

(de lay-pair-x-car (Any Base Car Lay)

  (let ((X Y) Base)

    # Instead do move part to pos
    (mov-part-bel> Car Any)

    (mov-cur-vert (last-vert> Car))
    
    #(prin "  ") (println 'x 'a (= (get Car 'lay) Lay))
    
    # Use X as baseline since CARs are drawn Y
    
    (mov-cur-vert (last-vert> Car))
    
    # Return min/max between base and ret lay-part
    (lay-part Car
              (list X (get *cur 2))
              Lay)))

(de lay-pair-x-cdr (Base Cdr Lay)

  (let ((X Y) Base)
      
    # Cursor should be in X pos after drawing CAR recursively

    (adv-cur 2.0)

    (set-cur-y Y)
    
    (let (Cur *cur)
    
      (mov> Cdr *cur)
      
      (mov-cur-vert (last-vert> Cdr))
      
      #(prin "  ") (println 'x 'b (= (get Cdr 'lay) Lay))
                
      (lay-part Cdr
                Cur
                Lay))))
            
##############

(de lay-pair-y (Any Base Lay)
  # Draw CAR=X, CDR=Y

  # RETURN MAX X

  (mov> Any Base)
  
  (with Any
  
    #(println (get Any 'data))
    
    # If previous pair was x layout, must keep y (not modify y)
  
    (let ((X-Car Y-Car) (lay-pair-y-car Base
                                        (: a)
                                        Lay
                                        (last-vert> Any)))
        
      # If Car layout diff, must use the min Y bounds
            
      (unless (= (get (: a) 'lay) (: lay))
        (println "CAR X; CDR Y" 
                 (format Y-Car *Scl)
                 (format (get *cur 2) *Scl))
        (set-cur-y Y-Car))
            
      (let ((X-Cdr Y-Cdr) (lay-pair-y-cdr Base
                                          (: b)
                                          Lay))

        (list (max X-Car X-Cdr)
              (min Y-Car Y-Cdr))))))

(de lay-pair-y-car (Base Car Lay Last-Vert)

  (let ((X Y) Base)

    # Move cursor to end of car into pos
    (mov-cur-vert Last-Vert)
    
    # Set to 1 to remove space or inc etc
    (adv-cur 2.0)
    
    # Save this pos as base
    (let (Cur *cur)
    
      (mov> Car *cur)
      (mov-cur-vert (last-vert> Car))
    
      #(prin "  ") (println 'y 'a (= (get Car 'lay) Lay))
      
      (lay-part Car
                Cur
                Lay))))

(de lay-pair-y-cdr (Base Cdr Lay)

  (let ((X Y) Base)
          
    # Cursor should be in Y pos after drawing CAR recursively
    # Move X back and under
    (set-cur-x X)
        
    # Inc to double space etc
    (nl-cur)
            
    (mov> Cdr *cur)
    (mov-cur-vert (last-vert> Cdr))
      
    #(prin "  ") (println 'y 'b (= (get Cdr 'lay) Lay))
            
    # Use X as baseline since CDRs are drawn Y
    (lay-part Cdr
              (list X (get *cur 2))
              Lay)))


(de lay-atom (Any Base)
  # Recurse as needed for sym, num, etc.
  
  #(println "lay-atom"
  #         (get Any 'data)
  #         (format (get *cur 1) *Scl)
  #         (format (get *cur 2) *Scl))
    
  (when (and (= (type Any) '(+Sym))
             (get Any 'b)) 
    (lay-sym Any Base))
  
  # Symbols have their value drawn
  *cur)


(de lay-sym (Any Base)

  (mov> Any *cur)

  (with Any
  
    # Then check where to put CDR of symbol
    (if (= (: lay) 'x)
      
      # Right
      (mov-part-aft> (: b) Any)
      
      # Below
      (mov-part-bel> (: b) Any))
        
    (mov-cur-vert (last-vert> (: b)))))
