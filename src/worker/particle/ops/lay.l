(def '*bnds-x)
(def '*bnds-y)
(def '*line-lay)


(de upd-tree (List)

  (setq *cur (get *master 'ori))
  (lay-part-start *master (get *master 'b)))    


(de upd-cur ()
  (setq *bnds-x (if *bnds-x
                   (max @ (car *cur))
                   (car *cur))
        *bnds-y (if *bnds-y
                   (min @ (cadr *cur))
                   (cadr *cur))))

              
(de upd-cur-debug ()

  #{
  
   O
    [X] CDR|
    CAR    |
    -------+

   O
    [X] CAR|
    CDR    |
    -------+          
  }#
  
  (let (X *bnds-x
        Y *bnds-y)
        
    (setq *bnds-x (if *bnds-x
                    (max @ (get *cur 1))
                    (get *cur 1))
          *bnds-y (if *bnds-y
                    (min @ (get *cur 2))
                    (get *cur 2)))
    
    (when (or (not (= X *bnds-x))
              (not (= Y *bnds-y)))
                            
      (println 'upd-cur
              (format X *Scl) '-> (format *bnds-x *Scl)
              (format Y *Scl) '-> (format *bnds-y *Scl)))))            
              
              
(de upd-dims (Part)
  (with Part
    (let Dims (list (abs (- *bnds-x (car (: ori))))
                    (abs (- *bnds-y (cadr (: ori)))))
      #(=: dims Dims)
      (when (: line)
        (put (: line) 'dims Dims)))))


(de lay-part-start (Part Line Skip)
  #{
    Only apply skip to immediate Part
    'skip-car
    'skip-cdr
    'skip-car-cdr
  }#
  (setq *bnds-x NIL
        *bnds-y NIL
        *line-lay Line)
  (lay-pair Part Line Skip))


(de lay-pair (Part Line Skip)

  # Iterative pre order traversal

  # Draw pair
  # Draw cars, deepest first
  # Draw cdr, deepest first

  # Push root
  (let L ()
    
    (push 'L (cons Part 'a))
    
    # Proccess root
    (while (setq Part (pop 'L))
      
      (let (Type (cdr Part)
            Part (car Part))
        
        (when NIL
          (println 'lay-pair 
                   Part
                   (get Part 'data)
                   '...
                   (if (= Type 'a) 'Car 'Cdr)
                   'of (get Part 'c) (get Part 'c 'data)))
                 
        (if (lay-x?> Part)
            (lay-pair-x Part Type)
            (lay-pair-y Part Type))
    
        (unless (= Skip 'skip-car-cdr)
            
          # Push right first
          (let B (get Part 'b)
            (if (= Skip 'skip-cdr)
                (off Skip)
                (unless (or (= B Part) (not B))
                  (push 'L (cons B 'b)))))
        
          (let A (get Part 'a)
            (if (= Skip 'skip-car)
                (off Skip)
                (unless (or (= A Part) (not A))
                  (push 'L (cons A 'a))))))))))


(de lay-pair-x (Part Type)

  (when (= Type 'b)
  
    # For non-newline Cdr:
    # X = bnds of Car = cur-x
    # Y = origin of Pair
    #
    # Newline will override
    
    # Don't bother since Cdr of Y always has newline
    # (if (= (get Part 'c 'lay) 'x)
    (set-cur-x *bnds-x) (adj-cur-x 2.0)
    (set-cur-y (get Part 'c 'ori 2))
    
    #(println 'lay-pair-x "Mov cur x to *bnds-x: " (format (car *cur) *Scl))
    #(println 'lay-pair-x "Mov cur y to origin: " (format (cadr *cur) *Scl))
    
    )

  #(println 'lay-pair-x Part (type Part) (get Part 'data) Type)
    
  (lay-x> Part))
  

(de lay-pair-y (Part Type)

  (when (= Type 'b)
    # For non-newline Cdr:
    # X = origin of Pair
    # Y = bnds of Car
    
    # Newline will override...

    # or check for pair
    (if (lay-x?> (get Part 'c))
        (prog
          # X should be in place, else get from cur bnds
          (set-cur-x *bnds-x) (adj-cur-x)
          (set-cur-y (get Part 'c 'ori 2)))

        (prog
          (set-cur-x (get Part 'c 'ori 1))
          # Y should be in place, else get from cur bnds
          (set-cur-y *bnds-y) (adj-cur-y (+ 1.0 *sublist-space-y)))))

  #(println 'lay-pair-y Part (get Part 'data) Type)          
          
  (lay-y> Part))

