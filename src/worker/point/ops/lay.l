(def '*bnds-x)
(def '*bnds-y)
(def '*line-lay)


(de upd-tree (List)
  (unless List (setq List *master))
  (mov-cur-ori List)
  (lay-part-start List List))


(de upd-cur ()
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

  (setq *bnds-x (if *bnds-x
                    (max @ (car *cur))
                    (car *cur))
        *bnds-y (if *bnds-y
                    (min @ (cadr *cur))
                    (cadr *cur))))

              
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
  (lay-pair Part Skip))


(de lay-pair (Part Skip)

  # Iterative pre order traversal

  # Draw pair
  # Draw cars, deepest first
  # Draw cdr, deepest first

  # Push root
  (let L ()
    
    (push 'L (cons Part 'a))
    
    # Proccess root
    (while (setq Part (pop 'L))
      
      (let (Rel (cdr Part)
            Part (car Part))
        
        (when NIL
          (println 'lay-pair 
                   Part
                   (get Part 'data)
                   '...
                   (if (= Rel 'a) 'Car 'Cdr)
                   'of (get Part 'c) (get Part 'c 'data)))
                 
        (if (lay-x?> Part)
            (lay-pair-x Part Rel)
            (lay-pair-y Part Rel))
    
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


(de lay-pair-x (Part Rel)

  (when (= Rel 'b)
  
    # For non-newline Cdr:
    # X = bnds of Car = cur-x
    # Y = origin of Pair
    #
    # Newline will override
    
    # Don't bother since Cdr of Y always has newline
    # (if (= (get Part 'c 'lay) 'x)
    (mov-cur *bnds-x (get Part 'c 'ori 2))
    (adj-cur-x 2.0)
    
    #(println 'lay-pair-x "Mov cur x to *bnds-x: " (format (car *cur) *Scl))
    #(println 'lay-pair-x "Mov cur y to origin: " (format (cadr *cur) *Scl))
    
    )

  #(println 'lay-pair-x Part (type Part) (get Part 'data) Rel)
    
  (lay-x> Part))
  

(de lay-pair-y (Part Rel)

  (when (= Rel 'b)
    # For non-newline Cdr:
    # X = origin of Pair
    # Y = bnds of Car
    
    # Newline will override...

    # or check for pair
    (if (lay-x?> (get Part 'c))
        (prog
          # X should be in place, else get from cur bnds
          (mov-cur *bnds-x (get Part 'c 'ori 2))
          (adj-cur-x))

        (prog
          # Y should be in place, else get from cur bnds
          (mov-cur (get Part 'c 'ori 1) *bnds-y)
          (adj-cur-y (+ 1.0 *sublist-space-y)))))

  #(println 'lay-pair-y Part (get Part 'data) Rel)          
          
  (lay-y> Part))

