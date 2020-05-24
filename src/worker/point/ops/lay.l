(def '*bnds-x)
(def '*bnds-y)
(def '*line-lay)
(def '*list-lay)

(de upd-tree (List)
  (unless List (setq List *master))
  (mov-cur-ori List)
  (lay-part-start List List))

  
(de upd-list-lay (End)
  
  # Update head of list
  (when (caar *list-lay)
  
    # Cache end
    (put (get @ 'line) 'e End)

    # Update list dims from cur
    (upd-lst-dims)
    
    # Update current list on pop
    # instead of after every item

    #(upd-dims-2> @ (cadar *list-lay))
    
    (println 'upd-list-lay (caar *list-lay)
      (format (car (cadar *list-lay)) *Scl)
      (format (cadr (cadar *list-lay)) *Scl))
      
    ))
    
        
(de pop-list-lay ()

  (let? A (pop '*list-lay)

    (let (Dims-A (cadr A)
          B (car *list-lay)
          Dims-B (cadr B))
      
      # Don't use upd-dims-2>
      # until final
      (when (car B)
        (set Dims-B (max (car Dims-B)
                         (car Dims-A)))
        (set (cdr Dims-B) (min (cadr Dims-B)
                                (cadr Dims-A))))
    
      # Set dims to list
      # Already called upd-lst-dims
      (upd-dims-2> (car A) Dims-A)
    
      (println 'pop-list-lay "Pop list: " 
        A (car (get (car A) 'data)))
      
    )))
  

(de upd-lst-dims ()
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
  
  (let Bnds (cadr (car *list-lay))
    (set Bnds (max (car Bnds)
                   (car *cur)))
    (set (cdr Bnds) (min (cadr Bnds)
                         (cadr *cur)))))
  
  
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
        *line-lay Line
        *list-lay NIL)
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
                
        (prinl)
        (println 'lay-pair Part Rel (type Part) (get Part 'data)) 
                
        (when (= Rel 'b)
          (lay-pair-pre Part))
                
        (if (lay-x?> Part)
            (lay-x> Part)
            (lay-y> Part))
    
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

                  
(de lay-pair-pre (Part)
        
  (let? C (get Part 'c)
  
    (let (Ori (get C 'ori)
          Dims (get C 'dims)) # or line?

      (when NIL
        (prin "  ") (println 'lay-pair-pre Part Rel
          "C: " C (get C 'data))
        
        (prin "  ") (println 'lay-pair-pre Part Rel
          "Dims of C: " (format (car Dims) *Scl)
                        (format (cadr Dims) *Scl)))
          
      (if (lay-x?> C)

          # If Pair get from Car
          # If Sym (exp) get from Val        

          (let (Car (get C 'a)
                Val (get Car 'b)
                X (if (is-sym? C)
                      (if Val # Get from Cdr else Car
                          (+ (get Val 'ori 1) (get Val 'dims 1))
                          (+ (get Car 'ori 1) (get Car 'dims 1)))
                      (if Val # Get from Cdr else Car
                          (+ (get Val 'ori 1) (get Val 'dims 1))
                          (+ (get Car 'ori 1) (get Car 'dims 1)))))
            
            (when NIL
              (prin "  ") (println 'lay-pair-pre Part Rel
                "...Sym: " Car (get Car 'data) (get Car 'dims))
              (prin "  ") (println 'lay-pair-pre Part Rel
                "Sym Car: " (get Car 'a) (get Car 'a 'data) (get Car 'a 'dims))
              (prin "  ") (println 'lay-pair-pre Part Rel
                "Sym Cdr: " Val (car (get Val 'data)) (get Val 'dims))
              (prin "  ") (println 'lay-pair-pre Part Rel
                "X: " (format X *Scl)))
              
            (mov-cur X
                     (cadr Ori))
            (adj-cur-x 2.0))
          
          (prog
            # Position under
            (mov-cur (car Ori)
                      (- (cadr Ori) (cadr Dims)))
            (adj-cur-y (+ 1.0 *sublist-space-y)))))))
