(def '*bnds-x)
(def '*bnds-y)
(def '*line-lay)
(def '*list-lay)


(de lay-pt-start (Po Ln Lay-A Lay-B)
  (setq *bnds-x NIL
        *bnds-y NIL
        *line-lay (lup-pt Ln)
        *list-lay NIL)
  (lay-pair Po Lay-A Lay-B))

  
(de lay-pair (Any Lay-A Lay-B)

  # Iterative pre order traversal
  #
  # Draw pair
  # Draw cars, deepest first
  # Draw cdr, deepest first
  
  # Lay only applies to root

  # Push root
  (let L (list (list Any))
  
    (while (setq Any (pop 'L))

      #(println Any)
    
      (let ((Any Rel Po-C) Any
            Po (if Any
                   (lup-pt Any)
                   (if (= Rel 'a)
                       (get Po-C 'a)
                       (get Po-C 'b))))
        
        #(println (h64 (adr Any)) (adr Any) Any Po (type Po))
        (println 'lay-pair Any Po (type Po) "A:" (get Po 'a) "B:" (get Po 'b))
        ~(assert Po)
        
        # Draw Pair
        # Pair will position cur for car
        #   +Int will pos and draw car itself
        # Get next item
        
        # Must do this here
        # Cannot guarantee Cdr will be drawn next (after lay>)
        # if Car is deep
        (when (and (= Rel 'b) Po-C)
          # Relies on car
          # If not drawn, use Po-C dims
          (if Lay-A
              (pos-cur-cdr> Po-C)
              (prog
                (pos-cur-car-y> Po-C))))
        
                
        # Lay should return next obj to draw
        (let N (lay> Po Lay-A Lay-B)
          (when (and (car N) Lay-B)
            #(prin "  lay-pair  Push CDR: ") (println (car N))
            (push 'L (car N)))
          (when (and (cdr N) Lay-A)
            #(prin "  lay-pair  Push CAR: ") (println (cdr N))
            (push 'L (cdr N))))))))

                    
###################

(de upd-tree ()
  (mov-cur 0.0 0.0)
  (lay-pt-start '*start '*start T T))

        
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
    
      (when NIL
        (println 'pop-list-lay "Pop list: " 
          A (car (get (car A) 'data)))))))
  

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
