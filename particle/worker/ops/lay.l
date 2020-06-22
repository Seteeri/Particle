(def '*dims-lay)
(def '*line-lay)
(def '*list-lay)


(de lay-*start ()
  (mov-cur 0.0 0.0)
  (lay-pt-start '*start '*start T T))


(de lay-pt-start (Po Ln Lay-A Lay-B)
  (setq *dims-lay *cur
        *line-lay (lup-pt Ln)
        *list-lay (list NIL T))
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
    
      (let ((Any Reg Pt-C) Any
            Po (if Any
                   (lup-pt Any)
                   (if (= Reg 'a)
                       (get Pt-C 'a)
                       (get Pt-C 'b))))
        
        #(println (h64 (adr Any)) (adr Any) Any Po (type Po))
        #(println 'lay-pair Any Po (type Po) "A:" (get Po 'a) "B:" (get Po 'b))
        ~(assert Po)
        
        # Draw Pair
        # Pair will position cur for car
        #   +Int will pos and draw car itself
        # Get next item
        
        # Must do this here
        # Cannot guarantee Cdr will be drawn next (after lay>)
        # if Car is deep
        (when (and (= Reg 'b) Pt-C)
          # Regies on car
          # If not drawn, use Pt-C dims
          (if Lay-A
              (pos-cur-cdr> Pt-C)
              (prog
                (pos-cur-y-car> Pt-C))))
        
                
        # Lay should return next obj to draw
        (let N (lay> Po Lay-A Lay-B)
          (when (and (car N) Lay-B)
            #(prin "  lay-pair  Push CDR: ") (println (car N))
            (push 'L (car N)))
          (when (and (cdr N) Lay-A)
            #(prin "  lay-pair  Push CAR: ") (println (cdr N))
            (push 'L (cdr N))))))))
            

(de upd-dims-lay (Dims Cur)
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
  
  (set Dims (max (car Dims)
                 (car Cur)))
  (set (cdr Dims) (min (cadr Dims)
                       (cadr Cur))))
