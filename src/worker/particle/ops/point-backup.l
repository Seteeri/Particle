(de point-b-prv ()
  (println 'point-b-prv)

  #(setq *on-car)

  (let (Ref (get *0 'b)
        Prv (get Ref 'c))

    (when Prv

      # If symbol

      # Set list and line
      (let? Ln (get Ref 'line 'p)
        (setq *line Ln
              *main Ln)
        (println 'point-b-prv "Mov to super-line/list" Ln (get Ln 'data)))

      (println 'point-b-prv Prv (get Prv 'data))
      (put *0 'b Prv)
      (mov-part-abv> *0 Prv (cons T)))))


(de point-b-nxt ()
  (println 'point-b-nxt)

  # If ref is NIL, aka end of list, move to superlist
  (let Ref (get *0 'b)
  
    (println Ref (get Ref 'data) (get Ref 'b))

    (let? Cdr (get Ref 'b)

      # Set line
      (when (get Cdr 'line)
        (setq *line Cdr
              *main Cdr)
        (println 'point-b-nxt "Mov to sub-line/list" *line (get *line 'data)))

      (println 'point-b-nxt Cdr (get Cdr 'data))

      (put *0 'b Cdr)

      (if *on-car
          (let Dims (bounds> Cdr NIL T)
            # Move to lower bnd of Car
            (set-cur-x (get Cdr 'ori 1))
            (set-cur-y (get Dims 2)) (nl-cur)
            (lay-part-start *0 *cur 'skip-cdr))
          (prog
            (mov-part-abv> *0 Cdr (cons T)))))))


(de point-a-out ()
  (println 'point-a-out)

  (let (Ref (get *0 'b)
        Prv (get Ref 'c))
  
    (setq *on-car)
    
    #(println Ref (get Ref 'data) (type Ref) (isa '+Nil Ref))
    
    (cond ((or (isa '+Nil Ref)
               (isa '+Nil (get Ref 'a)))
           (put *0 'b Ref)
           (mov-part-abv> *0 Ref (cons T)))
           
          ((isa '(+Pair) Ref)
           (put *0 'b Prv)
           (mov-part-abv> *0 Prv (cons T))
            
           (let? Ln (get Ref 'line 'p)
             (setq *line Ln
                   *main Ln)
             (println 'point-a-out "Mov to super-line/list" Ln (get Ln 'data))))
           
          (T # Sym
           (put *0 'b Prv)
           (mov-part-abv> *0 Prv (cons T))))))




(de point-a-in ()

  (println 'point-a-in)

  #{

  Str
  
  [+Pair]
  
  [+Str]]
  a (+Num)

  Sym
  
  [+Pair]
  
  [+Sym] 
  s (+Num)

  Ref will be either a pair or an atom  
  Pair will call sub> method, returning a Particle
  Set pointer to its origins
  }#

  (let (Ref (get *0 'b)
        Part (sub> Ref))

    (cond ((or (isa '+Nil Ref)
               (isa '+Nil (get Ref 'a)))
           (setq *on-car T)
           # Move to lower bnd of Car
           (set-cur-x (get Ref 'ori 1))
           (set-cur-y (get (bounds> Ref) 2)) (nl-cur)
           (lay-part-start *0 *cur 'skip-cdr))

          ((isa '(+Pair) Car)
           # Dont set Car flag          
           (put *0 'b Car)
           (println 'point-a-in "Mov to sub-line/list" Car (get Car 'data))
           (setq *line Car
                 *main *line)
           # Check layout
           (mov-part-abv> *0 Car (cons T)))

          (T
            (setq *on-car T)
            (put *0 'b Car)
            # Move to lower bnd of Car
            (set-cur-x (get Car 'ori 1))
            (set-cur-y (get (bounds> Car) 2)) (nl-cur)
            (lay-part-start *0 *cur 'skip-cdr)))))


#{
# Mov prv line
        (let? Cb (get *line 'c 'b)
          (setq *line (get *line 'line 'p)
                *main *line)
          (println 'point-b-nxt Cb (get Cb 'data))
          (put *0 'b Cb)
          (mov-part-abv> *0 Cb (cons T)))
}#
