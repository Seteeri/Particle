# Move to layout

# DEBUG
(de mov-*1 (Line)
  # Move to bnds of current line
  (let (Ori (origin> Line)
        Bnds (get Line 'nl 'dims))
    (set-cur-x (+ (get Ori 1)
                  (get Bnds 1)))
    (set-cur-y (- (get Ori 2)
                  (get Bnds 2)))
    (println 'mov-*1 "Moved *1 to: " (format (get *cur 1) *Scl) (format (get *cur 2) *Scl))
    (mov> *1 *cur)))

########################

(de upd-superlines (Line Dx)
  (while Line 
    (with Line
      (=: dims (list (+ (car (: dims)) Dx)
                     (cadr (: dims))))
      (setq Line (: p)))))
    

(de upd-superlists (List)
  # Adjust bounds of each line also
  (while List
    (when (= (get List 'c 'a) List)
      (align-cur-to-bnds (get List 'c))
      (lay-part (get List 'c 'b)
                (origin> *main)))
    (setq List (get List 'c))))
    
    
###########################

(de con-any (Data Lay Repl)
      
  (if Repl
  
      (repl-car Data Lay)

      (let (Ref (get *0 'b)
            Part (gen-part (list Data) Lay NIL T))
        
        # Connect Part before Ref
        # Update newline as needed
        (con-back Ref Part)

        # Since Part is before Ref, it will be in the same place
        # and everything after will move forward
        (mov-cur-part Ref 'start)
        (lay-part Part (origin> *line))
        
        # Adjust line bounds
        (println 'con-any "Part has nl" Part (get Part 'data) (get Part 'nl 'dims))
        (let (Dx (inc-line-by-part-x> *line Part)
              Dy (upd-line-by-part-y> *line Part))
          #(upd-superlines (get *line 'nl 'p) Dx)
          (println 'con-any "Adjust line by: " (format Dx *Scl)))
        
        (mov-*1 *line)
        
        # Update superlists
        # Assume bounds corresponds...
        (upd-superlists Part)
        
        # Create upd-ptr
        (put *0 'b Ref)
        (mov-part-abv> *0 Ref (cons T))
        
        Part)))


(de con-back (Ref Part)
  #{  
          *0
    [ ] - [ ] - [ ] - NIL
    a     b     c
    
    TO
    
          *0
    [ ] - [ ] - [ ] - [ ] - NIL
    a     d     b     c
    
    Ref=b
    Prv=a
    Cdr=c
    Part=d
  }#

  (let (Prv (get Ref 'c)
        Prv-Cdr (get Prv 'b))
                      
    # Handle car
    (if (= (get Prv 'a) Ref)
        (prog
          (put Prv 'a Part)
          (put Prv 'b Prv-Cdr))
        (put Prv 'b Part))

    # d <- b
    (put Ref 'c Part)

    # conn d -> b data
    (with Part
      (=: c Prv)    
      (=: b Ref)
      (con (car (:: data)) (get Ref 'data)))
                
    # Remember, *main's value is always the particle itself
    # The value of the symbol *main is actually 'b    
    (unless (isa '+Sym Prv)
      # a -> d
      (with Prv
        (con (car (:: data)) (get Part 'data))))
        
    # Set nl if Ref has it; toggle Ref
    (when (get Ref 'nl)
      
      (let (Nl (get Ref 'nl)
            Prv (get Nl 'p)
            Nxt (get Nl 'n))
            
        (when Prv
          (with (get Prv 'nl)
            (=: n Part)))
          
        (when Nxt
          (with (get Nxt 'nl)
            (=: p Part))))
      
      (put Part 'nl (get Ref 'nl))
      (put Ref 'nl NIL)
      
      # Set newline also
      (println 'con-back " *line change: " Ref " -> " Part)
      (setq *line Part))))
      

(de repl-list (Prv Ref Cdr Part)
  
  #{
    Replace Ref with Part
  
    [X] [X] [X]
    a   b   c
    
    * Del Ref
    * Prv 'b -> Part
    * Part 'b -> Cdr
    * Cdr 'c -> Part
    * Part 'c -> Prv 
  }#
    
  # Connect Prv-Part
  (with Prv
    (=: b Part)
    (unless (isa '+Sym Prv)
      (con (car (:: data)) (get Part 'data))))
  
  # Connect Part-Cdr
  (with Cdr
    (=: c Part))
  
  (with Part
    (=: b Cdr)
    (=: c Prv)
    (con (car (:: data)) (get Cdr 'data)))

  # Set nl if Ref has it; toggle Ref
  (when (get Ref 'nl)
    (put Part 'nl (get Ref 'nl))
    (put Ref 'nl))
    
  # Copy lay
  (force-lay Part (get Ref 'lay))

  # Do not delete CDR
  (del> Ref T NIL))


(de repl-car (Data Lay)

  (let (Ref (get *0 'b)
        Cdr (get Ref 'b)  
        Prv (get Ref 'c)
        Prv-B (get Prv 'b)
        Prv-C (get Prv 'c)
        Part (gen-part (list (list Data)) Lay NIL T)
        Car (get Part 'a))
        
    #{
    
    [] - [] - NIL
    a    b
         *0
         
    Cdr = NIL
    Prv = []b
    Prv-C = []a
    
    }#
          
    # Note, ref is Car so get Prv
    (repl-list Prv-C
               Prv
               Prv-B
               Part)
    
    (if (= Lay 'x)
    
        (prog
          # Draw Pair at Prv, no Car/Cdr
          # Then nl, draw Car
          (mov-cur-part Prv 'start)
          (lay-part Part (origin> *main) 'skip-car-cdr)
          
          # Adjust line bounds specifically to Part
          (adj-line-to-part> *line Part)
          (mov-*1 *line)          
                    
          # Create nl for Car
          #   This a new list so Prv will be Nil
          (put Car 'nl (new '(+Line) 
                            NIL
                            NIL
                            (get Car 'a 'dims)))
                      
          # Move Car to y bounds
          # Cur will be overriden
          (lay-part Car *cur)
                    
          # Move ptr to Cdr/NIL; must set ptr to Part to get Car
          (put *0 'b (get Car 'b))
          (mov-part-abv> *0 (get Car 'b) (cons T))
          
          (setq *line Car)
          (setq *main Car))
          
        (prog
          (println 'con-any "IMPLEMENT Y LAYOUT")))))


################################################################################

(de point-b-prv ()
  (println 'point-b-prv)

  (let (Ref (get *0 'b)
        Prv (get Ref 'c))
        
    (when Prv
    
      # Set list
      (when (= (type (get Prv 'a)) '(+Pair))
        (println 'point-a-out "Mov to superlist")
        (setq *main Prv))
        
      # Set line when Ref has nl
      (when (get Ref 'nl)
        (mov-line-p> Ref)
        (println 'point-a-out "Mov to line"))
    
      (println 'point-a-out Prv (get Prv 'data))
      (put *0 'b Prv)
      (mov-part-abv> *0 Prv (cons T)))))


(de point-b-nxt ()
  (println 'point-b-nxt)

  (let? Cdr (get (get *0 'b) 'b)
  
    # Set line
    (when (get Cdr 'nl)
      (setq *line Cdr)
      (println 'point-a-out "Mov to line"))
  
    (println 'point-b-nxt Cdr (get Cdr 'data))
    (put *0 'b Cdr)
    (mov-part-abv> *0 Cdr (cons T))))


(de point-a-out ()
  (println 'point-a-out)
    
  (point-b-prv))


(de point-a-in ()

  (println 'point-a-in)

  # TODO:
  # * Support mov to NIL
  # * Check line change

  (let (Ref (get *0 'b)
        Car (get Ref 'a))
    
    (when (and Car
               (not (isa '+Sym Ref)))
    
      (put *0 'b Car)
      
      (let Dims (bounds> Car)

        # Set list
        (if (= (type Car) '(+Pair))

          (prog
            (println 'point-a-in "Mov to sublist")
            (setq *main Car)
            (mov-part-abv> *0 Car (cons T)))

          (if (= (get Ref 'lay) 'x)
          
              (prog
                # Move to lower bnd of Car
                (set-cur-x (get (origin> Car) 1))
                (set-cur-y (get Dims 2)) (nl-cur)
                (lay-part *0 *cur 'skip-cdr))
                
              (prog
                # Move above Car
                (mov-part-abv> *0 Car (cons T)))))))))


#######################

#{
  Find list

  (let Part (get (get *0 'b) 'c)
    (loop
      (T (or (isa '+Pair (get Part 'a))
             (= Part *main)
             (not (get Part 'c))))
      (setq Part (get Part 'c)))
}#
