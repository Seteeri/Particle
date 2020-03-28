# Move to layout

# DEBUG
(de mov-*1 (Line)
  # Move to bnds of current line
  (let (Ori (get-origin> Line)
        Bnds (get Line 'nl 'bnds))
    (set-cur-x (+ (get Ori 1)
                  (get Bnds 1)))
    (set-cur-y (+ (get Ori 2)
                  (get Bnds 2)))
    (mov> *1 *cur)))

########################
  

(de upd-main-bnds (Part)
  # Everytime an item is added to a line, upd its bounds
  # Subtract space between
  
  (let (Ori (get-origin> (get Part 'c))
        Bnds (calc-bounds> Part NIL T)
        Dx (- (get Bnds 1)
              (get Ori 1)
              *adv-vert))
    (with *main
      (let ((X Y) (: bnds))
        (=: bnds (list (+ X Dx)
                       (min Y (get Bnds 2))))))))
      

(de adj-line-to-part (Part)
  # Update until EOL
  
  (let (Ori (get-origin> (get Part 'c))
        Bnds (calc-bounds> Part NIL T)
        Dx (- (get Bnds 1)
              (get Ori 1)
              *adv-vert))
    (with (get *line 'nl)
      (let ((X Y) (: bnds))
        (=: bnds (list (+ X Dx)
                       (min Y (get Bnds 2))))))))

(de upd-super (Ref)  
  # Recurse
  (while (setq Super (get Ref 'c))
    (align-cur-to-bnds Super)
    (lay-part (get Super 'b)
              (get-origin> *main))))
    
###########################

(de con-any (Data Lay Repl)
      
  (if Repl
  
      (repl-car Data Lay)

      (let (Ref (get *0 'b)
            Cdr (get Ref 'b)  
            Prv (get Ref 'c)
            Part (gen-part (list Data) Lay NIL T))
            
        (con-back Prv Ref Part)

        # First must pos Part where Ref was
        (mov-cur-part Ref 'start)
        (lay-part Part (get-origin> *main) 'skip-cdr)
        
        # If Part's ymin < list ymin, must update list bnds
        # before updating Part+
        
        # Adjust line bounds specifically to Part
        (adj-line-to-part Part)
                  
        # Align cursor to bnds of Prv
        # Ideally, should call lay-part on any Part and it works
        (align-cur-to-bnds Part)
        (lay-part (get Part 'b) (get-origin> *main))
            
        # Update superlists
        #(upd-line-main)
        
        # Create upd-ptr
        (put *0 'b Ref)
        (mov-part-abv> *0 Ref (cons T))
        
        Part)))


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
          (lay-part Part (get-origin> *main) 'skip-car-cdr)
                    
          # Create nl for Car
          #   This a new list so Prv will be Nil
          (put Car 'nl (new '(+Line) 
                            NIL
                            NIL
                            (get Car 'a 'dims)))
          
          # MAKE ALIGN FN
          # Pos cur at Prv-C bounds
          (align-cur-to-bnds Prv-C)
          (nl-cur *sublist-space-y)
            
          # Move Car to y bounds
          # Base will be origin of Part          
          (lay-part (get Part 'a) (get-origin> *main))
                    
          # Move ptr to Cdr/NIL; must set ptr to Part to get Car
          (put *0 'b (get Part 'a 'b))
          (mov-part-abv> *0 (get Part 'a 'b) (cons T))
          
          (setq *line Car)
          (setq *main Car))
          
        (prog
          (println 'con-any "IMPLEMENT Y LAYOUT")))))


(de con-back (Prv Ref Part)
  #{  
                *0
    [ ] - [ ] - NIL
    a     b
    
    TO
    
                      *0
    [ ] - [ ] - [ ] - NIL
    a     b     c
    
  }#

  (put Prv 'b Part)    
  (put Ref 'c Part)    
  
  (with Part
    (=: b Ref)
    (=: c Prv))
  
  # Set nl if Ref has it; toggle Ref
  (when (get Ref 'nl)
    (put Part 'nl (get Ref 'nl))
    (put Ref 'nl))

  # Copy lay
  #(force-lay Part (get Ref 'lay))
      
  # Connect new part data to list part data
  # Must conn Cdr also (get (get Ref 'b) 'data)
  
  # Remember, *main's value is always the particle itself
  # The value of the symbol *main is actually 'b    
  
  (unless (isa '+Sym Prv)
    (with Prv
      (con (car (:: data)) (get Part 'data)))))
      

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


################################################################################

(de point-b-prv ()
  (println 'point-b-prv)

  (let (Ref (get *0 'b)
        Prv (get (get *0 'b) 'c))
        
    (when Prv
    
      # Set list
      (when (= (type (get Prv 'a)) '(+Pair))
        (println 'point-a-out "Mov to superlist")
        (setq *main Prv))
        
      # Set line
      (when (mov-line-p> Ref)
        (println 'point-a-out "Mov to line"))
        
    
      (println 'point-a-out Prv (get Prv 'data))
      (put *0 'b Prv)
      (mov-part-abv> *0 Prv (cons T)))))


(de point-b-nxt ()
  (println 'point-b-nxt)

  (let? Cdr (get (get *0 'b) 'b)
  
    # Set line
    (when (mov-line-p> Cdr)
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
      
      (let Dims (calc-bounds> Car)

        # Set list
        (if (= (type Car) '(+Pair))

          (prog
            (println 'point-a-in "Mov to sublist")
            (setq *main Car)
            (mov-part-abv> *0 Car (cons T)))

          (if (= (get Ref 'lay) 'x)
          
              (prog
                # Move to lower bnd of Car
                (set-cur-x (get (get-origin> Car) 1))
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
