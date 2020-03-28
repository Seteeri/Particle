# Move to layout

# DEBUG
(de mov-*1 (Line)
  # Move to bnds of current line
  (let (Ori (get-origin> Line)
        Bnds (get (get Line 'nl) 'bnds))
    (set-cur-x (+ (get Ori 1)
                  (get Bnds 1)))
    (set-cur-y (+ (get Ori 2)
                  (get Bnds 2)))
    (mov> *1 *cur)))
  

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
      

(de upd-line-bnds (Part)
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


###########################

(de con-any (Data Lay Repl)
        
    (if Repl
    
        (repl-car Data Lay)

        (let (Ref (val> *0)
              Cdr (get Ref 'b)  
              Prv (get Ref 'c)
              Prv-B (get Prv 'b)
              Prv-C (get Prv 'c)
              Part (gen-part (list Data) Lay NIL T))
              
          (con-back Prv Ref Part)

          # First must pos Part where Ref was
          (mov-cur-part Ref 'start)
          (lay-part Part (get-origin> *main) 'skip-cdr)
          
          # If Part's ymin < list ymin, must update list bnds
          # before updating Part+
          #(upd-main-bnds Part)
          
          # Update line bounds also
          (upd-line-bnds Part)
                    
          # Align cursor to bnds of Prv
          # Ideally, should call lay-part on any Part and it works
          (base-cur Part)
          (lay-part (get Part 'b) (get-origin> *main))
              
          # Update superlists
          #(upd-line-main)
          
          # Create upd-ptr
          (put *0 'b Ref)
          (mov-part-abv> *0 Ref (cons T))
          
          Part)))


(de repl-car (Data Lay)

  (let (Ref (val> *0)
        Cdr (get Ref 'b)  
        Prv (get Ref 'c)
        Prv-B (get Prv 'b)
        Prv-C (get Prv 'c)
        Part (gen-part (list (list Data)) Lay NIL T))
        
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
          
          # Move to y bounds; set nl flag
          (let Dims (calc-bounds> Prv-C NIL T)
            (setq *cur Dims)
            (adv-cur 2.0)
            (nl-cur *sublist-space-y)
            (lay-part (get Part 'a) (get-origin> *main)))
          
          # Move ptr to Cdr/NIL; must set ptr to Part to get Car
          (put *0 'b (get (get Part 'a) 'b))
          (mov-part-abv> *0 (get (get Part 'a) 'b) (cons T)))
          
        (prog
          (println 'con-any "IMPLEMENT Y LAYOUT")))

    # Update lists...
    # 1. Calc min y for current line -> use *main bnds
    # 2. Update Part Cdr

    # Poss reuse make-nl

    # Set newline prop
    (put (get Part 'a)
         'nl
         (cons *main NIL))
    
    (setq *main Part)
    (with *main (=: bnds (copy (get (: a) 'dims))))))


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

  (let (Ref (val> *0)
        Prv (get (val> *0) 'c))
        
    (when Prv
    
      # Set list
      (when (= (type (get Prv 'a)) '(+Pair))
        (println 'point-a-out "Mov to superlist")
        (setq *main Prv))
    
      (println 'point-a-out Prv (get Prv 'data))
      (put *0 'b Prv)
      (mov-part-abv> *0 Prv (cons T)))))


(de point-b-nxt ()
  (println 'point-b-nxt)

  (let? Cdr (get (val> *0) 'b)
    (println 'point-b-nxt Cdr (get Cdr 'data))
    (put *0 'b Cdr)
    (mov-part-abv> *0 Cdr (cons T))))


(de point-a-out ()
  (println 'point-a-out)
    
  (point-b-prv))


(de point-a-in ()

  (println 'point-a-in)

  # TODO: Support mov to NIL

  (let (Ref (val> *0)
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

  (let Part (get (val> *0) 'c)
    (loop
      (T (or (isa '+Pair (get Part 'a))
             (= Part *main)
             (not (get Part 'c))))
      (setq Part (get Part 'c)))
}#
