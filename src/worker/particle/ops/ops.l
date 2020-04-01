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
      (lay-part-start (get List 'c 'b)
                (get *main 'ori)))
    (setq List (get List 'c))))


(de upd-superls (List)
  (while List
    (when (get List 'nl)
      (println 'upd-superls "Re-layout line: " List (get List 'data))
      (align-cur-to-bnds List)
      (lay-part-start (get List 'b)
                (get List 'ori))
      # Eh double traversal...use cursor from lay-part?
      # This is using whole line, instead do:
      # add to bnds = (- (newend - origin) (oldend/bnds- origin))
      (put List 'nl 'dims (calc-dims-nl> List)))
    (setq List (get List 'c))))


###########################


(de con-any (Data Lay)

  (if *on-car
  
      # Replacing NIL has same effect whether pointer is on Car or Pair
      # Only diff is keep pointer on Car
      (if (isa '+Nil (get *0 'b))
    
        (let Part (con-cdr Data Lay)
          (put *0 'b (get Part 'b))
          (mov-part-bel> *0 (get Part 'b) (cons T)))
          
        (repl-car-any Data Lay))
      
      (con-cdr Data Lay)))


(de con-cdr (Data Lay)

  (let (Ref (get *0 'b)
        Part (gen-part (list Data) Lay NIL T))

    # Connect Part before Ref
    # Update newline as needed
    (con-back Ref Part)

    # Since Part is before Ref, it will be in the same place
    # and everything after will move forward
    (mov-cur-part Ref 'start)
    (lay-part-start Part (get *line 'ori))

    # Update line bounds
    # Changes in X only affect Cdr of superlists
    # Changes in Y, however, affect subsequent lines, since they all need to be
    # offset by the Y change
    
    (let (Bef (get *line 'nl 'dims)
          Dx (inc-line-by-part-x> *line Part)
          Dy (upd-line-by-part-y> *line Part)
          Aft (get *line 'nl 'dims))
      (println 'con-any "Adjust line by: " (format Dx *Scl) ", " Bef " -> " Aft)
      # Update super lines/lists dims/cdrs
      # skip this line since already updated
      (upd-superls (get *line 'nl 'p)))

    (mov-*1 *line)

    # Create upd-ptr
    (put *0 'b Ref)
    (mov-part-abv> *0 Ref (cons T))

    Part))

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
    (when (get Ref 'nl 'p)

      (println 'con-back "Create newline for " Part (get Part 'data))

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
      (setq *line Part
            *main Part))))


##########################

(de repl-car-any (Data Lay)

  # Replace car with same type

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Prv-Cdr (get Prv 'b)
        Prv-C (get Prv 'c)
        Part (gen-part (list Data)
                       Lay 
                       NIL
                       T))

    #{

    [] - [] - NIL
    a    b
         *0

    Ref = b/Car
    Prv-Cdr = NIL
    Prv = b/Pair
    Prv-C = a
    Part = new

    [] - [] - NIL
    a    c
         *0
    }#
      
    (when (or (not (str? Data)) (not Data))
      (println 'repl-any "WARNING! Data is not a str or NIL; x bnds will be incorrect"))

    (repl-list Prv-C
               Prv
               Prv-Cdr
               Part)

    (if (= Lay 'x)

      (prog
                  
        (mov-cur-part Prv 'start)
        (lay-part-start Part (get *line 'ori))

        # Skip line upate
        (mov-*1 *line)
        
        # Move to next car - should always...
        # Handle NIL
        (if (isa '+Nil Prv-Cdr) 
            (prog
              (put *0 'b Prv-Cdr)
              (mov-part-bel> *0 Prv-Cdr (cons T)))
            (let Car (get Prv-Cdr 'a)
              (put *0 'b Car)
              (mov-part-bel> *0 (get Car 'a) (cons T)))))

      (prog
        (println 'repl-any "IMPLEMENT Y LAYOUT")))
        
  Part))


################################


(de repl-list (Prv Ref Cdr Part)

  #{
    Replace Ref with Part

        *
    [X] [X] [X]
    a   b   c

    * Del Ref
    * Prv 'b -> Part
    * Part 'b -> Cdr
    * Cdr 'c -> Part
    * Part 'c -> Prv
  }#

  (unless Cdr
    (setq Cdr (get Part 'b)))

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


# Newline will call this
(de repl-car-list (Data Lay)

  # Replace car with list underneath

  (let (Ref (get *0 'b)
        Cdr (get Ref 'b)
        Prv (get Ref 'c)
        Prv-Cdr (get Prv 'b)
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
    
    [] - [] - NIL
    a    
         [] - NIL
         x    
              *0

    }#

    # Note, ref is Car so get Prv
    (repl-list Prv-C
               Prv
               Prv-Cdr
               Part)

    (if (= Lay 'x)

        (prog
        
          # Draw Pair at Prv, no Car/Cdr
          # Then nl, draw Car
          (mov-cur-part Prv 'start)
          (lay-part-start Part (get *line 'ori) 'skip-car-cdr)

          # Create nl for Car
          # Layout will detect this
          # Line bnds is not used so NIL is ok for now
          (put Car 'nl (new '(+Line)
                            *line))

          # Cur will be moved to proper pos rel to Pair
          (lay-part-start Car *cur)

          # After layout, calc dims for newline
          (put Car 'nl 'dims (calc-dims-nl> Car))
          
          (mov-*1 Car)
          
          (upd-superls (get Car 'nl 'p))

          # Move ptr to Cdr/NIL; must set ptr to Part to get Car
          (put *0 'b (get Car 'b))
          (mov-part-abv> *0 (get Car 'b) (cons T))

          (setq *line Car
                *main Car))

        (prog
          (println 'repl-car-list "IMPLEMENT Y")))))
