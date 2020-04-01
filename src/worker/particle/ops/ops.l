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
    #(ins-bef Ref Part)
    (ins-aft (get Ref 'c) Part)

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


(de repl-car-any (Data Lay)

  #{
    Replace car with same type
    
    [ ]  [ ]  NIL
    a    b
         *0

    Ref = b/Car
    Prv = b/Pair
    Part = x

    [ ]  [ ]  NIL
    a    x
         *0
  }#    

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Prv-Cdr (get Prv 'b)
        Prv-C (get Prv 'c)
        Part (gen-part (list Data)
                       Lay 
                       NIL
                       T))

    (when (or (not (str? Data)) (not Data))
      (println 'repl-any "WARNING! Data is not a str or NIL; x bnds will be incorrect"))

    (repl-pair Prv Part)

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


(de repl-car-list (Data Lay)
  
  #{
    Newline will call this
    Replace car with list underneath
    
    [ ]  [ ]  NIL
    a    b
         *0

    Ref = b/Car
    Prv = b/Pair
    Part = x

    [ ]  [ ]  NIL
    a    
    
         [ ]  NIL
         x
              *0
  }#    

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Part (gen-part (list (list Data)) Lay NIL T)
        Car (get Part 'a))

    (repl-pair Prv Part)

    # Delete

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


########################

(de ins-aft (Ref Part)

  #{
  
    Insert x after a:
  
    *0
    [ ]  [ ]  NIL
    a    b   

           *0
    [ ]  [ ]  [ ]  NIL
    a    x    b

    Ref=a
    Prv=NIL
    Cdr=b
    Part=x

    Newline case:
    
    *0      
    [ ]
    a
    
    [ ] - NIL
    b
    
    
    [ ]
    a
    
    *0
    [ ] - [ ] - NIL
    x     b
  
  }#

  (let (Cdr (get Ref 'b))

    # Ref -> Part
    (con> Ref Part)

    #        Part -> Cdr
    # Ref <- Part
    (con> Part Cdr Ref)

    # Part <- Cdr
    (put Cdr 'c Part)
      
    # If Cdr is a new line, xfer it to new Part
    (when (get Cdr 'nl 'p)
      (xchg-nl Cdr Part))))


(de repl-pair (Ref Part)

  #{
    Replace b with x
  
          *0
    [ ]   [ ]  [ ]  NIL
    a     b    c

          *0
    [ ]   [ ]  [ ]
    a     x    c

    Ref=b
    Cdr=c
    Prv=a
    Part=x
  }#

  (let (Cdr (get Ref 'b)
        Prv (get Ref 'c))

    # Make method of Particle
    # Ref -> Part
    (con> Prv Part)

    #        Part -> Cdr
    # Prv <- Part
    (con> Part Cdr Prv)
    
    # Part <- Cdr
    (put Cdr 'c Part)

    # If Ref is a new line, xfer it to new Part
    (when (get Ref 'nl 'p)
      (xchg-nl Ref Part))

    # Copy layout?
    # (force-lay Part (get Ref 'lay))
      
    (del> Ref T NIL)))


(de xchg-nl (Src Dst)

  (let (Nl (get Src 'nl)
        Prv (get Nl 'p)
        Nxt (get Nl 'n))

    (when Prv
      (put (get Prv 'nl) 'n Dst))

    (when Nxt
      (put (get Nxt 'nl) 'p Dst))

    (put Dst 'nl Nl))

  (put Src 'nl NIL)

  (println 'xchg-nl "Exchange line: " Src " -> " Dst)

  (setq *line Dst
        *main Dst))
