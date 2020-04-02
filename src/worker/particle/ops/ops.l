(de con-any (Data Lay)

  (if *on-car
  
      (if (isa '(+Nil) (get *0 'b))
      
          (prog
            # For NIL ins-cdr
            # Diff being ptr points to Car of new item
            (ins-cdr Data Lay)
            (point-prv))
          
          (repl-car Data Lay))
      
      #(has-car-pair> (get *0 'b 'c)) # If direct sublist
      (if (is-sublist> (get *0 'b))
      
          (repl-pair Data Lay)
      
          (ins-cdr Data Lay))))


(de repl-pair (Data Lay)

  # Replace entire item with list item
  # For beginning of list

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Cdr (get Ref 'b)
        Part (gen-part (list (list Data)) Lay NIL T)
        Car (get Part 'a))
    
    # Txfer line before Ref gets deleted
    (xchg-nl Ref Car)

    (repl-pair Prv Part)

    # Relink to Cdr
    (con> Car Cdr)
    (put Cdr 'c Car)
    
    # If new particle and newline, origin will not have been set
    # However, newline will override *cur/Base
    (mov-cur-part Prv 'start)
    (lay-part-start Part *cur)

    (put Car 'line 'dims (calc-dims-nl> Car)) (mov-*1 Car)
    (upd-superls (get Car 'line 'p))
    
    (put *0 'b Car)
    (point-nxt)
    (test (= (get *0 'b) (get Car 'b)))
        
    Part))


(de ins-cdr (Data Lay)

  # Insert item before pair ptr

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Part (gen-part (list Data) Lay NIL T))

    (println 'ins-cdr "Ins-aft (bef Ptr): " Prv (get Prv 'data))

    (ins-aft Prv Part)
    
    (mov-cur-part Ref 'start)
    (lay-part-start Part (get *line 'ori))

    # Update line bounds
    # Changes in X only affect Cdr of superlists
    # Changes in Y, however, affect subsequent lines, since they all need to be
    # offset by the Y change
    
    (let (Bef (get *line 'line 'dims)
          Dx (inc-line-by-part-x> *line Part)
          Dy (upd-line-by-part-y> *line Part)
          Aft (get *line 'line 'dims))
      (println 'con-any "Adjust line by: " (format Dx *Scl) ", " Bef " -> " Aft)
      # Update super lines/lists dims/cdrs
      # skip this line since already updated
      (upd-superls (get *line 'line 'p)))

    (mov-*1 *line)

    # Same ref, relayout
    (mov-cur-abv Ref)
    (lay-part-start *0 *cur 'skip-cdr)

    Part))


(de repl-car (Data Lay)

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
        # Handle Y layout
        (println 'repl-car Prv-Cdr (get Prv-Cdr 'data) (type Prv-Cdr))
        (if (or (isa '+Nil Prv-Cdr) (isa '+Nil (get Prv-Cdr 'a)))
            (let Dims (bounds> Prv-Cdr NIL T)
              # Move to lower bnd of Car
              (put *0 'b Prv-Cdr)
              (set-cur-x (get Prv-Cdr 'ori 1))
              (set-cur-y (get Dims 2)) (nl-cur)
              (lay-part-start *0 *cur 'skip-cdr))
            (let Car (get Prv-Cdr 'a)
              (put *0 'b Car)
              (mov-part-bel> *0 (get Car 'a) (cons T)))))

      (prog
        (println 'repl-any "IMPLEMENT Y LAYOUT")))
        
  Part))


(de repl-car-list (Data Lay)
  
  #{
    Replace car with list underneath
    Implies new line
    
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
          (put Car 'line (new '(+Line)
                            *line))

          # Cur will be moved to proper pos rel to Pair
          (lay-part-start Car *cur)

          # After layout, calc dims for newline
          (put Car 'line 'dims (calc-dims-nl> Car))
          
          (mov-*1 Car)
          
          (upd-superls (get Car 'line 'p))

          # Move ptr to Car
          (put *0 'b Car)
          (mov-part-abv> *0 Car (cons T))
          
          # Disable car
          (setq *on-car)

          (setq *line Car
                *main Car))

        (prog
          (println 'repl-car-list "IMPLEMENT Y")))))


########################


(de ins-bef (Ref Part)

  #{
  
    Insert x before a, taking its pos:
  
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
    
    [ ]
    a
    
    *0
    [ ] - NIL
    b
    
    
    [ ]
    a
    
          *0
    [ ] - [ ] - NIL
    x     b
  
  }#

  (let (Prv (get Ref 'c))

    # Prv -> Part
    (con> Prv Part)

    #        Part -> Cdr
    # Prv <- Part
    (con> Part Ref Prv)
    
    # Part <- Cdr
    (put Ref 'c Part)

    # If Ref is a new line, xfer it to new Part
    (when (get Ref 'line 'p)
      (xchg-nl Ref Part))))


(de ins-aft (Ref Part)

  #{
  
    Insert x after a, taking pos of next:
  
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
    (when (get Cdr 'line 'p)
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

    # Ref -> Part
    (con> Prv Part)

    #        Part -> Cdr
    # Prv <- Part
    (con> Part Cdr Prv)
    
    # Part <- Cdr
    (put Cdr 'c Part)

    # If Ref is a new line, xfer it to new Part
    (when (get Ref 'line 'p)
      (xchg-nl Ref Part))

    # Copy layout?
    # (force-lay Part (get Ref 'lay))
      
    (del> Ref T NIL)))


(de xchg-nl (Src Dst)

  (let (Nl (get Src 'line)
        Prv (get Nl 'p)
        Nxt (get Nl 'n))

    (when Prv
      (put (get Prv 'line) 'n Dst))

    (when Nxt
      (put (get Nxt 'line) 'p Dst))

    (put Dst 'line Nl))

  (put Src 'line NIL)

  (println 'xchg-nl "Exchange line: " Src (get Src 'data) " -> " Dst (get Dst 'data))

  (setq *line Dst
        *main Dst))


###################


(de make-list (Ref)

  #{
    Move to newline and swap layout
    
    If user wants to maintain x layout, it can be done afterwards explicitly
        
    Given:

      [X]  [X]  [X]  NIL
      A    B    C
  
    
    These need to handle Y layout
  }#
  
  (if *on-car
      
      #{
        Create empty list aka NIL; move newline, x layout
        
        [X]  [X]  [X]  NIL
        A    B         *
                  [X]  NIL
                  NIL
        
      }#
      (repl-car-list NIL 'x)
      
      #{
        Create list with Pair; move newline, y layout
        - If list already, create nested list; call repl-cdr with Ref data

        [X]  [X]
        A    B    

        [Y]  [X]  NIL
        NIL  C
        
        * Calling swap-lay and backspace, will result in other example but
        with C instead of NIL as the CAR
      }#
      # Repl-any expects ptr on car
      (prog
      
        (let Part (gen-part (list (list (car (get Ref 'data))))
                             'x
                             NIL
                             T)

          (repl-pair Ref Part)
                
          # Make list y layout
          #(put Part 'lay 'y)
                    
          # Mov to newline
          (mov-part-nl Part 'line)
                  
          # Move pointer to deep NIL
          (let? End (get Part 'a 'b)
            (put *0 'b End)
            (mov-part-abv> *0 End (cons T)))
            
          (println 'mov-nl "Added Y+newline")
          
          Part))))


##################

# Move to line.l

(de mov-part-nl (Ref Base)

  # base = line or prv
    
  # For X layout, move to nl, toggle layout
  # Only do this for lists...and NIL?

  # Link lines
  (let Nxt (get *line 'line 'n)
  
    # Then create Line and calc its dims (from Ref to NIL/NL)
    (put Ref 'line (new '(+Line)
                      *line
                      Nxt
                      NIL
                      Base))

    # Link prev n to Ref
    (with (get *line 'line)
      (=: n Ref))
      
    # Link nxt p to Ref
    (when Nxt
      (with (get Nxt 'line)
        (=: p Ref))))
    
  (println 'mov-part-nl "Create new line: " Ref (get Ref 'data))
  (println 'mov-part-nl "Prv: " (get Ref 'line 'p) (get Ref 'line 'p 'data))
  (println 'mov-part-nl "Nxt: " (get Ref 'line 'n) (get Ref 'line 'n 'data))
    
  # Must calc y dims for both lines
  # Layout is not required to calc dims
  
  (put *line 'line 'dims (calc-dims-nl> *line))

  # Cannot position Ref/newline until current line bnds updated
  # Base irrelevant; lay-part will override it
  # Subsequent lines will be updated also
  (lay-part-start Ref *cur)
  
  # Start from Cdr since it skips lists  
  (put Ref 'line 'dims (calc-dims-nl> (get Ref 'a)))
  
  #(println 'mov-part-nl (get Ref 'line 'dims))
  
  (mov-*1 Ref)
  
  # Update previous lines/lists
  # Example, if line shortens, then must reposition cdrs
  (upd-superls (get *line 'line 'p))  
  
  (setq *line Ref
        *main Ref)
  
  T)


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
    (when (get List 'line)
      (println 'upd-superls "Re-layout line: " List (get List 'data))
      (align-cur-to-bnds List)
      (lay-part-start (get List 'b)
                (get List 'ori))
      # Eh double traversal...use cursor from lay-part?
      # This is using whole line, instead do:
      # add to bnds = (- (newend - origin) (oldend/bnds- origin))
      (put List 'line 'dims (calc-dims-nl> List)))
    (setq List (get List 'c))))

