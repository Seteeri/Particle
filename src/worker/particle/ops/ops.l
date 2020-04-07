(de con-any (Data Lay)

  (let Ref (get *0 'b)

    (if *on-car

        # If start of list, must replace entire thing
        # otherwise, normal repl
        (if (and (= (get Ref 'c 'c 'a) (get Ref 'c))
                 (= (get Ref 'c 'a) Ref))
              
            (prog
              (repl-list-car Data Lay))
              
            (prog
              # End NIL
              (if (and (isa '(+Nil) Ref)
                       (= (get Ref 'c 'b) Ref))
                  (prog
                    (ins-cdr Data Lay)
                    (point-sub Ref))
                  (prog
                    (repl-car Data Lay)))))
        
        (if (= (get Ref 'c 'a) Ref) # If direct sublist

            (prog
              (ins-list-car Data Lay))

            (prog
              (ins-cdr Data Lay))))))


(de ins-cdr (Data Lay)

  # Insert item before pair ptr

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Part (gen-part (list Data) Lay NIL T))

    (println 'ins-cdr "Ins-aft (bef Ptr): " Prv (get Prv 'data))
    
    (when (and (= (get Prv 'lay) 'y)
               (isa '(+Nil) Ref))
      # If replacing NIL and it is Cdr of Y layout
      # Make NIL on same line
      (put Ref 'lay 'x)
      (when (get Ref 'line)
        (xchg-nl Ref Part)))

    (when NIL
      (prinl) (prinl)
      (let Nl (get *master 'b 'line)
        (view (getl Nl))
        (let Child (get Nl 'n)
          (while Child
            (println 'xchg-nl "List: " Child (get Child 'data))
            (view (getl (get Child 'line)))
            (setq Child (get Child 'line 'n)))))
      (prinl) (prinl))
    
    # This will set *line to newline
    (ins-aft Prv Part)
    
    #{
    (when (and (= (get Ref 'lay) 'y)
               (isa '(+Pair) Ref))
       # If Ref is Y layout, create newline for Part
       (make-line Ref))
    }#
    
    # 1500 microseconds
    # 1.5 milliseconds to upd tree with 2 particles = 22 particles per frame max
    (upd-tree)

    (mov-*1 *line)
    
    # Same ref, relayout
    (mov-cur-abv Ref)
    
    (lay-part-start *0 *line 'skip-cdr)
    
    Part))



(de repl-list-car (Data Lay)
  #{
    Replace beginning of list when ptr on car
    
    [ ]  [ ]
    a    
    
         [ ]  [ ]
         b    c
         *

    [ ]  [ ]
    a    
    
         [ ]  [ ]
         x    c
         *
         
    Very similar to repl-car
  }#

  (let (Ref (get *0 'b)
        Prv (get Ref 'c 'c)
        Prv-Lay (get Prv 'lay)
        Prv-B-Lay (get Prv 'b 'lay)
        Cdr (get Ref 'c 'b)
        Part (gen-part (list (list Data)) Lay NIL T)
        Car (get Part 'a))
    
    # Move to Repl pair?
    # Txfer line before Ref gets deleted
    (when (get Ref 'c)
      (xchg-nl (get Ref 'c) Car))
    # Copy layout for Pair
    (put Part 'lay Prv-Lay)
    (put Part 'b 'lay Prv-B-Lay)

    (repl-pair Prv Part)

    # Relink Car to Cdr
    (con> Car Cdr)
    (put Cdr 'c Car)

    (upd-tree)
    (mov-*1 *line)    

    (put *0 'b Car)
    (point-nxt)
    #(test T (= (get *0 'b) (get Car 'b)))

    # Set line
    (setq *line Car)
    
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
      
        # nl check?

        (upd-tree)
        (mov-*1 *line)

        (put *0 'b Part)
        (point-nxt))

      (prog
        (println 'repl-car "IMPLEMENT Y LAYOUT")))

  Part))


(de ins-list-car (Data Lay)

  #{
  
    Replace beginning of list when ptr over
    
         *
    [ ]  [ ] NIL
         X
    NIL
    
              *
    [ ]  [ ]  [ ] NIL
         Z    X
    NIL    
    
  }#

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Cdr (get Ref 'b)
        Part (gen-part (list (list Data)) Lay NIL T)
        Car (get Part 'a))

    # Txfer line before Ref gets deleted
    (when (get Ref 'line)
      (xchg-nl Ref Car))
    
    (put Part 'lay (get Prv 'lay))

    (repl-pair Prv Part 'skip-del)

    # Relink to Ref
    (con> Car Ref)
    (put Ref 'c Car)

    (upd-tree)
    (mov-*1 *line)

    # Same ref, relayout
    (mov-cur-abv Ref)
    (lay-part-start *0 *line 'skip-cdr)

    Part))


############################


(de repl-car-list (Data Lay)

  #{
    UNUSED
  
    Replace entire item with list, i.e. replace car with list

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

          (put *line 'line 'n Car)
          (put Car 'line (new '(+Line) *line))
          
          (upd-tree)
          (mov-*1 *line)

          # This will move to car which is expected
          # Poss more convenient to move to top of Car
          (put *0 'b Part)
          (point-sub)
          (test T (= (get *0 'b) (get Car 'a)))

          # Disable car
          #(setq *on-car)
          
          (println 'repl-car-list "Prv line: " *line (get *line 'data))

          (setq *line Car))

        (prog
          (println 'repl-car-list "IMPLEMENT Y")))))


################################################################################


(de upd-tree (List)

  (setq *cur (get *master 'ori))
  (lay-part-start *master (get *master 'b)))
                  

(de make-list (Ref)

  #{
              *
    [X]  [X]  [X]  NIL
    A    B    C
      
  
    On-car: Create list with Pair; move newline, y layout

    [X]  [X]
    A    B

    [Y]  [X]  NIL
    NIL  C


    Mov to nl:
    

    [X]  [X]
    A    B       
    
    [X]  NIL
    C

  }#

  (if *on-car

      (let (Part (gen-part (list (list (car (get Ref 'data))))
                           'x
                           NIL
                           T)
            Prv (get Ref 'c)
            Prv-Cdr (get Prv 'b)
            Line *line
            Nxt (get *line 'line 'n))

        (repl-pair Prv Part)
        
        # P N D L
        
        # List is current list or NIL?
        # Prv is current list
        (put Part 'line (new '(+Line)
                          *line
                          (get Part 'b)
                          NIL
                          (if (get Line 'line 'l) @ *line)))
        (put Part 'lay 'y)
        (put Line 'line 'n Part)
        
        # Prv is Part
        # List is Part
        (let Cdr (get Part 'b)
          (put Cdr 'line (new '(+Line)
                            Part
                            Nxt
                            NIL
                            (if (get Line 'line 'l) @ *line)))
          (put Cdr 'lay (get Prv-Cdr 'lay))
          (put Nxt 'line 'p Cdr))
        
        # For a, no 'p = no newline
        (put (get Part 'a) 'line (new '(+Line)
                                      NIL
                                      NIL
                                      NIL
                                      NIL))

        (upd-tree)
        (mov-*1 *line)
        
        (put *0 'b (get Part 'c))
        (point-nxt)
        
        (println 'make-list "Added Y list, moved to newline")

        Part)

      # Mov current part to NL
      (prog
            
        (put Ref 'line (new '(+Line)
                          *line
                          (get *line 'line 'n)
                          NIL
                          (if (get *line 'line 'l) @ *line)))
        (put (get *line 'line) 'n Ref)
        (put (get *line 'line 'n) 'line 'p Ref)
        
        (upd-tree)
        (mov-*1 Ref)
        
        (mov-cur-abv Ref)
        (lay-part-start *0 *line 'skip-cdr)
        
        (setq *line Ref)
        
        )))


# Move to Particle?

(de make-line (Part)

  # For X layout, move to nl, toggle layout
  # Only do this for lists...and NIL?

  # Link lines
  (let Nxt (get *line 'line 'n)

    # Then create Line and calc its dims (from Part to NIL/NL)
    (put Part 'line (new '(+Line)
                      *line
                      Nxt
                      NIL
                      *line))

    # Push into list
    #(with (get *line 'line)
    #  (push (:: c) Part))

    # Link prev n to Part
    (with (get *line 'line)
      (=: n Part))

    # Link nxt p to Part
    (when Nxt
      (with (get Nxt 'line)
        (=: p Part))))

  (println 'make-line "Create new line: " Part (get Part 'data))
  #(println 'make-line "Prv: " (get Part 'line 'p) (get Part 'line 'p 'data))
  #(println 'make-line "Nxt: " (get Part 'line 'n) (get Part 'line 'n 'data))
  #(println 'make-line "Line: " (get Part 'line 'l) (get Part 'line 'l 'data))
  )


(de ins-line (Part)

  # For X layout, move to nl, toggle layout
  # Only do this for lists...and NIL?

  # Link lines
  (let Nxt (get *line 'line 'n)

    # Link prev n to Part
    (with (get *line 'line)
      (=: n Part))

    # Link nxt p to Part
    (when Nxt
      (with (get Nxt 'line)
        (=: p Part))))

  (println 'ins-line "Insert line: " Part (get Part 'data)))


#######################################
