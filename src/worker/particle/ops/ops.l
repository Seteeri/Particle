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

    (repl-pair Prv Part)

    (if (= Lay 'x)

      (prog
            
        #(when (get Prv 'line)
        #  (xchg-nl Prv Part))

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
                  

(de make-line (Ref)

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
            List (if (get Line 'line 'l) @ Line)
            Nxt (get *line 'line 'n))

        # Should swap newline
        (repl-pair Prv Part)
        
        # P N D L
        
        # List is current list or NIL?
        # Prv is current list
        (put Part 'line (new '(+Line)
                          Line
                          (get Part 'b)
                          List))
        (put Part 'lay 'y)
        (put Line 'line 'n Part)
        
        # Prv is Part
        # List is Part
        (let Cdr (get Part 'b)
          (put Cdr 'line (new '(+Line)
                            Part
                            Nxt
                            List))
          (put Cdr 'lay (get Prv-Cdr 'lay))
          (put Nxt 'line 'p Cdr))
        
        # For a, no 'p = no newline
        (put (get Part 'a) 'line (new '(+Line)))

        (upd-tree)
        (mov-*1 *line)
        
        (put *0 'b (get Part 'c))
        (point-nxt)
        
        (println 'make-line "Added Y list, moved to newline")

        Part)

      # Mov current part to NL
      (prog

        (put Ref 'line (new '(+Line)
                          *line
                          (get *line 'line 'n)
                          (if (get *line 'line 'l) @ *line)))
        (put (get *line 'line) 'n Ref)
        (put (get *line 'line 'n) 'line 'p Ref)
        
        (upd-tree)
        (mov-*1 Ref)
        
        (mov-cur-abv Ref)
        (lay-part-start *0 *line 'skip-cdr)
        
        (setq *line Ref)
        
        )))
