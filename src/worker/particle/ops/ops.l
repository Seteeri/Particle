# Move more stuff to methods
# Standardize checks
# Standardize prim ops

#####################
#####################
# SYM

(de con-any (Data Lay)

  (let Ref (get *0 'b)

    (if *on-car

        # If start of list, must replace entire thing
        (if (and (= (get Ref 'c 'c 'a) (get Ref 'c))
                 (= (get Ref 'c 'a) Ref))
              
            (prog
              (repl-list-car Data Lay))
              
            (prog
              # If end-of-list NIL
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
  #{
  
    Insert item before pair ptr
    
    Two special cases:
    
    [Y]  [X]  NIL
         a
    *
    NIL
    
    
    [X]  [X]
    a    b
    
    *
    [Y]  [X]  NIL
         c
         
    NIL
    
  }#

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Part (gen-part (list Data) Lay NIL T))

    (println 'ins-cdr "Ins-aft (bef Ptr): " Prv (get Prv 'data))
    
    (when (and (= (get Prv 'lay) 'y)
               (isa '(+Nil) Ref))
      # Copy layout
      (put Ref 'lay 'x))
  
    # This will set *line to newline
    (ins-aft Prv Part)

    (when (and (= (get Ref 'lay) 'y)
               (isa '(+Pair) Ref))
      (make-line Ref))
    
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
         
    Effectively, replace entire list
  }#

  (let (Ref (get *0 'b)
        Prv (get Ref 'c 'c)
        Prv-Lay (get Prv 'lay)
        Prv-B-Lay (get Prv 'b 'lay)
        Cdr (get Ref 'c 'b)
        Part (gen-part (list (list Data)) Lay NIL T)
        Car (get Part 'a))
        
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

    (setq *line Car)
    
    Part))


(de repl-car (Data Lay)

  #{
    Replace car with same type

    [ ]  [ ]  [ ]  NIL
    a    b    c
         *0

    Ref = b/Car
    Prv = b/Pair
    Part = x

    [ ]  [ ]  [ ]  NIL
    a    x    c
              *0
  }#

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Part (gen-part (list Data)
                       Lay
                       NIL
                       T))

    (repl-pair Prv Part)

    (upd-tree)
    (mov-*1 *line)    
    
    (if (= Lay 'x)

      (prog

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
    
    Effectively, replace entire list
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

  
###################
###################
# Pair/List
  

(de make-list (Ref)

  (if (and (= (get Ref 'c 'c 'a) (get Ref 'c))
           (= (get Ref 'c 'a) Ref))
      
      (prog
        # Take current Pair A and enclose in another Pair B
        # Attach Pair B to Prv
        
        (make-list-car Ref)
      
        (println 'make-list "Handle nested list!"))
        
      (make-list-cdr Ref)))

      
(de make-list-car (Ref)

  (let (Part (gen-part '(NIL)
                        'x
                        NIL
                        T)
        Prv (get Ref 'c)
        Line *line
        List (if (get Line 'line 'l) @ Line)
        Nxt (get *line 'line 'n))  
        
    (ins-aft Ref Part)

    (upd-tree)
    (mov-*1 *line)
    
    (put *0 'b (get Part 'c))
    (point-nxt)
    
    (println 'make-line "Made list on newline")

    Part))
      

(de make-list-cdr (Ref)

  #{

    [X]  [X]  [X]  NIL
    A    B    C
              *
  
    On-car: Create list with Pair; move newline, y layout

    [X]  [X]
    A    B

    [Y]  [X]  NIL
    NIL  C
         *
         
    Handle when Car is first item in list (no 'line 'l)

    [X]  
    A
    *

    ->
    
    [Y]  [X]  NIL
         A
         *    
    
    NIL
    
  }#

  (let (Part (gen-part (list (list (car (get Ref 'data))))
                        'x
                        NIL
                        T)
        Prv (get Ref 'c)
        Line *line
        List (if (get Line 'line 'l) @ Line)
        Nxt (get *line 'line 'n))  
    
    (if (and (isa '(+Nil) Ref)
             (get Ref 'line))

        (prog
          
          (ins-aft Prv Part)
          
          (put Line 'line 'n Part)
          
          # Create line if needed
          (unless (get Part 'line)
            (put Part 'line (new '(+Line)
                                  Line
                                  Ref
                                  List)))
          (put Part 'line 'n Ref)
          (put Part 'lay 'y)
          
          # Create line if needed
          (unless (get Ref 'line)
            # If here, Nxt always NIL?
            (put Ref 'line (new '(+Line)
                                  Part
                                  Nxt
                                  List))))
          
                
        (let Prv-Cdr (get Prv 'b)

          (repl-pair Prv Part)
          
          (put Line 'line 'n Part)
          
          # Create line if needed
          (unless (get Part 'line)
            (put Part 'line (new '(+Line)
                                  Line
                                  Prv-Cdr
                                  List)))
          (put Part 'line 'n Prv-Cdr)
          (put Part 'lay 'y)
          
          # Create line if needed
          (unless (get Prv-Cdr 'line)
            # If here, Nxt always NIL?
            (put Prv-Cdr 'line (new '(+Line)
                                    Part
                                    Nxt
                                    List)))))
          
    # For a, no 'p/'l = no newline
    (put (get Part 'a) 'line (new '(+Line)))

    (upd-tree)
    (mov-*1 *line)
    
    (put *0 'b (get Part 'c))
    (point-nxt)
    
    (println 'make-line "Made list on newline")

    Part))

        
(de make-line (Ref)

  #{

    [X]  [X]  [X]  NIL
    A    B    C  
  
    Mov to nl:
    

    [X]  [X]
    A    B       
    
    [X]  NIL
    C

  }#

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
  
  (setq *line Ref))

  
(de del-line (Part)
  # Assumes sublines will be deleted or no longer ref'd
  # or handled by caller

  (let (Nl (get Part 'line)
        Prv (get Nl 'p)
        Nxt (get Nl 'n))

    (when Prv
      (put (get Prv 'line) 'n Nxt))

    (when Nxt
      (put (get Nxt 'line) 'p Prv))))
  
        
(de upd-tree (List)

  (setq *cur (get *master 'ori))
  (lay-part-start *master (get *master 'b)))        
