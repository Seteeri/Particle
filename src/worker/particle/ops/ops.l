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
                  (repl-car Data Lay))))
        
        (if (= (get Ref 'c 'a) Ref) # If direct sublist

            (prog
              (repl-list Data Lay))

            (prog
              (ins-cdr Data Lay))))))


(de ins-cdr (Data Lay)

  # Insert item before pair ptr

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Part (gen-part (list Data) Lay NIL T))

    (println 'ins-cdr "Ins-aft (bef Ptr): " Prv (get Prv 'data))

    # Copy layout and NL
    #(put Part 'lay (get Prv 'lay))

    (ins-aft Prv Part)
    
    # 1500 microseconds
    # 1.5 milliseconds to upd tree with 2 particles = 22 particles per frame max
    (upd-tree)

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
      
        # nl check?

        #(mov-cur-part Prv 'start)
        #(lay-part-start Part (get *line 'ori))

        # Skip line upate?
        (upd-tree)
        (mov-*1 *line)

        (put *0 'b Part)
        (point-nxt))

      (prog
        (println 'repl-car "IMPLEMENT Y LAYOUT")))

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

  (when (and (not (str? Data))
             (not (<= (length Data) 3)))
    (println 'repl-list "WARNING! Length needs to be recalculated"))

  (let (Ref (get *0 'b)
        Prv (get Ref 'c 'c)
        Prv-Lay (get Prv 'lay)
        Prv-B-Lay (get Prv 'b 'lay)
        Cdr (get Ref 'c 'b)
        Part (gen-part (list (list Data)) Lay NIL T)
        Car (get Part 'a))
    
    # Txfer line before Ref gets deleted
    (xchg-nl (get Ref 'c) Car)

    (repl-pair Prv Part)

    # Relink Car to Cdr
    (con> Car Cdr)
    (put Cdr 'c Car)

    # Copy layout
    (put Part 'lay Prv-Lay)
    (put Part 'b 'lay Prv-B-Lay)

    (upd-tree)
    (mov-*1 *line)    

    (put *0 'b Car)
    (point-nxt)
    #(test T (= (get *0 'b) (get Car 'b)))

    #(setq *main Car)
    
    Part))


(de repl-list (Data Lay)

  # Replace beginning of list when ptr over

  (when (and (not (str? Data))
             (not (<= (length Data) 3)))
    (println 'repl-list "WARNING! Length needs to be recalculated"))

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

    #(mov-cur-part Prv 'start)
    #(lay-part-start Part *cur)

    # Calc dims for new line, and update super lines recursively
    #(upd-superls (get Car 'line 'p))
    (upd-tree)
    (mov-*1 *line)

    (put *0 'b Car)
    (point-nxt)
    (test T (= (get *0 'b) (get Car 'b)))

    #(setq *main Car)
    
    Part))


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

          # Draw Pair at Prv, no Car/Cdr
          # Then nl, draw Car
          #(mov-cur-part Prv 'start)
          #(lay-part-start Part (get *line 'ori) 'skip-car-cdr)

          # Create nl for Car
          # Layout will detect this
          # Line bnds is not used so NIL is ok for now
          (put *line 'line 'n Car)
          (put Car 'line (new '(+Line) *line))

          # Cur will be moved to proper pos rel to Pair
          #(lay-part-start Car *cur)
          
          # Super update:
          # * Re-position and draw particles if needed
          # * Calc bounds
          (upd-tree)
          (mov-*1 *line)

          # This will move to car which is expected
          # Poss more convenient to move to top of Car
          (put *0 'b Part)
          (point-sub)
          (test T (= (get *0 'b) (get Car 'a)))

          # Disable car
          #(setq *on-car)
          
          (view (getl (get Car 'line)))
          (println 'repl-car-list "Prv line: " *line (get *line 'data))

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

  (setq *line Dst))


###################


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
            Prv (get Ref 'c))

        (repl-pair Prv Part)

        (mov-part-nl Part 'line)
        (setq *line Part)

        (put Part 'lay 'y)
        (put Part 'b 'lay 'y)

        (mov-part-nl (get Part 'b) 'line)
        (setq *line (get Part 'b))

        (upd-tree)
        (mov-*1 *line)
        
        (put *0 'b (get Part 'c))
        (point-nxt)
        
        #(setq *line Part)

        (println 'mov-nl "Added Y+newline")

        Part)

      # Mov current part to NL
      (prog
        (mov-part-nl Ref)
        (upd-tree)
        (mov-*1 Ref)
        
        (mov-cur-abv Ref)
        (lay-part-start *0 *line 'skip-cdr)
        
        (setq *line Ref)
        
        )))


##################

# Move to line.l

(de mov-part-nl (Part)

  # For X layout, move to nl, toggle layout
  # Only do this for lists...and NIL?

  # Link lines
  (let Nxt (get *line 'line 'n)

    # Then create Line and calc its dims (from Part to NIL/NL)
    (put Part 'line (new '(+Line)
                      *line
                      Nxt
                      NIL
                      'line))

    # Link prev n to Part
    (with (get *line 'line)
      (=: n Part))

    # Link nxt p to Part
    (when Nxt
      (with (get Nxt 'line)
        (=: p Part))))

  (println 'mov-part-nl "Create new line: " Part (get Part 'data))
  (println 'mov-part-nl "Prv: " (get Part 'line 'p) (get Part 'line 'p 'data))
  (println 'mov-part-nl "Nxt: " (get Part 'line 'n) (get Part 'line 'n 'data)))


(de upd-tree (List)

  (setq *cur (get *master 'ori))
  (lay-part-start *master
                  *master))
                  
                  
#######################################


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
      (align-cur-to-car-bnds (get List 'c))
      (lay-part-start (get List 'c 'b)
                (get *main 'ori)))
    (setq List (get List 'c))))


(de upd-superls-2 (List)

  # Only update it pos change
  (while List

    (when (get List 'line)
      (println 'upd-superls "Layout line: " List (get List 'data))

      # Must skip sublists, else this becomes quadratic complexity
      # Pass to layout or make another fn like layout
      (align-cur-to-car-bnds List)
      (lay-part-start (get List 'b)
                      (get List 'ori))

      # add to bnds = (- (newend - origin) (oldend/bnds- origin))
      # Use cursor from lay-part
      #(println "MOV CUR")
      #(mov> *1 (setq *cur (list *cur-x *cur-y)))
      #(call 'sleep 2)

      (when NIL
        (let (Ori (get List 'b 'ori)
              Dims (list (- (get Ori 1) *cur-x)
                         (- (get Ori 2) *cur-y)))
          (println Ori Dims)
          (put List 'line 'dims Dims))))

    (setq List (get List 'c))))
