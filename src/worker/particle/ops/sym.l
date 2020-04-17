# Move more stuff to methods
# Standardize checks
# Standardize prim ops

#####################
#####################
# SYM

(de make-str (Data Lay)

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
    
    
    [Y]  [X]  NIL
         a
    *
    NIL
    
    - Prv needed to make a line for NIL...
    - NIL is X layout now
          
    
    Special cases:
    
    
    [X]  [X]
    a    b
    
    *
    [Y]  [X]  NIL
         c
         
    NIL
    
    - Normally Part would be right of B which is awkward,
    so ins Part as newline before Y
    
    
    [X]
    b
    
         *
    [Y]  [Y]  [X]  NIL
              c
         NIL
    NIL
    
    
  }#

  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Part (gen-part (list Data) Lay NIL T))

    (println 'ins-cdr "con-aft: " Prv (get Prv 'data))

    # When Prv is Y/Pair and Ref is Y/Pair
    
    (con-aft Prv Part)
  
    # Make line as necessary
    (if (and (= (get Ref 'lay) 'y)
                (isa '(+Pair) Ref))
        (prog
          (make-line Ref))
        (when (get Ref 'line)
          (xchg-nl Cdr Part)))
    
    (upd-tree)
    (mov-*1 *line)
    
    # Same ref, relayout
    (mov-cur-abv Ref)
    (lay-part-start *0 *line 'skip-cdr)
    
    Part))


(de repl-list-car (Data Lay)
  # Replace pair y car
  
  #{
    Replace beginning of list when ptr on car
    
    [Cc]  
    a    
    
    [C]  [P]  [B]
         b    c
         *

         
    [ ]
    a    
    
    [ ]  [ ]  [ ]
         x    c
         *
         
    Effectively, replace entire list
    
    Refactor:
    * Use original Pair
    
    CREATE METHOD FOR CAR/CDR
  }#

  (let (Ref (get *0 'b)
        Pair (get Ref 'c)
        C (get Pair 'c)
        Cc (get C 'c)
        Part (gen-part Data Lay))

    # Pair <-> Part (CAR)

    (put Pair 'a Part)
    (put Part 'c Pair)
    
    # Con data
    (put Pair 'data (cons Data
                          (get Ref 'b 'data)))
    (put C 'data (cons (get Pair 'data)
                       (get C 'b 'data)))
    (con> Cc C)
    
    # Delete Ref
    (del> Ref T T)
    
    (upd-tree)
    (mov-*1 *line)    

    (put *0 'b Part)
    (point-nxt)
        
    Part))
  

(de repl-car (Data Lay)
  # Replace pair x car
  #{
    Replace car
        
    [C]  [P]  [B]
         b    c
         *

    
    [ ]  [ ]  [ ]
         x    c
         *
  }#

  (let (Ref (get *0 'b)
        Pair (get Ref 'c)
        C (get Pair 'c)
        Part (gen-part Data Lay))

    # Pair <-> Part (CAR)

    (put Pair 'a Part)
    (put Part 'c Pair)
    
    # Con data
    (put Pair 'data (cons Data
                          (get Ref 'b 'data)))
    (con> C Pair)

    # Main diff here from repl-list-car
      
    # Delete Ref
    (del> Ref T T)
    
    (upd-tree)
    (mov-*1 *line)    

    (put *0 'b Part)
    (point-nxt)
        
    Part))


(de ins-list-car (Data Lay)

  #{
  
    Replace beginning of list when ptr over
    
    [Cc]
    
          *
    [Pa]  [Pr] NIL
          X
    NIL
    
    ->
    
    [Cc]
    
              *
    [ ]  [ ]  [ ] NIL
         Z    X
    NIL    
    
  }#
    
  (let (Ref (get *0 'b)
        Pair (get Ref 'c)
        Cc (get Pair 'c)
        Part (gen-part (list Data) Lay NIL T))
    
    # Part <-> Ref
    (con> Part Ref)
    (put Ref 'c Part)
           
    # Pair <-> Part
    (put Pair 'a Part)
    (put Part 'c Pair)
    
    # Con data
    (put Pair 'data (cons (get Part 'data)
                          (get Pair 'b 'data)))
    (con> Cc Pair)
    
    # Swap lines
    (xchg-nl Ref Part)
    
    # Prv (get Pair 'lay)
    #(put Part 'lay (get Prv 'lay))
    
    (upd-tree)
    (mov-*1 *line)

    # Same ref, relayout
    (mov-cur-abv Ref)
    (lay-part-start *0 *line 'skip-cdr)    
    
    Part))
    

(de ins-list-car-2 (Data Lay)

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
    
    Refactor:
    * Reuse Cc
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