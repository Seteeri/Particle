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
              (repl-sym-first Data Lay))
              
            # If end-of-list NIL
            (if (and (isa '(+Nil) Ref)
                     (= (get Ref 'c 'b) Ref))
                (prog
                  (ins-sym Data Lay)
                  (point-sub Ref))
                (prog
                  (repl-sym Data Lay))))
        
        (if (= (get Ref 'c 'a) Ref) # If direct sublist

            (prog
              (ins-sym-first Data Lay))

            (prog
              (ins-sym Data Lay))))))


(de ins-sym (Data Lay)
  #{
  
    Insert item before pair ptr
    
    
    [Y]  [X]  NIL
         a
    *
    NIL
    
    - C needed to make a line for NIL...
    - NIL is X layout now
          
    
    Special cases:
    
    
    [ ]  [ ]
    a    b
    
    *
    [Y]  [ ]  NIL
         c
         
    NIL
    
    - Normally Part would be right of B which is awkward,
    so ins Part as newline before Y
    
  }#
         
  (let (Ref (get *0 'b)
        C (get Ref 'c)
        Part (gen-part (list Data) Lay NIL T))

    (println 'ins-sym C (get C 'data))

    (con-sym C Part)
  
    # Make line as necessary
    (if (and (isa '(+Pair) (get Ref 'a))
             (isa '(+Pair) Ref))
        (prog
          (make-line Ref))
        (when (get Ref 'line)
          (put-line Cdr Part)))
    
    (upd-tree)
    (mov-*1 *line)
    
    # Same ref, relayout
    (mov-cur-abv Ref)
    (lay-part-start *0 *line 'skip-cdr)
    
    Part))


(de repl-sym-first (Data Lay)
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

    (a> Pair Part)
    
    (a> C Pair)

    (b> Cc C)
    
    # Delete Ref
    (del> Ref T T)
    
    (upd-tree)
    (mov-*1 *line)    

    (put *0 'b Part)
    (point-nxt)
        
    Part))
  

(de repl-sym (Data Lay)
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

    (a> Pair Part)

    (b> C Pair)

    # Main diff here from repl-sym-first
      
    # Delete Ref
    (del> Ref T T)
    
    (upd-tree)
    (mov-*1 *line)    

    (put *0 'b Part)
    (point-nxt)
        
    Part))


(de ins-sym-first (Data Lay)

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
    
    -------------------
    
    ((c))
    
         *
    [Y]  [Y]  [ ]  NIL
              c
         NIL
    NIL
    
    Y/Pair can only be on newline or after Y/Pair

    
    (x (c))
         
    [Y]  [ ]
         x
         
         *
         [Y]  [ ]  NIL
              c
         NIL
         
    NIL    
    
    - Diff is no exchange newline
    - Create line for x
  }#
    
  (let (Ref (get *0 'b)
        Pair (get Ref 'c)
        Cc (get Pair 'c)
        Part (gen-part (list Data) Lay NIL T))

    # Part <-> Ref
    (b> Part Ref)
          
    # Pair <-> Part
    (a> Pair Part)
        
    # Mov nl from Ref to Part
    (put-line Ref Part)        
          
    (c-a> Pair)
          
    (when (and (isa '(+Pair) (get Ref 'a))
               (isa '(+Pair) Pair))    
        # Create additional line for Ref
        # Ins after current line (Part)
        (put Ref 'line (new '(+Line)
                            Part
                            (get Ref 'b)
                            (if (get *line 'line 'l) @ *line)))
        (put *line 'line 'n Ref))
        
    (upd-tree)
    (mov-*1 *line)

    # Same ref, relayout
    (mov-cur-abv Ref)
    (lay-part-start *0 *line 'skip-cdr)    
    
    Part))
