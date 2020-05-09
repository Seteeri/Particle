(de make-str (Data Lay)

  (let Ref (get *0 'b)

    (if *on-car

        # If start of list, must replace entire thing
        (if (and (= (get Ref 'c 'c 'a) (get Ref 'c))
                 (= (get Ref 'c 'a) Ref))
              
            (prog
              (repl-str-first Data Lay))
              
            # If end-of-list NIL
            (if (and (isa '(+Nil) Ref)
                     (= (get Ref 'c 'b) Ref))
                (prog
                  (ins-str Data Lay)
                  (point-sub Ref))
                (prog
                  (repl-str Data Lay))))
        
        (if (= (get Ref 'c 'a) Ref) # If direct sublist

            (prog
              (ins-str-first Data Lay))

            (prog
              (ins-str Data Lay))))))


(de ins-str (Data Lay)
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

    (println 'ins-str C (get C 'data))

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


(de repl-str-first (Data Lay)
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
    
    #(c-a> Part)
    
    (b> Cc C)
    
    # Delete Ref
    (del> Ref T T)
    
    (upd-tree)
    (mov-*1 *line)    

    (put *0 'b Part)
    (point-nxt)
        
    Part))
  

(de repl-str (Data Lay)
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

    #(c-a> Part)
    
    (b> C Pair)
    
    # Main diff here from repl-str-first
      
    # Delete Ref
    (del> Ref T T)
    
    (upd-tree)
    (mov-*1 *line)    

    (put *0 'b Part)
    (point-nxt)
        
    Part))


(de ins-str-first (Data Lay)

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
    
    
(de repl-list-str (Sta End Data Skip-Upd)

  # Replace list/line of strings with single string

  (let (Prv (get Sta 'c)
        Part (gen-part (list Data) 'x))
    
    # If car of pair
    (if (= (get Sta 'c 'a) Sta)    
        (prog
          (a> Prv Part)
          (c-a> Part))
        (b> Prv Part))
    
    # Part <-> End'b
    (when (get End 'b)
      (del> (get Part 'b) T T)
      (b> Part (get End 'b)))
    
    (when (get Sta 'line)
      (put-line Sta Part))
    
    # Inclusive
    (del-rx Sta End)
    
    (put *0 'b (get Part 'b))
    
    # Later, move this group to undo/cmd list
    (unless Skip-Upd
      (upd-tree)
      (mov-*1 *line)        
      
      # Put Ptr above new part
      (mov-cur-abv (get Part 'b))
      (lay-part-start *0 *line 'skip-cdr))))
    

(de repl-str-list (Ref List Skip-Upd)

  # Replace single string with list/line of strings
  
  (let (Sta (get Ref 'c)
        End (get Ref 'b)
        Prv NIL)
  
    # Add list, item by item
    # Do car, then cdr

    (let (Data (car List)
          Part (gen-part (list Data) 'x NIL T))
      (if (= (get Sta 'a) Ref)
          (prog
            (a> Sta Part)
            (c-a> Part))
          (b> Sta Part))
  
      (put-line Ref Part)
  
      (setq Prv Part))
    
    (for Data (cdr List)
    
      (let Part (gen-part (list Data) 'x NIL T)
        
        # T must be NIL to skip
        (con-sym Prv Part T)
        
        (setq Prv Part)))

    # Con last Part to existing cdr
    (b> Prv End)        
    
    # Del Ref except Cdr
    (del-car> Ref)    
    
    (put *0 'b End)
    
    (unless Skip-Upd
      (upd-tree)
      (mov-*1 *line)        
              
      # Put Ptr above new part
      (mov-cur-abv End)
      (lay-part-start *0 *line 'skip-cdr))))
