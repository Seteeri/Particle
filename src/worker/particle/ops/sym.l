(de make-str (Data Lay)

  (let Ref (get *0 'b)

    (if *on-car
              
        # If end-of-list NIL
        (if (and (isa '(+Nil) Ref)
                 (= (get Ref 'c 'b) Ref))
            (prog
              (ins-str Data Lay)
              (point-sub Ref))

            (repl-str Data Lay))
        
        (if (= (get Ref 'c 'a) Ref) # If direct sublist

            (ins-str-first Data Lay)

            (ins-str Data Lay)))))


(de ins-str (Data Lay)
  #{
    Insert after ptr/pair (rel to Ref 'b 'c)
    or
    Insert before ptr/pair
    
          *
    [ ]  [ ]  ...
    a    c    

    ->
    
          *
    [ ]  [ ]  [ ]  ...
    a    b    c                 
    
    ---
    
    Special case: Insert before ptr/list
    
    [ ]  [ ]
    a    b
    
    *
    [Y]  [ ]  NIL
         c
         
    NIL
    
    - Normally Part would be right of B which is awkward,
    so ins Part as newline before Y
    - Or start newline and move Y to newline
    
  }#
         
  (let (Ref (get *0 'b)
        Pair (get Ref 'c)
        Part (gen-part (list Data) Lay NIL T))

    (b@ Pair Part Ref)
    
    # Make line as necessary if car is a list
    (if (and (isa '(+Pair) (get Ref 'a))
             (isa '(+Pair) Ref))
        (make-line Ref)
        (when (get Ref 'line)
          (put-line Ref Part)))
    
    (upd-tree)
    (mov-*1 *line)
        
    (mov-cur-abv Ref)
    (lay-part-start *0 *line 'skip-cdr)
    
    Part))
    

(de ins-str-first (Data Lay)

  #{
    Insert item before ptr

    [X]
    
          *
    [Y]  [ ] ...
          X
    NIL
    
    ->
    
    [X]
    
               *
    [Y]  [ ]  [ ] ...
          Z    X
    NIL    
    
    -------------------
    
          *
    [Y]  [Y]  ...
              
         NIL
    NIL
    
    ->
          *
    [Y]  [ ]
         a
         
         [Y]  ...
              
         NIL
         
    NIL
  }#
    
  (let (Ref (get *0 'b)
        Pair (get Ref 'c)
        Part (gen-part (list Data) Lay NIL T)
        Is-Ref-Pair-of-Pair (and (isa '(+Pair) (get Ref 'a))
                                 (isa '(+Pair) Pair)))

    (b> Part Ref)
          
    (a> Pair Part)
        
    (put-line Ref Part)        
          
    (when Is-Ref-Pair-of-Pair
      # Create additional line for Ref
      # Ins after current line (Part)
      (put Ref 'line (new '(+Line)
                          Part
                          (get Ref 'b)
                          (if (get *line 'line 'l) @ *line)))
      (put *line 'line 'n Ref))
        
    (upd-tree)
    (mov-*1 *line)

    (if Is-Ref-Pair-of-Pair
        (prog
          (put *0 'b Part)
          (mov-cur-abv Part))
        (mov-cur-abv Ref))
    
    (lay-part-start *0 *line 'skip-cdr)    
    
    Part))
    

(de repl-str (Data Lay)
  #{
    Replace car of y pair
        
    ---
        
    [C]  [P]  [B]
    a    b    c
         *

    
    [ ]  [ ]  [ ]
    a    x    c
         *

    ---
    
    [Ccc]
    a    
    
    [Cc]  [C]  [ ]  [ ]
               x    c
               *        

  }#

  (let (Ref (get *0 'b)
        Pair (get Ref 'c)
        C (get Pair 'c)
        Part (gen-part Data Lay))

    (a> Pair Part)
    
    (cond ((= (get C 'b) Pair)
           (b> C Pair))
          ((= (get C 'a) Pair)
           (a> C Pair))
          (T
           (quit "repl-str: invalid pair")))
          
    (del> Ref T T)
    
    (upd-tree)
    (mov-*1 *line)

    (put *0 'b Part)
    (point-nxt)
        
    Part))
    
    
(de repl-list-str (Sta End Data Skip-Upd)

  # Replace list/line of strings with single string
  
  (let (Prv (get Sta 'c)
        Part (gen-part (list Data) 'x))
    
    # If car of pair
    (if (= (get Sta 'c 'a) Sta)    
        (a> Prv Part)
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
          (a> Sta Part)
          (b> Sta Part))
  
      (put-line Ref Part)
  
      (setq Prv Part))
    
    (for Data (cdr List)
    
      (let Part (gen-part (list Data) 'x NIL T)
        
        (b> Prv Part)
        
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

      
(de build-str (Start)
  # Go prv until Sym, non-Str or newline
  (let (P Start
        B NIL
        D NIL)
        
    (while (and (isa '(+Str) (get P 'a))
                (not (isa '(+Sym) P))
                (not (= (get P 'a) B))
                (not (get P 'line)))
      
      (push 'D (get P 'a 'data))
      
      (setq B P)
      (setq P (get P 'c)))
    
    (if (and (isa '(+Str) (get P 'a))
             (not (isa '(+Sym) P))
             (not (= (get P 'a) B)))
      (push 'D (get P 'a 'data))
      (setq P (get P 'b)))
    
    (list P D)))

    
(de build-str-sp (Start)
  # Go prv until sp, non-str or line
  (let (P Start
        B NIL
        D NIL)
        
    (while (and (isa '(+Str) (get P 'a))
                (not (= (get P 'a 'data) " ")) # what about multiple spaces?
                (not (isa '(+Sym) P))
                (not (= (get P 'a) B))
                (not (get P 'line)))
      
      (push 'D (get P 'a 'data))
      
      (setq B P)
      (setq P (get P 'c)))
    
    (if (and (isa '(+Str) (get P 'a))
             (not (= (get P 'a 'data) " ")) # what about multiple spaces?
             (not (isa '(+Sym) P))
             (not (= (get P 'a) B)))
      (push 'D (get P 'a 'data))
      (setq P (get P 'b)))
    
    (list P D)))
      
