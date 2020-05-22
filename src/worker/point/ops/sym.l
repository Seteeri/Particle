(de make-str (Data Lay)

  # Refactor:
  # - Move ptr update here?
  # - Fns should take a tgt instead of manip ptr
  # - Move some fns elsewhere like repl-cdr works on pairs not strs
  # - Gen2-part should be in caller

  (let (Ref (get *fwd-in 'b)
        Part-Fn 
          (if *on-car
              (let Part (gen2-part Data Lay NIL T)
                (if (is-end?> Ref)
                    (cons (repl-cdr Ref Part) repl-cdr-post)
                    (cons (repl-car Ref Part) repl-car-post)))
              (let Part (gen2-part (list Data) Lay (cons NIL T) T)
                (if (is-car?> Ref)
                    (cons (ins-car Ref Part) ins-car-post)
                    (cons (ins-cdr Ref Part) ins-cdr-post)))))
                    
    (upd-tree)
    (mov-⇲ *line)
    
    ((cdr Part-Fn) (car Part-Fn))
    (lay-ptr *fwd-in)
    
    (car Part-Fn)))

    
(de repl-car-post (Part) 
  (set-ptr *fwd-in (sub> (get Part 'c 'b))))
  
  
(de repl-cdr-post (Part)
  (set-ptr *fwd-in (sub> Part)))
  
  
(de ins-car-post (Part)
  (set-ptr *fwd-in (if (cdr Part)
                        (sup> (car Part))
                        (sup> (get (car Part) 'b)))))
  
  
(de ins-cdr-post (Part) 
  (set-ptr *fwd-in (top> (get Part 'b))))
  

(de repl-cdr (Ref Part)
  #{
              *
    [ ]  [ ]  NIL
     A    B
     
    ->
    
    [ ]  [ ]  T
     A    B    
  
    Or
    
              *
    [ ]  [ ]  NIL
     A    B
     
    ->
    
    [ ]  [ ]  [ ]
     A    B    C  
  }#

  (println 'repl-cdr)
  
  (let (C (get Ref 'c)
        Cb (get Ref 'b))

    (b@ C Part Cb)

    (when (get Ref 'line)
      (put-line Ref Part))
    
    # Cdr is still used so don't del
    (del> Ref T NIL)

    Part))

    
(de repl-car (Ref Part)
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

  (println 'repl-car)
  
  (let (Pair (get Ref 'c)
        C (get Pair 'c))

    (a> Pair Part)
    
    (cond ((= (get C 'b) Pair)
           (b> C Pair))
          ((= (get C 'a) Pair)
           (a> C Pair))
          (T
           (quit "repl-car: invalid pair")))
          
    (del> Ref T T)
        
    Part))  

    
(de ins-cdr (Ref Part)
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
  
  (println 'ins-cdr)
  
  (let (Pair (get Ref 'c))

    (b@ Pair Part Ref)
    
    # Make line as necessary if car is a list
    (if (and (isa '(+Pair) (get Ref 'a))
             (isa '(+Pair) Ref))
        (make-line Ref)
        (when (get Ref 'line)
          (put-line Ref Part)))

    Part))
    

(de ins-car (Ref Part)

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
    
  (println 'ins-car)
    
  (let (Pair (get Ref 'c)
        Is-Ref-Pair-of-Pair (and (isa '(+Pair) Pair)
                                 (isa '(+Pair) (get Ref 'a))))

    (a> Pair Part)                                 
                                 
    (b> Part Ref)
                  
    # assert Ref has a line
                  
    (put-line Ref Part)        
          
    (when Is-Ref-Pair-of-Pair
      # Create additional line for Ref
      # Ins after current line (Part)
      (put Ref 'line (new '(+Line)
                          Part
                          (get Ref 'b)
                          (if (get *line 'line 'l) @ *line)))
      (put *line 'line 'n Ref))
        
    (cons Part Is-Ref-Pair-of-Pair)))
    

#########################################
    
    
(de repl-list-str (Sta End Data Skip-Upd)

  # Replace list/line of strings with single string
  
  (let (Prv (get Sta 'c)
        Part (gen2-part (list Data) 'x NIL T))
    
    # If car of pair
    (if (= (get Prv 'a) Sta)
        (a> Prv Part)
        (b> Prv Part))

    # Part <-> End'b
    (let (Eb (get End 'b)
          Pb (get Part 'b))
      (when Eb
        (when (get Pb 'line) (put-line Eb Pb))
        (del> Pb T T)
        (b> Part Eb)))
    
    (when (get Sta 'line)
      (put-line Sta Part))
    
    # Inclusive
    (del-rx Sta End)
    
    (set-ptr *fwd-in (get Part 'b))
    
    # Later, move this group to undo/cmd list
    (unless Skip-Upd
      (upd-tree)
      (mov-⇲ *line)        
      
      # Put Ptr above new part
      (mov-cur-abv (get Part 'b)))))


(de repl-str-list (Ref List Skip-Upd)

  # Replace single string with list/line of strings
  
  (let (Sta (get Ref 'c)
        End (get Ref 'b)
        Prv NIL)
  
    # Add list, item by item
    # Do car, then cdr

    (let (Data (car List)
          Part (gen2-part (list Data) 'x (cons NIL T) T))
          
      (if (= (get Sta 'a) Ref)
          (a> Sta Part)
          (b> Sta Part))
  
      (put-line Ref Part)
  
      (setq Prv Part))
    
    (for Data (cdr List)
    
      #(let Part (gen-part (list Data) 'x NIL T)
      (let Part (gen2-part (list Data) 'x (cons NIL T) T)
        
        (b> Prv Part)
        
        (setq Prv Part)))

    # Con last Part to existing cdr
    (b> Prv End)        
    
    # Del Ref except Cdr
    (del-car> Ref)    
    
    (set-ptr *fwd-in End)
    
    (unless Skip-Upd
      (upd-tree)
      (mov-⇲ *line)        
              
      # Put Ptr above new part
      (mov-cur-abv End))))

      
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
      
