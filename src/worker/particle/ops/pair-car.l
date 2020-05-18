#{

  X CASES:

    Parent is either a Pair or Sym

    Case 1: (and (lay-x?> Ref) (is-car?> Ref))
  
      [X]  [X]  Atm
      SYM   A
            *
      
      OR:
  
      [X]  [X]  Atm
        A    B
            *
    
      Produce:
      
        [X]  
          A   
          
        [Y]  [X]
              B
              *
              
        Atm
    
    Case 2: (and (lay-x?> Ref) (is-end?> Ref))
    
      [X]
        A
      
      Atm
      *

      Produce:
      
        [X]  
          A   
          
        [Y]  Atm
              *
              
        Atm        
    
      ---------------
    
      [X]  [X]  Atm
        A    B    *
      
      Produce:
      
        [X]  [X]
          A    B
          
        [Y]  Atm
              *
              
        Atm
      
      
    * Line can be done automatically in cdr case?
      * Transfer line to new part
    * Cdr can be end or not
          
  --------
  
  Y CASES:
          
    Case 3: (and (lay-y?> (get Ref 'c 'c)) (is-car?> (get Ref 'c)))
    
      [Y]  [X]  Atm
            A
            *
      
      PRODUCE:
      
        [Y]  [Y]  [X]  Atm
                    A
                    *          
      
      Handle Y Sym?
      
          
    Case 4: (and (lay-y?> (get Ref 'c)) (is-car?> Ref))
    
      [Y]  Atm
            *
      Atm

      PRODUCE:
      
        [Y]  [Y]  Atm
                    *                  
      
      Does not apply to sym...
      Unless did zap,intern combo
      
      
    Case 5: (and (lay-y?> (get Ref 'c)) (is-cdr?> Ref))
      
      [Y]  Atm
      
      Atm
      *

      Produce:
      
        [Y]  Atm
          
        [Y]  Atm
            
              
        Atm
        *
        
      Handle Y Sym? 
      
      -> SAME AS CASE 2
      
  GOAL: Replace old car with new Pair car pt to old car
    
    (a b c) -> (a (b) c)
    
    FROM:
    
      [X]  [X]  [X] 
      a    b    c
      
    TO:
      
      [X]  
      a 
      
      [Y]  [X]  NIL
            b
      
      [X]
      c
    
}#

(de make-list-car (Ref)
  
  # Check Y then x
  
  (cond
  
    ((and (lay-y?> (get Ref 'c)) (is-car?> Ref))
     (make-case-4 Ref))
    
    ((and (lay-y?> (get Ref 'c)) (is-cdr?> Ref))
     (make-case-2 Ref))
  
    ((and (lay-x?> Ref) (is-car?> Ref))
    
      (if (and (lay-y?> (get Ref 'c 'c))
               (is-car?> (get Ref 'c)))
          (make-case-3 Ref)
          (make-case-1 Ref)))
  
    ((and (lay-x?> Ref) (is-end?> Ref))
     (make-case-2 Ref))
                
    (T
      (quit "make-list-car: unknown case"))))
      
      
(de make-case-1 (Ref)
  
  #{
      
      Case 1: (and (lay-x?> Ref) (is-car?> Ref))
    
        [X]  [X]  ATOM
         A    B
              *
      
        Produce:
        
          [X]  
           A   
           
          [Y]  [X]
                B
                *
                
          ATOM
    
      Also handle if A is a symbol
    
  }#

  #{
              
        [Cc]  [C]  Cb
         .    Ref
               *

    ->
               
          [Cc]  
           A   
           
          [P]  [C]
               Ref
                *
                
          Cb
  }#
  
  (let (C (get Ref 'c)
        Cb (get C 'b)
        Cc (get C 'c)
        Part (gen2-part (cons) 'x (cons T NIL) T)
        End (get Part 'b))

    (b> Cc Part)
    (a> Part C)
    (b> C End)
    (if Cb
        (b> Part Cb)
        (setq Cb (get Part 'b)))
    
    # Creates line for Part, C and Cb
    
    (lay> Part 'y)
    (put Part 'line
      (new '(+Line)
           (unless (isa '(+Sym) Cc)
             *line)
           (: b)
           (unless (isa '(+Sym) Cc)
             (if (get *line 'line 'l) @ *line))))
                  
    # Create Line for Cdr
    (if (get Cb 'line)
        (put @ 'line 'p Part)
        (put Cb 'line
          (new '(+Line)
               Part
               (get *line 'line 'n)
               (if (isa '(+Sym) Cc)
                   Part
                   (get Part 'line 'l)))))
        
    # Create line for C
    (put C 'line (new '(+Line)))
        
    # Connect line: cur -> Part
    (unless (isa '(+Sym) Cc)
      (put *line 'line 'n Part))
        
    (upd-tree)
    (mov-⇲ *line)
    
    # Maintain on inner tree or outer?
    (setq *line Part)
    (set-ptr *fwd-in Part)
    (point-sub)
        
    (println 'make-case-1)

    Part))      
          

(de make-case-2 (Ref)
  
  #{
      
      Case 2: (and (lay-x?> Ref) (is-end?> Ref))
      
        [X]
         A
        
        ATOM
        *

        Produce:
        
          [X]  
           A   
           
          [Y]  ATOM
                *
                
          ATOM        
      
        ---------------
      
        [X]  [X]  ATOM
         A    B    *
       
        Produce:
       
          [X]  [X]
           A    B
           
          [Y]  ATOM
                *
                
          ATOM 
    
  }#

  #{
              
        [Cc]  Ref
         .     *

    ->
               
          [Cc]  
           A   
           
          [P]  Ref
                *
                
          Cb
                
  }#
  
  #{
  
  Diffs from Case 1:
  - Case 1 C = Case 2 Cc
  - No Cb or End
  - gen2-part: skip Cdr
  
  }#
  
  (let (Cc (get Ref 'c)
        Is-Cc-Sym (isa '(+Sym) Cc)
        Part (gen2-part (cons) 'x (cons T ) T)
        Cb (get Part 'b))

    (b> Cc Part)
    (a> Part Ref)
    
    (lay> Part 'y)
    
    # Creates line for Part, Cb, Ref
    (put Part 'line
      (new '(+Line)
           (unless Is-Cc-Sym
             *line)
           Cb
           (unless Is-Cc-Sym
             (if (get *line 'line 'l) @ *line))))
        
    # Connect line: cur -> Part
    (unless Is-Cc-Sym
      (put Part 'line 'p 'line 'n Part))        
        
    # Create Line for Cdr
    (put Cb 'line
      (new '(+Line)
            Part
            NIL
            (if Is-Cc-Sym
                Part
                (get Part 'line 'l))))
        
    # Create line for Ref
    (put Ref 'line (new '(+Line)))
                
    (upd-tree)
    (mov-⇲ *line)
    
    # Maintain on inner tree or outer?
    (setq *line Part)
    (set-ptr *fwd-in Part)
    (point-sub)
        
    (println 'make-case-2)

    Part))
    
    
(de make-case-3 (Ref)
  
  #{
      
      Case 3: (and (lay-y?> (get Ref 'c 'c)) (is-car?> (get Ref 'c)))
      
        [Y]  [X]  Atm
              A
              *
        
        PRODUCE:
        
          [Y]  [Y]  [X]  Atm
                     A
                     *      
    
      Handle Y sym?
    
  }#

  #{
              
        [Cc]  [C]  Cb
              Ref
               *

    ->
               
        [Cc]  [P]  [C]  Cb
                   Ref
                    *
              Pb
  }#
  
  (let (C (get Ref 'c)
        Cc (get C 'c)
        Part (gen2-part (cons) 'x (cons T NIL) T)
        Pb (get Part 'b))

    (when (isa '(+Sym) Cc)
      (quit "make-case-3: todo, no y sym support"))
        
    (a> Cc Part)
    (a> Part C)

    (lay> Part 'y)
    
    # Creates line for Part, C and Pb
    
    # Create Line for Part
    (put Part 'line
      (new '(+Line) NIL Pb NIL))
        
    # Create Line for Pb
    (put Pb 'line
      (new '(+Line) Part NIL Part))
        
    # Create line for C
    (if (get C 'line)
        (with @
          (put 'p)
          (put 'l))
        (put C 'line
          (new '(+Line))))
                
    (upd-tree)
    (mov-⇲ *line)
    
    # Maintain on inner tree or outer?
    (setq *line Part)
    (set-ptr *fwd-in Part)
    (point-sub)
        
    (println 'make-case-3)

    Part))
    
    
(de make-case-4 (Ref)
  
  #{
      
      Case 4: (and (lay-y?> (get Ref 'c)) (is-car?> Ref))
      
        [Y]  Atm
             *
        Atm

        PRODUCE:
        
          [Y]  [Y]  Atm
                     *    
    
      Handle Y Sym?
    
  }#

  #{
              
        [Cc]  Ref
               *

    ->
               
        [Cc]  [P]  Ref
                    *
              Pb
  }#
  
  (let (Cc (get Ref 'c)
        Part (gen2-part (cons) 'x (cons T NIL) T)
        Pb (get Part 'b))

    (when (isa '(+Sym) Cc)
      (quit "make-case-3: todo, no y sym support"))
        
    (a> Cc Part)
    (a> Part Ref)

    (lay> Part 'y)
    
    # Creates line for Part, Ref and Pb
    
    (put Part 'line (new '(+Line) NIL  Pb  NIL))        
    (put Pb   'line (new '(+Line) Part NIL Part))
    (put C    'line (new '(+Line)))
                
    (upd-tree)
    (mov-⇲ *line)
    
    # Maintain on inner tree or outer?
    (setq *line Part)
    (set-ptr *fwd-in Part)
    (point-sub)
        
    (println 'make-case-4)

    Part))           
