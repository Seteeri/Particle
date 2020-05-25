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
      * Transfer line to new point
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
  
  # Refactor upd into here like str cmds
  
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

  #{
              
        [Cc]  
         .  

         
        [C]  Cb
        Ref
        *
    ->
               
        [Cc]  
          A   
          
        [P]  [C]   
             Ref
              *
              
        Cb
  }#  
  
  #{
                      
        [Cc]  [C]  [Cb]  NIL
        NIL   NIL  NIL
               *
              
    ->
               
        [Cc]  
        NIL  

              
        [P]  [C]  NIL
             NIL  
              *
        
        [Cb] NIL
        NIL
  }#   
  
  (let (C (get Ref 'c)
        Cb (get C 'b)
        Cc (get C 'c)
        Po (gen-point (cons) 'x (cons T NIL) T)
        Pb (get Po 'b)
        Ln-N (get *line 'line 'n))

    (b> Cc Po)
    (a> Po C)
    (b> C Pb)
    (if Cb
        (b> Po Cb)
        (setq Cb (get Po 'b)))
    
    # Creates line for Po, C and Cb
       
    (lay> Po 'y)
    
    (if (get C 'line)
        (prog
          (put-line C Po)
          (put Po 'line 'n Cb))
        (prog
          # Connect line: cur -> Po
          (unless (is-sym? Cc)
            (put *line 'line 'n Po))
          (put Po 'line
            (new '(+Line)
                (unless (is-sym? Cc)
                  *line)
                Cb
                (unless (is-sym? Cc)
                  (if (get *line 'line 'l) @ *line))))))
                  
    # Create Line for Cdr
    (if (get Cb 'line)
        (put @ 'line 'p Po)
        (put Cb 'line
          (new '(+Line)
               Po
               Ln-N
               (if (is-sym? Cc)
                   Po
                   (get Po 'line 'l)))))
        
    # Create line for C
    (put C 'line (new '(+Line)))
                
    (upd-tree)
    (mov-⇲ *line)
    
    # Maintain on inner tree or outer?
    (setq *line Po)
    (set-ptr *fwd-in Po)
    (point-sub)
        
    (println 'make-case-1)

    Po))      
          

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
  - gen-point: skip Cdr
  
  }#
  
  
  (let (Cc (get Ref 'c)
        Is-Cc-Sym (is-sym? Cc)
        Po (gen-point (cons) 'x (cons T ) T)
        Cb (get Po 'b))

    (b> Cc Po)
    (a> Po Ref)
    
    (lay> Po 'y)
    
    # Creates line for Po, Cb, Ref
    (put Po 'line
      (new '(+Line)
           (unless Is-Cc-Sym
             *line)
           Cb
           (unless Is-Cc-Sym
             (if (get *line 'line 'l) @ *line))))
        
    # Connect line: cur -> Po
    (unless Is-Cc-Sym
      (put Po 'line 'p 'line 'n Po))        
        
    # Create Line for Cdr
    (put Cb 'line
      (new '(+Line)
            Po
            NIL
            (if Is-Cc-Sym
                Po
                (get Po 'line 'l))))
        
    # Create line for Ref
    (put Ref 'line (new '(+Line)))
                
    (upd-tree)
    (mov-⇲ *line)
    
    # Maintain on inner tree or outer?
    (setq *line Ref)
    (set-ptr *fwd-in Ref)
    (point-sub)
        
    (println 'make-case-2)

    Po))
    
    
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
        Po (gen-point (cons) 'x (cons T NIL) T)
        Pb (get Po 'b))

    (when (is-sym? Cc)
      (quit "make-case-3: todo, no y sym support"))
        
    (a> Cc Po)
    (a> Po C)

    (lay> Po 'y)
    
    # Creates line for Po, C and Pb
    
    # Create Line for Po
    (put Po 'line
      (new '(+Line) NIL Pb NIL))
        
    # Create Line for Pb
    (put Pb 'line
      (new '(+Line) Po NIL Po))
        
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
    (setq *line Po)
    (set-ptr *fwd-in Po)
    (point-sub)
        
    (println 'make-case-3)

    Po))
    
    
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
        Po (gen-point (cons) 'x (cons T NIL) T)
        Pb (get Po 'b))

    (when (is-sym? Cc)
      (quit "make-case-3: todo, no y sym support"))
        
    (a> Cc Po)
    (a> Po Ref)

    (lay> Po 'y)
    
    # Creates line for Po, Ref and Pb
    
    (put Po 'line (new '(+Line) NIL  Pb  NIL))        
    (put Pb   'line (new '(+Line) Po NIL Po))
    (put C    'line (new '(+Line)))
                
    (upd-tree)
    (mov-⇲ *line)
    
    # Maintain on inner tree or outer?
    (setq *line Ref)
    (set-ptr *fwd-in Ref)
    (point-sub)
        
    (println 'make-case-4)

    Po))           
