#{

    [X]  CDR/ATM
    
    CAR/ATM
    
    
    [Y]  CAR/ATM
    
    CDR/ATM
    
    
    X/Y dictate line changes
}#

(de make-list-car (Ptr)

  #{
      
    [X]  CDR/ATM
    
    CAR/ATM
    
    
    [Y]  CAR/ATM
    
    CDR/ATM
    
    
    3 Factors:
    
      * Car/Cdr      
      * Atm/Pair
      
      * X/Y of P-Cell
    
  }#

  (cond
    
    #####
    # NIL
    
    ((a-nil?> *ptr)
      (println 'make-list-car "A NIL")
      #(make-list-car-a-atom)
      T)
      
    ((a-atom?> *ptr)
      (println 'make-list-car "A ATOM")
      (make-list-car-a-atom))      
      
    ((b-nil?> *ptr)
      (println 'make-list-car "B NIL")
      T)      
      
    ((b-atom?> *ptr)
      (println 'make-list-car "B ATOM")
      T)
      
    ######
    # PAIR

    ((a-pair?> *ptr)
      (println 'make-list-car "A PAIR")
      T)
      
    ((b-pair?> *ptr)
      (println 'make-list-car "B PAIR")
      # See pair-cdr
      T)
      
    (T
      # root
      (println 'make-list-car "*ptr uninitialized"))))


(de make-list-car-a-atom ()

  #{
              
        [Cc]  
        NIL

         
        [X/C]  Cb
        NIL
        *
    ->
               
        [Cc]  
          A   
          
        [Po-Pair]  [Po-List]   
                    NIL
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

              
        [Y/P]  [C]  NIL
               NIL  
               *
        
        [Cb] NIL
        NIL
        
        Poss Cars:
        1. New list with Car
        2. Car itself
           * Same Pair with Y layout
           * Cdr has line also
        3. NIL
        
  }#   

  # Handle NIL
  
  (let (Tgt (ref> *ptr)
        Po-Tgt (lup-pt Tgt)
        Pair (find-back> *ptr (rel> *ptr))
        Po-Pair (lup-pt Pair))
    
    (println "Car/Pair: " Tgt Pair (get Po-Pair 'a))
  
    # otherwise set-ref will attempt uncol on ref
    # pass flag for this...
    (put *ptr 'ref)  
    # put NIL otherwise set will del new point
    (put Po-Pair 'a)
    (del> Po-Tgt)
    
    (let (List (list Tgt)
          Po-List (gen-point List 'x NIL)
          Ln (c-ln> *ptr)
          Ln-N (get Ln 'line 'n)
          Cdr (cdr Pair)
          Po-Cdr (if Cdr (lup-pt Cdr) (get Po-Pair 'b))
          Push-Pair (not (get Po-Pair 'line)))
    
      # Repl tgt with list with tgt list
      (set> Po-Pair List Po-List)
      
      # Set layout for Pair
      (set-lay> Po-Pair 'y)
      
      (if (get Po-Pair 'line)
          (put @ 'n Po-Cdr)      
          (ins-line-aft Ln Po-Pair))
      
      # Create Line for Cdr
      (if (get Po-Cdr 'line)
          (put @ 'p Po-Pair)
          (ins-line-aft Po-Pair Po-Cdr))
          
      # Create line for List
      (put Po-List 'line (new '(+Line)))
      
      (upd-tree)
      (mov-⇲ Ln)
            
      # Pt to list
      (set-ref> *ptr List 'a)
            
      # Push cell and car
      (when Push-Pair
        (ln> *ptr Po-Pair))
      (ln> *ptr Po-List)
      
      (top> Po-List)
      (draw> *ptr))))
      
      
######################################################
######################################################

#{
(de make-list-car (Ptr)
  
  # Check Y then x
  
  # Refactor upd into here like str cmds
  
  (cond
  
    ((and (bel-y?> Ref)
          (is-car?> Ref))
      #{
        [Y]  Atm
              *
        Atm      
      }#          
     (make-list-car-ya Ref))
      
    ((and (lay-x?> Ref)
          (is-car?> Ref))
      (if (and (bel-y?>  (get Ref 'c))
               (is-car?> (get Ref 'c)))
          #{
            [Y]  [X]  Atm
                  A
                  *          
          }#
          (make-list-car-yxa Ref)
          
          #{
            [X/Sym]  [X]  Atm
             .        A
                      *          
          }#
          (make-list-car-xxb Ref)))
  
    ((or (is-cdr?> Ref)
         (is-end?> Ref))
      #{
        [X+L/Y]
          A
        
        Atm
        *      
      }#
     (make-list-car-xyb Ref))
                
    (T
      (quit "make-list-car: unknown case"))))
}#

(de make-list-car-xxb (Ref)
  
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
                      
        [Cc]  [Po-Pair]  [Cb]  NIL
        NIL   NIL        NIL
               *
              
    ->
               
        [Cc]  
        NIL  

              
        [Po-Pair]  [Po-List]  NIL
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
        Ln-N (get *line 'line 'n)
        Cc-Is-Sym (is-sym? Cc))

    (b> Cc Po)
    (a> Po C)
    (b> C Pb)
    (if Cb
        (b> Po Cb)
        (setq Cb (get Po 'b)))
    
    # Creates line for Po, C and Cb
       
    (set-lay> Po 'y)
    
    (if (get C 'line)
    
        (prog
        
          (mov-line C Po)
          (put Po 'line 'n Cb))
          
        (prog
          
          # Connect line: cur -> Po
          (unless Cc-Is-Sym
            (put *line 'line 'n Po))
          (put Po 'line
            (new '(+Line)
              (unless Cc-Is-Sym *line)
              Cb
              (unless Cc-Is-Sym
                (if (get *line 'line 'l) @ *line))))))
                  
    # Create Line for Cdr
    (if (get Cb 'line)
        (put @ 'line 'p Po)
        (put Cb 'line
          (new '(+Line)
               Po
               Ln-N
               (if Cc-Is-Sym
                   Po
                   (get Po 'line 'l)))))
        
    # Create line for C
    (put C 'line (new '(+Line)))
                
    # P 'line 'p = Ref
    #~(assert (= (get Po 'line 'p) Cc))
                
    (upd-tree)
    (mov-⇲ (car (get *ptr 'ln)))
    
    # Maintain on inner tree or outer?
    (setq *line Po)
    (set-ptr-ref Po)
    (pt-sub) # Mov to C
        
    (println 'make-list-car-xxb)

    Po))      
          

(de make-list-car-xyb (Ref)
  
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
    
    (set-lay> Po 'y)
    
    (if Is-Cc-Sym
        
        (prog
    
          # Creates line for Po, Cb, Ref
          (put Po 'line
            (new '(+Line)
                NIL
                Cb
                NIL))
              
          # Create Line for Cdr
          (put Cb 'line
            (new '(+Line)
                  Po
                  NIL
                  Po))
              
          # Create line for Ref
          (put Ref 'line (new '(+Line))))

          
        (prog
    
          # Creates line for Po, Cb, Ref
          (put Po 'line
            (new '(+Line)
                 *line
                 Cb
                 (if (get *line 'line 'l) @ *line)))
              
          # Connect line: cur -> Po
          (put Po 'line 'p 'line 'n Po)
              
          # Create Line for Cdr
          (put Cb 'line
            (new '(+Line)
                  Po
                  NIL
                  (get Po 'line 'l)))
              
          # Create line for Ref
          (put Ref 'line (new '(+Line)))))
          
    # P 'line 'p = Ref
    #~(assert (and (not (= (get Po 'line 'p) Ref))
    #              (not (= (get Po 'line 'l) Ref))))    
    
    (upd-tree)
    (mov-⇲ (car (get *ptr 'ln)))
    
    # Maintain on inner tree or outer?
    (setq *line Ref)
    (set-ptr-ref Ref)
    (pt-sub)
        
    (println 'make-list-car-xyb)

    Po))
    
    
(de make-list-car-yxa (Ref)
  
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
      (quit "make-list-car-yxa: todo, no y sym support"))
        
    (a> Cc Po)
    (a> Po C)

    (set-lay> Po 'y)
    
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
    (mov-⇲ (car (get *ptr 'ln)))
    
    # Maintain on inner tree or outer?
    (setq *line Po)
    (set-ptr-ref Po)
    (pt-sub)
        
    (println 'make-list-car-yxa)

    Po))
    
    
(de make-list-car-ya (Ref)
  
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
      (quit "make-list-car-ya: todo, no y sym support"))
        
    (a> Cc Po)
    (a> Po Ref)

    (set-lay> Po 'y)
    
    # Creates line for Po, Ref and Pb
    
    (put Po 'line (new '(+Line) NIL  Pb  NIL))        
    (put Pb   'line (new '(+Line) Po NIL Po))
    (put C    'line (new '(+Line)))
                
    (upd-tree)
    (mov-⇲ (car (get *ptr 'ln)))
    
    # Maintain on inner tree or outer?
    (setq *line Ref)
    (set-ptr-ref Ref)
    (pt-sub)
        
    (println 'make-list-car-ya)

    Po))           
