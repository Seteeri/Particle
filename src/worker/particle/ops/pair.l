(de del-rx (Sta End)  
  (let P Sta
    (until (= P End)        
      (del-car> P)
      (setq P (get P 'b)))
    (del-car> P)))

      
(de make-list (Ref)

  (if (and (= (get Ref 'c 'c 'a) (get Ref 'c))
           (= (get Ref 'c 'a) Ref))
      
      (make-cons-car-y Ref)
        
      (if (isa '(+Nil) Ref)
      
          (make-cons-car-x-nil Ref)
      
          (make-cons-car-x-sym Ref))))

      
(de make-cons-car-y (Ref)
  
  #{
  
    1. Gen pair; skip Car
    2. Set car to current Pair
    3. Attach new Pair
    
    [Ccc]
    
    [Cc]  [C]  NIL
          a
    NIL   

    ->
    
    [Ccc]
    
    [Cc]  [P]  [C]  NIL
               a
          NIL
    NIL
    
    
    FIX:
    * Must set cars until Car is not a Pair
    
  }#
  
  (let (Part (gen-part (cons)
                       'x
                       'skip-car)
        C (get Ref 'c)
        Cc (get C 'c))
    
    (a> Part C)
    
    (a> Cc Part)
    
    # Line
    (with Part
      
      (=: lay 'y)
          
      # Create Line start
      (=: line (new '(+Line)
                    NIL (: b) NIL))
      
      # Create Line for NIL/Cdr
      (put (: b) 'line (new '(+Line)
                            Part NIL Part)))
    
    (upd-tree)
    (mov-*1 *line)
    
    # Maintain on inner tree or outer?
    (setq *line Cc)
    (put *0 'b Cc)
    (point-sub)
        
    (println 'make-cons-car-y "Made nested list")

    Part))
      
      
(de make-cons-car-x-sym (Ref)
  
  #{
      
    [ ]  [Cc]  [C]  NIL
    a    b     c*
  
    ->
  
    [ ]  [Cc]
    a    b

    [P]  [C]  NIL
         c*
    NIL
    
    ---
    
    [ ]  [ ]  NIL
    sym  a*
    
    -> 

    [ ]  [ ] [ ]  NIL
    sym      a*    
         NIL
  }#
  
  (let (Part (gen-part (cons)
                       'x
                       'skip-car)
        C (get Ref 'c)
        Cc (get C 'c))
    
    (a> Part C)

    (b> Cc Part)
    
    # Create lines for Part
    (with Part
     
      (=: lay 'y)
           
      # Use ins-line?        
      
      # Create Line
      (=: line (new '(+Line)
                    (unless (isa '(+Sym) Cc)
                      *line)
                    (: b)
                    (unless (isa '(+Sym) Cc)
                      (if (get *line 'line 'l) @ *line))))
                  
      # Create Line for Cdr
      (put (: b) 'line (new '(+Line)
                            Part
                            (get *line 'line 'n)
                            (if (isa '(+Sym) Cc)
                                Part
                                (if (get *line 'line 'l) @ *line))))
      
      # Connect lines
          
      # Con nxt line
      (put *line 'line 'n 'line 'p (: b))
          
      # Con prv line
      (put *line 'line 'n Part))
               
        
    # Create line for C
    (with C
      (=: line (new '(+Line))))
        
    (upd-tree)
    (mov-*1 *line)
    
    # Maintain on inner tree or outer?
    (setq *line Part)
    (put *0 'b Part)
    (point-sub)
        
    (println 'make-cons-car-x-sym "Made nested list")

    Part))

    
(de make-cons-car-x-nil (Ref)
  
  #{ 
    
    Case 1:
    =======
   
    [ ]
    a
      
    [C]  [ ]  NIL
         b
         
    NIL
    *
    
    ->
    
    ...

    [ ]  [ ]  NIL
         NIL
    
    NIL
    
    
    Case 2:
    =======
    
    [ ]     NIL
    *start  *

    ->
    
    [ ]     [ ]  NIL
    *start     
            NIL
  }#
  
  (let (Part (gen-part '(NIL)
                       'x
                       NIL
                       'skip-cdr)
        C (get Ref 'c))
         
    (b@ C Part Ref)
        
    # Create line for Part and Car
    (with Part
     
      (=: lay 'y)
      
      (=: line (if (isa '(+Sym) C)
                   (new '(+Line)
                        NIL
                        (: b)
                        NIL)
                   (new '(+Line)
                        C
                        (: b)
                        (if (get *line 'line 'l) @ *line))))
                  
      # Create Line for Car
      (put (: a) 'line (new '(+Line))))
      
    # Connect line for non-sym
    (unless (isa '(+Sym) C)
      (put C 'line 'n Part))
    
    # Create line for Part's Cdr
    (with Ref
      # keep x lay
    
      (if (: line)
          (with (: line)
            (=: p Part)
            (=: n (get *line 'line b))
            (=: l (if (get Part 'line 'l) @ Part)))
          (=: line (new '(+Line)
                        Part
                        (get *line 'line b)
                        (get Part 'line 'l)))))
    
    (when T
      (prinl) (prinl)
      
      (println "Part: " Part)
      (println "lay: " (get Part 'lay))
      (when NIL
        (println "line.p: " (get Part 'line 'p))
        (println "line.n: " (get Part 'line 'n))
        (println "line.l: " (get Part 'line 'l)))
      (when NIL
        (println "a: " (get Part 'a))
        (println "b: " (get Part 'b))
        (println "c: " (get Part 'c)))
      (prinl) (prinl)

      (println "Car: " (get Part 'a))
      (println "lay: " (get Part 'a 'lay))
      (when NIL
        (println "line.p: " (get Part 'a 'line 'p))
        (println "line.n: " (get Part 'a 'line 'n))
        (println "line.l: " (get Part 'a 'line 'l)))
      (when NIL
        (println "a: " (get Part 'a 'a))
        (println "b: " (get Part 'a 'b))
        (println "c: " (get Part 'a 'c)))
      (prinl) (prinl)
      
      (println "Cdr: " (get Part 'b))    
      (println "lay: " (get Part 'b 'lay))
      (when NIL
        (println "line.p: " (get Part 'b 'line 'p))
        (println "line.n: " (get Part 'b 'line 'n))
        (println "line.l: " (get Part 'b 'line 'l)))
      (when NIL    
        (println "a: " (get Part 'b 'a))
        (println "b: " (get Part 'b 'b))
        (println "c: " (get Part 'b 'c)))
      (prinl) (prinl))
        
    (upd-tree)
    (mov-*1 *line)
    
    (setq *line (get Part 'a))
    (put *0 'b (get Part 'a))
    (point-sub)
        
    (println 'make-cons-car-x-nil "Made nested list")

    Part))

    
(de del-pair (Ref)

  #{
  
    Delete previous

    
              *0
    [ ]  [ ]  NIL
    a    b

    #############
    
        *0
    [ ]  [ ]  NIL
    a    b

    #############
    
    *0
    [ ]  [ ]  NIL
    a    b    

    #############
    
    [ ]  [ ]  NIL
    a    b        

    *0
    [ ]  [ ]  NIL
        c        

    #############
    
    [ ]  [ ]  NIL
    a    b        

    
    [ ]  [ ]  NIL
        NIL        
        
    *0     
    NIL
    
  }#

  # Refactor into a fn in ops?

  # Similar to adding items, must update CDR and list CDR

  (let (Tgt (if *on-car (get Ref 'c 'c) (get Ref 'c))
        Tgt-Prv (get Tgt 'c)
        Tgt-Cdr (get Tgt 'b)
        New-Ref NIL)

    (unless (= (get Tgt 'data) '*start)
    
      (cond ((or (isa '(+Pair) (get Tgt 'a))
                 (isa '(+Nil)  (get Tgt 'a)))
              (prinl) (prinl) (println "COND 1") (prinl) (prinl)
              # Remove line
              (del-line Tgt)
              (b> Tgt-Prv Tgt-Cdr)
              (put Tgt-Cdr 'line 'l (get Tgt 'line 'l))
              (setq New-Ref Tgt-Cdr))
    
            # If Tgt prv is Car of Pair, set Car...
            ((= (get Ref 'c 'a) Ref)
              (prinl) (prinl) (println "COND 2") (prinl) (prinl)
              (put Tgt-Prv 'a Tgt-Cdr)
              # To connect, pop, then push
              (with Tgt-Prv
                (pop (:: data))
                (push (:: data) (get Tgt-Cdr 'data)))
              (put Tgt-Cdr 'c Tgt-Prv)
              (put-line Tgt Tgt-Cdr)
              
              # Tgt-Prv = Y pair
              # It's list has to be modified also
              # so get item before and connect new data
              (with (get Tgt-Prv 'c)
                (con (car (:: data)) (get Tgt-Prv 'data)))
                
              (setq New-Ref Ref))
                
            (T
              (b> Tgt-Prv Tgt-Cdr)
              (put Tgt-Cdr 'c Tgt-Prv)
              (when (get Tgt 'line)
                (put-line Tgt Tgt-Cdr))
              (setq New-Ref Ref)))
      
      # Unlink b before del
      (put Tgt 'b)
      (del> Tgt T T)
      
      (upd-tree)
      (mov-*1 *line)      
      
      # Ptr remains on Ref
      # How about for on-car?
      (put *0 'b New-Ref)
      (mov-cur-abv New-Ref)
      (lay-part-start *0 *line 'skip-cdr)
          
      T)))      
    
