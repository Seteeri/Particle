(de del-rx (Sta End)  
  (let P Sta
    (until (or (= P End) (not P))
      (del-car> P)
      (setq P (get P 'b)))
    (when P
      (del-car> P))))

      
(de make-list (Ref)

  (if (isa '(+Nil) Ref)
  
      (if (= (get Ref 'c 'a) Ref)
  
          (make-cons-car-y Ref)    

          (make-cons-car-x-nil Ref))
      
      (if (and (= (get Ref 'c 'c 'a) (get Ref 'c))
               (= (get Ref 'c 'a) Ref))
          
          (make-cons-car-y Ref)

          (make-cons-car-x Ref))))

      
(de make-cons-car-y (Ref)
  
  #{
    Handle nested  
  
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
  
  (let (C (if (isa '(+Nil) Ref) Ref (get Ref 'c))
        Cc (get C 'c)
        Part (gen2-part (cons) 'x (cons T NIL) T))
    
    (a> Part C)
    
    (a> Cc Part)
    
    # Line
    (with Part
      
      (lay> This 'y)
          
      # Create Line start
      (=: line (new '(+Line)
                    NIL (: b) NIL))
      
      # Create Line for NIL/Cdr
      (put (: b) 'line (new '(+Line)
                            Part NIL Part)))
    
    (upd-tree)
    (mov-⇲ *line)
    
    # Maintain on inner tree or outer?
    (setq *line Cc)
    (set-ptr *fwd-in Cc)
    (point-sub)
        
    (println 'make-cons-car-y "Made nested list")

    Part))
      
      
(de make-cons-car-x (Ref)
  
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
    
    Handles when NIL is car - no
    
    [ ]  [ ]  [ ]
    a    b    c
    
    [ ]  NIL
    
    NIL
    
  }#
      
  (let (C (get Ref 'c)
        Cc (get C 'c)
        Part (gen2-part (cons) 'x (cons T NIL) T))    
    
    (a> Part C)

    (b> Cc Part)
    
    # Create lines for Part
    (with Part
     
      (lay> This 'y)
           
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
    (mov-⇲ *line)
    
    # Maintain on inner tree or outer?
    (setq *line Part)
    (set-ptr *fwd-in Part)
    (point-sub)
        
    (println 'make-cons-car-x "Made nested list")

    Part))

    
(de make-cons-car-x-nil (Ref)
  
  #{ 
    When NIL is end
    
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
    *
    
    Case 2:
    =======
    
    [ ]     NIL
    *start  *

    ->
    
    [ ]     [ ]  NIL
    *start     
            NIL
  }#
    
  (let (C (get Ref 'c)
        Part (gen2-part (list NIL) 'x (cons NIL T) T))
         
    (b@ C Part Ref)
        
    # Create line for Part and Car
    (with Part
     
      (lay> This 'y)
      
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
    
    (when NIL
      (prinl) (prinl)
      
      (println "Part: " Part)
      (println "lay: " (get Part 'lay))
      (when NIL
        (println "line.p: " (get Part 'line 'p))
        (println "line.n: " (get Part 'line 'n))
        (println "line.l: " (get Part 'line 'l)))
      (when T
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
      (when T
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
      (when T  
        (println "a: " (get Part 'b 'a))
        (println "b: " (get Part 'b 'b))
        (println "c: " (get Part 'b 'c)))
      (prinl) (prinl))
        
    (upd-tree)
    (mov-⇲ *line)
    
    (setq *line (get Part 'a))
    (set-ptr *fwd-in (get Part 'a))
    (point-sub)
                
    (println 'make-cons-car-x-nil "Made nested list")

    Part))

    
(de del-pair (Ref)

  #{
  
    Delete previous

    
              *fwd-in
    [ ]  [ ]  NIL
    a    b

    #############
    
        *fwd-in
    [ ]  [ ]  NIL
    a    b

    #############
    
    *fwd-in
    [ ]  [ ]  NIL
    a    b    

    #############
    
    [ ]  [ ]  NIL
    a    b        

    *fwd-in
    [ ]  [ ]  NIL
        c        

    #############
    
    [ ]  [ ]  NIL
    a    b        

    
    [ ]  [ ]  NIL
        NIL        
        
    *fwd-in     
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
      (mov-⇲ *line)      
      
      # Ptr remains on Ref
      # How about for on-car?
      (set-ptr *fwd-in New-Ref)
      (mov-cur-abv New-Ref)
      (lay-ptr *fwd-in)
          
      T)))      
    
