# Move more stuff to methods
# Standardize checks
# Standardize prim ops

  
###################
###################
# Pair/List
  

(de make-list (Ref)

  (if (and (= (get Ref 'c 'c 'a) (get Ref 'c))
           (= (get Ref 'c 'a) Ref))
      
      (make-cons-car-y Ref)
        
      (make-cons-car-x Ref)))

      
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
  
  (let (Part (gen-part (cons NIL NIL)
                       'x
                       'skip-car)
        C (get Ref 'c)
        Cc (get C 'c))
    
    # Con Part/Car -> C
    (put Part 'a C)
    
    # Con data, P -> C
    (put Part 'data (cons (get C 'data)))    
    
    # Con Part <- C
    (put C 'c Part)

    # Con CAR
    # Con Cc -> P
    # Con data, Cc -> P    
    (put Cc 'a Part)
    (put Cc 'data (cons (get Part 'data)
                        (cdr (get Cc 'data))))
        
    # Line
    (with Part
      
      (=: lay 'y)
    
      # Con Cc <- Part
      (=: c Cc)
      
      # Create Line start
      (=: line (new '(+Line)
                    NIL (: b) NIL))
      
      # Create Line for NIL/Cdr
      (put (: b) 'line (new '(+Line)
                            Part NIL Part)))
    
    # Con cars backwards
    (con-cars-c> C)
    
    (upd-tree)
    (mov-*1 *line)
    
    # Maintain on inner tree or outer?
    (setq *line Cc)
    (put *0 'b Cc)
    (point-sub)
        
    (println 'make-list-car "Made nested list")

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
    
  }#
  
  (let (Part (gen-part (cons NIL NIL)
                       'x
                       'skip-car)
        C (get Ref 'c)
        Cc (get C 'c))
         
    # Con Part/Car -> C
    (put Part 'a C)
    
    # Con data, P -> C
    (put Part 'data (cons (get C 'data)))    
    
    # Con Part <- C
    (put C 'c Part)

    # Con CDR - Diff from Y layout
    # Con Cc -> P
    # Con data, Cc -> P    
    (con> Cc Part)
    
    # Create lines for Part
    (with Part
     
      (=: lay 'y)
     
      # Con Cc <- Part
      (=: c Cc)
      
      # Use ins-line?        
      
      # Create Line
      (=: line (new '(+Line)
                    *line
                    (: b)
                    (if (get *line 'line 'l) @ *line)))
                  
      # Create Line for Cdr
      (put (: b) 'line (new '(+Line)
                            Part
                            (get *line 'line 'n)
                            (if (get *line 'line 'l) @ *line)))
      
      # Connect lines
          
      # Con nxt line
      (put (get *line 'line 'n 'line) 'p (: b))
          
      # Con prv line
      (put (get *line 'line) 'n Part))
               
        
    # Create line for C
    (with C
      (=: line (new '(+Line))))
        
    (upd-tree)
    (mov-*1 *line)
    
    # Maintain on inner tree or outer?
    (setq *line Part)
    (put *0 'b Part)
    (point-sub)
        
    (println 'make-list-car "Made nested list x")

    Part))

        
(de make-line (Part)

  #{

    [X]  [X]  [X]  NIL
    A    B    C  
  
    Mov to nl:
    

    [X]  [X]
    A    B       
    
    [X]  NIL
    C

  }#

  (put Part 'line (new '(+Line)
                      *line
                      (get *line 'line 'n)
                      (if (get *line 'line 'l) @ *line)))
  (put (get *line 'line) 'n Part)
  (put (get *line 'line 'n 'line) 'p Part)
  
  (upd-tree)
  (mov-*1 Part)
  
  (mov-cur-abv Part)
  (lay-part-start *0 *line 'skip-cdr)
  
  (setq *line Part))

  
###################


(de ins-line-pre (Part)

  (let? Prv (get *line 'p)
    (put (get Prv 'line) 'n Part))

  (put (get *line 'line) 'p Part))


(de del-line (Part)
  # Assumes sublines will be deleted or no longer ref'd
  # or handled by caller

  (let (Nl (get Part 'line)
        Prv (get Nl 'p)
        Nxt (get Nl 'n))

    (when Prv
      (put (get Prv 'line) 'n Nxt))

    (when Nxt
      (put (get Nxt 'line) 'p Prv))))
  
        
(de upd-tree (List)

  (setq *cur (get *master 'ori))
  (lay-part-start *master (get *master 'b)))        
