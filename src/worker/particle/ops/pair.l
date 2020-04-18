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
  
  (let (Part (gen-part (cons NIL NIL)
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
    
    # Con cars backwards
    (con-cars-c> C)
    
    (upd-tree)
    (mov-*1 *line)
    
    # Maintain on inner tree or outer?
    (setq *line Cc)
    (put *0 'b Cc)
    (point-sub)
        
    (println 'make-cons-car-y "Made nested list")

    Part))
      
      
(de make-cons-car-x-sym (Ref)
  #..-sym
  
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
    
    (a> Part C)

    (b> Cc Part)
    
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
        
    (println 'make-cons-car-x-sym "Made nested list")

    Part))

    
(de make-cons-car-x-nil (Ref)
  
  #{ 
    
    For NIL
    
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
    
  }#
  
  (let (Part (gen-part '((NIL))
                       'x
                       NIL
                       'skip-cdr)
        C (get Ref 'c))
         
    (b> C Part)

    (b> Part Ref)
    
    # Create lines for Part
    (with Part
     
      (=: lay 'y)
           
      # Use ins-line?        
      
      # Create Line
      (=: line (new '(+Line)
                    C
                    (: b)
                    (if (get *line 'line 'l) @ *line)))
                  
      # Create Line for Car
      (put (: a) 'line (new '(+Line))))
      
    # Connect lines
    # C -> Part -> Ref
    (put C 'line 'n Part)
    (put Ref 'line 'p Part)
        
    (upd-tree)
    (mov-*1 *line)
    
    # Maintain on inner tree or outer?
    (setq *line Part)
    (put *0 'b Part)
    (point-sub)
        
    (println 'make-cons-car-x-nil "Made nested list")

    Part))
    
    
####################
        
        
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
  (let? Nxt (get *line 'line 'n)
    (put Nxt 'line 'p Part))
  
  (upd-tree)
  (mov-*1 Part)
  
  (setq *line Part)
  (mov-cur-abv Part)
  (lay-part-start *0 *line 'skip-cdr)
  
  (println 'make-line "Made newline")
  
  Part)

  
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
