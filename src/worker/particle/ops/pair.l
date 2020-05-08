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
    
    # Con cars backwards
    (c-a> C)
    
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
    #(c-a> C)

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
      
    # Connect lines
    # C -> Part -> Ref
    (unless (isa '(+Sym) C)
      (put C 'line 'n Part))
      
    (with Ref
      (if (: line)
          (with (: line)
            (=: p Part)
            (=: n (get *line 'line b))
            (=: l (get Part 'line 'l)))
          (=: line (new '(+Line)
                        Part
                        (get *line 'line b)
                        (get Part 'line 'l)))))
        
    (println 'make-cons-car-x-nil Ref (get Ref 'data) (get Ref 'line))
    (view (getl Ref))
        
    (upd-tree)
    (mov-*1 *line)
    
    # Maintain on inner tree or outer?
    (setq *line Part)
    (put *0 'b Part)
    (point-sub)
        
    (println 'make-cons-car-x-nil "Made nested list")

    Part))
    
    
####################
        
        
(de make-line (Ref)

  #{

    [X]  [X]  [X]  NIL
    A    B    C  
  
    Mov to nl:
    

    [X]  [X]
    A    B       
    
    [X]  NIL
    C

  }#

  (println "LINE: " *line (get *line 'data))
  
  (put Ref 'line (new '(+Line)
                      *line
                      (get *line 'line 'n)
                      (if (get *line 'line 'l) @ *line)))
  (when (get *line 'line 'n)
    (put @ 'line 'p Ref))
  (put *line 'line 'n Ref)    
  
  (upd-tree)
  (mov-*1 Ref)
  
  (setq *line Ref)
  (mov-cur-abv Ref)
  (lay-part-start *0 *line 'skip-cdr)
  
  (println 'make-line "Made newline")
    
  Ref)

  
###################


(de ins-line-pre (Part)

  (when (get *line 'p)
    (put @ 'line 'n Part))

  (put *line 'line 'p Part))


(de del-line (Part)

  (let? Nl (get Part 'line)

    (when (get Nl 'p)
      (put @ 'line 'n Nxt))

    (when (get Nl 'n)
      (put @ 'line 'p Prv))))
  
        
(de upd-tree (List)

  (setq *cur (get *master 'ori))
  (lay-part-start *master (get *master 'b)))        
