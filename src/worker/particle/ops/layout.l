# Refactor these into methods

(def '*cur-x)
(def '*cur-y)

(de lay-part-start (Any Base Skip)
  (setq *cur-x NIL
        *cur-y NIL)
  (lay-part Any Base Skip))
  

(de lay-part (Any Base Skip)
  
  #{
    Skip:
    'skip-car
    'skip-cdr
    'skip-car-cdr

    Only apply to immediate Part
  }#
  
  (if (= (type Any) '(+Pair))
  
      (lay-pair Any Base Skip)
      
      (lay-atom Any Base Skip)))


(de lay-pair (Any Base Skip)
    
  # TODO: Handle Y layout
  (when (get Any 'nl 'p)
    (println 'lay-pair "Sublist detected" (get Any 'nl 'base))
    (if (= (get Any 'nl 'base) 'line)
        (align-cur-to-line Any)
        (align-cur-to-prv Any))
    (setq Base *cur))

  # Move Pair first to get correct pos since dims rel to origin
  (mov> Any *cur)
  
  (if (= (get Any 'lay) 'x)    
    (lay-pair-x Any Base Skip)
    (lay-pair-y Any Base Skip)))
  

(de lay-pair-x (Any Base Skip)
  #{

    Draw CAR -Y
    Draw CDR +X
  
    [ ]      CDR  
    CAR ...   
  
    CAR X has precedence
    
    Base is the pos of the list start; lay-pair sets it
  
  }#

  (with Any
  
    (let ((X-Ori Y-Ori) (: ori))
  
      # Mov cur to origin of Pair
      (set-cur-x X-Ori) (set-cur-y Y-Ori)
      
      # Move below or use its Y dims - origin is same as dims for single line
      # This assumes Pair is always single line with dot or 2x dot
      (nl-cur)

      # If Car is a list, mov extra newlines
      (when (has-car-pair> Any)
        (nl-cur (+ *sublist-space-y 1.0)))

      # Lay CAR first since its X bounds has precedence
      
      (let ((X-Dims-A Y-Dims-A) (get (: a) 'dims)
            (X-Dims-B Y-Dims-B) (get (: b) 'dims))
      
        (when (and (: a)
                   (not (= (: a) Any))
                   (or (= Skip 'skip-cdr)
                       (not Skip)))
          (lay-part (: a) Base))

        # To draw Cdr:
        # X =  Ori 1 + Dims-Car 1, plus an adv
        # Y = origin of Pair
        (set-cur-y Y-Ori)

        # Similar to y, after drawing Y Cdr, cur will not be at Car bnds
        # so move cur to Car bnds (max X)
        (if (= (get (: a) 'lay) 'y)
          (prog
            (println 'lay-part-x "Y layout detected for Car")
            (setq *cur (list *cur-x (get *cur 2)))
            (adv-cur))
          (prog                        
            # X should be in place, else get from cur bnds
            (adv-cur)))
        
        # Str's val/b is NIL (or poss later point to itself)
        (when (and (: b)
                   (not (= (: b) Any))
                   (or (= Skip 'skip-car)
                       (not Skip)))
          (lay-part (: b) Base))))))

    
(de lay-pair-y (Any Base Skip)
  #{

    Draw CAR +X
    Draw CDR -Y
  
    [ ] CAR
        ...        
    CDR
  
    CAR Y has precedence
  
    Must return the max X and min Y to calc bounds
  
    Base is the pos of the list start; lay-pair sets it
  }#
  
  (with Any
  
    (let ((X-Ori Y-Ori) (: ori))
    
      # Mov cur to origin of Pair
      (set-cur-y Y-Ori)
      
      # Use X dims
      (set-cur-x (+ X-Ori (get (: dims) 1)))
      (adv-cur)
    
      # Lay CAR first since its Y bounds has precedence

      (let ((X-Dims-A Y-Dims-A) (get (: a) 'dims)
            (X-Dims-B Y-Dims-B) (get (: b) 'dims))
                  
        (when (and (: a)
                   (not (= (: a) Any))
                   (or (= Skip 'skip-cdr)
                       (not Skip)))
          (lay-part (: a) Base))

        # To draw Cdr:
        # X = Origin
        # Y = Ori 2 + Dims-Car 2, plus nl
        (set-cur-x X-Ori)

        # If Car layout is X, cur will be back at Cdr bnds,
        # which means Y is level with Any - must set cur to Y bnds (min Y)
        (when (= (get (: a) 'lay) 'x)
            (println 'lay-part-y "X layout detected for Car")
            (setq *cur (list (get *cur 1) *cur-y)))
            
        (when (: b)
          # Y should be in place, else get from cur bnds
          (nl-cur 2.0))
        
        # Str's val/b is NIL (or poss later point to itself)
        (when (and (: b)
                   (not (= (: b) Any))
                   (or (= Skip 'skip-car)
                       (not Skip)))
          (lay-part (: b) Base))))))
    
  
(de lay-atom (Any Base Skip)
    
  (if (or (isa '+Num Any)
          (isa '+Nil Any))
      
      (prog

        # Move Pair first so below to get correct pos
        (mov> Any *cur)
      
        # Mov cur to bounds, aka last vert start pos
        # Adv to move to end pos of last vert
        (mov-cur-part Any 'end)
        (adv-cur)

        #{
        
         O
          [X] CDR|
          CAR    |
          -------+

         O
          [X] CAR|
          CDR    |
          -------+          
        }#
        
        # max X, min Y
        (setq *cur-x (if *cur-x
                        (max *cur-x (get *cur 1))
                        (get *cur 1)))
        (setq *cur-y (if *cur-y
                         (min *cur-y (get *cur 2))
                         (get *cur 2))))

      (prog
  
        # Should not call lay pair but rather the method
        # lay-sym, lay-str
        (lay-pair Any Base Skip))))


(de force-lay (Part Lay)
  (with Part
    (=: lay Lay)
    (when (and (: a)
                (not (= (: a) Part)))
      (force-lay (: a) Lay))
    (when (and (: b)
               (not (= (: b) Part)))
      (force-lay (: b) Lay))))
              

(de swap-lay (Tgt)
  (force-lay Tgt 
             (if (= (get Tgt 'lay) 'x) 'y 'x))
        
  # Note this causes bounds change also
        
  (mov-cur-part Tgt 'start)
  # Opt: force layout
  (lay-part Tgt (get *main 'ori)))


(de mov-nl (Ref)

  #{
    Move to newline and swap layout
    
    If user wants to maintain x layout, it can be done afterwards explicitly
        
    Given:

      [X]  [X]  [X]  NIL
      A    B    C
  
  }#
  (if *on-car
      
      #{
        Create empty list aka NIL; move newline, x layout
        
        [X]  [X]  [X]  NIL
        A    B         *
                  [X]  NIL
                  NIL
        
      }#
      (repl-car-list NIL 'x)
      
      #{
        Create list with Pair; move newline, y layout
        - If list already, create nested list; call repl-cdr with Ref data

        [X]  [X]
        A    B    

        [Y]  [X]  NIL
        NIL  C
        
        * Calling swap-lay and backspace, will result in other example but
        with C instead of NIL as the CAR
      }#
      # Repl-any expects ptr on car
      (prog
        (let Part (repl-cdr-list Ref (car (get Ref 'data)) 'x)
                
          # Make list y layout
          (put Part 'lay 'y)
                    
          # Mov to newline
          (mov-part-nl Part 'line)
                  
          # Move pointer to deep NIL
          (let? End (get Part 'a 'b)
            (put *0 'b End)
            (mov-part-abv> *0 End (cons T)))
            
          (println 'mov-nl "Added Y+newline")
          
          Part))))


##################

(de mov-nl-2 (Ref)

  # Move to Car
  (point-a-in)
  
  # Create new sublist  
  (let Part (con-any (car (get Ref 'data)) 'x T)
  
    # Swap car layout
    #(swap-lay (get Part 'a))
    
    Part))



(de repl-cdr-list (Ref Data Lay)

  # Similar to repl-car-list but Ref is cdr

  #{
       *0
  [] - [] - NIL
  a    b

  Ref = b
  Prv = a
  Cdr = NIL
  Part = new

       *0
  [] - [] - NIL
  a    c
       
  }#

  (let Part (gen-part (list (list Data))
                       Lay 
                       NIL
                       T)

    (repl-pair Ref Part)
               
    # Delete
        
    Part))


(de mov-part-nl (Ref Base)

  # base = line or prv
    
  # For X layout, move to nl, toggle layout
  # Only do this for lists...and NIL?

  # Link lines
  (let Nxt (get *line 'nl 'n)
  
    # Then create Line and calc its dims (from Ref to NIL/NL)
    (put Ref 'nl (new '(+Line)
                      *line
                      Nxt
                      NIL
                      Base))

    # Link prev n to Ref
    (with (get *line 'nl)
      (=: n Ref))
      
    # Link nxt p to Ref
    (when Nxt
      (with (get Nxt 'nl)
        (=: p Ref))))
    
  (println 'mov-part-nl "Create new line: " Ref (get Ref 'data))
  (println 'mov-part-nl "Prv: " (get Ref 'nl 'p) (get Ref 'nl 'p 'data))
  (println 'mov-part-nl "Nxt: " (get Ref 'nl 'n) (get Ref 'nl 'n 'data))
    
  # Must calc y dims for both lines
  # Layout is not required to calc dims
  
  (put *line 'nl 'dims (calc-dims-nl> *line))

  # Cannot position Ref/newline until current line bnds updated
  # Base irrelevant; lay-part will override it
  # Subsequent lines will be updated also
  (lay-part-start Ref *cur)
  
  # Start from Cdr since it skips lists  
  (put Ref 'nl 'dims (calc-dims-nl> (get Ref 'a)))
  
  #(println 'mov-part-nl (get Ref 'nl 'dims))
  
  (mov-*1 Ref)
  
  # Update previous lines/lists
  # Example, if line shortens, then must reposition cdrs
  (upd-superls (get *line 'nl 'p))  
  
  (setq *line Ref
        *main Ref)
  
  T)
