(de force-lay (Part Lay)
  (with Part
    (=: lay Lay)
    (when (and (: a)
                (not (= (: a) Part)))
      (force-lay (: a) Lay))
    (when (and (: b)
               (not (= (: b) Part)))
      (force-lay (: b) Lay))))
      

# Refactor these into methods

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

  (println 'lay-pair Any (get Any 'data))
        
  # Handle newline - same fn as mov-nl
  (let? Nl-Prv (get Any 'nl 'p)
  
    (println 'lay-pair 'NL Nl-Prv (get Nl-Prv 'data))

    # Align cursor to start of prv line
    # x = origin of prv line
    # y = bnds of prv line
    
    # if sym, get val, else itself
    (let (Ori (if (isa '+Sym Nl-Prv)
                  (get-origin> (get Nl-Prv 'b))
                  (get-origin> Nl-Prv)))
    
      (set-cur-x (get Ori 1))
      
      (set-cur-y (+ (get Ori 2)
                    (get Nl-Prv 'nl 'bnds 2)))
      (nl-cur *sublist-space-y)
      
      # Use first item origin as base
      (setq Base *cur)))

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
  
    (let ((X-Ori Y-Ori) (get-origin> Any))
  
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
      
        (when (or (= Skip 'skip-cdr)
                  (not Skip))
          (lay-part (: a) Base))
      
        # To draw Cdr: (use align-cur-to-bnds? that recalcs...)
        # X =  Ori 1 + Dims-Car 1, plus an adv
        # Y = origin of Pair
        
        # X should be in place
        # else have to trace Car
        #(set-cur-x (+ X-Ori X-Dims-A)) (adv-cur)
        (adv-cur)
        
        (set-cur-y Y-Ori)
        
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
  
    (let ((X-Ori Y-Ori) (get-origin> Any))
    
      # Mov cur to origin of Pair
      (set-cur-y Y-Ori)
      
      # Use X dims
      (set-cur-x (+ X-Ori (get (: dims) 1)))
      (adv-cur)
    
      # Lay CAR first since its Y bounds has precedence

      (let ((X-Dims-A Y-Dims-A) (get (: a) 'dims)
            (X-Dims-B Y-Dims-B) (get (: b) 'dims))
      
        (when (or (= Skip 'skip-cdr)
                  (not Skip))
          (lay-part (: a) Base))
        
        # To draw Cdr:
        # X = Origin
        # Y = Ori 2 + Dims-Car 2, plus nl
        (set-cur-x X-Ori)

        #(set-cur-y (+ Y-Ori Y-Dims-A)) (nl-cur 3.0)
        # Y should be in place
        # else have to trace Car
        (nl-cur)
                
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
        (adv-cur))

      (prog
  
        # Should not call lay pair but rather the method
        # lay-sym, lay-str
        (lay-pair Any Base Skip))))


################################################################################

        
(de swap-lay (Tgt)
  (force-lay Tgt 
             (if (= (get Tgt 'lay) 'x) 'y 'x))
        
  # Note this causes bounds change also
        
  (mov-cur-part Tgt 'start)
  # Opt: force layout
  (lay-part Tgt (get-origin> *main)))


(de mov-nl (Ref)
  # Is this appropriate for Y layout? ...since it already moves to newline
  # -> make new column...
  
  (if (= (get Ref 'lay) 'x)
  
      (prog
            
        #(mov-*1 *line)
            
        # Shorten line dims to Ref c origin
        # since Ref itself will be on a newline
        (adj-line-to-part> *line (get Ref 'c))
                      
        # Then create Line and calc its dims (from Ref to NIL/NL)
        (put Ref 'nl (new '(+Line)
                          *line
                          NIL
                          (calc-bounds-nl> Ref)))

        # Link prev line also
        (with *line (put (: nl) 'n Ref))
                           
        # Base irrelevant; lay-part will override it
        (lay-part Ref *cur)
        
        (setq *line Ref)

        # update cdr etc.
        T)
        
      (prog
        (println "IMPLEMENT MOV-NL FOR Y LAYOUT"))))
