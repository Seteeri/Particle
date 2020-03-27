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

  # Track lists here

  #(println 'lay-pair Any (get Any 'lay))

  # Move Pair first to get correct pos since dims rel to origin
  (mov> Any *cur)

  #{
  # If newline detected, must follow cmd-new-line
  # However, unlike there, here we can store last NL while traversing
  
  (setq Base (if (get Any 'nl)
                 (prog
                   (println 'lay-pair "Detected NL")
                   (get-origin> Any))
                 Base))
  }#
  
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
      
        # To draw Cdr: (use base-cur? that recalcs...)
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
