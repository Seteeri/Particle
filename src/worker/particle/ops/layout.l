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
  
  (if (isa '(+Pair) Any)
  
      (lay-pair Any Base Skip)
      
      (lay-atom Any Base Skip)))


(de lay-pair (Any Base Skip)
    
  # TODO: Handle Y layout
  (when (get Any 'line 'p)
    (println 'lay-pair "Sublist detected" Any (get Any 'data))
    (if (= (get Any 'line 'base) 'line)
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
            #(println 'lay-part-x "Y layout detected for Car")
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
            #(println 'lay-part-y "X layout detected for Car")
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
