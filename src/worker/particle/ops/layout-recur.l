# Refactor these into methods

(def '*cur-x)
(def '*cur-y)

(de upd-cur ()

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
  
  (setq *cur-x (if *cur-x
                  (max *cur-x (get *cur 1))
                  (get *cur 1)))
  (setq *cur-y (if *cur-y
                   (min *cur-y (get *cur 2))
                   (get *cur 2))))

(de upd-dims (Any)
  (with Any
    (let Dims (list (abs (- *cur-x (get (: ori) 1)))
                    (abs (- *cur-y (get (: ori) 2))))
      (=: dims Dims)
      (when (: line)
        (put (: line) 'dims Dims)))))


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
    
  (when (get Any 'line)
    (setq Base Any))
    
  (when (get Any 'line 'p)
    # Need to set dims from cur for current line then reset to NIL    
    (println 'lay-pair "Sublist detected" Any (get Any 'data))
    (if (= (get Any 'line 'base) 'line)
        (align-cur-to-line Any)
        (align-cur-to-prv Any)))

  # Move Pair first to get correct pos since dims rel to origin
  (setq *cur (mov> Any *cur))
  (upd-cur)
  
  # Lay-pair returns last drawn item, but is unused
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

    # If a has a newline, store cur y, then restore after
    # keep tracking x for cdr base
  
    (let ((X-Ori Y-Ori) (: ori)
          Cur-Y *cur-y)
  
      # Mov cur to origin of Pair
      (set-cur-x X-Ori) (set-cur-y Y-Ori)
      
      # Move below or use its Y dims - origin is same as dims for single line
      # This assumes Pair is always single line with dot or 2x dot
      (nl-cur)

      # Lay CAR first since its X bounds has precedence
      (when (and (: a)
                 (not (= (: a) Any))
                 (or (= Skip 'skip-cdr)
                     (not Skip)))      
        
        (lay-part (: a) Base))

      # Store car dims

      (when (get (: a) 'line)
        (setq *cur-y Cur-Y))

      # If line, save current bnds to line before Cdr
      # else, save after Cdr
      (when (get (: b) 'line)
        #(println "Set dims for :" Base (get Base 'data))
        (upd-cur)
        (upd-dims Base)
        (setq Base (: b)))
            
      (when (and (: b)
                 (not (= (: b) Any))
                 (or (= Skip 'skip-car)
                     (not Skip)))

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
            (set-cur-x *cur-x)
            (adv-cur 1.0)))
      
        # Str's val/b is NIL (or poss later point to itself)
        (lay-part (: b) Base))
        
      # Store cdr dims
      (unless (get Any 'line)
        #(println "Set dims for (2):" Any (get Any 'data))
        (upd-cur)
        (upd-dims Any))
      
      # Or NIL
      (when (isa '(+Nil) (: b))
        #(println "Set dims for (3):" Base (get Base 'data))
        (upd-cur)
        (upd-dims Base))
        
    )))

    
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
      (set-cur-x (+ X-Ori (get (: a) 'dims 1)))
      (adv-cur)
    
      # Lay CAR first since its Y bounds has precedence
                  
      (when (and (: a)
                 (not (= (: a) Any))
                 (or (= Skip 'skip-cdr)
                     (not Skip)))
        (lay-part (: a) Base))

      # Str's val/b is NIL (or poss later point to itself)
      (when (and (: b)
                 (not (= (: b) Any))
                 (or (= Skip 'skip-car)
                     (not Skip)))

        # To draw Cdr:
        # X = Origin
        # Y = Ori 2 + Dims-Car 2, plus nl
        (set-cur-x X-Ori)

        # If Car layout is X, cur will be back at Cdr bnds,
        # which means Y is level with Any - must set cur to Y bnds (min Y)
        (if (= (get (: a) 'lay) 'x)
            #(println 'lay-part-y "X layout detected for Car")
            (setq *cur (list (get *cur 1) *cur-y))
            
            # Y should be in place, else get from cur bnds
            (nl-cur 2.0))
        
        (lay-part (: b) Base)))))
    
  
(de lay-atom (Any Base Skip)
    
  (if (or (isa '+Num Any)
          (isa '+Nil Any))
      
      (prog

        # Move Pair first so below to get correct pos
        # mov> does not use draw fn so it does not use *cur
        (setq *cur (mov> Any *cur))
        #(adv-cur)

        (upd-cur)

        Any)

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
  (lay-part Tgt (get *start 'ori)))
