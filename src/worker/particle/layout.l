(de is-start-of-list (Part) (= (type (get Part 'a)) '(+Pair)))

(de lay-part (Any Base Car-Skip Cdr-Skip)
  
  (if (= (type Any) '(+Pair))
  
      (lay-pair Any Base Car-Skip Cdr-Skip)
      
      (lay-atom Any Base Car-Skip Cdr-Skip)))


(de lay-pair (Any Base Car-Skip Cdr-Skip)

  (println 'lay-pair Any (get Any 'data) (get Any 'lay))

  # If Any is on a newline
  # move rel to Base - at x and underneath it
  # this is assuming min y
  # so if calling on nl, cur should be set already before calling this
  # and use origin as new base
  (setq Base (if (get Any 'nl)
                 (prog
                   (println 'lay-pair "Detected NL")
                   (set-cur-x (get Base 1))
                   (nl-cur 3.0)
                   (get-origin> Any))
                 Base))

  # Move Pair first so below can get correct pos
  (mov> Any *cur)
  
  (if (= (get Any 'lay) 'x)    
    (lay-pair-x Any Base Car-Skip Cdr-Skip)
    (lay-pair-y Any Base Car-Skip Cdr-Skip)))
  

(de lay-pair-x (Any Base Car-Skip Cdr-Skip)
  #{

    Draw CAR -Y
    Draw CDR +X
  
    [ ]      CDR  
    CAR ...   
  
    CAR X has precedence
    
    Base is the pos of the list start; lay-pair sets it

    [Y]  [X]
         1    
         
         [Y]  [X]  NIL
              3
              
         NIL
         
    NIL

    [X]
    let
         
    [Y]  [X]  [X] NIL
         A    1

    NIL
    
    For X layout, if CDR is a list, move it rel to base
  
    If Base is [Y], adv 2.0 to get into pos
    else first item, so no adv, simply draw under
  
  }#
  
  (let ((X Y) *cur)
      
    (with Any
    
      # Mov cur to bounds, aka first vert start pos
      (mov-cur-vert (first-vert> Any))
      # Move below
      (nl-cur)
      
      # Lay CAR first since its X bounds has precedence
      (let (Bnd-Car (lay-part (: a) Base)
            Bnd-Cdr Bnd-Car)
        
        #{
        (if (: nl)
            (prog
              # Align cur to bounds of main 'b or last nl
              # Y is already at extents based on drawn Cars
              (set-cur-x (get Base 1))
              #(nl-cur 4.0)
              )
        }#
              
            (prog
              # At this point, cur is at extents of Car
              # X is valid except for one more adv
              # Y must be moved back to base
              (adv-cur)
              (set-cur-y Y))
        
        # Str's val/b is NIL (or poss later point to itself)
        (when (and (: b) (not (= (: b) Any)))
          (setq Bnd-Cdr (lay-part (: b) Base)))
            
        (list (max (get Bnd-Car 1) (get Bnd-Cdr 1))
              (min (get Bnd-Car 2) (get Bnd-Cdr 2)))))))

    
(de lay-pair-y (Any Base Car-Skip Cdr-Skip)
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

  #(println Any (get Any 'data))
  
  (let ((X Y) *cur)
      
    (with Any
    
      # Mov cur to bounds, aka last vert start pos
      (mov-cur-vert (last-vert> Any))
      # Adv two spaces, after last vert, then one more space
      (adv-cur 2.0)
    
      # Lay CAR first since its Y bounds has precedence
      (let (Bnd-Car (lay-part (: a) Base)
            Bnd-Cdr Bnd-Car)
        
        # At this point, cur is at extents of Car
        # X must be moved back to base      
        # Y is valid except for one more newline
        (set-cur-x X)
        (set-cur-y (get Bnd-Car 2)) (nl-cur)
        
        # Str's val/b is NIL (or poss later point to itself)
        (when (and (: b) (not (= (: b) Any)))
          (setq Bnd-Cdr (lay-part (: b) Base)))
            
        (list (max (get Bnd-Car 1) (get Bnd-Cdr 1))
              (min (get Bnd-Car 2) (get Bnd-Cdr 2)))))))
    
  
(de lay-atom (Any Base Car-Skip Cdr-Skip)
  
  # Move Pair first so below to get correct pos
  (mov> Any *cur)  
  
  (if (or (isa '+Num Any)
          (isa '+Nil Any))
      
      (prog
      
        # Mov cur to bounds, aka last vert start pos
        # Adv to move to end pos of last vert
        (mov-cur-vert (last-vert> Any))
        (adv-cur)        
        *cur)

      (prog
  
        # Poss create lay-sym but not needed since same fn'y
        (if (= (get Any 'lay) 'x)
          (lay-pair-x Any Base Car-Skip Cdr-Skip)
          (lay-pair-y Any Base Car-Skip Cdr-Skip)))))
