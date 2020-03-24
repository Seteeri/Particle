(de is-start-of-list (Part) (isa '+Pair (get Part 'a)))

(de lay-part (Any Base)
  
  (if (= (type Any) '(+Pair))
  
      (lay-pair Any Base)
      
      (lay-atom Any Base)))


(de lay-pair (Any Base)
  
  # Ignore p0 also
  (put Any 'lay (if (is-start-of-list Any) 'y 'x))

  #(println 'lay-pair (get Any 'lay) (get Any 'data))

  # Move Pair first so below can get correct pos
  (mov> Any *cur)
  
  (let Base-2 (if (is-start-of-list Any)
                  (get-origin> Any)
                  Base)

    (if (= (get Any 'lay) 'x)
      
      (lay-pair-x Any Base-2)
      
      (lay-pair-y Any Base-2))))
    

(de lay-pair-x (Any Base)
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
      (let Bnd-Car (lay-part (: a)
                             Base)
        
        # or check 'nl...
        # wouldn't make sense for y layout except to add more spacing
        # if interpreted directly
        (if (is-start-of-list (: b)) # or nl
            (prog
              #(println 'lay-pair'x)
              (set-cur-x (get Base 1))
              (when (get Any 'c)
                # Really should use last vert of og Base
                (adv-cur 2.0))
              (nl-cur))
              
            (prog
              # At this point, cur is at extents of Car
              # X is valid except for one more adv
              # Y must be moved back to base
              (adv-cur)
              (set-cur-y Y)))
        
        (let Bnd-Cdr (lay-part (: b)
                               Base)

          (list (max (get Bnd-Car 1) (get Bnd-Cdr 1))
                (min (get Bnd-Car 2) (get Bnd-Cdr 2))))))))

    
(de lay-pair-y (Any Base)
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
  
  (let ((X Y) *cur)
      
    (with Any
    
      # Mov cur to bounds, aka last vert start pos
      (mov-cur-vert (last-vert> Any))
      # Adv two spaces, after last vert, then one more space
      (adv-cur 2.0)
    
      # Lay CAR first since its Y bounds has precedence
      (let Bnd-Car (lay-part (: a)
                             Base)
        
        # At this point, cur is at extents of Car
        # X must be moved back to base      
        # Y is valid except for one more newline
        (set-cur-x X)
        (set-cur-y (get Bnd-Car 2)) (nl-cur)
        
        (let Bnd-Cdr (lay-part (: b)
                               Base)

          (list (max (get Bnd-Car 1) (get Bnd-Cdr 1))
                (min (get Bnd-Car 2) (get Bnd-Cdr 2))))))))
    
  
(de lay-atom (Any Base)
  
  # Move Pair first so below can get correct pos
  (mov> Any *cur)  
  
  (if (or (isa '+Num Any)
          (isa '+Nil Any))
      
      (prog
      
        # Mov cur to bounds, aka last vert start pos
        (mov-cur-vert (last-vert> Any))
        # Adv to move to end pos of last vert
        (adv-cur)        
        *cur)

      (prog
        
        # Poss create lay-sym but not needed since same fn'y
        (if (= (get Any 'lay) 'x)
          (lay-pair-x Any Base)
          (lay-pair-y Any Base)))))
