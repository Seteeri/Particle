(de is-start-of-list (Part) (= (type (get Part 'a)) '(+Pair)))

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

  # Must handle nested lists so logic must be here
  #{
  (setq Base (if (get Any 'nl)
                 (prog
                   (println 'lay-pair "Detected NL")
                   (set-cur-x (get Base 1))
                   (nl-cur 3.0)
                   (get-origin> Any))
                 Base))
  }#

  # Move Pair first to get correct pos since bnds rel to origin
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
      
      # Move below or use its Y bnds - origin is same as bnds for single line
      # This assumes Pair is always single line with dot or 2x dot
      (nl-cur)
            
      # Lay CAR first since its X bounds has precedence
      
      (let ((X-Bnd-A Y-Bnd-A) (get (: a) 'bnds)
            (X-Bnd-B Y-Bnd-B) (get (: b) 'bnds))
      
        (when (or (= Skip 'skip-cdr)
                  (not Skip))
          (lay-part (: a) Base))
      
        # To draw Cdr: (use base-cur? that recalcs...)
        # X should match Ori 1 + Bnd-Car 1, plus an adv
        # Y must be moved back to origin of Pair
        (set-cur-x (+ X-Ori X-Bnd-A)) (adv-cur)
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
    
  
(de lay-atom (Any Base Skip)
  
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
          (lay-pair-x Any Base Skip)
          (lay-pair-y Any Base Skip)))))
