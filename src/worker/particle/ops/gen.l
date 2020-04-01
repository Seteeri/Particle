(de gen-part (Any Lay Car-Skip Cdr-Skip)
  # TODO: Handle circular lists...use another dot at the end

  (if (pair Any)
  
      (if (lst? (cdr Any))
      
          # Handle specific forms here
          (cond (T
                 (gen-pair Any Lay Car-Skip Cdr-Skip)))
                 
          (gen-pair Any Lay Car-Skip Cdr-Skip))
      
      (gen-atom Any Lay T Car-Skip Cdr-Skip)))


(de gen-atom (Any Lay Adv Car-Skip Cdr-Skip)
  (if (num? Any)
      (gen-num Any Lay Adv Car-Skip Cdr-Skip)
      (if (not Any)
          (gen-nil Any Lay Adv Car-Skip Cdr-Skip)
          (gen-sym Any Lay Adv Car-Skip Cdr-Skip))))


(de gen-num (Any Lay Adv Car-Skip Cdr-Skip)
  (let ((Verts Wid) (draw-atom Any (get-color-type Any) Adv)
        Part (new '(+Num)
                  Any
                  NIL NIL
                  Verts
                  Lay))
    (put Part 'dims (list Wid *adv-vert-2))
    Part))


(de gen-nil (Any Lay Adv Car-Skip Cdr-Skip)
  # CAR/CDR point to itself
  (let ((Verts Wid) (draw-atom Any (get-color-type Any) Adv)
        Part (new '(+Nil)
                  Any
                  Part Part
                  Verts
                  Lay))
    (put Part 'dims (list Wid *adv-vert-2))
    Part))
        

(de gen-sym (Any Lay Adv Car-Skip Cdr-Skip)

  # Make Pair and Car both +Sym or just Car?
  # If property list is shown also...

  # Technically, name is a number

  # Str's default val is itself so watch for inf recursion
  # Should set Part-Car to Part-Pair ... but leave NIL for now
  # Draw dot again

  # Name any will fail for box; must use 'sym

  (let (Ar (name Any) 
        Val (val Any)
        Dr (if (== Val Any) NIL Val)
        (Verts-Car Wid-Car) (draw-atom Ar (get-color-type 0) Adv)
        Part-Car (unless Car-Skip
                  # Num type but content is a string
                  (new '(+Num)
                       Ar
                       NIL NIL
                       Verts-Car
                       Lay))
        Part-Cdr (when (and (not Cdr-Skip)
                            (not (== Val Any)))
                  (gen-part Dr Lay))
        (Verts-Pair Wid-Pair) (draw-cell (get-color-type Any) (== Val Any))
        Part-Pair (new (if (str? Any) '(+Str) '(+Int)) # Color as a symbol
                       Any
                       Part-Car Part-Cdr
                       Verts-Pair
                       Lay))
                        
      # Set dims
      (put Part-Car 'dims (list Wid-Car *adv-vert-2))
      (put Part-Pair 'dims (list Wid-Pair *adv-vert-2))
                        
      # If CAR/CDR, is a list, set C to pair

      (unless Car-Skip 
        (put Part-Car 'c Part-Pair))
      (when (and (not Car-Skip)
                 (not (== Val Any)))
        (put Part-Cdr 'c Part-Pair))
              
      Part-Pair))


(de gen-pair (Any Lay Car-Skip Cdr-Skip)
  #{
    Root is baseline of cons
    so for a list Root would be coordinates of first cons
  }#

  # Check draw-cell! Could return single list...

  (let (Ar (car Any)
        Dr (cdr Any)
        Part-Car (unless Car-Skip (gen-part Ar Lay))
        Part-Cdr (unless Cdr-Skip (gen-part Dr Lay))        
        (Verts-Pair Wid-Pair) (draw-cell)
        Part-Pair (new '(+Pair)
                        Any
                        Part-Car Part-Cdr
                        Verts-Pair
                        Lay))
                        
      (put Part-Pair 'dims (list Wid-Pair *adv-vert-2))
                        
      # If CAR/CDR, is a list, set C to pair
      (unless Car-Skip (put Part-Car 'c Part-Pair))
      (unless Cdr-Skip (put Part-Cdr 'c Part-Pair))
              
      Part-Pair))
