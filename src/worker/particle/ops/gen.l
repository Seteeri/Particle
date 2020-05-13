(de gen-part (Any Lay Car-Skip Cdr-Skip)
  # TODO: Handle circular lists...use another dot at the end

  (if (pair Any)
  
      (if (lst? (cdr Any))
      
          # Handle specific forms here
          (gen-pair Any Lay Car-Skip Cdr-Skip)
                 
          (gen-pair Any Lay Car-Skip Cdr-Skip))
      
      (gen-atom Any Lay)))


(de gen-atom (Any Lay)
  (if (num? Any)
      (gen-num Any Lay)
      (if Any
          (gen-sym Any Lay)
          (gen-nil Any Lay))))


(de gen-num (Any Lay)
  (let ((Verts Wid) (gen-vert-atom Any (get-color-type Any))
        Part (new '(+Num)
                  Any
                  NIL NIL
                  Verts
                  Lay))
    (put Part 'dims (list Wid (meta '(+Vertex) 'adv-vert-2)))
    Part))


(de gen-nil (Any Lay)
  (let ((Verts Wid) (gen-vert-atom Any (get-color-type Any))
        Part (new '(+Nil)
                  Any
                  NIL NIL
                  Verts
                  Lay))
    (put Part 'dims (list Wid (meta '(+Vertex) 'adv-vert-2)))
    Part))

    
(de gen-sym (Any Lay)
  (let ((Verts Wid) (gen-vert-atom Any (get-color-type Any))
        Cls (cond ((box? Any) '(+Box))
                  ((str? Any) '(+Str))
                  ((ext? Any) '(+Ext))
                  ((sym? Any) '(+Int)))
        Part (new Cls
                  Any
                  NIL NIL
                  Verts
                  Lay))
    (put Part 'dims (list Wid (meta '(+Vertex) 'adv-vert-2)))
    Part))
    
        
(de gen-sym-pair (Any Lay Car-Skip Cdr-Skip Char-Pair)

  # Technically, name is a number cell

  # Use name for str since it exclude quotes
  # Name any will fail for box, use 'sym
  
  (let (Name (if (str? Any) (name Any) (sym Any))
        Val (val Any)
        Dr (if (= Val Any) NIL Val)
        (Verts-Car Wid-Car) (gen-vert-atom Name (get-color-type Any))
        Part-Car (unless Car-Skip
                  # Num type but content is a string
                  (new '(+Num)
                       Name
                       NIL NIL
                       Verts-Car
                       Lay))
        Part-Cdr (when (and (not Cdr-Skip)
                            (not (= Val Any)))
                  (gen-part Dr Lay))
        (Verts-Pair Wid-Pair) (gen-vert-pair (get-color-type Any) (= Val Any) Char-Pair)
        Part-Pair (new (if (str? Any) '(+Str) '(+Int)) # Color as a symbol
                       Any
                       Part-Car Part-Cdr
                       Verts-Pair
                       Lay))
                       
      # Set dims
      (put Part-Car 'dims (list Wid-Car (meta '(+Vertex) 'adv-vert-2)))
      (put Part-Pair 'dims (list Wid-Pair (meta '(+Vertex) 'adv-vert-2)))
                        
      # If CAR/CDR, is a list, set C to pair

      (unless Car-Skip 
        (put Part-Car 'c Part-Pair))

      (when (and (not Cdr-Skip)
                 (not (= Val Any)))
        (put Part-Cdr 'c Part-Pair))
              
      Part-Pair))


(de gen-pair (Any Lay Car-Skip Cdr-Skip)
  #{
    Root is baseline of cons
    so for a list Root would be coordinates of first cons
  }#

  # Check gen-vert-pair! Could return single list...

  (let (Ar (car Any)
        Dr (cdr Any)
        Part-Car (unless Car-Skip (gen-part Ar Lay))
        Part-Cdr (unless Cdr-Skip (gen-part Dr Lay))        
        (Verts-Pair Wid-Pair) (gen-vert-pair)
        Part-Pair (new '(+Pair)
                        Any
                        Part-Car Part-Cdr
                        Verts-Pair
                        Lay))
                        
      (put Part-Pair 'dims (list Wid-Pair (meta '(+Vertex) 'adv-vert-2)))
                        
      # If CAR/CDR, is a list, set C to pair
      (unless Car-Skip (put Part-Car 'c Part-Pair))
      (unless Cdr-Skip (put Part-Cdr 'c Part-Pair))
              
      Part-Pair))
