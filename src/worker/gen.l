(def '*gen-lay-y-pair)

(de gen-part (Any Lay Skip-Car Skip-Cdr)
  # TODO: Handle circular lists...use another dot at the end

  (if (pair Any)
  
      (if (lst? (cdr Any))
      
          # Handle specific forms here
          (gen-pair Any Lay Skip-Car Skip-Cdr)
                 
          (gen-pair Any Lay Skip-Car Skip-Cdr))
      
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
    
        
(de gen-sym-pair (Any Lay Skip-Car Skip-Cdr Char-Pair Col)

  # Technically, name is a number cell

  # Use name for str since it exclude quotes
  # Name any will fail for box, use 'sym
  
  (unless Col (setq Col (get-color-type Any)))
  
  (let (Name (if (str? Any) (name Any) (sym Any))
        Val (val Any)
        Dr (if (= Val Any) NIL Val)
        (Verts-Car Wid-Car) (gen-vert-atom Name Col)
        Car (unless Skip-Car
                  # Num type but content is a string
                  (new '(+Num)
                       Name
                       NIL NIL
                       Verts-Car
                       Lay))
        Cdr (when (and (not Skip-Cdr)
                            (not (= Val Any)))
                  (gen-part Dr Lay))
        (Verts-Pair Wid-Pair) (gen-vert-pair Char-Pair Col (= Val Any))
        Pair (new (if (str? Any) '(+Str) '(+Int)) # Color as a symbol
                       Any
                       Car Cdr
                       Verts-Pair
                       Lay))
                       
      # Set dims
      (put Car 'dims (list Wid-Car (meta '(+Vertex) 'adv-vert-2)))
      (put Pair 'dims (list Wid-Pair (meta '(+Vertex) 'adv-vert-2)))
                        
      # If CAR/CDR, is a list, set C to pair

      (unless Skip-Car 
        (put Car 'c Pair))

      (when (and (not Skip-Cdr)
                 (not (= Val Any)))
        (put Cdr 'c Pair))
              
      Pair))


(de gen-pair (Any Lay Skip-Car Skip-Cdr)
  #{
    Root is baseline of cons
    so for a list Root would be coordinates of first cons
  }#

  (let (Ar (car Any)
        Dr (cdr Any)
        Car (unless Skip-Car (gen-part Ar Lay))
        Cdr (unless Skip-Cdr (gen-part Dr Lay))        
        Col (if (= Lay 'x) (car *pair-col-def) (cadr *pair-col-def))
        (Verts-Pair Wid-Pair) (gen-vert-pair NIL Col)
        Pair (new '(+Pair)
                        Any
                        Car Cdr
                        Verts-Pair
                        Lay))
                        
      (put Pair 'dims (list Wid-Pair (meta '(+Vertex) 'adv-vert-2)))
                      
      # If CAR/CDR, is a list, set C to pair
      (unless Skip-Car (put Car 'c Pair))
      (unless Skip-Cdr (put Cdr 'c Pair))
              
      Pair))
