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

  # Pass flag to draw sym val
  
  # Handle internal symbol excl. T - draw the value (CDR)
  # Transient sym's value is itself...

  # Must do after so drawn after
  
  (let (B (when (and (not Cdr-Skip)
                     (sym-int? Any))
            (gen-sym-int Any Lay Car-Skip Cdr-Skip)))
            
    (new (if (num? Any)
             '(+Num)
             (if (str? Any)
                 '(+Str)
                 '(+Sym)))
         Any
         NIL
         B
         (draw-atom Any
                     (get-color-type Any)
                     Adv)
         Lay)))


(de gen-sym-int (Any Lay Car-Skip Cdr-Skip)
  # Opt: Ignore NIL?

  (gen-part (val Any) Lay Car-Skip Cdr-Skip))


(de gen-pair (Any Lay Car-Skip Cdr-Skip)
  #{
    Root is baseline of cons
    so for a list Root would be coordinates of first cons
  }#

  # Check draw-cell! Could return single list...

  (let (Ar (car Any)
        Dr (cdr Any)
        (Vert-Ar Dot Vert-Dr) (draw-cell Ar Dr)
        Part-Car (unless Car-Skip (gen-part Ar Lay))
        Part-Cdr (unless Cdr-Skip (gen-part Dr Lay))        
        Part-Pair (new '(+Pair)
                        Any
                        Part-Car
                        Part-Cdr
                        (append Vert-Ar Dot Vert-Dr)
                        Lay))
                        
      # If CAR/CDR, is a list, set C to pair
      (unless Car-Skip (put Part-Car 'c Part-Pair))
      (unless Cdr-Skip (put Part-Cdr 'c Part-Pair))
        
      #{
        Skip the list containing the list so:
          a
          . 1 2 3
          c
      }#
      (when (num? Ar)
        (lay-num-ds Part-Pair 'x))
      
      Part-Pair))


(de lay-num-ds (Part Lay)
  # If nested lists, must keep checking until non-pair/list
  #
  # Move to caller fn or gen-part...
  
  (with Part
    
    (let (P-Typ (type Part)
          A-Typ (type (: a))
          B-Typ (type (: b)))
  
      (when (and (isa '+Pair P-Typ)
                 (or (isa '+Pair A-Typ)
                     (isa '+Num A-Typ)))
        
        (=: lay Lay)
        (put (: a) 'lay Lay))
      
      (when (isa +Pair A-Typ) (lay-num-ds (: a) Lay))
      (when (isa +Pair B-Typ) (lay-num-ds (: b) Lay)))))
