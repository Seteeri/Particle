(de gen-part (Any Car Cdr)
  # TODO: Handle circular lists...use another dot at the end

  (if (pair Any)
  
      (if (lst? (cdr Any))
      
          # Handle specific forms here
          (cond (T
                 (gen-pair Any Car Cdr)))
                 
          (gen-pair Any Car Cdr))
      
      (gen-atom Any T Car Cdr)))


(de gen-atom (Any Adv Car Cdr)

  # Pass flag to draw sym val
  
  # Handle internal symbol excl. T - draw the value (CDR)
  # Transient sym's value is itself...

  # Must do after so drawn after
  
  (let (B (when (and (not Cdr)
                     (sym-int? Any))
            (gen-sym-int Any Car Cdr)))
            
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
                     Adv))))


(de gen-sym-int (Any Car Cdr)
  # Opt: Ignore NIL?

  (gen-part (val Any) Car Cdr))


(de gen-pair (Any Ignore-Car Ignore-Cdr)
  #{
    Root is baseline of cons
    so for a list Root would be coordinates of first cons
  }#

  # TODO: REFACTOR CAR/CDR - FLIP FLAGS?

  (let (Ar (car Any)
        Dr (cdr Any)
        (Vert-Ar Dot Vert-Dr) (draw-cell Ar Dr)
        Part (new '(+Pair)
                   Any
                  (unless Ignore-Car (gen-part Ar))
                  (unless Ignore-Cdr (gen-part Dr))
                  (append Vert-Ar Dot Vert-Dr)))
      #{
        Skip the list containing the list so:
          a
          . 1 2 3
          c
      }#
      (when (num? Ar)
        (lay-num-ds Part 'x))

      #(when (str? Ar)
      #  (lay-str-ds Part 'x))
      
      Part))


(de lay-str-ds (Part Lay)
  # If nested lists, must keep checking until non-pair/list
  
  (with Part
    
    (let (P-Typ (type Part)
          A-Typ (type (: a))
          B-Typ (type (: b)))
  
      (when (and (= P-Typ '(+Pair))
                 (or (= A-Typ '(+Pair))
                     (= A-Typ '(+Str))))
        
        (=: lay Lay)
        (put (: a) 'lay Lay))
      
      (when (= A-Typ '(+Pair)) (lay-num-ds (: a) Lay))
      (when (= B-Typ '(+Pair)) (lay-num-ds (: b) Lay)))))


(de lay-num-ds (Part Lay)
  # If nested lists, must keep checking until non-pair/list
  #
  # Move to caller fn or gen-part...
  
  (with Part
    
    (let (P-Typ (type Part)
          A-Typ (type (: a))
          B-Typ (type (: b)))
  
      (when (and (= P-Typ '(+Pair))
                 (or (= A-Typ '(+Pair))
                     (= A-Typ '(+Num))))
        
        (=: lay Lay)
        (put (: a) 'lay Lay))
      
      (when (= A-Typ '(+Pair)) (lay-num-ds (: a) Lay))
      (when (= B-Typ '(+Pair)) (lay-num-ds (: b) Lay)))))
