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
  (new '(+Num)
       Any
       NIL NIL
       (draw-atom Any (get-color-type Any) Adv)
       Lay))


(de gen-nil (Any Lay Adv Car-Skip Cdr-Skip)
  (new '(+Nil)
       Any
       NIL NIL
       (draw-atom Any (get-color-type Any) Adv)
       Lay))
        

(de gen-sym (Any Lay Adv Car-Skip Cdr-Skip)

  # Make Pair and Car both +Sym or just Car?
  # If property list is shown also...

  # Technically, name is a number

  # Str's default val is itself so watch for inf recursion
  # Should set Part-Car to Part-Pair ... but leave NIL for now
  # Draw dot again

  (let (Ar (sym Any)
        Val (val Any)
        Dr (if (== Val Any) NIL Val)
        Part-Car (unless Car-Skip
                  # Num type but content is a string
                  (new '(+Num)
                       Ar
                       NIL NIL
                       (draw-atom Ar (get-color-type 0) Adv)
                       Lay))
        Part-Cdr (when (and (not Cdr-Skip)
                            (not (== Val Any)))
                  (gen-part Dr Lay))
        Part-Pair (new (if (str? Any) '(+Str) '(+Sym)) # Color as a symbol
                       Any
                       Part-Car Part-Cdr
                       (draw-cell (get-color-type Any) (== Val Any))
                       Lay))
                        
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
        #(Vert-Ar Dot Vert-Dr) (draw-cell Ar Dr)
        Part-Car (unless Car-Skip (gen-part Ar Lay))
        Part-Cdr (unless Cdr-Skip (gen-part Dr Lay))        
        Part-Pair (new '(+Pair)
                        Any
                        Part-Car Part-Cdr
                        (draw-cell) # (append Vert-Ar Dot Vert-Dr)
                        Lay))
                        
      # If CAR/CDR, is a list, set C to pair
      (unless Car-Skip (put Part-Car 'c Part-Pair))
      (unless Cdr-Skip (put Part-Cdr 'c Part-Pair))
              
      Part-Pair))


################################################################################

# Recusrively set layout

#(when (num? Ar)
#  (lay-num-ds Part-Pair 'x))
#(when (str? Ar)
#  (lay-str-ds Part-Pair 'x))

(de lay-num-ds (Part Lay)
  # If nested lists, must keep checking until non-pair/list
  #
  # Move to caller fn or gen-part...
  
  (with Part
  
    (when (and (isa '+Pair Part)
               (or (isa '+Pair (: a))
                   (isa '+Num (: a))))
      
      (=: lay Lay)
      (put (: a) 'lay Lay))
    
    (when (isa +Pair (: a)) (lay-num-ds (: a) Lay))
    (when (isa +Pair (: b)) (lay-num-ds (: b) Lay))))


(de lay-str-ds (Part Lay)
  # If nested lists, must keep checking until non-pair/list
  #
  # Move to caller fn or gen-part...
  
  (with Part
  
    (when (and (isa '+Pair Part)
               (or (isa '+Pair (: a))
                   (isa '+Str (: a))))
      
      (=: lay Lay)
      (put (: a) 'lay Lay))
    
    (when (isa +Pair (: a)) (lay-str-ds (: a) Lay))
    (when (isa +Pair (: b)) (lay-str-ds (: b) Lay))))
