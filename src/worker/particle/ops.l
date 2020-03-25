(de base-cur (Ref)  
  # x: CDR is to the right; use X bounds
  # y: CDR is below; use Y bounds
  (let Bounds (calc-bounds> Ref NIL T) # Skip CDR
    (if (= (get Ref 'lay) 'x)
        (prog
          # Move cur to x base of Ref and y origin of Ref
          (set-cur-x (get Bounds 1))
          (set-cur-y (get (get-origin> Ref) 2))
          (adv-cur 2.0))
        (prog
          # Move cur to x origin of Ref and y base of Ref
          (set-cur-x (get (get-origin> Ref) 1))
          (set-cur-y (get Bounds 2))
          (nl-cur 2.0)))))

######################

(de repl-cdr (Ref Any Lay)
  # Replace CDR which is typically NIL
  
  (let Part (con-list Ref Any Lay)

    (mov-cur-part Ref 'start)
    (lay-part Part *cur)
    
    Part))


(de con-list (Ref Any Lay)
  #{  
                p0
    [ ] - [ ] - NIL
    "a"   "b"
  }#

  (let (Prv (get Ref 'c)
        Lst-Any (list Any)
        Part (gen-part Lst-Any Lay NIL T))

    (put Prv 'b Part)    
    (put Ref 'c Part)    
    
    (with Part
      (=: b Ref)
      (=: c Prv))
        
    # Connect new part data to list part data
    # Must conn Cdr also (get (get Ref 'b) 'data)
    
    # Remember, *main's value is always the particle itself
    # The value of the symbol *main is actually 'b    
    
    (unless (isa '+Sym Prv)
      (with Prv
        (con (car (:: data)) Lst-Any)))
        
    Part))

######################

(de repl-ref (Any Lay)
  #{  
    Pointer is pointing to Pair to be replaced

    [X] [X] [X]
    a   b   c
    
    p0
    [Y] NIL
    NIL
  }#

  (let (Ref (val> p0)
        Cdr (get Ref 'b)  
        Prv (get Ref 'c)  
        Lst-Any (list (list Any))
        Part (gen-part Lst-Any Lay NIL T))
        
    # Don't make Cdr/NIL since it will be connected to existing
        
    (repl-list Prv Ref Cdr Part)

    # Cursor will have already been put in position,
    # so this will handle when list is Y, 
    # but what about if user enters list with X layout?
    # -> Should be in pos already?
    
    (mov-cur-part Ref 'start)

    # Particle should already have proper layout...
    (put (get Part 'a) 'lay 'x)
    (lay-part Part *cur)
    
    #{
    (if (= Lay 'x)
    
        (prog

          # Move new Part rel to extents of Prv
          (base-cur Prv)
          (lay-part Part *cur)      
          
          # Move Car one space further down
          (mov-cur-part Part 'start)
          (nl-cur 3.0)
          (with Part
            (put (: a) 'lay Lay)
            (lay-part (: a) *cur)))
            
        (prog
                
          (lay-part Part *cur)))
    }#
    
    Part))


(de repl-list (Prv Ref Cdr Part)
  

  #{
    [X] [X] [X]
    a   b   c
    
    p0
    [Y] NIL
    NIL
    
    * Del Ref
    * Prv 'b -> Part
    * Part 'b -> Cdr
    * Cdr 'c -> Part
    * Part 'c -> Prv 
  }#
    
  # Connect Prv-Part
  (with Prv
    (=: b Part)
    (con (car (:: data)) (get Part 'data)))
  
  # Connect Part-Cdr
  (with Cdr
    (=: c Part))
  
  (with Part
    (=: b Cdr)
    (=: c Prv)
    (con (car (:: data)) (get Cdr 'data)))
    
  # Do not delete CDR
  (del> Ref T NIL))
