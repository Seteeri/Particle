# Move to layout

# 2 parts
# - Calc min y for current line
# - Update subsequent items/lines

# Laying out an entire tree, nls can be tracked
# So for piecewise building, must store nl info somewhere in the particle

# Calc min y for all previous item's Cars
# X is based on list origin
#
# Again, need only do this from previous nl to current item

# How to track nls?
# * Walk backwards until NL encountered
# * Put nl item in list of lines as list property
# * Link lines to each other...? nl <-> nl
#   -> Faster than checking every item every time 
#      but not as much overhead as storing a list of lines

#{

[] [] [] []
a  b  c  d

[] [] [] []
e  f  g  h

[] [] [] []
i  j  k  l


a - e - i

}#

(de upd-cdr-n (Part)
  (let (Ori (get-origin> *main)
        I (get Part 'b))
    (while (and I (not (get I 'nl)))
      # Handle layout specific
      (let? Dims (calc-bounds> (get I 'c) NIL T)
        (set-cur-x (get Dims 1)) (adv-cur 2.0))
      (println 'upd-cdr-n "Update" I (get I 'data))
      (lay-part I Ori 'skip-cdr)
      (setq I (get I 'b)))))


(de upd-main-n ()
  (unless (= (get *main 'data) '*main)
    (let (Ori (get-origin> *main)
          I (get *main 'b))
      (while (and I (not (get I 'nl)))
        # Handle layout specific
        (let? Dims (calc-bounds> (get I 'c) NIL T)
          (set-cur-x (get Dims 1)) (adv-cur 2.0))
        (lay-part I Ori 'skip-cdr)
        (setq I (get I 'b))))))
        
        
######################

(de con-any (Data Lay Repl)

  (let (Ref (val> *0)
        Cdr (get Ref 'b)  
        Prv (get Ref 'c)
        Prv-B (get Prv 'b)
        Prv-C (get Prv 'c))
        
    (if Repl
    
        (let Part (gen-part (list (list Data)) Lay NIL T)
        
          #{
          
          [] - [] - NIL
          a    b
               *0
               
          Cdr = NIL
          Prv = []b
          Prv-C = []a
          
          }#
                
          # Note, ref is Car so get Prv
          (repl-list Prv-C
                     Prv
                     Prv-B
                     Part)
          
          (if (= Lay 'x)
          
              (prog
                # Draw Pair at Prv, no Car/Cdr
                # Then nl, draw Car
                (mov-cur-part Prv 'start)
                (lay-part Part (get-origin> *main) 'skip-car-cdr)
                
                # Move to y bounds
                (let Dims (calc-bounds> Prv-C NIL T)
                  (setq *cur Dims)
                  (adv-cur 2.0)
                  (nl-cur *sublist-space-y)
                  (lay-part (get Part 'a) (get-origin> *main)))
                
                # Move ptr to Cdr/NIL; must set ptr to Part to get Car
                (put *0 'b (get (get Part 'a) 'b))
                (mov-part-abv> *0 (get (get Part 'a) 'b) (cons T)))
                
              (prog
                (println 'con-any "IMPLEMENT Y LAYOUT")))
          
          (println 'con-any "Update CDR:" Part (get Part 'data))
          (upd-cdr-n Part)
          
          (setq *main Part))
        
        (let Part (gen-part (list Data) Lay NIL T)
          (con-back Prv Ref Part)
                    
          # Base is origin of Pair containing Ref/Part
          # Cur is where Part should be - use Ref origin
          (mov-cur-part Ref 'start)
          (lay-part Part (get-origin> *main) 'skip-cdr)
        
          # Need only update until EOL so traverse Cdr until NIL or nl
          (upd-cdr-n Part)
          
          # Update Main CDR similarly
          # This needs to update any sublists
          (when T
            (upd-main-n))
          
          (put *0 'b Ref)
          (mov-part-abv> *0 Ref (cons T))))
  
    Part))


(de con-back (Prv Ref Part)
  #{  
                *0
    [ ] - [ ] - NIL
    a     b
    
    TO
    
                      *0
    [ ] - [ ] - [ ] - NIL
    a     b     c
    
  }#

  (put Prv 'b Part)    
  (put Ref 'c Part)    
  
  (with Part
    (=: b Ref)
    (=: c Prv))
  
  # Set nl if Ref has it; toggle Ref
  (when (get Ref 'nl)
    (put Part 'nl T)
    (put Ref 'nl))
      
  # Connect new part data to list part data
  # Must conn Cdr also (get (get Ref 'b) 'data)
  
  # Remember, *main's value is always the particle itself
  # The value of the symbol *main is actually 'b    
  
  (unless (isa '+Sym Prv)
    (with Prv
      (con (car (:: data)) (get Part 'data)))))
      

(de repl-list (Prv Ref Cdr Part)
  
  #{
    Replace Ref with Part
  
    [X] [X] [X]
    a   b   c
    
    * Del Ref
    * Prv 'b -> Part
    * Part 'b -> Cdr
    * Cdr 'c -> Part
    * Part 'c -> Prv 
  }#
    
  # Connect Prv-Part
  (with Prv
    (=: b Part)
    (unless (isa '+Sym Prv)
      (con (car (:: data)) (get Part 'data))))
  
  # Connect Part-Cdr
  (with Cdr
    (=: c Part))
  
  (with Part
    (=: b Cdr)
    (=: c Prv)
    (con (car (:: data)) (get Cdr 'data)))

  # Set nl if Ref has it; toggle Ref
  (when (get Ref 'nl)
    (put Part 'nl T)
    (put Ref 'nl))
    
  # Copy lay
  (force-lay Part (get Ref 'lay))

  # Do not delete CDR
  (del> Ref T NIL))


################################################################################


(de swap-lay (Tgt)
  (force-lay Tgt 
             (if (= (get Tgt 'lay) 'x) 'y 'x))
        
  # Note this causes bounds change also
        
  (mov-cur-part Tgt 'start)
  # Opt: force layout
  (lay-part Tgt (get-origin> *main)))


(de point-b-prv ()
  (println 'point-b-prv)

  (let (Ref (val> *0)
        Prv (get (val> *0) 'c))
        
    (when Prv
    
      # Set list
      (when (= (type (get Prv 'a)) '(+Pair))
        (println 'point-a-out "Mov to superlist")
        (setq *main Prv))
    
      (println 'point-a-out Prv (get Prv 'data))
      (put *0 'b Prv)
      (mov-part-abv> *0 Prv (cons T)))))


(de point-b-nxt ()
  (println 'point-b-nxt)

  (let? Cdr (get (val> *0) 'b)
    (println 'point-b-nxt Cdr (get Cdr 'data))
    (put *0 'b Cdr)
    (mov-part-abv> *0 Cdr (cons T))))


(de point-a-out ()
  (println 'point-a-out)
    
  (point-b-prv))


(de point-a-in ()

  (println 'point-a-in)

  # TODO: Support mov to NIL

  (let (Ref (val> *0)
        Car (get Ref 'a))
    
    (when (and Car
               (not (isa '+Sym Ref)))
    
      (put *0 'b Car)
      
      (let Dims (calc-bounds> Car)

        # Set list
        (if (= (type Car) '(+Pair))

          (prog
            (println 'point-a-in "Mov to sublist")
            (setq *main Car)
            (mov-part-abv> *0 Car (cons T)))

          (if (= (get Ref 'lay) 'x)
          
              (prog
                # Move to lower bnd of Car
                (set-cur-x (get (get-origin> Car) 1))
                (set-cur-y (get Dims 2)) (nl-cur)
                (lay-part *0 *cur 'skip-cdr))
                
              (prog
                # Move above Car
                (mov-part-abv> *0 Car (cons T)))))))))


#######################

#{
  Find list

  (let Part (get (val> *0) 'c)
    (loop
      (T (or (isa '+Pair (get Part 'a))
             (= Part *main)
             (not (get Part 'c))))
      (setq Part (get Part 'c)))
}#


(de mov-nl (Ref)
  # Is this appropriate for Y layout? ...since it already moves to newline
  # Should make a new column
  
  # Calc min Y until NIL or nl
  (let (I Ref
        Y (get (get-origin> Ref) 2))
        
    (while (and I (not (get I 'nl)))
      (let Dims (calc-bounds> I NIL T) # Only CAR, Skip CDR
        (setq Y (min Y (get Dims 2))))
      (setq I (get I 'c)))
    
    (set-cur-y Y)
    
    # Newline + Ptr
    # This assumes pointer is single line
    (nl-cur 3.0)
    
    # Calc X
    (let Ori (get-origin> (get *main 'b))
      (set-cur-x (get Ori 1))
    
      (put Ref 'nl T)
    
      # Remember base is list, cur is for Ref
      # This updates Cdr
      (lay-part Ref Ori))
    
    # And update list Cdr also
    T))
