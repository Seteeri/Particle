(de upd-cdr-n (Part)
  (let (Ori (get-origin> *main)
        I (get Part 'b))
    (while (and I (not (get I 'nl)))
      # Cursor is set by lay-part regardless of cdr; unless that fn'y dep on flag
      (lay-part I Ori 'skip-cdr)
      (setq I (get I 'b)))))


(de upd-main-n ()
  (unless (= (get *main 'data) '*main)
    (let (Ori (get-origin> *main)
          I (get *main 'b))
      (while (and I (not (get I 'nl)))
        # Cursor is set by lay-part regardless of cdr; unless that fn'y dep on flag
        (lay-part I Ori 'skip-cdr)
        (setq I (get I 'b))))))
        
        
######################

#{
  # repl-ref assumes list has y layout
  (let (Part (repl-ref Data Lay)
        Cdr (get (get Part 'a) 'b))
    
    # Point to Cdr of first item
    (put *0 'b Cdr)
    (mov-part-abv> *0 Cdr (cons T))
      
    # ONLY WHEN REPLACING NIL OR PAIR WITH CAR PAIR
    # Set new list
    (setq *main Part))
}#

(de con-any (Data Lay Repl)

  (let (Ref (val> *0)
        Cdr (get Ref 'b)  
        Prv (get Ref 'c)  
        Part (gen-part (list Data) Lay NIL T))
        
    (if Repl
    
        (prog
          # Note, ref is Car so get Prv
          (repl-list (get Prv 'c)
                     Prv
                     (get Prv 'b)
                     Part)
          
          (println Ref (get Ref 'data))
          
          (mov-cur-part Prv 'start)
          (lay-part Part (get-origin> *main) 'skip-cdr)
          
          (put *0 'b (get Part 'a)))
        
        (prog
          (con-back Prv Ref Part)
                    
          # Base is origin of Pair containing Ref/Part
          # Cur is where Part should be - use Ref origin
          (mov-cur-part Ref 'start)
          (lay-part Part (get-origin> *main) 'skip-cdr)
        
          # Need only update until EOL so traverse Cdr until NIL or nl
          (upd-cdr-n Part)
          
          # Update Main CDR similarly
          # TEST THIS
          # Set cur before calling
          (when NIL
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
      

######################

(de repl-ref (Any Lay)
  #{  
    Pointer is pointing to Pair to be replaced

    [X] [X] [X]
    a   b   c
    
    *0
    [Y] NIL
    NIL
  }#

  (let (Ref (val> *0)
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
    
    Part))


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

  # Do not delete CDR
  (del> Ref T NIL))


################################################################################

(de swap-lay (Tgt)
  (force-lay Tgt 
             (if (= (get Tgt 'lay) 'x) 'y 'x))
        
  (mov-cur-part Tgt 'start)
  # Opt: force layout
  (lay-part Tgt (get-origin> *main)))

(de mov-ptr-b-prv ()
  (println 'mov-ptr-b-prv)
  (let? Prv (get (val> *0) 'c)
    (println "Moved to " Prv (get Prv 'data))
    (put *0 'b Prv)
    (mov-part-abv> *0 Prv (cons T))))

(de mov-ptr-b-nxt ()
  (println 'mov-ptr-b-nxt)
  (let? Cdr (get (val> *0) 'b)
    (println "Moved to " Cdr (get Cdr 'data))
    (put *0 'b Cdr)
    (mov-part-abv> *0 Cdr (cons T))))

(de mov-ptr-a-out ()
  # If ref Car, then move to Pair instead of list

  (println 'mov-ptr-a-out)
  
  # Same as prv
  (let? Prv (get (val> *0) 'c)
    (println "Moved to " Prv (get Prv 'data))
    (put *0 'b Prv)
    (mov-part-abv> *0 Prv (cons T))))


(de mov-ptr-a-in ()

  (println 'mov-ptr-a-in)

  (let (Ref (val> *0)
        Car (get Ref 'a))
  
    (put *0 'b Car)
    
    # Pos depends on layout...

    (if (= (get Ref 'lay) 'x)
        (prog
          # Move to lower bnd of Car
          (let Bnds (calc-bounds> Car)
            (set-cur-x (get (get-origin> Car) 1))
            (set-cur-y (get Bnds 2)) (nl-cur)
            (lay-part *0 *cur 'skip-cdr)))
        (prog))))


#######################


(de mov-ptr-a-out-2 ()
  # If ref Car, then move to Pair instead of list

  (println 'mov-ptr-a-out)

  (let Part (get (val> *0) 'c)
    (loop
      (T (or (isa '+Pair (get Part 'a))
             (= Part *main)
             (not (get Part 'c))))
      (setq Part (get Part 'c)))
      
    (println 'leave-list Part (get Part 'data) (get (get Part 'c) 'data))
    
    (when Part
      (put *0 'b Part)
      (mov-part-abv> *0 Part (cons T)))))


(de mov-ptr-a-in-2 ()
  #{
  
    * X
      * Move to NIL/list (see below)
    * Y
      * Move to CAR/CDR
    
    Car:
  
    If non-NIL:
    * Move ptr to it
  
    If NIL:
    * Move under
    * Set Y layout
    * Move ptr to Pair instead of NIL (like normal mode)
    * Set flag for cmd-make-char...
    * User must move to CAR
          
    User has entered NIL already:
    
                    *0
    [X] [X] [X] [X] NIL
    a   b   c   NIL
    
    TO:

    [X] [X] [X]
    a   b   c
    
    *0
    [Y] [X] [X] [X] NIL
    NIL d   e   f

    ...
    
    [Y] [X] [X] [X] NIL
    NIL h   i   j
    
    Effectively, this cmd turns NIL into a list instead of an atom
    If user exits list, keep structure the same
    Allows user to create "bullets"
    User can explicitly move it back and toggle layout
        
  }#  
  
  (let (Ref (val> *0)
        Car (get Ref 'a)
        Tgt (get Ref 'c))
    
    (cond ((= (type Car) '(+Pair))
            (put *0 'b Car)
            (mov-part-abv> *0 Car (cons T)))
            
          ((isa '+Nil Ref)

            # Mov under
            (cmd-mov-nl)

            # Set Y layout
            (with Tgt (=: lay (if (= (: lay) 'x) 'y 'x)))
            (mov-cur-part Tgt 'start)
            (lay-part Tgt (get-origin> *main))

            # Update ptr
            (put *0 'b Tgt)
            (mov-part-abv> *0 Tgt (cons T))

            # If not set, Cdr of NIL will be replaced instead of the CAR/entire list
            (setq *mode-repl T)))))
            
#{
              
  # If symbol use 'b
  # If pair use 'a
  (let (Base (get *main (if (= (type *main) '(+Pair)) 'a 'b))
        Ori (get-origin> Base)
        Bnds (if (= (get Base 'lay) 'x)
                 (calc-bounds> Base NIL T)
                 (calc-bounds> Base T NIL)))
    (setq *cur (list (get Ori 1)
                     (get Bnds 2)
                     (get *cur 3)))
    (nl-cur 2.0))

  (put Tgt 'lay 'y)
  (put Tgt 'nl 1.0) # set nl
  (lay-part Tgt *cur)

  (put *0 'b Tgt)
  (mov-part-abv> *0 Tgt (cons T))
            
}#            
