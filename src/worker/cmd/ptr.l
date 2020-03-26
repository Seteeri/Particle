(de cmd-swap-layout (Keysym)
  (println "Swap layout")
  
  # If NIL, get prev, else do ref  
  
  (let (Ref (val> p0)
        Prv (get Ref 'c)
        Tgt (if (= (get Ref 'data) NIL) Prv Ref))
        
    (when Tgt
    
      (println "Swap!" Tgt (get Tgt 'data) (get Tgt 'lay) '-> (if (= (get Tgt 'lay) 'x) 'y 'x))
      
      (force-lay Tgt 
                 (if (= (get Tgt 'lay) 'x) 'y 'x))
            
      (mov-cur-part Tgt 'start)      
      # Opt: force layout
      (lay-part Tgt (get-origin> *main) 'skip-cdr))))


(de cmd-mov-nl (Keysym)
  (println "Move newline")
  
  #{
  
  [] [] [] [] [] []
  
  TO
  
  [] [] []
  
  [] [] []
  
  }#
  
  # Is this appropriate for Y layout? ...since it already moves to newline
  
  # Move pointer/item under first item of list
  # Character after this will replace item

  # Calc min y for all previous item's Cars
  # X is based on list origin
  #
  # Again, need only do this from previous nl to current item
  # How to track nls?
  # * Walk backwards until NL encountered
  # * Put nl item in list of lines as list property
  # * Link lines to each other...?

  # Calc min Y until NIL or nl
  (let (Ref (val> p0)
        I Ref
        Y (get (get-origin> Ref) 2))
        
    (while (and I (not (get I 'nl)))
      (let Bnds (calc-bounds> I NIL T) # Only CAR, Skip CDR
        (setq Y (min Y (get Bnds 2))))
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
    
    #(put p0 'b Ref)
    (mov-part-abv> p0 Ref (cons T))))
        
  
(de cmd-mov-bl (Keysym)

  # This moves in front of prev

  (let Ref (val> p0)
  
    (base-cur (get Ref 'c))
    (put Ref 'nl NIL)
    (lay-part Ref *cur)
    
    (put p0 'b Car Ref)
    (mov-part-abv> p0 Ref (cons T))))
      

###############################
# Semantic Traversal (Pair/Ptr)

(de cmd-point-ptr-pre (Keysym)
  (println "Move to prev item")

  (let Ref (val> p0)
  
    (if (= (get Ref 'lay) 'x)
  
        (let? Prv (get Ref 'c)
          (println "Moved to " Prv (get Prv 'data))
          (put p0 'b Prv)
          (mov-part-abv> p0 Prv (cons T)))
          
        (prog))))
  

(de cmd-point-ptr-nxt (Keysym)
  (println "Move to next item")

  (let Ref (val> p0)
  
    (if (= (get Ref 'lay) 'x)
        
        (let? Cdr (get (val> p0) 'b)
          (println "Moved to " Cdr (get Cdr 'data))
          (put p0 'b Cdr)
          (mov-part-abv> p0 Cdr (cons T)))
          
        (prog))))


(de cmd-point-ptr-out (Keysym)
  # Move to super list
    
  (println "Move to outer list")
  
  # Move to *main
  
  # Below searches
  (let Part (get (val> p0) 'c)
    (loop
      (T (or (isa '+Pair (get Part 'a))
             (= Part *main)
             (not (get Part 'c))))
      (setq Part (get Part 'c)))
      
    (println 'leave-list Part (get Part 'data) (get (get Part 'c) 'data))
    
    (when Part
      (put p0 'b Part)
      (mov-part-abv> p0 Part (cons T)))))


(de cmd-point-ptr-in (Keysym)
  
  (println "Move to inner list")
  
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
    
                    p0
    [X] [X] [X] [X] NIL
    a   b   c   NIL
    
    TO:

    [X] [X] [X]
    a   b   c
    
    p0
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
  
  (let (Ref (val> p0)
        Car (get Ref 'a)
        Tgt (get Ref 'c))
    
    (cond ((= (type Car) '(+Pair))
            (put p0 'b Car)
            (mov-part-abv> p0 Car (cons T)))
            
          ((isa '+Nil Ref)

            # Mov under
            (cmd-mov-nl)

            # Set Y layout
            (with Tgt (=: lay (if (= (: lay) 'x) 'y 'x)))
            (mov-cur-part Tgt 'start)
            (lay-part Tgt (get-origin> *main))

            # Update ptr
            (put p0 'b Tgt)
            (mov-part-abv> p0 Tgt (cons T))

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

  (put p0 'b Tgt)
  (mov-part-abv> p0 Tgt (cons T))
            
}#

###########
# Euclidean


(de cmd-trans-ptr (Cnt Fn)
  
  (let Ref (val> p0)

    (mov-cur-part p0 'start)
    (eval Fn)

    # Ignore Ref/Cdr
    (put p0 'b)
    (lay-part p0 *cur)
    (put p0 'b Ref)))
    

(de cmd-trans-ptr-px (Keysym)
  (cmd-trans-ptr 1 '(adv-cur 1.0)))
  
(de cmd-trans-ptr-nx (Keysym)
  (cmd-trans-ptr 1 '(adv-cur -1.0)))
  
(de cmd-trans-ptr-py (Keysym)
  (cmd-trans-ptr 2 '(nl-cur -1.0)))
  
(de cmd-trans-ptr-ny (Keysym)
  (cmd-trans-ptr 2 '(nl-cur 1.0)))


###########
# Manhatten
