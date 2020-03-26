(de cmd-swap-layout (Keysym)
  (println "Swap layout")
  
  (let Prv (get (val> p0) 'c)
      
    (with Prv
      (=: lay (if (= (: lay) 'x) 'y 'x)))
  
    (mov-cur-part Prv 'start)
    
    (setq *nl-pair NIL)
    (lay-part Prv *cur))
    (setq *nl-pair T))


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
  
  (let? Prv (get (val> p0) 'c)
    (println "Moved to " Prv (get Prv 'data))
    (put p0 'b Prv)
    (mov-part-abv> p0 Prv (cons T))))
  

(de cmd-point-ptr-nxt (Keysym)
  (println "Move to next item")
  
  (let? Cdr (get (val> p0) 'b)
    (println "Moved to " Cdr (get Cdr 'data))
    (put p0 'b Cdr)
    (mov-part-abv> p0 Cdr (cons T))))


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
  # Move to CAR if it is a list...or any CAR?
  # Only makes sense if modify a Pair...above logic works for Pairs also
  
  (println "Move to inner list")
  
  #{
    Car:
  
    If non-NIL, move ptr to it
  
    If NIL, move below, set y layout, keep ptr to CDR; cmd-make-char replaces it
    * Do not implicitly mov Car down - user explicitly does this
    * Outline format would have this done automatically
          
    User has entered NIL already:
    
                    p0
    [X] [X] [X] [X] NIL
    a   b   c   NIL
    

    [X] [X] [X]
    a   b   c
    
    p0
    [Y] NIL
    NIL
    
  }#  
  
  (let (Ref (val> p0)
        Car (get Ref 'a)
        Tgt (get Ref 'c))
    
    (cond ((= (type Car) '(+Pair))
            (put p0 'b Car)
            (mov-part-abv> p0 Car (cons T)))
            
          ((isa '+Nil Ref)

            # Set cur in position for cmd-make-char
            
            # Move under first item
            # This assumes X orientation...
            
            #(println 'enter-list (get *main 'lay) (get *main 'data))
                        
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

            # If not set, Cdr of NIL will be replaced instead of the CAR/entire list
            # Poss to make function that will replace both Cdr and Pair?
            # -> Probably not since they are different...
            (setq *mode-repl T)
            
            ))))
  

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
