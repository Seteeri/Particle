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
  
  # Is this appropriate for Y layout? ...since it already moves to newline
  
  # Move pointer and move NIL (CDR)
  # Character after this will replace NIL
    
  (let (Ref (val> p0)
        Ori (get-origin> (get *main 'b))
        Bnds (calc-bounds> (get *main 'b)))
      
    (setq *cur (list (get Ori 1)
                     (get Bnds 2)
                     (get *cur 3)))

    # If this op is random
    # must know min y for all previous elements )Cars)
    #
    # When parts gen, cache bnds for atoms, since they don't change
    # Easy to calc with fixed-width since bnds = num verts * adv
    #
    # Based on layout, and vert len and adv, the bnds can be determined
    #
    # Everytime list is updated, update the len and bounds
    # Every item is at the border or within the bnds
    # So pos cursor at min Y, comparing the list and the item
    
    (println 'cmd-mov-nl "Set 'nl for " Ref (get Ref 'data))
    
    (put Ref 'nl 1.0)
    (lay-part Ref Ori)
    
    (put p0 'b Ref)
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
    (put p0 'b Prv)
    (mov-part-abv> p0 Prv (cons T))))
  

(de cmd-point-ptr-nxt (Keysym)
  (println "Move to next item")
  
  (let? Cdr (get (val> p0) 'b)
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
