(de cmd-swap-layout (Keysym)
  (println "Swap layout")
  
  # If NIL, get prev, else do ref  
  
  (let (Ref (val> *0)
        Prv (get Ref 'c)
        Tgt (if (= (get Ref 'data) NIL) Prv Ref))
    (when Tgt
      (println "Swap!" Tgt (get Tgt 'data) (get Tgt 'lay) '-> (if (= (get Tgt 'lay) 'x) 'y 'x))
      (swap-lay Tgt))))


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
  (let (Ref (val> *0)
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
    
    #(put *0 'b Ref)
    (mov-part-abv> *0 Ref (cons T))))
        
  
(de cmd-mov-bl (Keysym)

  # This moves in front of prev

  (let Ref (val> *0)
  
    (base-cur (get Ref 'c))
    (put Ref 'nl NIL)
    (lay-part Ref *cur)
    
    (put *0 'b Car Ref)
    (mov-part-abv> *0 Ref (cons T))))
      

###############################
# Semantic Traversal (Pair/Ptr)

(de cmd-ptr-left (Keysym)
  (println "Move to prev item")

  (let Ref (val> *0)
  
    (if (= (get Ref 'lay) 'x)
  
        (mov-ptr-b-prv)
          
        (prog))))
  

(de cmd-ptr-right (Keysym)
  (println "Move to next item")

  (let Ref (val> *0)
  
    (if (= (get Ref 'lay) 'x)
        
        (mov-ptr-b-nxt)
          
        (prog))))


(de cmd-ptr-up (Keysym)
  # Move to super list
    
  (println "Move to outer list")
  
  (let Ref (val> *0)
  
    (if (= (get Ref 'lay) 'x)

      (mov-ptr-a-out)
          
      (prog))))


(de cmd-ptr-down (Keysym)
  
  (println "Move to inner list")
  
  (let Ref (val> *0)
  
    (if (= (get Ref 'lay) 'x)
  
      (mov-ptr-a-in)
                
      (prog))))


###########
# Euclidean


(de cmd-trans-ptr (Cnt Fn)
  
  (let Ref (val> *0)

    (mov-cur-part *0 'start)
    (eval Fn)

    # Ignore Ref/Cdr
    (put *0 'b)
    (lay-part *0 *cur)
    (put *0 'b Ref)))
    

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
