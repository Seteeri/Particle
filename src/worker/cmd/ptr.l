(de cmd-swap-layout (Keysym)
  (println "Swap layout")
  
  # Note this causes bounds change also
  
  # If NIL, get prev, else do ref  
  
  (let (Ref (get *0 'b)
        Prv (get Ref 'c)
        Tgt (if (= (get Ref 'data) NIL) Prv Ref))
    (when Tgt
      (println "Swap!" Tgt (get Tgt 'data) (get Tgt 'lay) '-> (if (= (get Tgt 'lay) 'x) 'y 'x))
      (swap-lay Tgt))))


(de cmd-mov-nl (Keysym)
  (println "Move newline")

  # Calc min Y until NIL or nl
  (let Ref (get *0 'b)
  
    # If already newline, don't do anything
    # TODO: Add support for this later?
    (unless (get Ref 'nl)
  
      (mov-nl Ref)
      
      # Move to Cdr if there
      (let Cdr (get Ref 'b)
        (if Cdr
            (prog
              (put *0 'b Cdr)
              (mov-part-abv> *0 Cdr (cons T)))
            (prog
              (put *0 'b Ref)
              (mov-part-abv> *0 Ref (cons T))))))))
              
  
(de cmd-mov-bl (Keysym)

  # This moves in front of prev

  (let (Ref (get *0 'b)
        Line (get Ref 'nl)
        Prv-Line (get Line 'p)
        Nxt-Line (get Line 'n))
    
      # Make this a fn
      # Unlink lines
      
      (when Prv-Line
        (with (get Prv-Line 'nl)
          (=: n Nxt-Line)))
        
      (when Nxt-Line
        (with (get Nxt-Line 'nl)
          (=: p Prv-Line)))
      
      (setq *line Prv-Line)
      
      (put Ref 'nl NIL)
    
      (align-cur-to-bnds (get Ref 'c))
      (lay-part Ref *cur)
      
      (put *0 'b Car Ref)
      (mov-part-abv> *0 Ref (cons T))))
      

###############################
# Semantic Traversal (Pair/Ptr)

(de cmd-ptr-left (Keysym)
  (if (= (get (get *0 'b) 'lay) 'x)
    (point-b-prv)
    (point-a-out)))
  

(de cmd-ptr-right (Keysym)
  (if (= (get (get *0 'b) 'lay) 'x)
      (point-b-nxt)
      (point-a-in)))


(de cmd-ptr-up (Keysym)
  (if (= (get (get *0 'b) 'lay) 'x)
      (point-a-out)
      (point-b-prv)))


(de cmd-ptr-down (Keysym)
  (if (= (get (get *0 'b) 'lay) 'x)
      (point-a-in)
      (point-b-nxt)))


###########
# Euclidean


(de cmd-trans-ptr (Cnt Fn)
  
  (let Ref (get *0 'b)

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
