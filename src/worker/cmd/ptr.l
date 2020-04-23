###############################
# Semantic Traversal (Pair/Ptr)

(de cmd-ptr-left (Keysym)    
  # Only if Ref and Prv both are Y layout
  (if (and (= (get *0 'b 'lay) 'y)
           (= (get *0 'b 'c 'lay) 'y))
      (point-sup)
      (point-prv)))


(de cmd-ptr-right (Keysym)
  (if (= (get *0 'b 'lay) 'x)
      (point-nxt)
      (point-sub)))


(de cmd-ptr-up (Keysym)
  (if (= (get *0 'b 'lay) 'x)
      (point-sup)
      (point-prv)))


(de cmd-ptr-down (Keysym)
  (if (= (get *0 'b 'lay) 'x)
      (point-sub)
      (point-nxt)))

      
(de cmd-ptr-ln-first (Keysym)
  (point-ln-first))


(de cmd-ptr-ln-last (Keysym)
  (point-ln-last))
      

###########
# Euclidean


(de cmd-trans-ptr (Cnt Fn)
  
  (let Ref (get *0 'b)

    (mov-cur-part *0 'start)
    (eval Fn)

    # Ignore Ref/Cdr
    (put *0 'b)
    (lay-part-start *0 *cur)
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
