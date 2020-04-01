###############################
# Semantic Traversal (Pair/Ptr)

(de cmd-ptr-left (Keysym)
  (if (= (get *0 'b 'lay) 'x)
    (point-b-prv)
    (point-a-out)))
  

(de cmd-ptr-right (Keysym)
  (if (= (get *0 'b 'lay) 'x)
      (point-b-nxt)
      (point-a-in)))


(de cmd-ptr-up (Keysym)
  (if (= (get *0 'b 'lay) 'x)
      (point-a-out)
      (point-b-prv)))


(de cmd-ptr-down (Keysym)
  (if (= (get *0 'b 'lay) 'x)
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
