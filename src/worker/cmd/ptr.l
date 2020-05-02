###############################
# Semantic Traversal (Pair/Ptr)

(de cmd-ptr-left (Keysym)    
  # Only if Ref and Prv both are Y layout
  (if (and (lay-y?> (get *0 'b))
           (lay-y?> (get *0 'b 'c)))
      (point-sup)
      (point-prv)))


(de cmd-ptr-right (Keysym)
  (if (lay-x?> (get *0 'b))
      (point-nxt)
      (point-sub)))


(de cmd-ptr-up (Keysym)
  (if (lay-x?> (get *0 'b))
      (point-sup)
      (point-prv)))


(de cmd-ptr-down (Keysym)
  (if (lay-x?> (get *0 'b))
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


(de cmd-print-ptr ()

  # Print ptr
  (let Ref (get *0 'b)
    (println Ref (get Ref 'data)))

  (println "Master Line: " *master (get *master 'b 'data))

  # Print lines out
  (let Ref (get *0 'b)
  
    # Print current line
    (println "Current Ref: " Ref (get Ref 'data))
    (println "*on-car: " *on-car)
    (println "Current Line:" *line (get *line 'data))
    #(view (getl (get *line 'line)))
    (println 'p (get *line 'line 'p) (get *line 'line 'p 'data))
    (println 'n (get *line 'line 'n) (get *line 'line 'n 'data))
    (println 'l (get *line 'line 'l) (get *line 'line 'l 'data))

    (upd-tree)
    (mov-*1 *line)      
    
    (mov-*1 Line)))
