###############################
# Semantic Traversal (Pair/Ptr)

(de cmd-ptr-left (Ks)    
  # Only if Ref and Prv both are Y layout
  (if (and (lay-y?> (get *cdr 'b))
           (lay-y?> (get *cdr 'b 'c)))
      (point-sup)
      (point-prv)))


(de cmd-ptr-right (Ks)
  (if (lay-x?> (get *cdr 'b))
      (point-nxt)
      (point-sub)))


(de cmd-ptr-up (Ks)
  (if (lay-x?> (get *cdr 'b))
      (point-sup)
      (point-prv)))


(de cmd-ptr-down (Ks)
  (if (lay-x?> (get *cdr 'b))
      (point-sub)
      (point-nxt)))

      
(de cmd-ptr-ln-first (Ks)
  (point-ln-first))


(de cmd-ptr-ln-last (Ks)
  (point-ln-last))
      

###########
# Euclidean


(de cmd-trans-ptr (Cnt Fn)
  
  (let Ref (get *cdr 'b)

    (mov-cur-part *cdr 'start)
    (eval Fn)

    (lay-ptr *cdr)))
    

(de cmd-trans-ptr-px (Ks)
  (cmd-trans-ptr 1 '(adv-cur 1.0)))
  
(de cmd-trans-ptr-nx (Ks)
  (cmd-trans-ptr 1 '(adv-cur -1.0)))
  
(de cmd-trans-ptr-py (Ks)
  (cmd-trans-ptr 2 '(nl-cur -1.0)))
  
(de cmd-trans-ptr-ny (Ks)
  (cmd-trans-ptr 2 '(nl-cur 1.0)))


(de cmd-print-ptr ()

  # Print ptr
  (let Ref (get *cdr 'b)
    (println Ref (get Ref 'data)))

  (println "Master Line: " *master (get *master 'b 'data))

  (let Ref (get *cdr 'b)
  
    (println "*on-car: " *on-car)
    (println "Ref: " Ref (get Ref 'data))
    (when T
      (println "lay: " (get Ref 'lay))
      (when (get Ref 'line)
        (println "line.p: " (get Ref 'line 'p))
        (println "line.n: " (get Ref 'line 'n))
        (println "line.l: " (get Ref 'line 'l)))
      (when T
        (println "a: " (get Ref 'a))
        (println "b: " (get Ref 'b))
        (println "c: " (get Ref 'c)))
      
      (println "====")
      (println "Car: " (get Ref 'a))
      (println "lay: " (get Ref 'a 'lay))
      (when (get Ref 'a 'line)
        (println "line.p: " (get Ref 'a 'line 'p))
        (println "line.n: " (get Ref 'a 'line 'n))
        (println "line.l: " (get Ref 'a 'line 'l)))
      (when T
        (println "a: " (get Ref 'a 'a))
        (println "b: " (get Ref 'a 'b))
        (println "c: " (get Ref 'a 'c)))
        
      (println "====")
      (println "Cdr: " (get Ref 'b))    
      (println "lay: " (get Ref 'b 'lay))
      (when (get Ref 'b 'line)
        (println "line.p: " (get Ref 'b 'line 'p))
        (println "line.n: " (get Ref 'b 'line 'n))
        (println "line.l: " (get Ref 'b 'line 'l)))
      (when T  
        (println "a: " (get Ref 'b 'a))
        (println "b: " (get Ref 'b 'b))
        (println "c: " (get Ref 'b 'c))))
        
    (println "====")
    (println "Line:" *line (get *line 'data))
    (println 'p (get *line 'line 'p) (get *line 'line 'p 'data))
    (println 'n (get *line 'line 'n) (get *line 'line 'n 'data))
    (println 'l (get *line 'line 'l) (get *line 'line 'l 'data))

    (upd-tree)
    (mov-*1 *line)))
