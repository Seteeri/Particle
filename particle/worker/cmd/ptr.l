###############################
# Semantic Traversal (Pair/Ptr)

(de cmd-ptr-left (Ks)    
  # Only if Ref and Prv both are Y layout
  (let Ref (get *fwd-in 'b)
    (if *on-car
        (point-prv)
        (if (is-car?> Ref)
            (point-sup)
            (point-prv)))))


(de cmd-ptr-right (Ks)
  (if (lay-x?> (get *fwd-in 'b))
      (point-nxt)
      (point-sub)))


(de cmd-ptr-up (Ks)
  (if (lay-x?> (get *fwd-in 'b))
      (point-sup)
      (point-prv)))


(de cmd-ptr-down (Ks)
  (if (lay-x?> (get *fwd-in 'b))
      (point-sub)
      (point-nxt)))

      
(de cmd-ptr-ln-first (Ks)
  (point-ln-first))


(de cmd-ptr-ln-last (Ks)
  (point-ln-last))
      

###########
# Euclidean


(de cmd-trans-ptr (Cnt Fn)
  
  (let Ref (get *fwd-in 'b)

    (mov-cur-pt *fwd-in 'start)
    (eval Fn)

    (lay-ptr *fwd-in)))
    

(de cmd-trans-ptr-px (Ks)
  (cmd-trans-ptr 1 '(adj-cur-x 1.0)))
  
(de cmd-trans-ptr-nx (Ks)
  (cmd-trans-ptr 1 '(adj-cur-x -1.0)))
  
(de cmd-trans-ptr-py (Ks)
  (cmd-trans-ptr 2 '(adj-cur-y -1.0)))
  
(de cmd-trans-ptr-ny (Ks)
  (cmd-trans-ptr 2 '(adj-cur-y 1.0)))


(de cmd-print-ptr ()

  # Print ptr
  (let Ref (get *fwd-in 'b)
    (println Ref (get Ref 'data)))

  (println "Master Line: " *line (get *master 'b 'data))

  (let Ref (get *fwd-in 'b)
  
    (println "*on-car: " *on-car)
    (println "Ref: " Ref (get Ref 'data))
    (when T
      (println "ori: " (get Ref 'ori))
      (println "dims: " (get Ref 'dims))
      (println "lay: " (get Ref 'lay))
      (when (get Ref 'line)
        (println "line.p: " (get Ref 'line 'p))
        (println "line.n: " (get Ref 'line 'n))
        (println "line.l: " (get Ref 'line 'l))
        (println "line.dims: " (get Ref 'line 'dims)))
      (when NIL
        (println "a: " (get Ref 'a))
        (println "b: " (get Ref 'b))
        (println "c: " (get Ref 'c)))
      
      (when NIL
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
          (println "c: " (get Ref 'b 'c)))))
        
    (when NIL
      (println "====")
      (println "Line:" *line (get *line 'data))
      (println 'p (get *line 'line 'p) (get *line 'line 'p 'data))
      (println 'n (get *line 'line 'n) (get *line 'line 'n 'data))
      (println 'l (get *line 'line 'l) (get *line 'line 'l 'data)))

    (upd-tree)
    (mov-⇲ *line)))
