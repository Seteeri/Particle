###############################
# Semantic Traversal (Pair/Ptr)

(de cmd-ptr-left (Ks)
  (if (and (= (ptr-rel '*ptr) 'a)
           (is-pair? *ptr)
           (not (lay-y?> *ptr)))
    (sup> *ptr)
    (pre> *ptr)))


(de cmd-ptr-right (Ks)
  (if (lay-y?> *ptr)
      (sub> *ptr)
      (nex> *ptr)))


(de cmd-ptr-up (Ks)
  (if (or (and (= (ptr-rel '*ptr) 'a)
               (is-pair? *ptr))
          (lay-y?> *ptr))
      (pre> *ptr)
      (sup> *ptr)))


(de cmd-ptr-down (Ks)
  (if (lay-y?> *ptr)
      (nex> *ptr)
      (sub> *ptr)))

      
(de cmd-ptr-ln-first (Ks)
  (pt-ln-fir))


(de cmd-ptr-ln-last (Ks)
  (pt-ln-las))
      

###########
# Euclidean


(de cmd-trans-ptr (Cnt Fn)
  
  (let Ref (get-ptr-ref)

    (mov-cur-pt *ptr 'start)
    (eval Fn)

    (lay-ptr *ptr)))
    

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
  (let Ref *ptr
    (println Ref (get Ref 'data)))

  #(println "Master Line: " *line (get *master 'b 'data))

  (let Ref *ptr
  
    (prinl "CURRENT REFERENCE:")
    (prin "Ref: ") (println Ref (get Ref 'data))
    
    (when T
      (prin "ori: ")  (println (get Ref 'ori))
      (prin "dims: ") (println (get Ref 'dims))
      (prin "lay: ")  (println (get Ref 'lay))
      (when (get Ref 'line)
        (prin "line.p: ")    (println (get Ref 'line 'p))
        (prin "line.n: ")    (println (get Ref 'line 'n))
        (prin "line.l: ")    (println (get Ref 'line 'l))
        (prin "line.dims: ") (println (get Ref 'line 'dims)))
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
        
    (when T
      (prinl "=============")
      (prinl "CURRENT LINE:")
      (prin "Line: ") (println *line (get *line 'data))
      (println 'p (get *line 'line 'p) (get *line 'line 'p 'data))
      (println 'n (get *line 'line 'n) (get *line 'line 'n 'data))
      (println 'l (get *line 'line 'l) (get *line 'line 'l 'data)))

    #(upd-tree)
    (mov-⇲ *line)
    ))
