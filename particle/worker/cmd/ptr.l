###############################
# Semantic Traversal (Pair/Ptr)

(de cmd-ptr-left (Ks)
  (if (and (rel-a?> *ptr)
           (is-pair? (lup-pt (ref> *ptr)))
           (not (lay-y?> *ptr)))
    (sup> *ptr T)
    (pre> *ptr T)))


(de cmd-ptr-right (Ks)
  (if (lay-y?> *ptr)
      (sub> *ptr T)
      (nex> *ptr T)))


(de cmd-ptr-up (Ks)
  (if (or (and (rel-a?> *ptr)
               (is-pair? (lup-pt (ref> *ptr))))
          (lay-y?> *ptr))
      (pre> *ptr T)
      (sup> *ptr T)))


(de cmd-ptr-down (Ks)
  (if (lay-y?> *ptr)
      (nex> *ptr T)
      (sub> *ptr T)))

      
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

  (with *ptr
        
    (let Ref (if (: cell)
                 (: ref)
                 (lup-pt (: ref)))
    
      (prinl "CURRENT REFERENCE:")
      (prin "Ref: ") (println Ref)
    
      (prin "dat: ")  (println (get Ref 'data))
      (prin "rel: ")  (println (: rel))
      (prin "ori: ")  (println (get Ref 'ori))
      (prin "dims: ") (println (get Ref 'dims))
      (prin "lay: ")  (println (get Ref 'lay))
      
      (prin "a: ")  (println (get Ref 'a))
      (prin "b: ")  (println (get Ref 'b))
      
      (when (get Ref 'line)
        (prin "line.p: ")    (println (get Ref 'line 'p))
        (prin "line.n: ")    (println (get Ref 'line 'n))
        (prin "line.l: ")    (println (get Ref 'line 'l))
        (prin "line.dims: ") (println (get Ref 'line 'dims)))))
      
  (when T
    (prinl "=============")
    (prinl "CURRENT LINE:")
    (prin "Lines: ") (println (get *ptr 'ln))
    (let Ptr-Ln (car (get *ptr 'ln))
      (prin "Ptr Ln: ") (println Ptr-Ln (get Ptr-Ln 'data)))
    #(prin "*line: ") (println *line (get *line 'data))
    #(println 'p (get *line 'line 'p) (get *line 'line 'p 'data))
    #(println 'n (get *line 'line 'n) (get *line 'line 'n 'data))
    #(println 'l (get *line 'line 'l) (get *line 'line 'l 'data))
    )

  #(upd-tree)
  (mov-⇲ (c-ln> *ptr)))
