(de cmd-chg-mode (Ks)
  (println 'cmd-chg-mode)
  
  # Relationship among:
  # * Mode
  # * Ptr
  # * Binds/Maps
  
  # Ptr has mode which has binds/maps
  
  (let M *modes
  
    (setq *modes
          (if (= M *mode-txt)
              *mode-cmd
              *mode-txt))

    (setq *cur (list 1000.0 1000.0))
    (let (Po  (get *ptr 'pt)
          Dat (get Po 'any))
      (mov> Po)
      (lay-pt-start Dat Dat T T))
              
    (put *ptr 'pt
      (if (= M *mode-txt)
          *po-ptr-cmd
          *po-ptr-txt))
    
    (top-ref> *ptr)
    (draw> *ptr)))


###############################
# Semantic Traversal (Pair/Ptr)

(de cmd-ptr-left (Ks)
  (if (and (reg-a?> *ptr)
           (is-pair? (lup-pt (get *ptr 'ref)))
           (not (lay-y?> *ptr)))
    (sup> *ptr T)
    (pre> *ptr T)))


(de cmd-ptr-right (Ks)
  (if (lay-y?> *ptr)
      (sub> *ptr T)
      (nex> *ptr T)))


(de cmd-ptr-up (Ks)
  (if (or (and (reg-a?> *ptr)
               (is-pair? (lup-pt (get *ptr 'ref))))
          (lay-y?> *ptr))
      (pre> *ptr T)
      (sup> *ptr T)))


(de cmd-ptr-down (Ks)
  (if (lay-y?> *ptr)
      (nex> *ptr T)
      (sub> *ptr T)))

      
(de cmd-ptr-ln-first (Ks)
  T)


(de cmd-ptr-ln-last (Ks)
  T)
      
      
(de cmd-ptr-ls-first (Ks) 
  (ptr-ls-first))


(de cmd-ptr-ls-last (Ks) 
  (ptr-ls-last))
  

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


#######
# Debug
  
(de cmd-print-ptr ()

  #(prinl "START:")
  #(println *start)

  (prinl)
  
  (let ((Pt-Pair Ref) (get-ref-pt> *ptr))
                
    (prinl "PTR:")
    (prin "Ref: ") (println Ref)
    (prin "any: ")  (println (get Ref 'any))    
    (prin "a: ")  (println (get Ref 'a))
    (prin "b: ")  (println (get Ref 'b))      
    #(prin "ori: ")  (println (get Ref 'ori))
    #(prin "dims: ") (println (get Ref 'dims))
    #(prin "lay: ")  (println (get Ref 'lay))
        
    (when (get Ref 'line)
      (prin "line.p: ")    (println (get Ref 'line 'p))
      (prin "line.n: ")    (println (get Ref 'line 'n))
      (prin "line.dims: ") (println (get Ref 'line 'dims))))
      
  (when T
    (prinl "=============")
    (prinl "LINE:")
    (prin "Lines: ") (println (get *ptr 'lns))
    (let Ptr-Ln (car (get *ptr 'lns))
      (prin "Ptr Ln: ") (println Ptr-Ln (get Ptr-Ln 'any)))
    #(prin "*line: ") (println *line (get *line 'any))
    #(println 'p (get *line 'line 'p) (get *line 'line 'p 'any))
    #(println 'n (get *line 'line 'n) (get *line 'line 'n 'any))
    #(println 'l (get *line 'line 'l) (get *line 'line 'l 'any))
    )

  #(lay-*start)
  (mov-⇲ (c-lns> *ptr)))

  
(de cmd-reset-start (Ks)
  (del-start-val (lup-pt '*start) T))
