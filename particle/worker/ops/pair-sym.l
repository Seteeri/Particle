(de make-pair-str (Dat Lay)
  
  (cond
    
    ######
    # ATOM
                
    ((b-nil?> *ptr)
      (println 'make-pair-str "B NIL")
      # For Y-Pairs and Symbols: ins
      # For X-Pair: repl
      # Should replace with char instead of pair-char
      # (repl-b-nil Dat)
      (ins-b-nil Dat))
     
    ((b-atom?> *ptr)
      (println 'make-pair-str "B ATOM")
      (repl-b-atom Dat))     
     
    ((or (a-atom?> *ptr)
         (a-nil?> *ptr))
      (println 'make-pair-str "A ATOM")
      # Poss make exception for Y-Pair
      # Instead similar to repl-b-nil...
      (repl-a-atom Dat))
            
    ######
    # PAIR

    ((a-pair?> *ptr)
      (println 'make-pair-str "A PAIR")
      
      # ins bef cur
      # ((a)) -> (b (a))
      
      (con-a-pair-x Dat))
      
    ((b-pair?> *ptr)
      (println 'make-pair-str "B PAIR")
      
      # ins bef cur
      # (a) -> (b a)
      
      #(lay-y?> *ptr) -> (con-b-pair-y Dat)
      (con-b-pair-x Dat))
      
    (T
      # root
      (println 'make-pair-str "*ptr uninitialized"))))

      
(de ins-b-nil (Dat)

  (println 'ins-b-nil)
  
  (let (Cell (cell> *ptr)
        Po-Cell (lup-pt Cell)        
        Po-Del  (ref> *ptr)
        List-Dat (list Dat)
        Po-Dat (gen-point List-Dat 'x NIL T)
        Atomic-Cell (atom Cell))
        
    (con> Po-Cell List-Dat (unless Dat Po-Dat))              

    ~(assert Po-Del)
    
    (when (get Po-Del 'line)
      (mov-line Po-Del Po-Dat)
      (ln> *ptr))
    (mov-line-n> *ptr Po-Dat)
    
    (set-ref> *ptr List-Dat 'b))
  
  (upd-tree)
  (mov-⇲ (c-ln> *ptr))
  
  #(top> Po-Dat)
  #(draw> *ptr)  
  
  (nex> *ptr))
  
  
(de repl-b-atom (Dat)
  T)        
      
      
(de repl-a-atom (Dat)

  #{
  
    Handles both X and Y pairs
    
  }#

  (println 'repl-a-atom)
  
  (let (Cell (if (cell> *ptr)
                 @
                 (find-back> *ptr (rel> *ptr)))
        Po-Cell (lup-pt Cell)
        Po-Del (if (cell> *ptr)
                   (get Po-Cell (rel> *ptr))
                   (lup-pt (ref> *ptr)))
        Po-Dat (gen-point Dat 'x NIL T))

    (when (get Po-Del 'line)
      (mov-line Po-Del Po-Dat)
      (ln> *ptr))
    (mov-line-n> *ptr Po-Dat)
        
    (if Dat
        (set-ref> *ptr Dat 'a)
        (set-ref> *ptr Cell 'a Po-Dat))
    
    (println Cell (type Po-Cell))
    (set> Po-Cell Dat Po-Dat T)        
        
    (upd-tree)
    (mov-⇲ (c-ln> *ptr))))

    
(de con-b-pair-x (Dat)
  
  (let (Cell (ref> *ptr)
        Po-Cell (lup-pt Cell)
        List-Dat (list Dat)
        Po-Dat (gen-point List-Dat 'x NIL T)
        Pre (if (cell> *ptr)
                @
                (find-back> *ptr (rel> *ptr)))
        Po-Pre (lup-pt Pre))
        
    # Insert before current object
    (con> Po-Pre List-Dat NIL)
    (con> Po-Dat Cell     NIL)
    
    # Swap line
    (when (get Po-Cell 'line)
      (mov-line Po-Cell Po-Dat)
      (ln> *ptr))
    (mov-line-n> *ptr Po-Dat)
                
    # ref does not change
                
    (upd-tree)
    (mov-⇲ (c-ln> *ptr))
    
    (top> Po-Cell)
    (draw> *ptr)))


(de con-a-pair-x (Dat)
  T)    
