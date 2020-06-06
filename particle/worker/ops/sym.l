#{

      [X]
      .
            *
      [Y]  NIL
      
      .

  -->

      [X]
      .
                  *
      [Y]  [X]  NIL
            .
            
      .    
  
}#

(de make-pair-str (Dat Lay)
  
  (cond
    
    ######
    # ATOM
    
    ((or (a-atom?> *ptr)
         (a-nil?> *ptr))
      (println 'make-pair-str "B ATOM/NIL")
      (repl-atom Dat 'a)
      (upd-tree)
      (mov-⇲ (c-ln> *ptr)))
      
    ((or (b-nil?> *ptr)
         (b-atom?> *ptr))
      (println 'make-pair-str "B ATOM/NIL")
      #(repl-atom Dat 'b)
      (ins-b-atom Dat)
      (nex> *ptr)
      (upd-tree)
      (mov-⇲ (c-ln> *ptr)))
            
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
  
  
(de repl-atom (Dat Rel)

  #{
  
    Handles both X and Y pairs
  
  }#

  (println 'repl-atom)
  
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
        (set-ref> *ptr Dat Rel)
        (set-ref> *ptr Cell Rel Po-Dat))
    
    (if (= Rel 'a)
        (set> Po-Cell Dat Po-Dat T)
        (con> Po-Cell Dat Po-Dat T))))


(de ins-b-atom (Dat)

  (println 'ins-b-atom)
  
  (let (Pair (cell> *ptr)
        Po-Pair (lup-pt Pair)
        Po-Del (get Po-Pair 'b)
        List (list Dat)
        Po-List (gen-point List 'x NIL T))
    
    (when (get Po-Del 'line)
      (mov-line Po-Del Po-List)
      (ln> *ptr))
    (mov-line-n> *ptr Po-List)
    
    (con> Po-Pair List Po-List)
        
    (set-ref> *ptr List 'b))
  
  (upd-tree)
  (mov-⇲ (c-ln> *ptr)))        
        

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
  #{

              *
        [Y]  [Y] .
              .
            
        .
            
    -->
      
              *
        [Y]  [X]
              .
        
             [Y] .
              .
            
        .  
  }#
  
  T)
