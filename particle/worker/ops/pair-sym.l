(de make-pair-str (Dat Lay)
  #{
  
    - Repl str on atoms
    - Ins str (before) on pairs
    - Use list command
      - Make list with atom    
      - Make list with list

  }#

  (println (get *ptr 'rel) (get *ptr 'ref) (get *ptr 'nil))
  
  (cond
    
    #####
    # NIL
                
    ((b-nil?> *ptr)
      (println 'make-pair-str "B NIL")      
      (repl-b-nil Dat))
     
    ######
    # ATOM
      
    ((or (a-atom?> *ptr)
         (a-nil?> *ptr))
      (println 'make-pair-str "A ATOM")
      (repl-a-atom Dat))
      
      
    ((b-atom?> *ptr)
      (println 'make-pair-str "B ATOM")
      (repl-b-atom Dat))
      
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
      
      
(de repl-a-atom (Dat)

  (println 'repl-a-atom)
  
  (let (Cell (if (nil> *ptr)
                 @
                 (find-back> *ptr (rel> *ptr)))
        Po-Cell (lup-pt Cell)
        Po-Dat (gen-point Dat 'x NIL T))
                
    (if Dat
        (set-ref> *ptr Dat 'a)
        (set-ref> *ptr Cell 'a Po-Dat))

    (set> Po-Cell Dat Po-Dat T)        
        
    (upd-tree)
    (mov-⇲ (c-ln> *ptr))))
      
      
(de repl-b-atom (Dat)
  T)        

  
(de con-a-pair-x (Dat)
  T)

  
###
      

(de repl-b-nil (Dat)

  (println 'repl-b-nil)
  
  (let (Cell (nil> *ptr)
        Po-Cell (lup-pt Cell)        
        Po-Del  (ref> *ptr)
        List-Dat (list Dat)
        Po-Dat (gen-point List-Dat 'x NIL T)
        Atomic-Cell (atom Cell))
        
    (con> Po-Cell List-Dat (unless Dat Po-Dat))              

    ~(assert Po-Del)
    
    (when (get Po-Del 'line)
      (mov-line Po-Del Po-Dat)
      (with *ptr (pop (:: ln))))
      
    (mov-line-n> *ptr Po-Dat)
    
    (set-ref> *ptr List-Dat 'b))
  
  (upd-tree)
  (mov-⇲ (c-ln> *ptr))
  
  #(top> Po-Dat)
  #(draw> *ptr)  
  
  (nex> *ptr))

    
(de con-b-pair-x (Dat)
  
  (let (Cell (ref> *ptr)
        Po-Cell (lup-pt Cell)
        List-Dat (list Dat)
        Po-Dat (gen-point List-Dat 'x NIL T)
        Pre (if (nil> *ptr)
                @
                (find-back> *ptr (rel> *ptr)))
        Po-Pre (lup-pt Pre))
        
    # Insert before current object
    (con> Po-Pre List-Dat NIL)
    (con> Po-Dat Cell     NIL)
    
    # Handle lines
                
    # ref does not change
                
    (upd-tree)
    (mov-⇲ (c-ln> *ptr))
    
    (top> Po-Cell)
    (draw> *ptr)))

