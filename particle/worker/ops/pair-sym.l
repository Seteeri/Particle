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
    
    ((a-nil?> *ptr)
      (println 'make-pair-str "A NIL")
      
      # repl nil, only applies to pairs
      # (NIL . a) -> (b . NIL)
      
      (repl-a-nil Dat)
      
      T)
      
      
    ((b-nil?> *ptr)
      (println 'make-pair-str "B NIL")
      
      # repl nil for sym and pairs
      # (a . NIL) -> (a b . NIL)
      
      # Handle when P-Cell is a Pair
      
      (repl-b-nil Dat)
      T)      
     
    ######
    # ATOM
      
    ((a-atom?> *ptr)
      (println 'make-pair-str "A ATOM")
      
      # repl
      # (a . b) -> (c . a)
      
      (repl-a-atom Dat)
      T)      
      
      
    ((b-atom?> *ptr)
      (println 'make-pair-str "B ATOM")
      
      # Skip when pt'ing to symbol for now
      (unless (is-sym?> *ptr)
      
        # repl atom
        (repl-b-atom Dat)))
      
    ######
    # PAIR

    ((a-pair?> *ptr)
      (println 'make-pair-str "A PAIR")
      
      # ins bef cur
      # ((a)) -> ((b a))
      
      (if (lay-x?> *ptr)
          (con-a-pair-x Dat)
          (con-a-pair-y Dat)))
      
    ((b-pair?> *ptr)
      (println 'make-pair-str "B PAIR")
      
      # ins bef cur
      # (a) -> (b a)
      (if (lay-x?> *ptr)
          (con-b-pair-x Dat)
          (con-b-pair-y Dat)))
      
    (T
      # root
      (println 'make-pair-str "*ptr uninitialized"))))
      
      
(de repl-a-atom (Dat)

  (println 'repl-a-atom)

  (let (Po-Cell (ptr-car-sup '*ptr)
        Cell (get Po-Cell 'data)
        Po-Del  *ptr
        Po-Dat (gen-point Dat 'x NIL T))
            
    (set Cell Dat)
    
    (if Dat
        (ptr-ref '*ptr Po-Dat 'a)
        (ptr-ref '*ptr Po-Cell 'a Po-Dat))

    (put Po-Cell 'a (unless Dat Po-Dat))
    (del> Po-Del)

    (upd-tree)
    (mov-⇲ *line)))
     
      
(de repl-a-nil (Dat)

  (println 'repl-a-nil)

  (let (Po-Cell (ptr-nil '*ptr)
        Cell (get Po-Cell 'data)
        Po-Del  (get Po-Cell 'a)
        Po-Dat (gen-point Dat 'x NIL T))
            
    (set Cell Dat)
    
    (if Dat
        (ptr-ref '*ptr Po-Dat 'a)
        (ptr-ref '*ptr Po-Cell 'a Po-Dat))
        
    (put Po-Cell 'a (unless Dat Po-Dat))        
    (del> Po-Del)
    
    (upd-tree)
    (mov-⇲ *line)))
      
      
(de repl-b-atom (Dat)
  T)        

  
(de con-a-pair-x (Dat)
  T)

  
(de con-a-pair-y (Dat)
  T)  
  
  
###
      

(de repl-b-nil (Dat)

  (println 'repl-b-nil)
  
  (let (Po-Del  (get *ptr 'ref)
        Po-Cell (lup-pt (get *ptr 'nil))
        Cell (get Po-Cell 'data)
        List-Dat (list Dat)
        Po-Dat (gen-point List-Dat 'x NIL T)
        Atomic-Cell (atom Cell))
        
    # Depends on cell type and tgt
    (if Atomic-Cell
    
        # Pop line on del if necessary
    
        # Sym
        (prog
          (con> Po-Cell
                List-Dat
                (unless Dat Po-Dat))
          (when (get Po-Del 'line)
            (mov-line Po-Del Po-Dat)
            (with *ptr (pop (:: ln))))
          (mov-line-n> *ptr Po-Dat)
          (ref> *ptr List-Dat 'b))
          
        # Pair
        (prog
          (con> Po-Cell
                List-Dat
                (unless Dat Po-Dat))
          (when (get Po-Del 'line)
            (mov-line Po-Del Po-Dat)
            (with *ptr (pop (:: ln))))
          (mov-line-n> *ptr Po-Dat)
          (ref> *ptr List-Dat 'b)))
    
    
    (upd-tree)
    (mov-⇲ *line)
    
    (println "CALL NEXT")
    (nex> *ptr)
    
    #(top> Po-Dat)
    #(draw> *ptr)
    
    T))

    
(de con-b-pair-x (Dat)
  
  # Incomplete
  
  (let (Po-Cell *ptr
        Cell (get Po-Cell 'data)
        List-Dat (list Dat)
        Po-Dat (gen-point List-Dat 'x NIL T)
        Po-Pre (ptr-car-pre '*ptr)
        Pre (get Po-Pre 'data))
        
    # Insert before current object
    (con> Po-Pre List-Dat NIL)
    (con> Po-Dat Cell     NIL)
      
    (p-pre> (get '*ptr 'ptr) Po-Dat)
          
    (upd-tree)
    (mov-⇲ *line)
    (top> Po-Cell)
    (ptr-lay '*ptr)))
    
    
(de con-b-pair-y (Dat)
  T)
