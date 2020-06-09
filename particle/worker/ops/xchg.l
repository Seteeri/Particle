(de swap-lay-xy ()
  #{
  
    Replace atom with list with atom
  
        [X1]  [X2]  [X3] .
         .     *     .
              
    ->
               
        [X1]
         .  

             *
        [Y]  .

        
        [X3] .
         .
        
        
    * Change layout for C
    * Create lines
    
    * Equivalent to swap-layout?
    
    * When performing this cmd on Y-Pair car,
    this will do nothing since it's already done
    * If ptrs is on Y-Pair it will create
    the nested list effect
    
    * If output was X-Pair, then this would be
    equivalent to list doing cmd on Pair cdr
    
  }#       
  
  (let (Po-Tgt (if (cell> *ptr)
                   (ref> *ptr)
                   (lup-pt (ref> *ptr)))
        Pair (if (cell> *ptr)
                 @
                 (find-back> *ptr (rel> *ptr)))
        Po-Pair (lup-pt Pair))
    
    (when (lay-x?> Po-Pair)
      
      (println repl-a-atom "Pair/Tgt: "
        Pair (get Po-Pair 'a) Po-Tgt)
        
      (let (Ln (c-ln> *ptr)
            Ln-N (get Ln 'line 'n)
            Cdr (cdr Pair)
            Po-Cdr (if Cdr (lup-pt Cdr) (get Po-Pair 'b))
            Push-Pair (not (get Po-Pair 'line)))
        
        (set-lay> Po-Pair 'y)
        
        (if (get Po-Pair 'line)
            (put @ 'n Po-Cdr)      
            (ins-line-aft Ln Po-Pair))
        
        (if (get Po-Cdr 'line)
            (put @ 'p Po-Pair)
            (ins-line-aft Po-Pair Po-Cdr))
            
        (put Po-Tgt 'line (new '(+Line)))
        
        (upd-tree)
        (mov-⇲ Po-Tgt)
                          
        # If Pair has line, it will already exist
        # so ignore
        (when Push-Pair
          (ln> *ptr Po-Pair))
        (ln> *ptr Po-Tgt)
        
        (top> Po-Tgt)
        (draw> *ptr)))))     

        
(de swap-lay-yx ()
  T)
