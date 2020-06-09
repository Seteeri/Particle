(de make-pair-a-atom ()
  
  #{
  
    Replace pair with pair.car=pair

    Later output X-Pair
    
    Case 1:
    
               *
        [X1]  [X2]  [X3]  .
         .     .     .
          
      -->
            
        [X1]
         .
         
                 *  
        [Y/X2]  [X]  .
                 .
        
        [X3]  .
         .    

        X-Pair -->
        
          [X1]
          .
          
          [X4] 
          
          [X2]  .
          .
          
          [X3]  .
          .          
         
    Case 2:
      
        [X] 
         .

         * 
        [X]  .
         .
        
      -->
               
        [X] 
         .

              * 
        [Y]  [X]  .
              .
        
         .
         
    Case 3:
      
      Handled in other fn
         
    * Note repeating cmd on output ptr,
    will use prv cmd, since pair is now a car of a pair
            
  }#   
    
  (println 'make-pair-a-atom)
  
  (sup> *ptr)  
  
  (let (Pair (ref> *ptr)
        Po-Pair (lup-pt Pair)
        Tgt (car Pair)
        Po-Tgt (if (get Po-Pair 'a)
                   @
                   (lup-pt Tgt)))
    
    (println 'repl-b-pair-x "Pair/Tgt: "
      Pair Tgt)
  
    # Delete:
    # * Nil ptr ref otherwise set-ref will attempt uncol on ref
    # * Put NIL otherwise set will del new point
    # * Pop line if necessary
    #
    # Does this functionality need to be in the methods?
    (put *ptr 'ref)  
    (put Po-Pair 'a)
    (when (= (c-ln> *ptr) Po-Tgt) (ln> *ptr))
    (del> Po-Tgt)    
    
    (let (List (list Tgt)
          Po-List (gen-point List 'x NIL T)
          Ln (c-ln> *ptr)
          Ln-N (get Ln 'line 'n)
          Cdr (cdr Pair)
          Po-Cdr (if Cdr (lup-pt Cdr) (get Po-Pair 'b))
          Push-Pair (not (get Po-Pair 'line)))
    
      (set-lay> Po-Pair 'y)    
    
      (set> Po-Pair List Po-List)
            
      (if (get Po-Pair 'line)
          (put @ 'n Po-Cdr)      
          (ins-line-aft Ln Po-Pair))
      
      (if (get Po-Cdr 'line)
          (put @ 'p Po-Pair)
          (ins-line-aft Po-Pair Po-Cdr))
          
      (put Po-List 'line (new '(+Line)))
      
      (upd-tree)
      (mov-⇲ (c-ln> *ptr))
            
      (set-ref> *ptr List 'a)
            
      (when Push-Pair (ln> *ptr Po-Pair))
      (ln> *ptr Po-List)
      (top> Po-List)
      (draw> *ptr))))

        
(de make-pair-b-atom ()

  #{
  
    Replace cdr with list with cdr

                    * 
        [X1]  [X2]  .
         .     .
               
              
    ->
               
        [X1]  [X2]
         .     .

         *
        [Y]  .
        
         .
    
    
    * Delete original Point
    * Make new list
    * Create lines
  
    * Later, output X-Pair
    
    * Poss, do similar as repl-a-atom
    convert to Y-Pair with Atom as the Cdr
    
  }#   

  (println 'make-pair-b-atom)
  
  (let (Po-Tgt (if (cell> *ptr)
                   (ref> *ptr)
                   (lup-pt (ref> *ptr)))
        Pair (if (cell> *ptr)
                 @
                 (find-back> *ptr (rel> *ptr)))
        Po-Pair (lup-pt Pair))
    
    (println 'repl-b-atom "Pair/Cdr: " 
      Pair (get Po-Pair 'b) Po-Tgt)
  
    # Delete:
    # * Nil ptr ref otherwise set-ref will attempt uncol on ref
    # * Put NIL otherwise set will del new point
    # * Pop line if necessary
    (put *ptr 'ref)  
    (put Po-Pair 'b)
    (when (= (c-ln> *ptr) Po-Tgt) (ln> *ptr))
    (del> Po-Tgt)
    
    (let (List (list Tgt)
          Po-List (gen-point List 'x NIL T)
          Po-Car (get Po-List 'a)
          Po-Cdr (get Po-List 'b)
          Ln (c-ln> *ptr))
    
      (set-lay> Po-List 'y)    
    
      (con> Po-Pair List)
            
      (if (get Po-List 'line)
          (put @ 'n Po-Cdr)
          (ins-line-aft Ln Po-List))
          
      (if (get Po-Cdr 'line)
          (put @ 'p Po-List)
          (ins-line-aft Po-List Po-Cdr))
      
      (put Po-Car 'line (new '(+Line)))
      
      (upd-tree)
      (mov-⇲ Ln)
            
      (set-ref> *ptr List 'b)            
      (ln> *ptr Po-List)
      (top> Po-List)
      (draw> *ptr))))          
