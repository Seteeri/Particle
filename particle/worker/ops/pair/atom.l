(de make-pair-a-atom-x ()  

  #{
  
    Replace atom with pair.car = atom
    
      [X]  [X]  [X]  .
       .    .    .
            *
          
    -->

      [X]
       .    
    
            *
      [Y]  [X]  .
            .
      
      [X]
       .      
      
  }#

  #(println 'make-pair-a-atom-x)
    
  (let (Po-Tgt (if (cell> *ptr)
                   (ref> *ptr)
                   (lup-pt (ref> *ptr)))
        Tgt (get Po-Tgt 'any)
        Pair (get Po-Tgt 'c 'any)
        Po-Pair (lup-pt Pair))
                  
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
      
      (set-ref> *ptr List 'a)
      
      (upd-tree)
      (mov-⇲ (c-ln> *ptr))
      
      (when Push-Pair (ln> *ptr Po-Pair))
      (ln> *ptr Po-List)
      (top> Po-List)
      (draw> *ptr))))    


(de make-pair-a-atom-y ()
  
  #{
  
    Replace atom with pair.car = atom
    
      [X]
       .
    
           *
      [Y]  z
       
       
      [X]
       .
          
    -->

      [X]
       .    
    
            *
      [Y]  [X]  .
            z
      
      [X]
       .      
      
  }#

  (let (Po-Tgt (if (cell> *ptr)
                   (ref> *ptr)
                   (lup-pt (ref> *ptr)))
        Tgt (get Po-Tgt 'any)
        Pair (get Po-Tgt 'c 'any)
        Po-Pair (lup-pt Pair))
    
    ~(assert (get Po-Tgt 'line))
      
    # Delete:
    # * Nil ptr ref otherwise set-ref will attempt uncol on ref
    # * Put NIL otherwise set will del new point
    # * Pop line if necessary
    (put *ptr 'ref)  
    (put Po-Pair 'a)
    (when (= (c-ln> *ptr) Po-Tgt) (ln> *ptr))
    (del> Po-Tgt)
    
    (let (List (list Tgt)
          Po-List (gen-point List 'x NIL T)
          Po-Car (get Po-List 'a)
          Po-Cdr (get Po-List 'b)
          Ln (c-ln> *ptr))
    
      (set-lay> Po-List 'y)
    
      (set> Po-Pair List)

      (put Po-List 'line (new '(+Line)))
      
      (set-ref> *ptr List 'a)

      (upd-tree)
      (mov-⇲ Po-List)

      (ln> *ptr)
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

  #(println 'make-pair-b-atom)

  (let (Po-Tgt (ref> *ptr)
        Tgt (get Po-Tgt 'any)  
        Po-Pair (get Po-Tgt 'c)
        Pair (get Po-Pair 'any))  
    
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
          (prog
            (ins-line-aft Ln Po-List)
            (put Po-List 'line 'n Po-Cdr)))

      (when (get Po-Pair 'exp) # handle sym
        (put Po-Pair 'line 'n NIL)
        (put Po-List 'line 'p NIL))
                
      (put Po-Cdr 'line (new '(+Line) Po-List))
      
      (put Po-Car 'line (new '(+Line)))
            
      (set-ref> *ptr List 'b)
      
      (upd-tree)
      (mov-⇲ Po-List)
            
      (ln> *ptr Po-List)
      (top> Po-List)
      (draw> *ptr))))            
