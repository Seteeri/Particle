(de make-pair-cdr ()
  # This function outputs a Y-Pair
  # with list > 1 and atoms
  
  (cond

    ((or (a-nil?> *ptr)
         (a-atom?> *ptr))
      (println 'make-list "A ATOM/NIL")
      (repl-a-atom->list))
      
    ((or (b-nil?> *ptr)
         (b-atom?> *ptr))
      (println 'make-list "B ATOM/NIL")
      (repl-b-atom->list))  
  
    ((a-pair?> *ptr)
      (println 'make-list "A PAIR")
      (repl-a-pair->y-pair))
      
    ((b-pair?> *ptr)
      (println 'make-list "B PAIR")
      (if (lay-x?> *ptr)
          (repl-b-pair->list)
          (repl-a-pair->y-pair)))))


(de repl-a-atom->list ()
  
  (sup> *ptr)
  (repl-b-pair-x->y-pair))

        
(de repl-b-atom->list ()

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

  (let (Po-Tgt (if (get *ptr 'pair)
                   (get *ptr 'ref)
                   (lup-pt (get *ptr 'ref)))
        Pair (if (get *ptr 'pair)
                 @
                 (find-back> *ptr (get *ptr 'rel)))
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
          
          
(de repl-b-pair->list ()
  #{
  
    Make list with list  
    
    Case 1: No line
    
             *
      [X1]  [X2]  [X3]  [X4]  .
       .     .     .     .

      ->
      
      [X1]
            *
      [Y]  [X2]  ...
            .
      
      .

       
    Case 2: Yes line, yes p
    
      [X1]
    
       *
      [X2]  [X3]  [X4]  .
       .     .     .

      ->
      
      [X1]
            *
      [Y]  [X2]  ...
            .    
      
      .
      
      
    Case 3: Yes line, no p

       *
      [X1]  [X2]  [X3]  [X4]  .
       .     .     .     .

      ->
      
            *
      [Y]  [X1]  ...
            .
      
      .  
    
    Case 4: Yes line, yes p
    
      [Y1]
    
       *
      [X2]  [X3]  [X4]  .
       .     .     .

      ->
      
      [Y1]
            *
      [Y2]  [X2]  ...
            .    
      
      .    
    
  }#
  
  (println 'make-list)  
  
  (let (Tgt (get *ptr 'ref)
        Po-Tgt (lup-pt Tgt)
        Pair (find-back> *ptr (get *ptr 'rel))
        Po-Pair (lup-pt Pair)
        Cons (cons)
        Po-Cons (gen-point Cons 'x NIL T)
        Po-Cdr (get Po-Cons 'b)
        Ln (c-ln> *ptr))
    
    (set-lay> Po-Cons 'y)    
  
    (con> Po-Pair Cons Po-Cons)
    (set> Po-Cons Tgt Po-Tgt)
                
    (if (get Po-Tgt 'line)
        (prog
          (if (get @ 'p)
              (prog
                (pop-line Po-Tgt)
                (setq Ln (get Po-Tgt 'line 'p))
                (put Po-Tgt 'line 'p))
              (prog
                (setq Ln (get Po-Tgt 'line 'p))))
          # Only pop if replacing line
          (ln> *ptr))
        (prog
          (put Po-Tgt 'line (new '(+Line)))))
    
    (ins-line-aft Ln Po-Cons)
    
    (ins-line-aft Po-Cons Po-Cdr)    
    
    (upd-tree)
    (mov-⇲ (c-ln> *ptr))
                
    # Pt to list
    (set-ref> *ptr Cons 'a)
    (ln> *ptr Po-Cons)
    (top> Po-Cons)
    (draw> *ptr)))
