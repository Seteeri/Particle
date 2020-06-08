#{

    [X]  CDR/ATM
    
    CAR/ATM
    
    
    [Y]  CAR/ATM
    
    CDR/ATM
    
    
    X/Y dictate line changes
}#

(de make-pt-pair (Ptr)

  (cond
  
    ((or (a-nil?> *ptr)
         (a-atom?> *ptr))
      (println 'make-pt-pair "A ATOM/NIL")
      (repl-pair-car-atom-y-pair))
      
    ((or (b-nil?> *ptr)
         (b-atom?> *ptr))
      (println 'make-pt-pair "B ATOM/NIL")
      (repl-pair-cdr-atom-y-pair))
      
    # PAIR

    ((a-pair?> *ptr)
      (println 'make-pt-pair "A PAIR")
      # create lst around ptr
      (repl-pair-car-pair-y-pair))
      
    ((b-pair?> *ptr)
      (println 'make-pt-pair "B PAIR")
      (if (lay-x?> *ptr)
          (repl-pair-cdr-pair-y-pair)
          (repl-pair-cdr-pair-y-pair-2)))
      
    (T
      # root
      (println 'make-pt-pair "*ptr uninitialized"))))

      
(de repl-pair-car-atom-y-pair ()
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
      
      (println repl-pair-car-nil "Pair/Tgt: "
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
  

(de repl-pair-cdr-atom-y-pair ()

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
    
  }#   

  (let (Po-Tgt (if (cell> *ptr)
                   (ref> *ptr)
                   (lup-pt (ref> *ptr)))
        Pair (if (cell> *ptr)
                 @
                 (find-back> *ptr (rel> *ptr)))
        Po-Pair (lup-pt Pair))
    
    (println 'repl-pair-cdr-atom "Pair/Cdr: " 
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
            
      # Push new list
      (ln> *ptr Po-List)
      
      (top> Po-List)
      (draw> *ptr))))

      
############################


(de repl-pair-car-pair-y-pair ()

  #{
    
    Replace pair with pair.car=pair
    
    Later output X-Pair
    
    Handle both X/Y-Pair
          
    Case 1:
    
        [X]
         .
         
               *
        [Y1]  [X1]  .
               .
              
        [X]  .
         .
        
      -->
            
        [X]
         .
         
               *
        [Y1]  [Y2]  [X1]  .
                     .
               .
            
        [X]  .
         .    
    
    Case 2:
    
        
        [X]
         .
         
               *
        [Y1]  [Y2]  .
               .
              
        [X]  .
         .
        
      -->
      
            
        [X]
         .
         
               *
        [Y1]  [Y3]  [Y2]  .
                     .
               .
            
        [X]  .
         .        
    
    * Repeating this command will create nested lists    
    
    * Pt to new pair
    
  }#
  
  
  (let (Tgt (ref> *ptr)
        Po-Tgt (lup-pt Tgt)
        Pair (find-back> *ptr (rel> *ptr))
        Po-Pair (lup-pt Pair))
        
    (println 'repl-pair-car-pair-y-pair "Pair/Tgt: "
      Pair Tgt)
    
    # Produce empty Pair
    (let (Cons (cons)
          Po-Cons (gen-point Cons 'x NIL T)
          Po-Cdr (get Po-Cons 'b))
    
      (set-lay> Po-Cons 'y)    
    
      (set> Po-Pair Cons Po-Cons)
      (set> Po-Cons Tgt Po-Tgt)
      
      (put Po-Cons 'line (new '(+Line) NIL Po-Cdr))
      (put Po-Cdr 'line (new '(+Line) Po-Cons NIL))
      
      (upd-tree)
      (mov-⇲ (c-ln> *ptr))
                  
      # Pt to list
      (set-ref> *ptr Cons 'a)
      
      # Assumes Pair always has line
      ~(assert (get Po-Pair 'line))
      
      # Since pt to cons
      # Pop old, push cons
      (ln> *ptr)
      (ln> *ptr Po-Cons)
      
      (top> Po-Cons)
      (draw> *ptr))))
  

(de repl-pair-cdr-pair-y-pair ()

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
        [Y]  [X2]  .
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
  
  (let (Pair (ref> *ptr)
        Po-Pair (lup-pt Pair)
        Tgt (car Pair)
        Po-Tgt (if (get Po-Pair 'a)
                   @
                   (lup-pt Tgt)))
    
    (println 'repl-pair-cdr-pair-y-pair "Pair/Tgt: "
      Pair Tgt)
  
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
            
      # Push cell and car
      (when Push-Pair
        (ln> *ptr Po-Pair))
      (ln> *ptr Po-List)
      
      (top> Po-List)
      (draw> *ptr))))

      
(de repl-pair-cdr-pair-y-pair-2 ()

  #{
  
    Replace pair with pair.car=pair

    Later output X-Pair
             
    Case 3:
      
        [X] 
         .

         * 
        [Y1]  .
        
        [X]  .
         .        
        
      -->
               
        [X]
         .
         
               *
        [Y1]  [Y2]  .
               .
            
        [X]  .
         .       
         
      * Create new list
      * Conn car
         
    * Con Y2 cdr -> Y1 ?
    
        [X]
         .
         
               *
        [Y2]  [Y1]  .
            
              [X]  .
               .      
              
      -> make-list fn!
         
  }#   
  
  (let (Tgt (ref> *ptr)
        Po-Tgt (lup-pt Tgt)
        Tgt-Car (car Tgt)
        Po-Tgt-Car (lup-pt Tgt-Car))
        
    (println 'repl-pair-cdr-pair-y-pair-2 "Pair/Tgt: "
      Tgt Tgt-Car)
    
    # Produce empty Pair
    (let (Cons (cons)
          Po-Cons (gen-point Cons 'x NIL T)
          Po-Cdr (get Po-Cons 'b))
    
      (set-lay> Po-Cons 'y)    
    
      (set> Po-Tgt Cons Po-Cons)
      (set> Po-Cons Tgt-Car Po-Tgt-Car)
      
      (put Po-Cons 'line (new '(+Line) NIL Po-Cdr))
      (put Po-Cdr 'line (new '(+Line) Po-Cons NIL))
      
      (upd-tree)
      (mov-⇲ (c-ln> *ptr))
                  
      # Pt to list
      (set-ref> *ptr Cons 'a)
            
      # Since pt to cons
      (ln> *ptr Po-Cons)
            
      (top> Po-Cons)
      (draw> *ptr))))
