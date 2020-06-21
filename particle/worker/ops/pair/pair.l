#{
(de make-pair-b-pair-y ()

  #{
  
    Replace pair with pair.car=pair

    Later output X-Pair
             
    Case 3:
      
      (1 (2) 3)
      
        [X] 
         .

         * 
        [Y1]  .
        
        [X]  .
         .        
        
      -->
      
      (1 ((2)) 3)
      
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
    
      -->
      
      (1 ((2 3)))
    
        [X]
         .
         
               *
        [Y2]  [Y1]  .
            
              [X]  .
               .      
              
      * Flatten operation
        * Flatten + enclose in pair
      * Ptr ref (2), not ((2))
      * To achieve this requires del/bsp
        * Or cut/copy/paste
         
  }#   
  
  (println 'make-pair-b-pair-y)
  
  (let (Tgt (get *ptr 'ref)
        Po-Tgt (lup-pt Tgt)
        Tgt-Car (car Tgt)
        Po-Tgt-Car (if Tgt-Car
                       (lup-pt @)
                       (get Po-Tgt 'a))
        Cons (cons)
        Po-Cons (gen-point Cons 'x NIL T)
        Po-Car (get Po-Cons 'a)
        Po-Cdr (get Po-Cons 'b))

    (set-lay> Po-Cons 'y)    
  
    (set> Po-Tgt Cons Po-Cons)   
    
    (if Tgt-Car
        (set> Po-Cons Tgt-Car Po-Tgt-Car)
        (put Po-Car 'line (new '(+Line))))
    
    (put Po-Cons 'line (new '(+Line) NIL Po-Cdr))
    (put Po-Cdr 'line (new '(+Line) Po-Cons NIL))
    
    (pt> *ptr Cons 'a)
    
    (upd-tree)
    (mov-⇲ (c-ln> *ptr))
                
    (ln> *ptr Po-Cons)      
    (top> Po-Cons)
    (draw> *ptr)))
}#
          
          
(de make-pair-b-pair ()
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
  
  #(println 'make-pair-b-pair)  
  
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
          # Make line start
          (put Po-Tgt 'line 'p NIL)
          # Only pop if replacing line
          (ln> *ptr)
          # Set current ln to prv line
          (setq Ln (c-ln> *ptr)))
        (prog
          # Modify subsequent line
          (let Nxt (get Ln 'line 'n)
            (put Po-Tgt 'line
              (new '(+Line)
                NIL
                Nxt))
            (when Nxt
              (put Nxt 'line 'p Po-Tgt)))))
    
    (when (is-sym? Ln) (setq Ln NIL))
    
    (put Ln 'line 'n Po-Cons)
    
    (put Po-Cons 'line (new '(+Line) Ln Po-Cdr))
    
    (put Po-Cdr 'line (new '(+Line) Po-Cons NIL))
      
    (pt> *ptr Cons 'b)
    
    (upd-tree)
    (mov-⇲ (c-ln> *ptr))
                
    (ln> *ptr Po-Cons)
    (top> Po-Cons)
    (draw> *ptr)))
          
          
(de make-pair-a-pair ()

  #{
    
    Replace pair with pair.car=pair
    
    Later output X-Pair
              
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
      
  }#
  
  #(println 'make-pair-a-pair)
  
  (let (Tgt (get *ptr 'ref)
        Po-Tgt (lup-pt Tgt)
        Pair (find-back> *ptr (get *ptr 'rel))
        Po-Pair (lup-pt Pair)
        Cons (cons)
        Po-Cons (gen-point Cons 'x NIL T)
        Po-Cdr (get Po-Cons 'b))
  
    (set-lay> Po-Cons 'y)    
  
    (set> Po-Pair Cons Po-Cons)
    (set> Po-Cons Tgt Po-Tgt)
    
    (put Po-Cons 'line (new '(+Line) NIL Po-Cdr))
    (put Po-Cdr 'line (new '(+Line) Po-Cons NIL))
    
    (pt> *ptr Cons 'a)
    
    (upd-tree)
    (mov-⇲ (c-ln> *ptr))
                
    ~(assert (get Po-Pair 'line))
    # Pt to new cons so pop old line, push cons
    (ln> *ptr)
    (ln> *ptr Po-Cons)
    (top> Po-Cons)
    (draw> *ptr)))            
