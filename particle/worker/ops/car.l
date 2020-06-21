#{

    [X]  CDR/ATM
    
    CAR/ATM
    
    
    [Y]  CAR/ATM
    
    CDR/ATM
    
    
    X/Y dictate line changes
}#    

(de make-pair-car ()

  # Make these methods?
  
  # car = a
  # cdr = b
  # atom = t
  # pair = p
  
  (cond
  
    #{
    ((or (a-nil?> *ptr)
         (a-atom?> *ptr))
      (println 'make-pt-pair "A ATOM/NIL")
      # Same as swap layout
      (repl-a-atom->y-pair))
      
    ((or (b-nil?> *ptr)
         (b-atom?> *ptr))
      (println 'make-pt-pair "B ATOM/NIL")
      (repl-b-atom->y-pair))
    }#
    
    ((a-pair?> *ptr)
      (println 'make-pt-pair "A PAIR")
      # create lst around ptr
      (repl-a-pair->y-pair))
      
    ((b-pair?> *ptr)
      (println 'make-pt-pair "B PAIR")
      (if (lay-x?> *ptr)
          (repl-b-pair-x->y-pair)
          (repl-b-pair-y->y-pair)))))

      
############################


(de repl-a-pair->y-pair ()

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
  
  (println 'repl-a-pair->y-pair)
  
  (let (Tgt (get *ptr 'ref)
        Po-Tgt (lup-pt Tgt)
        Pair (find-back> *ptr (get *ptr 'reg))
        Po-Pair (lup-pt Pair)
        Cons (cons)
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
    (pt> *ptr Cons 'a)
    ~(assert (get Po-Pair 'line))
    # Pt to new cons so pop old line, push cons
    (ln> *ptr)
    (ln> *ptr Po-Cons)
    (top> Po-Cons)
    (draw> *ptr)))
  

(de repl-b-pair-x->y-pair ()

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
  
  (println 'repl-b-pair-x->y-pair)
  
  (let (Pair (get *ptr 'ref)
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
            
      (pt> *ptr List 'a)
            
      (when Push-Pair (ln> *ptr Po-Pair))
      (ln> *ptr Po-List)
      (top> Po-List)
      (draw> *ptr))))

      
(de repl-b-pair-y->y-pair ()

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
  
  (println 'repl-b-pair-y->y-pair)
  
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
    
    (upd-tree)
    (mov-⇲ (c-ln> *ptr))
                
    (pt> *ptr Cons 'a)
    (ln> *ptr Po-Cons)      
    (top> Po-Cons)
    (draw> *ptr)))
