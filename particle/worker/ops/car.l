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
        Pt-Tgt (lup-pt Tgt)
        Pair (find-back> *ptr (get *ptr 'reg))
        Pt-Pair (lup-pt Pair)
        Cons (cons)
        Pt-Cons (gen-point Cons 'x NIL T)
        Pt-Cdr (get Pt-Cons 'b))
  
    (set-lay> Pt-Cons 'y)    
  
    (set> Pt-Pair Cons Pt-Cons)
    (set> Pt-Cons Tgt Pt-Tgt)
    
    (put Pt-Cons 'line (new '(+Line) NIL Pt-Cdr))
    (put Pt-Cdr 'line (new '(+Line) Pt-Cons NIL))
    
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
                
    # Pt to list
    (pt> *ptr Cons 'a)
    ~(assert (get Pt-Pair 'line))
    # Pt to new cons so pop old line, push cons
    (lns> *ptr)
    (lns> *ptr Pt-Cons)
    (top> Pt-Cons)
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
        Pt-Pair (lup-pt Pair)
        Tgt (car Pair)
        Pt-Tgt (if (get Pt-Pair 'a)
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
    (put Pt-Pair 'a)
    (when (= (c-lns> *ptr) Pt-Tgt) (lns> *ptr))
    (del> Pt-Tgt)    
    
    (let (List (list Tgt)
          Pt-List (gen-point List 'x NIL T)
          Ln (c-lns> *ptr)
          Ln-N (get Ln 'line 'n)
          Cdr (cdr Pair)
          Pt-Cdr (if Cdr (lup-pt Cdr) (get Pt-Pair 'b))
          Push-Pair (not (get Pt-Pair 'line)))
    
      (set-lay> Pt-Pair 'y)    
    
      (set> Pt-Pair List Pt-List)
            
      (if (get Pt-Pair 'line)
          (put @ 'n Pt-Cdr)      
          (ins-line-aft Ln Pt-Pair))
      
      (if (get Pt-Cdr 'line)
          (put @ 'p Pt-Pair)
          (ins-line-aft Pt-Pair Pt-Cdr))
          
      (put Pt-List 'line (new '(+Line)))
      
      (lay-*start)
      (mov-⇲ (c-lns> *ptr))
            
      (pt> *ptr List 'a)
            
      (when Push-Pair (lns> *ptr Pt-Pair))
      (lns> *ptr Pt-List)
      (top> Pt-List)
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
        Pt-Tgt (lup-pt Tgt)
        Tgt-Car (car Tgt)
        Pt-Tgt-Car (if Tgt-Car
                       (lup-pt @)
                       (get Pt-Tgt 'a))
        Cons (cons)
        Pt-Cons (gen-point Cons 'x NIL T)
        Pt-Car (get Pt-Cons 'a)
        Pt-Cdr (get Pt-Cons 'b))

    (set-lay> Pt-Cons 'y)    
  
    (set> Pt-Tgt Cons Pt-Cons)   
    
    (if Tgt-Car
        (set> Pt-Cons Tgt-Car Pt-Tgt-Car)
        (put Pt-Car 'line (new '(+Line))))
    
    (put Pt-Cons 'line (new '(+Line) NIL Pt-Cdr))
    (put Pt-Cdr 'line (new '(+Line) Pt-Cons NIL))
    
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
                
    (pt> *ptr Cons 'a)
    (lns> *ptr Pt-Cons)      
    (top> Pt-Cons)
    (draw> *ptr)))
