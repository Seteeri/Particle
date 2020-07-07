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
        Pt-Tgt (lup-pt Tgt)
        Pair (find-back-ref> *ptr (get *ptr 'reg))
        Pt-Pair (lup-pt Pair)
        Cons (cons)
        Pt-Cons (gen-point Cons 'x NIL T)
        Pt-Cdr (get Pt-Cons 'b)
        Ln (c-lns> *ptr))
    
    (set-lay> Pt-Cons 'y)    
  
    (con> Pt-Pair Cons Pt-Cons)
    (set> Pt-Cons Tgt Pt-Tgt T)
    
    (if (get Pt-Tgt 'line)
        (prog
          # Make line start
          (put Pt-Tgt 'line 'p NIL)
          # Only pop if replacing line
          (lns> *ptr)
          # Set current ln to prv line
          (setq Ln (c-lns> *ptr)))
        (prog
          # Modify subsequent line
          (let Nxt (get Ln 'line 'n)
            (put Pt-Tgt 'line
              (new '(+Line)
                NIL
                Nxt))
            (when Nxt
              (put Nxt 'line 'p Pt-Tgt)))))
    
    (when (is-sym? Ln) (setq Ln NIL))
    
    (put Ln 'line 'n Pt-Cons)
    
    (put Pt-Cons 'line (new '(+Line) Ln Pt-Cdr))
    
    (put Pt-Cdr 'line (new '(+Line) Pt-Cons NIL))
      
    (pt> *ptr Cons 'b)
    
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
                
    (lns> *ptr Pt-Cons)
    (top> Pt-Cons)
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
        Pt-Tgt (lup-pt Tgt)
        Pair (find-back-ref> *ptr (get *ptr 'reg))
        Pt-Pair (lup-pt Pair)
        Cons (cons)
        Pt-Cons (gen-point Cons 'x NIL T)
        Pt-Cdr (get Pt-Cons 'b))
  
    (set-lay> Pt-Cons 'y)    
  
    (set> Pt-Pair Cons Pt-Cons)
    (set> Pt-Cons Tgt Pt-Tgt T)
    
    (put Pt-Cons 'line (new '(+Line) NIL Pt-Cdr))
    (put Pt-Cdr 'line (new '(+Line) Pt-Cons NIL))
    
    (pt> *ptr Cons 'a)
    
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
                
    ~(assert (get Pt-Pair 'line))
    # Pt to new cons so pop old line, push cons
    (lns> *ptr)
    (lns> *ptr Pt-Cons)
    (top> Pt-Cons)
    (draw> *ptr)))            
