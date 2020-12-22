(de make-pair-b-pair (Pt-B Pt-C)
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
  
  (println 'make-pair-b-pair)  
  
  (let (C (get Pt-C 'any)
        Cons (cons)
        Pt-Cons (gen-point Cons 'x NIL T)
        Pt-Cdr (get Pt-Cons 'b)
        Ln (c-lns> *ptr))
    
    (set-lay> Pt-Cons 'y)
  
    (con> Pt-B Cons Pt-Cons)
    (set> Pt-Cons C Pt-C T)
    
    # Still pt to og pair
    # must upd any
    (with *ptr
      (=: any Cons)
      (=: reg 'a))
    
    (if (get Pt-C 'line)
        (prog
          # Make line start
          (put Pt-C 'line 'p NIL)
          # Only pop if replacing line
          (lns> *ptr)
          # Set current ln to prv line
          (setq Ln (c-lns> *ptr)))
        (prog
          # Modify subsequent line
          (let Nxt (get Ln 'line 'n)
            (put Pt-C 'line
              (new '(+Line)
                NIL
                Nxt))
            (when Nxt
              (put Nxt 'line 'p Pt-C)))))
    
    (when (is-sym? Ln) (setq Ln NIL))
    
    (put Ln 'line 'n Pt-Cons)
    (put Pt-Cons 'line (new '(+Line) Ln Pt-Cdr))
    (put Pt-Cdr 'line (new '(+Line) Pt-Cons NIL))
    
    (lay-*start)
                
    (lns> *ptr Pt-Cons)
    (lns> *ptr Pt-C)
    
    (top-ref> *ptr)
    (draw> *ptr)))
          
          
(de make-pair-a-pair (Pt-B Pt-C)

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
  
  (println 'make-pair-a-pair)
  
  (let (C (get Pt-C 'any)
        Cons (cons)
        Pt-Cons (gen-point Cons 'x NIL T)
        Pt-Cdr (get Pt-Cons 'b))
  
    (set-lay> Pt-Cons 'y)    
  
    (set> Pt-B Cons Pt-Cons)
    (set> Pt-Cons C Pt-C T)
    
    (put Pt-Cons 'line (new '(+Line) NIL Pt-Cdr))
    (put Pt-Cdr 'line (new '(+Line) Pt-Cons NIL))
    
    # Still pt to og pair
    # must upd any
    (with *ptr
      (=: any Cons)
      (=: reg 'a))
    
    (lay-*start)
                
    ~(assert (get Pt-C 'line))
    
    # Pt to new cons so pop old line, push cons
    (lns> *ptr)
    (lns> *ptr Pt-Cons)
    (lns> *ptr Pt-C)
    
    (top-ref> *ptr)
    (draw> *ptr)))

    
      
# PAIR      
      
(de mak-pair-xa-x (Pt-B Pt-C) (make-pair-a-pair Pt-B Pt-C))
(de mak-pair-xb-x (Pt-B Pt-C) (make-pair-b-pair Pt-B Pt-C))

(de mak-pair-xa-y (Pt-B Pt-C) (make-pair-a-pair Pt-B Pt-C))
(de mak-pair-xb-y (Pt-B Pt-C) (make-pair-b-pair Pt-B Pt-C))


(de mak-pair-ya-x (Pt-B Pt-C) (make-pair-a-pair Pt-B Pt-C))
(de mak-pair-yb-x (Pt-B Pt-C) (make-pair-b-pair Pt-B Pt-C))

(de mak-pair-ya-y (Pt-B Pt-C) (make-pair-a-pair Pt-B Pt-C))
(de mak-pair-yb-y (Pt-B Pt-C) (make-pair-b-pair Pt-B Pt-C))
