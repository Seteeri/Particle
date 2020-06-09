(de make-list ()
    
  (cond

    ((a-pair?> *ptr)
      (println 'make-list "A PAIR")
      (repl-a-pair))
      
    ((b-pair?> *ptr)
      (println 'make-list "B PAIR")
      (if (lay-x?> *ptr)
          (make-list-b-pair->x)
          (repl-a-pair)))))


(de make-list-b-pair->x ()
  #{
  
    Make list with list  
    
    Handle Y-case?

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

    Case 4: Yes line, no p, bel Y-Pair
    
             *
      [Y1]  [X1]  ...
             .
      
       .    

      ->
      
                   *
      [Y1]  [Y2]  [X1]  ...
                   .
             .
       .    
      
      SAME AS PAIR FN
      
  }#
  
  (println 'make-list)  
  
  (let (Tgt (ref> *ptr)
        Po-Tgt (lup-pt Tgt)
        Pair (find-back> *ptr (rel> *ptr))
        Po-Pair (lup-pt Pair)
        Cons (cons)
        Po-Cons (gen-point Cons 'x NIL T)
        Po-Cdr (get Po-Cons 'b)
        Ln (c-ln> *ptr))
  
    ~(assert (not (lay-y?> Po-Pair)))
  
    (set-lay> Po-Cons 'y)    
  
    (con> Po-Pair Cons Po-Cons)
    (set> Po-Cons Tgt Po-Tgt)
                
    (if (get Po-Tgt 'line)
        (if (get @ 'p)
            (prog
              (pop-line Po-Tgt)
              (setq Ln (get Po-Tgt 'line 'p))
              (put Po-Tgt 'line 'p))
            (prog
              (setq Ln (get Po-Tgt 'line 'p))))
        (prog
          (put Po-Tgt 'line (new '(+Line)))))
    
    (ins-line-aft Ln Po-Cons)
    
    (ins-line-aft Po-Cons Po-Cdr)    
    
    (upd-tree)
    (mov-⇲ (c-ln> *ptr))
                
    # Pt to list
    (set-ref> *ptr Cons 'a)
    ~(assert (get Po-Pair 'line))
    # Pt to new cons so pop old line, push cons
    (ln> *ptr)
    (ln> *ptr Po-Cons)
    (top> Po-Cons)
    (draw> *ptr)))
