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

  (let (Pt-Tgt (if (get *ptr 'pair)
                   (get *ptr 'ref)
                   (lup-pt (get *ptr 'ref)))
        Pair (if (get *ptr 'pair)
                 @
                 (find-back> *ptr (get *ptr 'reg)))
        Pt-Pair (lup-pt Pair))
    
    (println 'repl-b-atom "Pair/Cdr: " 
      Pair (get Pt-Pair 'b) Pt-Tgt)
  
    # Delete:
    # * Nil ptr ref otherwise set-ref will attempt uncol on ref
    # * Put NIL otherwise set will del new point
    # * Pop line if necessary
    (put *ptr 'ref)  
    (put Pt-Pair 'b)
    (when (= (c-lns> *ptr) Pt-Tgt) (lns> *ptr))
    (del> Pt-Tgt)
    
    (let (List (list Tgt)
          Pt-List (gen-point List 'x NIL T)
          Pt-Car (get Pt-List 'a)
          Pt-Cdr (get Pt-List 'b)
          Ln (c-lns> *ptr))
    
      (set-lay> Pt-List 'y)    
    
      (con> Pt-Pair List)
            
      (if (get Pt-List 'line)
          (put @ 'n Pt-Cdr)
          (ins-line-aft Ln Pt-List))
          
      (if (get Pt-Cdr 'line)
          (put @ 'p Pt-List)
          (ins-line-aft Pt-List Pt-Cdr))
      
      (put Pt-Car 'line (new '(+Line)))
      
      (upd-tree)
      (mov-⇲ Ln)
            
      (pt> *ptr List 'b)            
      (lns> *ptr Pt-List)
      (top> Pt-List)
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
        Pt-Tgt (lup-pt Tgt)
        Pair (find-back> *ptr (get *ptr 'reg))
        Pt-Pair (lup-pt Pair)
        Cons (cons)
        Pt-Cons (gen-point Cons 'x NIL T)
        Pt-Cdr (get Pt-Cons 'b)
        Ln (c-lns> *ptr))
    
    (set-lay> Pt-Cons 'y)    
  
    (con> Pt-Pair Cons Pt-Cons)
    (set> Pt-Cons Tgt Pt-Tgt)
                
    (if (get Pt-Tgt 'line)
        (prog
          (if (get @ 'p)
              (prog
                (pop-line Pt-Tgt)
                (setq Ln (get Pt-Tgt 'line 'p))
                (put Pt-Tgt 'line 'p))
              (prog
                (setq Ln (get Pt-Tgt 'line 'p))))
          # Only pop if replacing line
          (lns> *ptr))
        (prog
          (put Pt-Tgt 'line (new '(+Line)))))
    
    (ins-line-aft Ln Pt-Cons)
    
    (ins-line-aft Pt-Cons Pt-Cdr)    
    
    (upd-tree)
    (mov-⇲ (c-lns> *ptr))
                
    # Pt to list
    (pt> *ptr Cons 'a)
    (lns> *ptr Pt-Cons)
    (top> Pt-Cons)
    (draw> *ptr)))
