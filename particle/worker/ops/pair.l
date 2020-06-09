(de make-pair ()

  # Make these methods?
  
  # car = a
  # cdr = b
  # atom = t
  # pair = p
  
  # 2 Factors:
  # * Car/Cdr
  # * Pair/Atom
  
  (cond
  
    ((a-atom?> *ptr)
      (println 'make-pair "A ATOM/NIL")
      (make-pair-a-atom))
      
    ((b-atom?> *ptr)
      (println 'make-pair "B ATOM/NIL")
      (make-pair-b-atom))
    
    ((a-pair?> *ptr)
      (println 'make-pair "A PAIR")
      (make-pair-a-pair))
      
    ((b-pair?> *ptr)
      (println 'make-pair "B PAIR")
      (if (lay-x?> *ptr)
          (make-pair-b-pair-x)
          (make-pair-b-pair-y)))))
          

(de make-pair-b-pair-y ()

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
  
  (println 'make-pair-b-pair-y)
  
  (let (Tgt (ref> *ptr)
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
                
    (set-ref> *ptr Cons 'a)
    (ln> *ptr Po-Cons)      
    (top> Po-Cons)
    (draw> *ptr)))
          
          
(de make-pair-b-pair-x ()
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
  
  (println 'make-pair-b-pair-x)  
  
  # Handle when Cdr has line
  
  (let (Tgt (ref> *ptr)
        Po-Tgt (lup-pt Tgt)
        Pair (find-back> *ptr (rel> *ptr))
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
          # Modify subsequent line
          (let Nxt (get Ln 'line 'n)
            (put Po-Tgt 'line
              (new '(+Line)
                NIL
                Nxt))
            (when Nxt
              (put Nxt 'line 'p Po-Tgt)))))
    
    (put Po-Cons 'line (new '(+Line) Ln Po-Cdr))
    
    (put Po-Cdr 'line (new '(+Line) Po-Cons NIL))
    
    (println "DONE")
    
    (upd-tree)
    (mov-⇲ (c-ln> *ptr))
                
    # Pt to list
    (set-ref> *ptr Cons 'a)
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
  
  (println 'make-pair-a-pair)
  
  (let (Tgt (ref> *ptr)
        Po-Tgt (lup-pt Tgt)
        Pair (find-back> *ptr (rel> *ptr))
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
    (set-ref> *ptr Cons 'a)
    ~(assert (get Po-Pair 'line))
    # Pt to new cons so pop old line, push cons
    (ln> *ptr)
    (ln> *ptr Po-Cons)
    (top> Po-Cons)
    (draw> *ptr)))            
          
          
#########################

(de make-pair-a-atom ()
  
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
    
  (println 'make-pair-a-atom)
  
  (sup> *ptr)  
  
  (let (Pair (ref> *ptr)
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
            
      (set-ref> *ptr List 'a)
            
      (when Push-Pair (ln> *ptr Po-Pair))
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

  (println 'make-pair-b-atom)
  
  (let (Po-Tgt (if (cell> *ptr)
                   (ref> *ptr)
                   (lup-pt (ref> *ptr)))
        Pair (if (cell> *ptr)
                 @
                 (find-back> *ptr (rel> *ptr)))
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
          
          
      
#{

  Given List:

    |
    V
    (a b c d)

    
    [X]  [X]  [X]  [X]  .
    a    b    c    d

  
  2 Base Cmds:


  ((a) b c d)

    [Y]  [X]
          a

    [X]  [X]  [X]  .
    b    c    d

    * cmd-make-pair-car
      -> Applies to both pairs and atoms
      -> Same fn for atoms as cmd-make-list


  ((a b c d))

    [Y]  [X]  [X]  [X]  [X]  .
          a    b    c    d
          
    NIL  

    * cmd-make-pair-cdr
    
  Given Atom:

    (a . b)
    ^
    |
    
    [X] b
    a
    
    
  ((a) . b)

    [Y] [X]
        a
    b
    
    * mov to car
    * make-pair
    
    
  (a . (b))

    [X]
    a
    
    [Y] [X]
        b
        
    * mov to cdr
    * make-pair
}#
