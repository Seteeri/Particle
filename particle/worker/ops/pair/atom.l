(de make-pair-a-atom-x ()  

  #{
  
    Replace atom with pair.car = atom
    
      [X]  [X]  [X]  .
       .    .    .
            *
          
    -->

      [X]
       .    
    
            *
      [Y]  [X]  .
            .
      
      [X]
       .      
      
  }#

  #(println 'make-pair-a-atom-x)
    
  (let ((Pair Pt-Pair) (get-ref-pair> *ptr)
        Tgt (car Pair)
        Pt-Tgt (get Pt-Pair 'a))
                  
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
    
    (let (Ls (list Tgt)
          Pt-Ls (gen-point Ls 'x NIL T)
          Ln (c-lns> *ptr)
          Ln-N (get Ln 'line 'n)
          Cdr (cdr Pair)
          Pt-Cdr (if Cdr (lup-pt Cdr) (get Pt-Pair 'b))
          Push-Pair (not (get Pt-Pair 'line)))
    
      (set-lay> Pt-Pair 'y)    
    
      (set> Pt-Pair Ls Pt-Ls)
            
      (if (get Pt-Pair 'line)
          (put @ 'n Pt-Cdr)      
          (ins-line-aft Ln Pt-Pair))
      
      (if (get Pt-Cdr 'line)
          (put @ 'p Pt-Pair)
          (ins-line-aft Pt-Pair Pt-Cdr))
          
      (put Pt-Ls 'line (new '(+Line)))
      
      (pt> *ptr Ls 'a)
      
      (lay-*start)
      (mov-⇲ (c-lns> *ptr))
      
      (when Push-Pair (lns> *ptr Pt-Pair))
      (lns> *ptr Pt-Ls)
      (top> Pt-Ls)
      (draw> *ptr))))    


(de make-pair-a-atom-y ()
  
  #{
  
    Replace atom with pair.car = atom
    
      [X]
       .
    
           *
      [Y]  z
       
       
      [X]
       .
          
    -->

      [X]
       .    
    
            *
      [Y]  [X]  .
            z
      
      [X]
       .      
      
  }#

  (let ((Pair Pt-Pair) (get-ref-pair> *ptr)
        Tgt (car Pair)
        Pt-Tgt (get Pt-Pair 'a))
    
    ~(assert (get Pt-Tgt 'line))
      
    # Delete:
    # * Nil ptr ref otherwise set-ref will attempt uncol on ref
    # * Put NIL otherwise set will del new point
    # * Pop line if necessary
    (put *ptr 'ref)  
    (put Pt-Pair 'a)
    (when (= (c-lns> *ptr) Pt-Tgt) (lns> *ptr))
    (del> Pt-Tgt)
    
    (let (Ls (list Tgt)
          Pt-Ls (gen-point Ls 'x NIL T)
          Pt-Car (get Pt-Ls 'a)
          Pt-Cdr (get Pt-Ls 'b)
          Ln (c-lns> *ptr))
    
      (set-lay> Pt-Ls 'y)
    
      (set> Pt-Pair Ls)

      (put Pt-Ls 'line (new '(+Line)))
      
      (pt> *ptr Ls 'a)

      (lay-*start)
      (mov-⇲ Pt-Ls)

      (lns> *ptr)
      (lns> *ptr Pt-Ls)
      (top> Pt-Ls)
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

  #(println 'make-pair-b-atom)
  
  (let ((Pair Pt-Pair) (get-ref-pair> *ptr)
        Tgt (if (sym? Pair) (val Pair) (cdr Pair))
        Pt-Tgt (get Pt-Pair 'b))

    # Delete:
    # * Nil ptr ref otherwise set-ref will attempt uncol on ref
    # * Put NIL otherwise set will del new point
    # * Pop line if necessary
    (put *ptr 'ref)  
    (put Pt-Pair 'b)
    (when (= (c-lns> *ptr) Pt-Tgt) (lns> *ptr))
    (del> Pt-Tgt)
    
    (let (Ls (list Tgt)
          Pt-Ls (gen-point Ls 'x NIL T)
          Pt-Car (get Pt-Ls 'a)
          Pt-Cdr (get Pt-Ls 'b)
          Ln (c-lns> *ptr))
        
      (set-lay> Pt-Ls 'y)    
    
      (con> Pt-Pair Ls)
      
      (if (get Pt-Ls 'line)
          (put @ 'n Pt-Cdr)
          (prog
            (ins-line-aft Ln Pt-Ls)
            (put Pt-Ls 'line 'n Pt-Cdr)))

      (when (get Pt-Pair 'exp) # handle sym
        (put Pt-Pair 'line 'n NIL)
        (put Pt-Ls 'line 'p NIL))
                
      (put Pt-Cdr 'line (new '(+Line) Pt-Ls))
      
      (put Pt-Car 'line (new '(+Line)))
            
      (pt> *ptr Ls 'b)
      
      (lay-*start)
      (mov-⇲ Pt-Ls)
            
      (lns> *ptr Pt-Ls)
      (top> Pt-Ls)
      (draw> *ptr))))            
