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
    
  (let (Pt-Tgt (if (get *ptr 'pair)
                   (get *ptr 'ref)
                   (lup-pt (get *ptr 'ref)))
        Tgt (get Pt-Tgt 'any)
        Pair (get Pt-Tgt 'c 'any)
        Pt-Pair (lup-pt Pair))
                  
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
      
      (pt> *ptr List 'a)
      
      (upd-tree)
      (mov-⇲ (c-lns> *ptr))
      
      (when Push-Pair (lns> *ptr Pt-Pair))
      (lns> *ptr Pt-List)
      (top> Pt-List)
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

  (let (Pt-Tgt (if (get *ptr 'pair)
                   (get *ptr 'ref)
                   (lup-pt (get *ptr 'ref)))
        Tgt (get Pt-Tgt 'any)
        Pair (get Pt-Tgt 'c 'any)
        Pt-Pair (lup-pt Pair))
    
    ~(assert (get Pt-Tgt 'line))
      
    # Delete:
    # * Nil ptr ref otherwise set-ref will attempt uncol on ref
    # * Put NIL otherwise set will del new point
    # * Pop line if necessary
    (put *ptr 'ref)  
    (put Pt-Pair 'a)
    (when (= (c-lns> *ptr) Pt-Tgt) (lns> *ptr))
    (del> Pt-Tgt)
    
    (let (List (list Tgt)
          Pt-List (gen-point List 'x NIL T)
          Pt-Car (get Pt-List 'a)
          Pt-Cdr (get Pt-List 'b)
          Ln (c-lns> *ptr))
    
      (set-lay> Pt-List 'y)
    
      (set> Pt-Pair List)

      (put Pt-List 'line (new '(+Line)))
      
      (pt> *ptr List 'a)

      (upd-tree)
      (mov-⇲ Pt-List)

      (lns> *ptr)
      (lns> *ptr Pt-List)
      (top> Pt-List)
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

  (let (Pt-Tgt (get *ptr 'ref)
        Tgt (get Pt-Tgt 'any)  
        Pt-Pair (get Pt-Tgt 'c)
        Pair (get Pt-Pair 'any))  
    
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
          (prog
            (ins-line-aft Ln Pt-List)
            (put Pt-List 'line 'n Pt-Cdr)))

      (when (get Pt-Pair 'exp) # handle sym
        (put Pt-Pair 'line 'n NIL)
        (put Pt-List 'line 'p NIL))
                
      (put Pt-Cdr 'line (new '(+Line) Pt-List))
      
      (put Pt-Car 'line (new '(+Line)))
            
      (pt> *ptr List 'b)
      
      (upd-tree)
      (mov-⇲ Pt-List)
            
      (lns> *ptr Pt-List)
      (top> Pt-List)
      (draw> *ptr))))            
