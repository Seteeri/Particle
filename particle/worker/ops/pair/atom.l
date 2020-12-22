(de mak-pair-x-a (Pt-B Pt-C)  

  #{
  
    Replace atom with pair car = atom
    
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

  (println 'mak-pair-x-a)

  (let (B (get Pt-B 'any)
        C (get Pt-C 'any))    

    (put Pt-B 'a)
    (when (= (c-lns> *ptr) Pt-C) (lns> *ptr))
    (del> Pt-C)    
    
    (let (Ls (list C)
          Pt-Ls (gen-point Ls 'x NIL T)
          Ln (c-lns> *ptr)
          Ln-N (get Ln 'line 'n)
          Cdr (cdr B)
          Pt-Cdr (if Cdr (lup-pt Cdr) (get Pt-B 'b))
          Push-Pair (not (get Pt-B 'line)))
    
      (set-lay> Pt-B 'y)    
    
      (set> Pt-B Ls Pt-Ls)
            
      (if (get Pt-B 'line)
          (put @ 'n Pt-Cdr)      
          (ins-line-aft Ln Pt-B))
      
      (if (get Pt-Cdr 'line)
          (put @ 'p Pt-B)
          (ins-line-aft Pt-B Pt-Cdr))
          
      (put Pt-Ls 'line (new '(+Line)))
      
      (pt> *ptr Ls 'a)
      
      (lay-*start)
      
      (when Push-Pair (lns> *ptr Pt-B))
      (lns> *ptr Pt-Ls)
      (top-ref> *ptr)
      (draw> *ptr))))    


(de mak-pair-y-a (Pt-B Pt-C)
  
  #{
  
    Replace atom with pair car = atom
    
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
  
  (println 'mak-pair-y-a)

  (let (B (get Pt-B 'any)
        C (get Pt-C 'any))
 
    (put Pt-B 'b)
    (when (= (c-lns> *ptr) Pt-C) (lns> *ptr))
    (del> Pt-C)
    
    (let (Ls (list C)
          Pt-Ls (gen-point Ls 'x NIL T)
          Pt-Car (get Pt-Ls 'a)
          Pt-Cdr (get Pt-Ls 'b)
          Ln (c-lns> *ptr))
    
      (set-lay> Pt-Ls 'y)
    
      (set> Pt-B Ls)

      (put Pt-Ls 'line (new '(+Line)))
      
      (pt> *ptr Ls 'a)

      (lay-*start)

      (lns> *ptr)
      (lns> *ptr Pt-Ls)
      (top-ref> *ptr)
      (draw> *ptr))))   
      

(de mak-pair-x/y-b (Pt-B Pt-C)

  #{
  
    Replace atom with pair cdr = atom

                    * 
        [X1]  [X2]  .
         .     .
               
              
    ->
                          *
        [X1]  [X2]  [X3]  .
         .     .     .


  }#   

  (println 'mak-pair-x/y-b)
  
  (let (B (get Pt-B 'any)
        C (get Pt-C 'any))
 
    (put Pt-B 'b)
    (when (= (c-lns> *ptr) Pt-C) (lns> *ptr))
    (del> Pt-C)
    
    (let (Ls (list C)
          Pt-Ls (gen-point Ls 'x NIL T)
          Pt-Car (get Pt-Ls 'a)
          Pt-Cdr (get Pt-Ls 'b)
          Ln (c-lns> *ptr))
    
      (con> Pt-B Ls)
      
      # txfer ln if atom has it
      # handles y case
      (when (get Pt-C 'line)
        (mov-line Pt-C Pt-Ls)
        (lns> *ptr Pt-Ls))
            
      (pt> *ptr Ls 'b)
      
      (lay-*start)
            
      (top-ref> *ptr)
      (draw> *ptr))))
      
# ATOM

(de mak-pair-x-b (Pt-B Pt-C) (mak-pair-x/y-b Pt-B Pt-C))
(de mak-pair-y-b (Pt-B Pt-C) (mak-pair-x/y-b Pt-B Pt-C))


##################################      
##################################

      
(de mak-pair-x/y-b-old (Pt-B Pt-C)

  #{
  
    Replace atom with pair cdr = atom

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

  }#   

  (println 'mak-pair-x/y-b)
  
  (let (B (get Pt-B 'any)
        C (get Pt-C 'any))

    # Delete:
    # * Nil ptr ref otherwise set-ref will attempt uncol on ref
    # * Put NIL otherwise set will del new point
    # * Pop line if necessary
    (put *ptr 'ref)  
    (put Pt-B 'b)
    (when (= (c-lns> *ptr) Pt-C) (lns> *ptr))
    (del> Pt-C)
    
    (let (Ls (list C)
          Pt-Ls (gen-point Ls 'x NIL T)
          Pt-Car (get Pt-Ls 'a)
          Pt-Cdr (get Pt-Ls 'b)
          Ln (c-lns> *ptr))
        
      (set-lay> Pt-Ls 'y)
    
      (con> Pt-B Ls)
      
      (if (get Pt-Ls 'line)
          (put @ 'n Pt-Cdr)
          (prog
            (ins-line-aft Ln Pt-Ls)
            (put Pt-Ls 'line 'n Pt-Cdr)))
      (put Pt-Cdr 'line (new '(+Line) Pt-Ls))
      (put Pt-Car 'line (new '(+Line)))
            
      (pt> *ptr Ls 'b)
      
      (lay-*start)
            
      (lns> *ptr Pt-Ls)
      (top-ref> *ptr)
      (draw> *ptr))))            
