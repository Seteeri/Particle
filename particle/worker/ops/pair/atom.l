(de mak-pair-x-a (Pt-B Pt-C)  

  #{
  
    Repl atom with pair car = atom
    
    
      [X]  [B]  [X]  .
       .    .    .
            *
            
      or

      [X]  
       .    
      
      [B]
       *
       
      [X]
       .
            
    -->

      [X]
       .    
    
            *
      [B]  [ ]  .
            .
      
      [X]
       .      
       
    convert existing to y
      
  }#

  (println 'mak-pair-x-a)

  (let (B (get Pt-B 'any)
        C (get Pt-C 'any))    

    (put Pt-B 'a)
    (when (= (c-lns> *ptr) Pt-C) (lns> *ptr))
    (del> Pt-C) 
    
    (let (Ls (list C)
          Pt-Ls (gen-point Ls 'x NIL T)
          Pt-Cdr (if (cdr B)
                     (lup-pt @)
                     (get Pt-B 'b))
          Push-B (not (get Pt-B 'line)))
    
      # When convert to y - use alt? then set
      (set-lay> Pt-B 'y)
    
      (set> Pt-B Ls Pt-Ls)
      
      # B poss has line, else create
      (if (get Pt-B 'line)
          (put @ 'n Pt-Cdr)
          (ins-line-aft (c-lns> *ptr) Pt-B))
      
      # same as above
      (if (get Pt-Cdr 'line)
          (put @ 'p Pt-B)
          (ins-line-aft Pt-B Pt-Cdr))
      
      (pt> *ptr Ls 'a)
      
      (lay-*start)
      
      (when Push-B (lns> *ptr Pt-B))
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
          Pt-Ls (gen-point Ls 'x NIL T))
    
      (set-lay> Pt-Ls 'y)
    
      (set> Pt-B Ls Pt-Ls)
      
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
          Pt-Ls (gen-point Ls 'x NIL T))
    
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
