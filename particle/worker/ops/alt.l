# Make these +Pair methods?


(de alt-lay-y ()
  #{
  
               *
        [X1]  [X2]  [X3] .
         .     .     .
              
    ->
               
        [X1]
         .  

         *
        [Y]  .

        
        [X3] .
         .
            
  }#
  
  (println 'alt-lay-y)
  
  # Calling cmd on atom does nothing
  # Poss add opt
  
  (let (Pair (ref> *ptr)
        Po-Pair (lup-pt Pair))
    
    (println 'alt-lay-y "Pair/Tgt: "
      Pair Po-Pair)
      
    (let (Ln (c-ln> *ptr)
          Po-Car (if (car Pair) (lup-pt @) (get Po-Pair 'a))
          Po-Cdr (if (cdr Pair) (lup-pt @) (get Po-Pair 'b)))
      
      (set-lay> Po-Pair 'y)
      
      (if (get Po-Pair 'line)
          (put @ 'n Po-Cdr)      
          (ins-line-aft Ln Po-Pair))
      
      (if (get Po-Cdr 'line)
          (put @ 'p Po-Pair)
          (ins-line-aft Po-Pair Po-Cdr))
          
      (put Po-Car 'line (new '(+Line)))

      (upd-tree)
      (mov-⇲ Po-Pair)

      (unless (= Ln Po-Pair)
        (ln> *ptr Po-Pair))
      
      (top> Po-Pair)
      (draw> *ptr))))

        
(de alt-lay-x (Ln-P Ln-B)
  #{
  
        [X1]
         .  

         *
        [Y]  .

        
        [X3] .
         .
    
    --> Remove pair+car lines
    
               *
        [X1]  [X2]  [X3] .
         .     .     .    
    
    --> Poss leave cdr line
  
               *
        [X1]  [X2]
         .     .  
                  
                  
        [X3] .                  
         .


    --> Poss leave pair line
    
               *
        [X1]  [X2]
         .     .  
                  
                  
        [X3] .                  
         .    
         
    --> Simplest option - change pair only

        [X1]
         .  

         *
        [X]
         .
        
        [X3] .
         .
    
  }#
  
  (println 'alt-lay-x)
    
  (let (Pair (ref> *ptr)
        Po-Pair (lup-pt Pair))
    
    (println 'alt-lay-x "Pair/Tgt: "
      Pair Po-Pair)
      
    (let (Ln (c-ln> *ptr)
          Po-Car (if (car Pair) (lup-pt @) (get Po-Pair 'a))
          Po-Cdr (if (cdr Pair) (lup-pt @) (get Po-Pair 'b)))
      
      (set-lay> Po-Pair 'x)
      
      (put Po-Car 'line)

      (upd-tree)
      (mov-⇲ Po-Pair)

      (unless (= Ln Po-Pair)
        (ln> *ptr Po-Pair))
      
      (top> Po-Pair)
      (draw> *ptr))))
