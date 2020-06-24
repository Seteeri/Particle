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
  
  (let (Pair (get *ptr 'ref)
        Pt-Pair (lup-pt Pair)
        Ln (c-lns> *ptr)
        Pt-Car (if (car Pair) (lup-pt @) (get Pt-Pair 'a))
        Pt-Cdr (if (cdr Pair) (lup-pt @) (get Pt-Pair 'b)))
    
    (println 'alt-lay-y "Pair/Tgt: "
      Pair Pt-Pair)    
  
    (set-lay> Pt-Pair 'y T)
    
    (if (get Pt-Pair 'line)
        (put @ 'n Pt-Cdr)      
        (ins-line-aft Ln Pt-Pair))
    
    (if (get Pt-Cdr 'line)
        (put @ 'p Pt-Pair)
        (ins-line-aft Pt-Pair Pt-Cdr))
        
    (put Pt-Car 'line (new '(+Line)))

    (lay-*start)
    (mov-⇲ Pt-Pair)

    (unless (= Ln Pt-Pair)
      (lns> *ptr Pt-Pair))
    
    (top> Pt-Pair)
    (draw> *ptr)))

        
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
    
  (let (Pair (get *ptr 'ref)
        Pt-Pair (lup-pt Pair)
        Ln (c-lns> *ptr)
        Pt-Car (if (car Pair) (lup-pt @) (get Pt-Pair 'a))
        Pt-Cdr (if (cdr Pair) (lup-pt @) (get Pt-Pair 'b)))
  
    (println 'alt-lay-x "Pair/Tgt: "
      Pair Pt-Pair)    
  
    (set-lay> Pt-Pair 'x T)
    
    (put Pt-Car 'line)

    (lay-*start)
    (mov-⇲ Pt-Pair)

    (unless (= Ln Pt-Pair)
      (lns> *ptr Pt-Pair))
    
    (top> Pt-Pair)
    (draw> *ptr)))
