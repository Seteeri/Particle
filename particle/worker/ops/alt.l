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
        Po-Pair (lup-pt Pair)
        Ln (c-ln> *ptr)
        Po-Car (if (car Pair) (lup-pt @) (get Po-Pair 'a))
        Po-Cdr (if (cdr Pair) (lup-pt @) (get Po-Pair 'b)))
    
    (println 'alt-lay-y "Pair/Tgt: "
      Pair Po-Pair)    
  
    (set-lay> Po-Pair 'y T)
    
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
        Po-Pair (lup-pt Pair)
        Ln (c-ln> *ptr)
        Po-Car (if (car Pair) (lup-pt @) (get Po-Pair 'a))
        Po-Cdr (if (cdr Pair) (lup-pt @) (get Po-Pair 'b)))
  
    (println 'alt-lay-x "Pair/Tgt: "
      Pair Po-Pair)    
  
    (set-lay> Po-Pair 'x T)
    
    (put Po-Car 'line)

    (upd-tree)
    (mov-⇲ Po-Pair)

    (unless (= Ln Po-Pair)
      (ln> *ptr Po-Pair))
    
    (top> Po-Pair)
    (draw> *ptr)))

    
(de alt-reg ()
  
  #{
  
    Meant for swapping atoms
    
    For pairs, will have to change layout if car is a pair
  
  }#

  (println 'alt-reg)
    
  (let (Pair (get *ptr 'ref)
        Po-Pair (lup-pt Pair)
        Po-A (get Po-Pair 'a)
        Po-B (get Po-Pair 'b)
        Car (car Pair)
        Cdr (cdr Pair)
        Po-Car (if Po-A @ (lup-pt Car))
        Po-Cdr (if Po-B @ (lup-pt Cdr))
        Ln-Car (get Po-Car 'line)
        Ln-Cdr (get Po-Cdr 'line))
    
    (set Pair Cdr)
    (con Pair Car)
    
    (put Po-Pair 'a Po-B)
    (put Po-Pair 'b Po-A)
    
    (put Po-Cdr 'line Ln-Car)
    (put Po-Car 'line Ln-Cdr)
    
    (let? N (get Ln-Car 'n)
      (put N 'line 'p Po-Cdr))
    
    (let? N (get Ln-Cdr 'n)
      (put N 'line 'p Po-Car))
        
    (upd-tree)
    (mov-⇲ Po-Pair)))
