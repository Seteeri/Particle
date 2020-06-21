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

    
(de alt-reg ()
  
  #{
  
    Meant for swapping atoms
    
    For pairs, will have to change layout if car is a pair
  
  }#

  (println 'alt-reg)
    
  (let (Pair (get *ptr 'ref)
        Pt-Pair (lup-pt Pair)
        Pt-A (get Pt-Pair 'a)
        Pt-B (get Pt-Pair 'b)
        Car (car Pair)
        Cdr (cdr Pair)
        Pt-Car (if Pt-A @ (lup-pt Car))
        Pt-Cdr (if Pt-B @ (lup-pt Cdr))
        Ln-Car (get Pt-Car 'line)
        Ln-Cdr (get Pt-Cdr 'line))
    
    (set Pair Cdr)
    (con Pair Car)
    
    (put Pt-Pair 'a Pt-B)
    (put Pt-Pair 'b Pt-A)
    
    (put Pt-Cdr 'line Ln-Car)
    (put Pt-Car 'line Ln-Cdr)
    
    (let? N (get Ln-Car 'n)
      (put N 'line 'p Pt-Cdr))
    
    (let? N (get Ln-Cdr 'n)
      (put N 'line 'p Pt-Car))
        
    (lay-*start)
    (mov-⇲ Pt-Pair)))
