# Make these +Pair methods?

(de alt-lay ()
  (let ((Pair Pt-Pair) (get-ref-pair> *ptr))
    ((if (lay-x?> *ptr)
         alt-lay-y
         alt-lay-x)
      Pair Pt-Pair)))

        
(de alt-lay-y (Pair Pt-Pair)
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
  
  #(println 'alt-lay-y)
  
  (let (Ln (c-lns> *ptr)
        Pt-Car (if (car Pair) (lup-pt @) (get Pt-Pair 'a))
        Pt-Cdr (if (cdr Pair) (lup-pt @) (get Pt-Pair 'b)))
    
    #(println 'alt-lay-y "Pair/Tgt: "
    #  Pair Pt-Pair)    
  
    (set-lay> Pt-Pair 'y T)
    
    (if (get Pt-Pair 'line)
        (put @ 'n Pt-Cdr)      
        (ins-line-aft Ln Pt-Pair))
    
    (if (get Pt-Cdr 'line)
        (put @ 'p Pt-Pair)
        (ins-line-aft Pt-Pair Pt-Cdr))
        
    (put Pt-Car 'line (new '(+Line)))

    (lay-*start)

    (unless (= Ln Pt-Pair)
      (lns> *ptr Pt-Pair))
    
    (top> Pt-Pair)
    (draw> *ptr)))

        
(de alt-lay-x (Pair Pt-Pair)
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
  
  #(println 'alt-lay-x)
    
  (let (Ln (c-lns> *ptr)
        Pt-Car (if (car Pair) (lup-pt @) (get Pt-Pair 'a))
        Pt-Cdr (if (cdr Pair) (lup-pt @) (get Pt-Pair 'b)))
  
    (if (pair (car Pair))
  
        (prinl "alt-lay-x: Only Y layout is possible when Car is a Pair")
  
        (prog
        
          #(println 'alt-lay-x "Pair/Tgt: "
          #  Pair Pt-Pair)    
        
          (set-lay> Pt-Pair 'x T)
          
          (put Pt-Car 'line)

          (lay-*start)

          (unless (= Ln Pt-Pair)
            (lns> *ptr Pt-Pair))
          
          (top> Pt-Pair)
          (draw> *ptr)))))
