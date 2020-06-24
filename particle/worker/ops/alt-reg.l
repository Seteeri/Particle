(de alt-reg ()
  
  #{
      
    For pairs, will have to change layout if car is a pair
  
    On pair, swap regs (don't get its pair - only atom does this)
    On atom, get its pair and swap etc
    
    Maintain pointer on current data

  }#  
    
  #(println 'alt-reg)
    
  (let (Pair (if (get *ptr 'pair) @ (get *ptr 'ref))
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
        
    # If Y pair, adj lines
    (when (lay-y?> Pt-Pair)
      (with Pt-Pair

        # New Car
        (if (get Pt-Cdr 'line)
            (put @ 'p NIL)
            (put Pt-Cdr 'line (new '(+Line))))
        
        # New Cdr
        (if (get Pt-Car 'line)
            (put @ 'p This)
            (put Pt-Car 'line (new '(+Line) This)))
            
        (put (: line) 'n Pt-Car)))
    
    # If new car is a Pair, must change to Y layout
    
    (lay-*start)
    (mov-⇲ Pt-Pair)
    
    (alt-reg> *ptr)
    (top-ref> *ptr)
    (draw> *ptr)))
