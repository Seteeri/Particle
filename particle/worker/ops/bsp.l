#{

  Backspace = repl prv with cur

  What about X Pair Car Atom?
  - Replace parent with car?
  - Like chopping list
  
  Case 1:
  
    * On X/Y cdr pair/atom: repl prv pair with its cdr (ref)
  
              *
    [X]  [X]  .
     .    .   
    
    ->
  
         *
    [X]  .
    .    

    
  Case 2:

    * On X/Y car Y pair: repl prv/pair with its car (ref)
  
      [X]
      .
    
            *
      [Y]  [Y] .
            c
      
      [X]
      .
     
    ->

      [X]
      .
    
       *
      [Y] .
       c
    
  Case 3:
  
    * On X/Y car atom: repl prv/pair with atom
    
}#


(de bsp-pt ()
  
  (cond
  
    ((a-atom?> *ptr) (bsp-a-atom))
      
    ((b-atom?> *ptr) (bsp-b-atom))
    
    ((a-pair?> *ptr) (bsp-a-pair))
      
    ((b-pair?> *ptr) (bsp-b-pair))))

     
(de bsp-a-atom ()

  (println 'bsp-a-atom)

  # Same as del-pair but removed unused cond blocks

  (let (Pair (get *ptr 'pair)
        Pt-Pair (lup-pt Pair)
        Back (find-back-any> *ptr Pair T)
        Pt-Back (lup-pt Back)
        Car (car Pair)
        Pt-Car (get Pt-Pair 'a)
        Reg-Pair (if (== (car Back) Pair) 'a 'b))
    
    #(prin "  ") (println Pair Pt-Pair Back Pt-Back #Reg-Pair)
    
    # Bugs when car of Y pair
    
    (when (get Pt-Pair 'line)
      (del-line Pt-Pair)
      (when (= Reg-Pair 'b)
        (when (get Pt-Back 'line)
          (put @ 'n NIL)))
      (lns> *ptr)
      (lns> *ptr))
    (set-ln-n> *ptr Pt-Car)
        
    (del> Pt-Pair NIL T)
    (put Pt-Pair 'a NIL)
    (put Pt-Pair 'b NIL)
        
    ###################
    
    (if (= Reg-Pair 'a)
    
        (prog
          (set> Pt-Back Car Pt-Car)
          (if (get Pt-Car 'line)
              (put @ 'p NIL)
              (put Pt-Car 'line
                (new '(+Line))))
          (pt> *ptr Back 'a Pt-Car))
    
        (prog
          # If Back is X pair and line, set p
          # If Back is Y pair, cdr must have line
          (when (get Pt-Back 'line)
            (if (lay-x?> Pt-Back)
                (when (get Pt-Car 'line)
                  (put @ 'p Pt-Back)
                  (put Pt-Back 'line 'n Pt-Car))
                (prog
                  (if (get Pt-Car 'line)
                      (put @ 'p Pt-Back)
                      (put Pt-Car 'line 
                        (new '(+Line) Pt-Back)))
                  (put Pt-Back 'line 'n Pt-Car))))
          (con> Pt-Back Car Pt-Car)
          (pt> *ptr Back 'b Pt-Car)))
                 
          
    # Update ptr pair
        
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
    (top> Pt-Car)
    (draw> *ptr)))
     
      
(de bsp-b-atom ()

  (println 'bsp-b-atom)

  # Same as del-pair but removed unused cond blocks

  (let (Pair (get *ptr 'pair)
        Pt-Pair (lup-pt Pair)
        Back (find-back-any> *ptr Pair T)
        Pt-Back (lup-pt Back)
        Cdr (cdr Pair)
        Pt-Cdr (get Pt-Pair 'b)
        Reg-Pair (if (== (car Back) Pair) 'a 'b))

    #(println 'bsp-b-atom
    #  Pair Pt-Pair Back Pt-Back Reg-Pair)
    #(println *start)
        
    (if (get Pt-Pair 'line)
        (prog
          (mov-line Pt-Pair Pt-Cdr)
          (del-line Pt-Pair)
          (lns> *ptr))
        (when (get Pt-Cdr 'line)
          (del-line Pt-Cdr)
          (lns> *ptr)))
          
    (set-ln-n> *ptr Pt-Cdr)
    
    (del> Pt-Pair T NIL)
    (put Pt-Pair 'b NIL)
    
    ###################
    
    (if (= Reg-Pair 'a)
    
        (prog
          (set> Pt-Back Cdr Pt-Cdr)
          (if (get Pt-Cdr 'line)
              (put @ 'p NIL)
              (put Pt-Cdr 'line
                (new '(+Line))))
          (pt> *ptr Back 'a Pt-Cdr))
        
        (prog
          # If Back is X pair and line, set p
          # If Back is Y pair, cdr must have line
          (when (get Pt-Back 'line)
            (if (lay-x?> Pt-Back)
                (when (get Pt-Cdr 'line)
                  (put @ 'p Pt-Back)
                  (put Pt-Back 'line 'n Pt-Cdr))
                (prog
                  (if (get Pt-Cdr 'line)
                      (put @ 'p Pt-Back)
                      (put Pt-Cdr 'line 
                        (new '(+Line) Pt-Back)))
                  (put Pt-Back 'line 'n Pt-Cdr))))
          (con> Pt-Back Cdr Pt-Cdr)
          (pt> *ptr Back 'b Pt-Cdr)))
          
       
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
    (top> Pt-Cdr)
    (draw> *ptr)))

    
(de bsp-a-pair ()
  
  (println 'bsp-a-pair)
  
  (let (Pair (get *ptr 'ref)
        Pt-Pair (lup-pt Pair)
        Tgt (find-back-any> *ptr Pair T)
        Pt-Tgt (lup-pt Tgt)
        Back (find-back-any> *ptr Tgt T)
        Pt-Back (lup-pt Back)
        Reg-Tgt (if (== (car Back) Tgt) 'a 'b))

    (when (get Pt-Tgt 'line)
      (del-line Pt-Tgt)
      (lns> *ptr)
      (lns> *ptr))
      
    (set-ln-n> *ptr Pt-Pair)
      
    (del> Pt-Tgt NIL T)
      
    (if (= Reg-Tgt 'a)
    
        (prog
          (set> Pt-Back Pair Pt-Pair)
          (if (get Pt-Pair 'line)
              (put @ 'p NIL)
              (put Pt-Pair 'line
                (new '(+Line))))
          (put *ptr 'reg 'a))
    
        (prog
          # If Back is X pair and line, set p
          # If Back is Y pair, cdr must have line
          (when (get Pt-Back 'line)
            (if (lay-x?> Pt-Back)
                (when (get Pt-Pair 'line)
                  (put @ 'p Pt-Back)
                  (put Pt-Back 'line 'n Pt-Pair))
                (prog
                  (if (get Pt-Pair 'line)
                      (put @ 'p Pt-Back)
                      (put Pt-Pair 'line 
                        (new '(+Line) Pt-Back)))
                  (put Pt-Back 'line 'n Pt-Pair))))
          (con> Pt-Back Pair Pt-Pair)
          (put *ptr 'reg 'b)))
            
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
    (top> Pt-Pair)
    (draw> *ptr)))    
    
    
(de bsp-b-pair ()

  #(println 'bsp-b-pair)

  (let (Pair (get *ptr 'ref)
        Pt-Pair (lup-pt Pair)
        Tgt (find-back-any> *ptr Pair T)
        Pt-Tgt (lup-pt Tgt)
        Back (find-back-any> *ptr Tgt T)
        Pt-Back (lup-pt Back)
        Reg-Tgt (if (== (car Back) Tgt) 'a 'b))

    (when (get Pt-Tgt 'line)
      (del-line Pt-Tgt)
      (lns> *ptr)
      (lns> *ptr))
      
    (set-ln-n> *ptr Pt-Pair)
    
    (del> Pt-Tgt T NIL)
    
    #{
    (if (= (car Back) Tgt)
        (prog
          (set> Pt-Back Pair Pt-Pair)
          (if (get Pt-Pair 'line)
              (put @ 'p NIL)
              (put Pt-Pair 'line
                (new '(+Line)))))
        (con> Pt-Back Pair Pt-Pair))
    }#
    
    (if (= Reg-Tgt 'a)
    
        (prog
          (set> Pt-Back Pair Pt-Pair)
          (if (get Pt-Pair 'line)
              (put @ 'p NIL)
              (put Pt-Pair 'line
                (new '(+Line))))
          (put *ptr 'reg 'a))
    
        (prog
          # If Back is X pair and line, set p
          # If Back is Y pair, cdr must have line
          (when (get Pt-Back 'line)
            (if (lay-x?> Pt-Back)
                (when (get Pt-Pair 'line)
                  (put @ 'p Pt-Back)
                  (put Pt-Back 'line 'n Pt-Pair))
                (prog
                  (if (get Pt-Pair 'line)
                      (put @ 'p Pt-Back)
                      (put Pt-Pair 'line 
                        (new '(+Line) Pt-Back)))
                  (put Pt-Back 'line 'n Pt-Pair))))
          (con> Pt-Back Pair Pt-Pair)
          (put *ptr 'reg 'b))) 
    
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
    (top> Pt-Pair)
    (draw> *ptr)))          
