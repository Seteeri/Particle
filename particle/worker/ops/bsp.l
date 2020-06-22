#{

  Backspace = repl prv with cur
  * on car, replaces cell with car
  resulting in chopping list

  Case 1:
  
    * On cdr pair/atom: repl prv pair with its cdr
  
              *
    [X]  [X]  .
     .    .   
    
    ->
  
         *
    [X]  .
    .    


  Case 2:
  
    * On car pair: repl prv pair with its car pair
  
          *
    [Y]  [Y]  .
          .
    .
    
    ->
  
     *
    [Y]  .
    .
         
    
    
  Case 3:

    * On car atom: repl prv/pair with its car
  
    [X]
     .
  
         *
    [Y]  .
     
    [X]
     .
     
    OR
    
    [X]  [X]  .
     .    *
      
}#


(de bsp-pt ()
  
  (cond
  
    ((a-atom?> *ptr)
      #(println 'bsp-pt "A ATOM/NIL")
      )
      
    ((b-atom?> *ptr)
      #(println 'bsp-pt "B ATOM/NIL")
      (bsp-b-atom))
    
    ((a-pair?> *ptr)
      #(println 'bsp-pt "A PAIR")
      (bsp-a-pair))
      
    ((b-pair?> *ptr)
      #(println 'bsp-pt "B PAIR")
      (bsp-b-pair))))


(de bsp-a-pair ()

  # Del back
  # Hook pair
  
  (let (Pair (get *ptr 'ref)
        Pt-Pair (lup-pt Pair)
        Tgt (find-back-any> *ptr Pair T)
        Pt-Tgt (lup-pt Tgt)
        Back (find-back-any> *ptr Tgt T)
        Pt-Back (lup-pt Back))

    (when (get Pt-Tgt 'line)
      (del-line Pt-Tgt)
      (let Ln (lns> *ptr)
        (lns> *ptr)
        (lns> *ptr Ln)))
    (del> Pt-Tgt NIL T)
    (put Pt-Tgt 'a NIL)
    
    (set> Pt-Back Pair Pt-Pair)
        
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
    (top> Pt-Pair)
    (draw> *ptr)))
      
      
(de bsp-b-atom ()

  # Del current
  # Hook atom up
  
  (let (Pt-Atom (get *ptr 'ref)
        Atom (get Pt-Atom 'any)
        Tgt (get *ptr 'pair)
        Pt-Tgt (lup-pt Tgt)
        Back (find-back-any> *ptr Tgt T)
        Pt-Back (lup-pt Back))
    
    (when (get Pt-Tgt 'line)
      (del-line Pt-Tgt)
      (let Ln (lns> *ptr)
        (lns> *ptr)
        (lns> *ptr Ln)))      
    (set-ln-n> *ptr Pt-Back)
    (del> Pt-Tgt T NIL)
    (put Pt-Tgt 'b NIL)
    
    (con> Pt-Back Atom Pt-Atom)
        
    # Update ptr cell
    (put *ptr 'pair Back)
        
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
    (top> Pt-Atom)
    (draw> *ptr)))
    
    
(de bsp-b-pair ()

  (let (Pair (get *ptr 'ref)
        Pt-Pair (lup-pt Pair)
        Tgt (find-back-any> *ptr Pair T)
        Pt-Tgt (lup-pt Tgt)
        Back (find-back-any> *ptr Tgt T)
        Pt-Back (lup-pt Back))

    (when (get Pt-Tgt 'line)
      (del-line Pt-Tgt)
      (lns> *ptr) (lns> *ptr))
    (set-ln-n> *ptr Pt-Pair)
    
    (del> Pt-Tgt T NIL)
    (put Pt-Tgt 'b NIL)
    
    (if (= (car Back) Tgt)
        (prog
          (set> Pt-Back Pair Pt-Pair)
          (if (get Pt-Pair 'line)
              (put @ 'p NIL)
              (put Pt-Pair 'line
                (new '(+Line)))))
        (con> Pt-Back Pair Pt-Pair))
        
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
    (top> Pt-Pair)
    (draw> *ptr)))          
