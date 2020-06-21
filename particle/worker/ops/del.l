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


(de del-bsp ()
  
  (cond
  
    ((a-atom?> *ptr)
      #(println 'del-bsp "A ATOM/NIL")
      )
      
    ((b-atom?> *ptr)
      #(println 'del-bsp "B ATOM/NIL")
      (del-b-atom))
    
    ((a-pair?> *ptr)
      #(println 'del-bsp "A PAIR")
      (del-a-pair))
      
    ((b-pair?> *ptr)
      #(println 'del-bsp "B PAIR")
      (del-b-pair))))


(de del-a-pair ()

  # Del back
  # Hook pair
  
  (let (Pair (get *ptr 'ref)
        Po-Pair (lup-pt Pair)
        Tgt (find-back-any> *ptr Pair T)
        Po-Tgt (lup-pt Tgt)
        Back (find-back-any> *ptr Tgt T)
        Po-Back (lup-pt Back))

    (when (get Po-Tgt 'line)
      (del-line Po-Tgt)
      (let Ln (ln> *ptr)
        (ln> *ptr)
        (ln> *ptr Ln)))
    (del> Po-Tgt NIL T)
    (put Po-Tgt 'a NIL)
    
    (set> Po-Back Pair Po-Pair)
        
    (upd-tree)
    (mov-⇲ (c-ln> *ptr))
    
    (top> Po-Pair)
    (draw> *ptr)))
      
      
(de del-b-atom ()

  # Del current
  # Hook atom up
  
  (let (Po-Atom (get *ptr 'ref)
        Atom (get Po-Atom 'any)
        Tgt (get *ptr 'pair)
        Po-Tgt (lup-pt Tgt)
        Back (find-back-any> *ptr Tgt T)
        Po-Back (lup-pt Back))
    
    (when (get Po-Tgt 'line)
      (del-line Po-Tgt)
      (let Ln (ln> *ptr)
        (ln> *ptr)
        (ln> *ptr Ln)))      
    (mov-line-n> *ptr Po-Back)
    (del> Po-Tgt T NIL)
    (put Po-Tgt 'b NIL)
    
    (con> Po-Back Atom Po-Atom)
        
    # Update ptr cell
    (put *ptr 'pair Back)
        
    (upd-tree)
    (mov-⇲ (c-ln> *ptr))
    
    (top> Po-Atom)
    (draw> *ptr)))
    
    
(de del-b-pair ()

  (let (Pair (get *ptr 'ref)
        Po-Pair (lup-pt Pair)
        Tgt (find-back-any> *ptr Pair T)
        Po-Tgt (lup-pt Tgt)
        Back (find-back-any> *ptr Tgt T)
        Po-Back (lup-pt Back))

    (when (get Po-Tgt 'line)
      (del-line Po-Tgt)
      (ln> *ptr) (ln> *ptr))
    (mov-line-n> *ptr Po-Pair)
    
    (del> Po-Tgt T NIL)
    (put Po-Tgt 'b NIL)
    
    (if (= (car Back) Tgt)
        (prog
          (set> Po-Back Pair Po-Pair)
          (if (get Po-Pair 'line)
              (put @ 'p NIL)
              (put Po-Pair 'line
                (new '(+Line)))))
        (con> Po-Back Pair Po-Pair))
        
    (upd-tree)
    (mov-⇲ (c-ln> *ptr))
    
    (top> Po-Pair)
    (draw> *ptr)))          
