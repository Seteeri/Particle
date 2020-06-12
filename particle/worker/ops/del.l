#{

  Case 1:
  
    * On cdr pair/atom: del prv pair
  
              *
    [X]  [X]  .
     .    .   
    
    ->
  
         *
    [X]  .
    .    


  Case 2:
  
    * On car pair: del prv pair
  
          *
    [Y]  [Y]  .
          .
    .
    
    ->
  
     *
    [Y]  .
    .
         
    
    
  Case 3:

    * On car atom: del cur pair
      * or del car itself
  
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


(de del-bs ()
  
  (println 'del-bs)
  
  (cond
  
    ((a-atom?> *ptr)
      (println 'del-bs "A ATOM/NIL")
      (del-a-atom))
      
    ((b-atom?> *ptr)
      (println 'del-bs "B ATOM/NIL")
      (del-b-atom))
    
    ((a-pair?> *ptr)
      (println 'del-bs "A PAIR")
      (del-a-pair))
      
    ((b-pair?> *ptr)
      (println 'del-bs "B PAIR")
      (del-b-pair))))


(de del-a-pair ()

  # Del back
  # Hook pair
  
  (let (Pair (ref> *ptr)
        Po-Pair (lup-pt Pair)
        Tgt (find-back-any> *ptr Pair T)
        Po-Tgt (lup-pt Tgt)
        Back (find-back-any> *ptr Tgt T)
        Po-Back (lup-pt Back))

    (when (get Po-Tgt 'line)
      (del-line Po-Tgt)
      (ln> *ptr))
    (del> Po-Tgt NIL T)
    (put Po-Tgt 'a)
    
    (set> Po-Back Pair Po-Pair)
        
    (upd-tree)
    (mov-⇲ (c-ln> *ptr))
    
    (top> Po-Pair)
    (draw> *ptr)))
      
      
(de del-b-atom (Tgt)

  # Del current
  # Hook atom up
  
  (let (Po-Atom (ref> *ptr)
        Atom (get Po-Atom 'data)
        Tgt (cell> *ptr)
        Po-Tgt (lup-pt Tgt)
        Back (find-back-any> *ptr Tgt T)
        Po-Back (lup-pt Back))
    
    #{
    (prin "Po-Atom: ") (println Po-Atom)
    (prin "Atom: ") (println Atom)
    (prin "Tgt: ") (println Tgt)
    (prin "Po-Tgt: ") (println Po-Tgt)
    (prin "Back: ") (println Back)
    (prin "Po-Back: ") (println Po-Back)
    }#
    
    (when (get Po-Tgt 'line)
      (del-line Po-Tgt))
    (mov-line-n> *ptr Po-Back)
    (del> Po-Tgt T NIL)
    (put Po-Tgt 'b)
    
    (con> Po-Back Atom Po-Atom)
        
    # Update ptr cell
    (put *ptr 'cell Back)
        
    (upd-tree)
    (mov-⇲ (c-ln> *ptr))
    
    (top> Po-Atom)
    (draw> *ptr)))
    
    
(de del-b-pair (Tgt)

  # Del current
  # Hook atom up
  
  (let (Pair (ref> *ptr)
        Po-Pair (lup-pt Pair)
        Tgt (find-back-any> *ptr Pair T)
        Po-Tgt (lup-pt Tgt)
        Back (find-back-any> *ptr Tgt T)
        Po-Back (lup-pt Back))

    (when (get Po-Tgt 'line)
      (del-line Po-Tgt))
    (mov-line-n> *ptr Po-Back)
    (del> Po-Tgt T NIL)
    (put Po-Tgt 'b)
    
    (con> Po-Back Pair Po-Pair)
        
    (upd-tree)
    (mov-⇲ (c-ln> *ptr))
    
    (top> Po-Pair)
    (draw> *ptr)))
    
    
##################

(de del-pair (Tgt)
  (let Tc (get Tgt 'c)
    (if (lay-x?> Tgt)    
        (let Tb (get Tgt 'b)
          (if (lay-x?> Tc)
              (b> Tc Tb)
              (a> Tc Tb))
          (if (get Tgt 'line)
              (prog
                (mov-line Tgt Tb)
                (setq *line Tb))
              # If tgt has no line but Tb has it, remove it
              (when (get Tb 'line)
                (setq *line
                      (if (get *line 'line 'p) @ *line))
                (del-line Tb)))
          (del> Tgt T NIL)
          Tb)
        (let Ta (get Tgt 'a)
          (if (lay-x?> Tc)
              (b> Tc Ta)
              (a> Tc Ta))
          (when (get Tgt 'line)
            (mov-line Tgt Ta))
          (del> Tgt NIL T)
          Ta))))


(de del-rx (Sta End)  
  (let P Sta
    (until (or (= P End) (not P))
      (del-car> P)
      (setq P (get P 'b)))
    (when P
      (del-car> P))))   
