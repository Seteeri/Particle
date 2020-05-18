(de make-list-cdr (Ref)
  (println 'make-list-cdr)
  
  # Pair/X or Y
  # Atom
  
  )

  
(de del-bs (Ref)

  #{
  
    For list, del list and promote items to superlist:
    
                    *
    [Y]  [Y]  [Y]  [ ]  [ ]  [ ]  NIL
                    .    .    .
               .
          .
    .
   
    ->
    
               *
    [Y]  [Y]  [ ]  [ ]  [ ]  NIL
               .    .    .
          .
    .
    
    Basic:
    
            *
    Tc Tgt Tb
  
    Ex:
    
    A Tc Tgt
    
    *
    Tb
              
   
  }#

  (let (C (get Ref 'c)
        Tgt (if *on-car (get C 'c) C)
        Tc (get Tgt 'c)
        Tb (get Tgt 'b)
        New-Ref Ref)

    (unless (= (get Tgt 'data) '*start)
    
      (cond ((and (lay-y?> Tgt) (isa '(+Pair) Tgt) (is-car?> C))
             (put Tgt 'a)
             (a> Tc Ref))
                    
            (T
            
              (b> Tc Tb)
              
              (if (get Tgt 'line)
              
                  # If tgt has line, txfer it
                  (put-line Tgt Tb)
                  
                  # If tgt has no line but Tb has it, remove it
                  (when (get Tb 'line)
                    (put Tb 'line)))
              
                
              (put Tgt 'b)))
      
      (del> Tgt T T)
      
      (upd-tree)
      (mov-⇲ *line)      
      
      (set-ptr *fwd-in New-Ref)
      (mov-cur-abv New-Ref)
      (lay-ptr *fwd-in))))    
    

(de del-rx (Sta End)  
  (let P Sta
    (until (or (= P End) (not P))
      (del-car> P)
      (setq P (get P 'b)))
    (when P
      (del-car> P))))   
