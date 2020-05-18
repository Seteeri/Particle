(de del-tgt (Ref)

  #{
    Delete current target and connect next
  
               *
    [ ]  [ ]  [ ]  NIL
     A    B    C
      
          Tc   T   Tb
    
    ->
    
          Tc   Tb
              
    Doing this on end-atom replaces it with NIL?
   
  }#

  (let (Tgt (if *on-car (get Ref 'c) Ref)
        Tc (get Tgt 'c)
        Tb (get Tgt 'b)
        New-Ref Ref)

    (unless (= (get Tgt 'data) '*start)
    
      (cond ((not Tb)
              # Replace item with NIL
              # If NIL ignore?
              (setq New-Ref (repl-cdr Ref
                                      (gen2-part NIL 'x))))
      
            ((and (lay-y?> Tgt) (isa '(+Pair) Tgt))
              # Promote Car
              (a> Tc Tb)
              (put Tgt 'a)
              (setq New-Ref Tb))
                    
            (T
              # Del normal
              (b> Tc Tb)
              
              (if (get Tgt 'line)
              
                  # If tgt has line, txfer it
                  (put-line Tgt Tb)
                  
                  # If tgt has no line but Tb has it, remove it
                  (when (get Tb 'line)
                    (put Tb 'line)))
              
              (put Tgt 'b)
              (setq New-Ref Tb)))

      (del> Tgt T T)

      (upd-tree)
      (mov-⇲ *line)      
      
      # If on-car maintain car
      (if *on-car
          (set-ptr *fwd-in (sub> New-Ref))
          (prog
            (set-ptr *fwd-in New-Ref)
            (mov-cur-abv New-Ref)))
            
      (lay-ptr *fwd-in))))


(de del-c (Ref)

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
            
      # If on-car maintain car
      (if *on-car
          (set-ptr *fwd-in (sub> New-Ref))
          (prog
            (set-ptr *fwd-in New-Ref)
            (mov-cur-abv New-Ref)))      
      
      (lay-ptr *fwd-in))))    
    

(de del-rx (Sta End)  
  (let P Sta
    (until (or (= P End) (not P))
      (del-car> P)
      (setq P (get P 'b)))
    (when P
      (del-car> P))))   
