#{

  CASES:
  
  These only delete the first cell instead of everything,
  similar to text,
  It promotes either the Car/Cdr to replace it
  
  del-x-pair
  
         *
  [X]  [X/Y]  NIL
   .    .
   
    If y: use car
    If x: use cdr
   
  del-x-atom
  
       *
  [X]  NIL
   .   
   
    Replace with NIL?
   
  ---
  
  
  del-y-pair
  
        *
  [Y]  [X/Y]  NIL
   .    .
  
    If y: use car
    If x: use cdr  
  
  del-y-atm
  
       *
  [Y]  NIL
   .   
  
    Replace with NIL?
    
  ---
  
  CAR:
  
  * Bsp: Replace car with NIL
  * Del: Replace car with NIL
  
  If inverse of ins was done,
  would require shifting all subsequent cars bwd
  
  Handle line within each fn
     
}#


(de del-pt (Tgt)

  # Similar to make-pair-str
  (when (get Tgt 'b)
  
    (let (Tc (get Tgt 'c)
          New-Ref 
            (cond ((is-pair? Tgt)
                  (del-pair Tgt))
                  
                  (T
                  T)))
            
        (upd-tree)
        (mov-⇲ *line)      
        
        # If on-car maintain car
        (if *on-car
            (set-ptr *ptr (sub> New-Ref))
            (prog
              (set-ptr *ptr New-Ref)
              (mov-cur-abv New-Ref)))
              
        (lay-ptr *ptr))))


(de del-pair (Tgt)
  (let Tc (get Tgt 'c)
    (if (lay-x?> Tgt)    
        (let Tb (get Tgt 'b)
          (if (lay-x?> Tc)
              (b> Tc Tb)
              (a> Tc Tb))
          (if (get Tgt 'line)
              (put-line Tgt Tb)
              # If tgt has no line but Tb has it, remove it
              (when (get Tb 'line)
                (del-line Tb)))
          (del> Tgt T NIL)
          Tb)
        (let Ta (get Tgt 'a)
          (if (lay-x?> Tc)
              (b> Tc Ta)
              (a> Tc Ta))
          (when (get Tgt 'line)
            (put-line Tgt Ta))
          (del> Tgt NIL T)
          Ta))))

          
#################


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
        Ta (get Tgt 'a)
        Tb (get Tgt 'b)
        New-Ref Ref)

    (unless (= (get Tgt 'data) '*start)
    
      (cond ((not Tb)
              (println "del-end")
              # Replace item with NIL
              # If NIL ignore?
              (setq New-Ref (repl-cdr Ref
                                      (gen-point NIL 'x))))
      
            ((and (lay-y?> Tgt) (is-pair? Tgt))
              (println "del-car")
              # Promote Car
              (a> Tc Ta)
              (put Tgt 'a)
              (setq New-Ref Ta))
                    
            (T
              (println "del-pair")
              # Del normal
              
              # Two options for lines:
              # - Delete line (same as txt, expected)
              # - Keep line and txfer
              
              (cond ((= (get Tc 'b) Tgt)
                      (b> Tc Tb)
                      (put Tgt 'b))
                    ((= (get Tc 'a) Tgt)
                      (a> Tc Tb)
                      (put Tgt 'a)))
              
              (if (get Tgt 'line)
              
                  # If tgt has line, txfer it
                  (put-line Tgt Tb)
                  
                  # If tgt has no line but Tb has it, remove it
                  (when (get Tb 'line)
                    (put Tb 'line)))
              
              (setq New-Ref Tb)))

      (del> Tgt T T)

      (upd-tree)
      (mov-⇲ *line)      
      
      # If on-car maintain car
      (if *on-car
          (set-ptr *ptr (sub> New-Ref))
          (prog
            (set-ptr *ptr New-Ref)
            (mov-cur-abv New-Ref)))
            
      (lay-ptr *ptr))))


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
        Tgt (if (and *on-car
                     (not (is-nil? Ref)))
                (get C 'c)
                C)
        Tc (get Tgt 'c)
        Ta (get Tgt 'a)
        Tb (get Tgt 'b)
        New-Ref Ref)

    (unless (= (get Tgt 'data) '*start)
    
      (cond ((and (lay-y?> Tc)
                  (is-pair? Tc)
                  (is-car?> Tgt))
             (println 'del-c "+Pair")
             
             (a> Tc Ref)
             
             (cond ((= Tb Ref)
                    (put Tgt 'b))
                   ((= Ta Ref)
                    (put Tgt 'a)))
             (del> Tgt T T)
             
             # Put line in Ref
             (unless (get Ref 'line)
              (put Ref 'line (new '(+Line)))))
                    
            (T
              # Del normal, try to keep similar to text
              
              # Two options for lines:
              # - Delete line (same as txt, expected)
              # - Keep line and txfer
              
              (println 'del-c
                "Tc, Tgt, Tb =" (get Tc 'data) (get Tgt 'data) (get Tb 'data))
              
              # If line, delete line only
              (if (get Tb 'line)
              
                  # If tgt has line, txfer it
                  (del-line Tb)
                  
                  # If tgt has no line but Tb has it, remove it
                  (prog
                    (b> Tc Tb)
                    (when (get Tb 'line)
                      (put Tb 'line))
                    (put Tgt 'b)
                    (del> Tgt T T)))))
            
      (upd-tree)
      (mov-⇲ *line)      
            
      # If on-car maintain car
      (if *on-car
          (set-ptr *ptr (sub> New-Ref))
          (prog
            (set-ptr *ptr New-Ref)
            (mov-cur-abv New-Ref)))      
      
      (lay-ptr *ptr))))    
    

(de del-rx (Sta End)  
  (let P Sta
    (until (or (= P End) (not P))
      (del-car> P)
      (setq P (get P 'b)))
    (when P
      (del-car> P))))   
