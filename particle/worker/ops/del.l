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
            
        ~(assert (get *line 'line))
            
        (upd-tree)
        (mov-⇲ (car (get *ptr 'ln)))      
        
        # If on-car maintain car
        (if *ptr-on-car
            (set-ptr-ref (sub> New-Ref))
            (prog
              (set-ptr-ref New-Ref)
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
