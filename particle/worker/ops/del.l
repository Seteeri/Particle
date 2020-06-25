#{

  Del: Repl cur with NIL/cdr

  Case 1:

    * On X/Y car/cdr atom: repl cur with NIL
  
         *
    [Y]  .
     
     .
     
    OR
    
    [X]  [X]  .
     .    *  
  
  
  Case 2:
  
    * On X cdr pair: repl cur with cdr
  
          *
    [X]  [X]  .
     .    .   
    
    ->
  
         *
    [X]  .
    .    
    

    
  Case 3:
  
    * On X pair with Y cdr: repl cur with cdr
    
      [X]
       .
    
       *
      [Y]  [Y] .
            .
      
       c

       ->
       
      [X]
       .
    
       *
       c

      - Car promo: bsp car
      - Cdr promo: del pair
        - Consistent with X del
       
       
  Case 4:
  
    * On Y car pair: repl cur with car
  
          *
    [Y]  [Y]  .
          .
    .
    
    ->
  
     *
    [Y]  .
    .   
    
}#


(de del-pt ()
  
  (cond
  
    ((a-atom?> *ptr)
     (repl-atom NIL 'a))

    ((b-atom?> *ptr)
     (repl-atom NIL 'b))
      
    ((or (a-pair?> *ptr)
         (b-pair?> *ptr))
     (del-pair))))
      
    
(de del-pair ()

  (let (Pair (get *ptr 'ref)
        Pt-Pair (lup-pt Pair)
        Back (find-back-any> *ptr Pair T)
        Pt-Back (lup-pt Back)
        Cdr (cdr Pair)
        Atom-Cdr (atom Cdr)
        Pt-Cdr (if Atom-Cdr
                   (get Pt-Pair 'b)
                   (lup-pt Cdr))
        Reg-Pair (if (= (car Back) Pair) 'a 'b))

    (when (get Pt-Pair 'line)
      (if (= Reg-Pair 'b)
          (mov-line Pt-Pair Pt-Cdr)
          (del-line Pt-Pair))
      (lns> *ptr))
    (set-ln-n> *ptr Pt-Cdr)
    
    (del> Pt-Pair T NIL)
    (put Pt-Pair 'b NIL)
        
    (cond 
      ((and (= Reg-Pair 'a) Atom-Cdr)
       (set> Pt-Back Cdr Pt-Cdr)
       (if (get Pt-Cdr 'line)
           (put @ 'p NIL)
           (put Pt-Cdr 'line
             (new '(+Line))))
       (pt> *ptr Back 'a Pt-Cdr)
       T)
       
      ((and (= Reg-Pair 'a) (not Atom-Cdr))
       (set> Pt-Back Cdr Pt-Cdr)
       (if (get Pt-Cdr 'line)
           (put @ 'p NIL)
           (put Pt-Cdr 'line
             (new '(+Line))))
       (pt> *ptr Cdr 'a)
       T)
       
      ((and (= Reg-Pair 'b) Atom-Cdr)
       (con> Pt-Back Cdr Pt-Cdr)
       (pt> *ptr Back 'b Pt-Cdr)
       T)
       
      ((and (= Reg-Pair 'b) (not Atom-Cdr))
       (con> Pt-Back Cdr Pt-Cdr)
       (pt> *ptr Cdr 'b)
       T))

       
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
    (top> Pt-Cdr)
    (draw> *ptr)))

    
###################


(de del-y-pair-2 ()

  # Deletes entire pair and promotes car

  (let (Pair (get *ptr 'ref)
        Pt-Pair (lup-pt Pair)
        Back (find-back-any> *ptr Pair T)
        Pt-Back (lup-pt Back)
        Car (car Pair)
        Atom-Car (atom Car)
        Pt-Car (if Atom-Car
                   (get Pt-Pair 'a)
                   (lup-pt Car))
        Reg-Pair (if (= (car Back) Pair) 'a 'b))
        
    (when (get Pt-Pair 'line)
      (del-line Pt-Pair)
      (lns> *ptr))
    (set-ln-n> *ptr Pt-Car)
        
    (del> Pt-Pair NIL T)
    (put Pt-Pair 'a NIL)
    (put Pt-Pair 'b NIL)
    
    (cond 
      ((and (= Reg-Pair 'a) Atom-Car)
       (set> Pt-Back Car Pt-Car)
       (if (get Pt-Car 'line)
           (put @ 'p NIL)
           (put Pt-Car 'line
             (new '(+Line))))
       (pt> *ptr Back 'a Pt-Car)
       T)
       
      ((and (= Reg-Pair 'a) (not Atom-Car))
       (set> Pt-Back Car Pt-Car)
       (if (get Pt-Car 'line)
           (put @ 'p NIL)
           (put Pt-Car 'line
             (new '(+Line))))
       (pt> *ptr Car 'a)
       T)
       
      ((and (= Reg-Pair 'b) Atom-Car)
       (con> Pt-Back Car Pt-Car)
       (pt> *ptr Back 'b Pt-Car)
       T)
       
      ((and (= Reg-Pair 'b) (not Atom-Car))
       (con> Pt-Back Car Pt-Car)
       (pt> *ptr Car 'b)
       T))
        
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
    (top> Pt-Car)
    (draw> *ptr)))
