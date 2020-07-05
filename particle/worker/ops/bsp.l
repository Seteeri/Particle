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
  
  (let Pt-Pair (cond
                  ((a-atom?> *ptr) (bsp-a-atom))
                  ((b-atom?> *ptr) (bsp-b-atom))
                  ((a-pair?> *ptr) (bsp-a-pair))
                  ((b-pair?> *ptr) (bsp-b-pair)))
    
    # Mov to cmd/caller?
    
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
    (top> Pt-Pair)
    (draw> *ptr)))

     
(de bsp-a-atom ()

  (println 'bsp-a-atom)

  (let (Pair (get *ptr 'pair)
        Pt-Pair (lup-pt Pair)
        Back (find-back-any> *ptr Pair T)
        Pt-Back (lup-pt Back)
        Reg-Pair (if (== (car Back) Pair) 'a 'b)
        Tgt (car Pair)
        Pt-Tgt (get Pt-Pair 'a))
        
    (when (get Pt-Pair 'line)
      (del-line Pt-Pair)
      (when (= Reg-Pair 'b)
        (when (get Pt-Back 'line)
          (put @ 'n NIL)))
      (lns> *ptr)
      (lns> *ptr))
    (set-ln-n> *ptr Pt-Tgt)
        
    (del> Pt-Pair NIL T)
    (put Pt-Pair 'a NIL)
    (put Pt-Pair 'b NIL)
        
    ###################
    
    (if (= Reg-Pair 'a)
    
        (prog
          (set> Pt-Back Tgt Pt-Tgt)
          (if (get Pt-Tgt 'line)
              (put @ 'p NIL)
              (put Pt-Tgt 'line
                (new '(+Line))))
          (pt> *ptr Back 'a Pt-Tgt))
    
        (prog
          # If Back is X pair and line, set p
          # If Back is Y pair, cdr must have line
          (when (get Pt-Back 'line)
            (if (lay-x?> Pt-Back)
                (when (get Pt-Tgt 'line)
                  (put @ 'p Pt-Back)
                  (put Pt-Back 'line 'n Pt-Tgt))
                (prog
                  (if (get Pt-Tgt 'line)
                      (put @ 'p Pt-Back)
                      (put Pt-Tgt 'line 
                        (new '(+Line) Pt-Back)))
                  (put Pt-Back 'line 'n Pt-Tgt))))
          (con> Pt-Back Tgt Pt-Tgt)
          (pt> *ptr Back 'b Pt-Tgt)))
          
      Pt-Pair))
     
      
(de bsp-b-atom ()

  (println 'bsp-b-atom)

  (let (Pair (get *ptr 'pair)
        Pt-Pair (lup-pt Pair)
        Back (find-back-any> *ptr Pair T)
        Pt-Back (lup-pt Back)
        Reg-Pair (if (== (car Back) Pair) 'a 'b)
        Tgt (cdr Pair)
        Pt-Tgt (get Pt-Pair 'b))

    (if (get Pt-Pair 'line)
        (prog
          (mov-line Pt-Pair Pt-Tgt)
          (del-line Pt-Pair)
          (lns> *ptr))
        (when (get Pt-Tgt 'line)
          (del-line Pt-Tgt)
          (lns> *ptr)))
          
    (set-ln-n> *ptr Pt-Tgt)
    
    (del> Pt-Pair T NIL)
    (put Pt-Pair 'b NIL)
    
    ###################
    
    (if (= Reg-Pair 'a)
    
        (prog
          (set> Pt-Back Tgt Pt-Tgt)
          (if (get Pt-Tgt 'line)
              (put @ 'p NIL)
              (put Pt-Tgt 'line
                (new '(+Line))))
          (pt> *ptr Back 'a Pt-Tgt))
        
        (prog
          # If Back is X pair and line, set p
          # If Back is Y pair, cdr must have line
          (when (get Pt-Back 'line)
            (if (lay-x?> Pt-Back)
                (when (get Pt-Tgt 'line)
                  (put @ 'p Pt-Back)
                  (put Pt-Back 'line 'n Pt-Tgt))
                (prog
                  (if (get Pt-Tgt 'line)
                      (put @ 'p Pt-Back)
                      (put Pt-Tgt 'line 
                        (new '(+Line) Pt-Back)))
                  (put Pt-Back 'line 'n Pt-Tgt))))
          (con> Pt-Back Tgt Pt-Tgt)
          (pt> *ptr Back 'b Pt-Tgt)))
          
      Pt-Pair))

    
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
          
      Pt-Pair))
    
    
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
          
      Pt-Pair))     
