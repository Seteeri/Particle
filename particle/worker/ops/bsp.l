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
                  ((or (a-atom?> *ptr)
                       (b-atom?> *ptr))
                   (bsp-atom))
                  ((or (a-pair?> *ptr)
                       (b-pair?> *ptr))
                   (bsp-pair)))
    
    # Mov to cmd/caller?
    
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
    (top> Pt-Pair)
    (draw> *ptr)))


(de bsp-atom ()

  (let ((C Pt-C Reg-C
         B Pt-B Reg-B
         A Pt-A) (refs-2> *ptr))
    
    (if (a-atom?> *ptr)
        (bsp-a-atom Pt-C Pt-B Pt-A (= Reg-B 'b))
        (bsp-b-atom Pt-C Pt-B Pt-A))
        
    (if (= Reg-B 'a)
    
        (prog
          (set> Pt-A C Pt-C)
          (if (get Pt-C 'line)
              (put @ 'p NIL)
              (put Pt-C 'line
                (new '(+Line))))
          (pt> *ptr A 'a Pt-C))
    
        (prog
          # If A is X pair and line, set p
          # If A is Y pair, cdr must have line
          (when (get Pt-A 'line)
            (if (lay-x?> Pt-A)
                (if (sym? C)
                    (if (get Pt-C 'line)
                        (put @ 'p NIL)
                        (put Pt-C 'line (new '(+Line) Pt-A)))
                    (when (get Pt-C 'line)
                      (put @ 'p Pt-A)
                      (put Pt-A 'line 'n Pt-C)))
                (prog
                  (if (get Pt-C 'line)
                      (put @ 'p Pt-A)
                      (put Pt-C 'line 
                        (new '(+Line) Pt-A)))
                  (put Pt-A 'line 'n Pt-C))))
          (con> Pt-A C Pt-C)
          (pt> *ptr A 'b Pt-C)))
          
      Pt-B))
  
  
(de bsp-a-atom (Pt-C Pt-B Pt-A B-Cdr)

  (when (get Pt-B 'line)
  
    (del-line Pt-B)
    
    (when (and B-Cdr
               (get Pt-A 'line))
      (put @ 'n NIL))
      
    (lns> *ptr)
    (lns> *ptr))
    
  (set-ln-n> *ptr Pt-C)
      
  (del> Pt-B NIL T))
     
      
(de bsp-b-atom (Pt-C Pt-B Pt-A)
  
  (if (get Pt-B 'line)
  
      (prog
        (mov-line Pt-B Pt-C)
        (del-line Pt-B)
        (lns> *ptr))
        
      (when (get Pt-C 'line)
        (del-line Pt-C)
        (lns> *ptr)))
        
  (set-ln-n> *ptr Pt-C)
  
  (del> Pt-B T NIL))


#################    
    
(de bsp-pair ()    
    
  (let ((C Pt-C Reg-C
         B Pt-B Reg-B
         A Pt-A) (refs-2> *ptr))
          
    (when (get Pt-B 'line)
    
      (del-line Pt-B)
      (lns> *ptr)
      (lns> *ptr))
      
    (set-ln-n> *ptr Pt-C)         
          
    (if (a-pair?> *ptr)
        (del> Pt-B NIL T)
        (del> Pt-B T NIL))
          
    (if (= Reg-B 'a)
    
        (prog
          (set> Pt-A C Pt-C)
          (if (get Pt-C 'line)
              (put @ 'p NIL)
              (put Pt-C 'line
                (new '(+Line))))
          (put *ptr 'reg 'a))
    
        (prog
          # If A is X pair and line, set p
          # If A is Y pair, cdr must have line
          (when (get Pt-A 'line)
            (if (lay-x?> Pt-A)
                (when (get Pt-C 'line)
                  (put @ 'p Pt-A)
                  (put Pt-A 'line 'n Pt-C))
                (prog
                  (if (get Pt-C 'line)
                      (put @ 'p Pt-A)
                      (put Pt-C 'line 
                        (new '(+Line) Pt-A)))
                  (put Pt-A 'line 'n Pt-C))))
          (con> Pt-A C Pt-C)
          (put *ptr 'reg 'b)))
          
      Pt-C))
