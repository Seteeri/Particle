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
    
    (top-ref> *ptr)
    (draw> *ptr)))


(de bsp-atom ()

  (let ((C Pt-C Reg-C
         B Pt-B Reg-B
         A Pt-A) (refs-2> *ptr))
    
    (when T
      (prinl)
      (println 'bsp-atom C Pt-C Reg-C)
      (println 'bsp-atom B Pt-B Reg-B)
      (println 'bsp-atom A Pt-A))
    
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
          (put *ptr 'any A))
    
        (prog
          # If A is X pair and line, set p
          # If A is Y pair, cdr must have line
          (when (get Pt-A 'line)
            (if (lay-x?> Pt-A)
                (if (sym? A)
                    (if (get Pt-C 'line)
                        (put @ 'p NIL)
                        (put Pt-C 'line (new '(+Line))))
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
          (if (= Reg-C 'a)
              (with *ptr
                (=: any A)
                (=: reg 'b))
              (put *ptr 'any A))))
          
      Pt-B))
  
  
(de bsp-a-atom (Pt-C Pt-B Pt-A B-Cdr)

  (when (get Pt-B 'line)
  
    (del-line Pt-B)
    
    (when B-Cdr
      (when (get Pt-A 'line)
        (put @ 'n NIL)))
      
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
    
   # opt: del line or keep it 
    
  (let ((C Pt-C Reg-C
         B Pt-B Reg-B
         A Pt-A) (refs-2> *ptr))

    (when T
      (prinl)
      (println 'bsp-pair C Pt-C Reg-C)
      (println 'bsp-pair B Pt-B Reg-B)
      (println 'bsp-pair A Pt-A))         
         
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
          (put *ptr 'any A)
          (put *ptr 'reg 'a))
    
        (prog
          # If A is X pair and line, set p
          # If A is Y pair, cdr must have line
          (when (get Pt-A 'line)
            (if (lay-x?> Pt-A)
                (if (sym? A)
                    (if (get Pt-C 'line)
                        (put @ 'p NIL)
                        (put Pt-C 'line (new '(+Line))))
                    (when (get Pt-C 'line)
                      (put @ 'p Pt-A)
                      (put Pt-A 'line 'n Pt-C)))
                (prog
                  (if (get Pt-C 'line)
                      (put @ 'p Pt-A)
                      (put Pt-C 'line 
                        (new '(+Line) Pt-A)))
                  (put Pt-A 'line 'n Pt-C))))
          # handle y car top left
          (con> Pt-A C Pt-C)
          (put *ptr 'any A)
          (put *ptr 'reg 'b)))
          
      Pt-C))
