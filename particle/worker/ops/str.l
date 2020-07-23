#{

      [X]
      .
            *
      [Y]  NIL
      
      .

  -->

      [X]
      .
                  *
      [Y]  [X]  NIL
            .
            
      .    
  
}#


(de make-str-xp (Str)
  
  # Refactor this cond into a ptr method
  # i.e. any>
  # returns a Point which a method can be called

  (let ((Pt-Pair Pt-Atom) (get-ref-pt> *ptr))
  
    (println Pt-Pair Pt-Atom)
  
    (if Pt-Atom
        (con-pair-str> @ Pt-Pair Str)
        (con-pair-str> Pt-Pair Str))))


(de make-str (Str Lay)
        
  (cond
    
    ######
    # ATOM
    
    ((or (a-atom?> *ptr)
         (a-nil?> *ptr))
      # Insert on Y pair
      # - More likely user will insert rather than replace
      # Replace on X pair
      (if (lay-y?> *ptr)
          # Add tests:
          (ins-a-atom Str)
          (repl-atom Str 'a)))
      
    ((or (b-nil?> *ptr)
         (b-atom?> *ptr))
      # To replace b atom, use alt-reg
      (ins-b-atom Str))
            
    ######
    # PAIR
    # - pair for pair
    # - ins bef ptr
    
    ((a-pair?> *ptr)
      (if (lay-x?> *ptr)
          (con-a-pair-x Str)
          (con-a-pair-y Str)))
      
    ((b-pair?> *ptr)
      (if (lay-x?> *ptr)
          (con-b-pair-x Str)
          (con-b-pair-y Str)))))
      
  
(de repl-atom (Str Reg)

  #{
  
    Handles both X and Y pairs
  
    Need to move to next car
    
  }#

  (println 'repl-atom)
  
  (let ((Pt-Pair Pt-Del) (get-ref-pt> *ptr)
        Pair (get Pt-Pair 'any)
        #Pt-Del (if (get *ptr 'pair)
        #           (get Pt-Pair (get *ptr 'reg))
        #           (lup-pt (get *ptr 'atom)))
        Pt-Str (gen-point Str 'x NIL T))

    (when (get Pt-Del 'line)
      (mov-line Pt-Del Pt-Str)
      (lns> *ptr))
    (set-ln-n> *ptr Pt-Str)
    
    (if (= Reg 'a)
        (set> Pt-Pair Str Pt-Str T)
        (con> Pt-Pair Str Pt-Str T))
    
    (pt> *ptr Pair Reg T T)
    
    (lay-*start)
    
    (when (and (pair Pair)
               (pair (cdr Pair))
               (str? (car @)))
      (sup> *ptr NIL)
      (nex> *ptr NIL)
      (sub> *ptr T))))


(de ins-b-atom (Str)

  #{
  
    Connect new list cdr to cur atom
    Update pointer before updating cur pair
    Then connect new list to cur pair
    Draw main
    Mov cur to ptr
    Draw ptr
    
  }#

  (println 'ins-b-atom)
  
  (let ((Pt-Pair Pt-Reg) (get-ref-pt> *ptr)
        Pt-Cdr (get Pt-Pair 'b)
        Ls (list Str)
        Pt-Ls (gen-point Ls 'x NIL T))
    
    (when (get Pt-Cdr 'line)
      (mov-line Pt-Cdr Pt-Ls)
      (lns> *ptr))
    (set-ln-n> *ptr Pt-Ls)

    (con> Pt-Ls (get Pt-Cdr 'any) Pt-Cdr T)
    
    # Point to cdr of new list
    # aka cur atom
    (pt> *ptr Ls 'b T)
    
    (con> Pt-Pair Ls)
    
    (lay-*start)
    
    (top> Pt-Cdr)
    (draw> *ptr)))
        
        
(de ins-a-atom (Str)

  (println 'ins-a-atom)
  
  (let ((Pt-Pair Pt-Reg) (get-ref-pt> *ptr)
        Pt-Car (get Pt-Pair 'a)
        Ls (list Str)
        Pt-Ls (gen-point Ls 'x NIL T))
    
    ~(assert (get Pt-Car 'line))
    (mov-line Pt-Car Pt-Ls)
    (lns> *ptr)
    (set-ln-n> *ptr Pt-Ls)
    
    (set> Pt-Pair Ls Pt-Ls)
    (con> Pt-Ls (get Pt-Car 'any) Pt-Car T)
    
    (with *ptr
      (=: any Ls)
      (=: reg 'b))
  
    (lay-*start)
    
    (top> Pt-Car)
    (draw> *ptr)))
        

(de con-b-pair-x (Str)
  
  #{
    
    * If X-Pair on newline (start of line), 
    mov newline to new Pair
    
    In:

                   *
        [X]  [X]  [X]  .
         .    .    .
          
    Out:

                        *
        [X]  [X]  [X]  [X]  .
         .    .   ..    .
      
    Repeat:

                             *
        [X]  [X]  [X]  [X]  [X]  .
         .    .   ..   ...   .


    In:

              *
        [Y]  [X1] .
              .
            
         .
        
    Out:

                    *
        [Y]  [X2]  [X1] .
              .     .
            
         .
      
  }#
  
  (println 'con-b-pair-x)
  
  (let ((C Pt-C Reg-C
         B Pt-B) (refs-2> *ptr)
        Ls (list Str)
        Pt-Ls (gen-point Ls 'x NIL T))
    
    # Insert before current object
    (con> Pt-B Ls)
    (con> Pt-Ls C Pt-C T)
    
    # Mov line
    (when (get Pt-C 'line)
      (mov-line Pt-C Pt-Ls)
      (lns> *ptr))
      
    (set-ln-n> *ptr Pt-Ls)
                
    (lay-*start)
    
    # dont use pt since it will set prev col to sel
    # update any
    (put *ptr 'any Ls)
    
    (top> Pt-C)
    (draw> *ptr)))


(de con-b-pair-y (Str)
  #{
  
    Case 1: (1 2) -> (1 3 2)
        
        [X1]
         .
      
         *
        [Y]  .
            
         .
            
    -->
               
        [X1]
         .
      
         *
        [X2]
         .
         
        [Y]  .
            
         .
    
    Put X2 after X1
    
  }#
  
  (println 'con-b-pair-y)
  
  (let ((C Pt-C Reg-C
         B Pt-B) (refs-1> *ptr)
        Ls (list Str)
        Pt-Ls (gen-point Ls 'x NIL))
    
    # B -> Ls -> C
    #
    # B = poss
    # Ls = yes
    # C = yes
        
    (con> Pt-B Ls Pt-Ls NIL)
    (con> Pt-Ls C Pt-C 'del)
    
    # Con lines
    
    (when (get Pt-B 'line)
      (put @ 'n Pt-Ls))
    
    (with (get Pt-Ls 'line)
      (=: p (cadr (get *ptr 'lns)))
      (=: n Pt-C))

    (put Pt-C 'line 'p Pt-Ls)      
      
    # Expanded symbol
    # Handle this better...call method
    (when (get Pt-B 'exp)
      (put Pt-B 'line 'n NIL)
      (put Pt-Ls 'line 'p NIL))
        
    # Pop cur line (Pt-C), push new list
    (lns> *ptr) (lns> *ptr Pt-Ls) 
    
    (lay-*start)    
    
    # Need new fn
    (uncol> *ptr Pt-C T)
    (pt> *ptr B 'b T T)
    
    (top> Pt-Ls)
    (draw> *ptr)))


(de con-a-pair-y (Str)
  #{
  
    Case 1: ((1)) -> ((2 1))

              *
        [Y]  [Y] .
              .
            
        .
            
    -->
      
              *
        [Y]  [X]
              .
        
             [Y] .
              .
            
        .  
        
  }#
  
  (let ((C Pt-C Reg-C
         B Pt-B) (refs-1> *ptr)
        Ls (list Str)
        Pt-Ls (gen-point Ls 'x NIL))
        
    (set> Pt-B Ls Pt-Ls)
    (con> Pt-Ls C Pt-C 'del)
        
    (with (get Pt-Ls 'line)
      (=: p NIL)
      (=: n Pt-C))
    
    (put Pt-C 'line 'p Pt-Ls)
                
    # Pop cur line (Pt-C), push new list
    (lns> *ptr) (lns> *ptr Pt-Ls)        
        
    (pt> *ptr Ls 'a)
        
    (lay-*start)    
    (top> Pt-Ls)
    (draw> *ptr)))


(de con-a-pair-x (Str)
  #{
          
    Case 2: ((1)) -> ((2 1))

              *
        [Y]  [X1]  .
              .
            
        .
            
    -->
      
                    *
        [Y]  [X2]  [X1]  .
              .     .
            
        .  
        
  }#
  
  (let ((C Pt-C Reg-C
         B Pt-B) (refs-1> *ptr)
        Ls (list Str)
        Pt-Ls (gen-point Ls 'x NIL T))
    
    (set> Pt-B Ls Pt-Ls)
    (con> Pt-Ls C Pt-C 'del)
    
    # Mov line from C -> Ls
    (when (get Pt-C 'line)    
      (mov-line Pt-C Pt-Ls)
      (lns> *ptr))
    (set-ln-n> *ptr Pt-Ls)        
        
    # Keep ref - change rel
    (put *ptr 'reg 'b)
        
    (lay-*start)
    (top> Pt-C)
    (draw> *ptr)))
    
