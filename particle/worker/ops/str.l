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

(de make-str (Str Lay)
  
  (cond
    
    ######
    # ATOM
    
    ((or (a-atom?> *ptr)
         (a-nil?> *ptr))
      # Insert on Y pair
      # - More likely user will insert rather than replace
      # Replace on X pair
      (if (lay-y?> (lup-pt (get *ptr 'pair)))
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

  #(println 'repl-atom)
  
  (let (Pt-Pair (if (get *ptr 'pair) (lup-pt @) (: ref))
        Pair (get Pt-Pair 'any)
        Pt-Del (if (get *ptr 'pair)
                   (get Pt-Pair (get *ptr 'reg))
                   (lup-pt (get *ptr 'ref)))
        Pt-Str (gen-point Str 'x NIL T))

    (when (get Pt-Del 'line)
      (mov-line Pt-Del Pt-Str)
      (lns> *ptr))
    (set-ln-n> *ptr Pt-Str)
    
    (pt> *ptr Pair Reg Pt-Str)
    
    (if (= Reg 'a)
        (set> Pt-Pair Str Pt-Str T)
        (con> Pt-Pair Str Pt-Str T))
        
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
    (when (and (pair (cdr Pair))
               (str? (car @)))
      (sup> *ptr NIL)
      (nex> *ptr NIL)
      (sub> *ptr T))))


(de ins-b-atom (Str)

  #(println 'ins-b-atom)
  
  (let (Pair (get *ptr 'pair)
        Pt-Pair (lup-pt Pair)
        Pt-Cdr (get Pt-Pair 'b)
        Ls (list Str)
        Pt-Ls (gen-point Ls 'x NIL T))
    
    (when (get Pt-Cdr 'line)
      (mov-line Pt-Cdr Pt-Ls)
      (lns> *ptr))
    (set-ln-n> *ptr Pt-Ls)
  
    (con> Pt-Pair Ls)
    (con> Pt-Ls (get Pt-Cdr 'any) Pt-Cdr T)
          
    (lay-*start) 
    (mov-⇲ (c-lns> *ptr))
    
    (pt> *ptr Ls 'b Pt-Cdr)
    
    (top> Pt-Cdr)
    (draw> *ptr)))
        
        
(de ins-a-atom (Str)

  #(println 'ins-a-atom)
  
  (let (Pair (get *ptr 'pair)
        Pt-Pair (lup-pt Pair)
        Pt-Car (get Pt-Pair 'a)
        Ls (list Str)
        Pt-Ls (gen-point Ls 'x NIL T))
    
    ~(assert (get Pt-Car 'line))
    (mov-line Pt-Car Pt-Ls)
    (lns> *ptr)
    (set-ln-n> *ptr Pt-Ls)
    
    (set> Pt-Pair Ls Pt-Ls)
    (con> Pt-Ls (get Pt-Car 'any) Pt-Car T)
    
    #(pt> *ptr Ls 'b Pt-Car)
    (with *ptr
      (=: pair Ls)
      (=: reg 'b))
  
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
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
  
  #(println 'con-b-pair-x)
  
  (let ((C Pt-C Reg-C
         B Pt-B) (refs-1> *ptr)
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
                
    # ref does not change
                
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
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
         
  }#
  
  #(println 'con-b-pair-y)
  
  (let ((C Pt-C Reg-C
         B Pt-B) (refs-1> *ptr)
        Ls (list Str)
        Pt-Ls (gen-point Ls 'x NIL T))
    
    (con> Pt-B Ls Pt-Ls)
    (con> Pt-Ls C Pt-C T)
    
    (if (get Pt-B 'line)
        (put Pt-B 'line 'n Pt-Ls)
        (put Pt-B 'line (new '(+Line) NIL Pt-C)))
        
    (put Pt-Ls 'line (new '(+Line) Pt-B Pt-C))

    (when (get Pt-B 'exp)
      (put Pt-B 'line 'n NIL)
      (put Pt-Ls 'line 'p NIL))
    
    (put Pt-C 'line 'p Pt-Ls)
    
    (pt> *ptr Ls 'b)
    
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
        
    # Pop cur line = Pt-C
    # Push new list
    (lns> *ptr)
    (lns> *ptr Pt-Ls)
    
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
        Pt-Ls (gen-point Ls 'x NIL T))
    
    # handle X parent elsewhere
    ~(assert (lay-y?> Pt-B))
    
    (set> Pt-B Ls Pt-Ls)
    (con> Pt-Ls C Pt-C T)
        
    (put Pt-Ls 'line (new '(+Line) NIL Pt-C))
    
    (put Pt-C 'line 'p Pt-Ls)
        
    (pt> *ptr Ls 'a)
        
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
    # Pop cur line = Pt-C
    # Push new list
    (lns> *ptr)
    (lns> *ptr Pt-Ls)
    
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
    (con> Pt-Ls C Pt-C T)
        
    # Mov line
    (when (get Pt-C 'line)
    
      (mov-line Pt-C Pt-Ls)
      (lns> *ptr))
      
    (set-ln-n> *ptr Pt-Ls)
    
    # Keep ref - change rel
    (pt> *ptr C 'b)    
    
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
        
    (top> Pt-C)
    (draw> *ptr)))
    
