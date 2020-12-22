(de mak-str-xa-x (Str) (con-a-pair-x Str))
(de mak-str-xb-x (Str) (con-b-pair-x Str))

(de mak-str-xa-y (Str) (con-a-pair-x Str))
(de mak-str-xb-y (Str) (con-b-pair-x Str))


(de mak-str-ya-x (Str) (con-a-pair-y Str))
(de mak-str-yb-x (Str) (con-b-pair-y Str))

(de mak-str-ya-y (Str) (con-a-pair-y Str))
(de mak-str-yb-y (Str) (con-b-pair-y Str))


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
    
