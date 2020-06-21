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

(de make-pair-str (Dat Lay)
    
  (cond
    
    ######
    # ATOM
    # - atom for atom
    # - repl atom except NIL, ins like pair
    # - use swap-reg to set cdr
    
    ((or (a-atom?> *ptr)
         (a-nil?> *ptr))
      #(println 'make-pair-str "A ATOM/NIL")
      # repl str for both x/y
      # if user wanted a pair they can use other cmd
      (repl-atom Dat 'a))
      
    ((or (b-nil?> *ptr)
         (b-atom?> *ptr))
      #(println 'make-pair-str "B ATOM/NIL")
      #(repl-atom Dat 'b)
      (ins-b-atom Dat)
      (nex> *ptr T))
            
    ######
    # PAIR
    # - pair for pair
    # - ins bef ptr

    ((a-pair?> *ptr)
      #(println 'make-pair-str "A PAIR")
      
      (if (lay-x?> *ptr)
          (con-a-pair-x Dat)
          (con-a-pair-y Dat)))
      
    ((b-pair?> *ptr)
      #(println 'make-pair-str "B PAIR")
      
      (if (lay-x?> *ptr)
          (con-b-pair-x Dat)
          (con-b-pair-y Dat)))
      
    (T
      # root
      (println 'make-pair-str "T"))))
  
  
(de repl-atom (Dat Reg)

  #{
  
    Handles both X and Y pairs
  
    Need to move to next car
    
  }#

  #(println 'repl-atom)
  
  (let (Cell (if (get *ptr 'pair)
                 @
                 (find-back> *ptr (get *ptr 'reg)))
        Po-Cell (lup-pt Cell)
        Po-Del (if (get *ptr 'pair)
                   (get Po-Cell (get *ptr 'reg))
                   (lup-pt (get *ptr 'ref)))
        Po-Dat (gen-point Dat 'x NIL T))

    (when (get Po-Del 'line)
      (mov-line Po-Del Po-Dat)
      (lns> *ptr))
    (mov-line-n> *ptr Po-Dat)
    
    (if Dat
        (pt> *ptr Dat Reg)
        (pt> *ptr Cell Reg Po-Dat))
    
    (if (= Reg 'a)
        (set> Po-Cell Dat Po-Dat T)
        (con> Po-Cell Dat Po-Dat T))
        
    (upd-tree)
    (mov-⇲ (c-lns> *ptr))
    
    (when (str? (cadr Cell))
      (sup> *ptr NIL)
      (nex> *ptr NIL)
      (sub> *ptr T))))


(de ins-b-atom (Dat)

  # This will not keep cdr if non-NIL
  # since it is deleted

  #(println 'ins-b-atom)
  
  (let (Pair (get *ptr 'pair)
        Po-Pair (lup-pt Pair)
        Po-Del (get Po-Pair 'b)
        List (list Dat)
        Po-List (gen-point List 'x NIL T))
   
    (when (get Po-Del 'line)
      (mov-line Po-Del Po-List)
      (lns> *ptr))
    (mov-line-n> *ptr Po-List)
    
    (con> Po-Pair List Po-List)
        
    (pt> *ptr List 'b))
    
  (upd-tree)
  (mov-⇲ (c-lns> *ptr)))
        
        
(de ins-a-atom (Dat)

  #(println 'ins-a-atom)
  
  (let (Pair (get *ptr 'pair)
        Po-Pair (lup-pt Pair)
        Po-Del (get Po-Pair 'b)
        List (list Dat)
        Po-List (gen-point List 'x NIL T))
    
    (when (get Po-Del 'line)
      (mov-line Po-Del Po-List)
      (lns> *ptr))
    (mov-line-n> *ptr Po-List)
    
    (con> Po-Pair List Po-List)
        
    (pt> *ptr List 'b))
  
  (upd-tree)
  (mov-⇲ (c-lns> *ptr)))          
        

(de con-b-pair-x (Dat)
  
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
  
  (let (Cell (get *ptr 'ref)
        Po-Cell (lup-pt Cell)
        Pre (if (get *ptr 'pair)
                @
                (find-back> *ptr (get *ptr 'reg)))
        Po-Pre (lup-pt Pre)
        List-Dat (list Dat)
        Po-Dat (gen-point List-Dat 'x NIL T))        
    
    # Insert before current object
    (con> Po-Pre List-Dat NIL)
    (con> Po-Dat Cell     NIL)
    
    # Mov line
    (when (get Po-Cell 'line)
      (mov-line Po-Cell Po-Dat)
      (lns> *ptr))
    (mov-line-n> *ptr Po-Dat)
                
    # ref does not change
                
    (upd-tree)
    (mov-⇲ (c-lns> *ptr))
    
    (top> Po-Cell)
    (draw> *ptr)))


(de con-b-pair-y (Dat)
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
  
  (let (Pair (get *ptr 'ref)
        Po-Pair (lup-pt Pair)
        Pre (find-back> *ptr (get *ptr 'reg))
        Po-Pre (lup-pt Pre)
        List (list Dat)
        Po-List (gen-point List 'x NIL T))
    
    (con> Po-Pre List Po-List)
    (con> Po-List Pair Po-Pair)
    
    (if (get Po-Pre 'line)
        (put Po-Pre 'line 'n Po-List)
        (put Po-Pre 'line (new '(+Line) NIL Po-Pair)))
        
    (put Po-List 'line (new '(+Line) Po-Pre Po-Pair))

    (when (get Po-Pre 'exp)
      (put Po-Pre 'line 'n NIL)
      (put Po-List 'line 'p NIL))
    
    (put Po-Pair 'line 'p Po-List)
    
    (pt> *ptr List 'b)
    
    (upd-tree)
    (mov-⇲ (c-lns> *ptr))
        
    # Pop cur line = Po-Pair
    # Push new list
    (lns> *ptr)
    (lns> *ptr Po-List)
    
    (top> Po-List)
    (draw> *ptr)))

    
    
(de con-a-pair-y (Dat)
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
  
  (let (Pair (get *ptr 'ref)
        Po-Pair (lup-pt Pair)
        Pre (find-back> *ptr (get *ptr 'reg))
        Po-Pre (lup-pt Pre)
        List (list Dat)
        Po-List (gen-point List 'x NIL T))
    
    # handle X parent elsewhere
    ~(assert (lay-y?> Po-Pre))
    
    (set> Po-Pre List Po-List)
    (con> Po-List Pair Po-Pair)
        
    (put Po-List 'line (new '(+Line) NIL Po-Pair))
    
    (put Po-Pair 'line 'p Po-List)
        
    (pt> *ptr List 'a)
        
    (upd-tree)
    (mov-⇲ (c-lns> *ptr))
    
    # Pop cur line = Po-Pair
    # Push new list
    (lns> *ptr)
    (lns> *ptr Po-List)
    
    (top> Po-List)
    (draw> *ptr)))


(de con-a-pair-x (Dat)
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
  
  (let (Pair (get *ptr 'ref)
        Po-Pair (lup-pt Pair)
        Pre (find-back> *ptr (get *ptr 'reg))
        Po-Pre (lup-pt Pre)
        List (list Dat)
        Po-List (gen-point List 'x NIL T))
    
    (set> Po-Pre List Po-List)
    (con> Po-List Pair Po-Pair)
        
    # Mov line
    (when (get Po-Pair 'line)
      (mov-line Po-Pair Po-List)
      (lns> *ptr))
    (mov-line-n> *ptr Po-List)
    
    # Keep ref - change rel
    (pt> *ptr Pair 'b)    
    
    (upd-tree)
    (mov-⇲ (c-lns> *ptr))
        
    (top> Po-Pair)
    (draw> *ptr)))
    
