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
        Pt-Cell (lup-pt Cell)
        Pt-Del (if (get *ptr 'pair)
                   (get Pt-Cell (get *ptr 'reg))
                   (lup-pt (get *ptr 'ref)))
        Pt-Dat (gen-point Dat 'x NIL T))

    (when (get Pt-Del 'line)
      (mov-line Pt-Del Pt-Dat)
      (lns> *ptr))
    (mov-line-n> *ptr Pt-Dat)
    
    (if Dat
        (pt> *ptr Dat Reg)
        (pt> *ptr Cell Reg Pt-Dat))
    
    (if (= Reg 'a)
        (set> Pt-Cell Dat Pt-Dat T)
        (con> Pt-Cell Dat Pt-Dat T))
        
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
        Pt-Pair (lup-pt Pair)
        Pt-Del (get Pt-Pair 'b)
        List (list Dat)
        Pt-List (gen-point List 'x NIL T))
   
    (when (get Pt-Del 'line)
      (mov-line Pt-Del Pt-List)
      (lns> *ptr))
    (mov-line-n> *ptr Pt-List)
    
    (con> Pt-Pair List Pt-List)
        
    (pt> *ptr List 'b))
    
  (upd-tree)
  (mov-⇲ (c-lns> *ptr)))
        
        
(de ins-a-atom (Dat)

  #(println 'ins-a-atom)
  
  (let (Pair (get *ptr 'pair)
        Pt-Pair (lup-pt Pair)
        Pt-Del (get Pt-Pair 'b)
        List (list Dat)
        Pt-List (gen-point List 'x NIL T))
    
    (when (get Pt-Del 'line)
      (mov-line Pt-Del Pt-List)
      (lns> *ptr))
    (mov-line-n> *ptr Pt-List)
    
    (con> Pt-Pair List Pt-List)
        
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
        Pt-Cell (lup-pt Cell)
        Pre (if (get *ptr 'pair)
                @
                (find-back> *ptr (get *ptr 'reg)))
        Pt-Pre (lup-pt Pre)
        List-Dat (list Dat)
        Pt-Dat (gen-point List-Dat 'x NIL T))        
    
    # Insert before current object
    (con> Pt-Pre List-Dat NIL)
    (con> Pt-Dat Cell     NIL)
    
    # Mov line
    (when (get Pt-Cell 'line)
      (mov-line Pt-Cell Pt-Dat)
      (lns> *ptr))
    (mov-line-n> *ptr Pt-Dat)
                
    # ref does not change
                
    (upd-tree)
    (mov-⇲ (c-lns> *ptr))
    
    (top> Pt-Cell)
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
        Pt-Pair (lup-pt Pair)
        Pre (find-back> *ptr (get *ptr 'reg))
        Pt-Pre (lup-pt Pre)
        List (list Dat)
        Pt-List (gen-point List 'x NIL T))
    
    (con> Pt-Pre List Pt-List)
    (con> Pt-List Pair Pt-Pair)
    
    (if (get Pt-Pre 'line)
        (put Pt-Pre 'line 'n Pt-List)
        (put Pt-Pre 'line (new '(+Line) NIL Pt-Pair)))
        
    (put Pt-List 'line (new '(+Line) Pt-Pre Pt-Pair))

    (when (get Pt-Pre 'exp)
      (put Pt-Pre 'line 'n NIL)
      (put Pt-List 'line 'p NIL))
    
    (put Pt-Pair 'line 'p Pt-List)
    
    (pt> *ptr List 'b)
    
    (upd-tree)
    (mov-⇲ (c-lns> *ptr))
        
    # Pop cur line = Pt-Pair
    # Push new list
    (lns> *ptr)
    (lns> *ptr Pt-List)
    
    (top> Pt-List)
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
        Pt-Pair (lup-pt Pair)
        Pre (find-back> *ptr (get *ptr 'reg))
        Pt-Pre (lup-pt Pre)
        List (list Dat)
        Pt-List (gen-point List 'x NIL T))
    
    # handle X parent elsewhere
    ~(assert (lay-y?> Pt-Pre))
    
    (set> Pt-Pre List Pt-List)
    (con> Pt-List Pair Pt-Pair)
        
    (put Pt-List 'line (new '(+Line) NIL Pt-Pair))
    
    (put Pt-Pair 'line 'p Pt-List)
        
    (pt> *ptr List 'a)
        
    (upd-tree)
    (mov-⇲ (c-lns> *ptr))
    
    # Pop cur line = Pt-Pair
    # Push new list
    (lns> *ptr)
    (lns> *ptr Pt-List)
    
    (top> Pt-List)
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
        Pt-Pair (lup-pt Pair)
        Pre (find-back> *ptr (get *ptr 'reg))
        Pt-Pre (lup-pt Pre)
        List (list Dat)
        Pt-List (gen-point List 'x NIL T))
    
    (set> Pt-Pre List Pt-List)
    (con> Pt-List Pair Pt-Pair)
        
    # Mov line
    (when (get Pt-Pair 'line)
      (mov-line Pt-Pair Pt-List)
      (lns> *ptr))
    (mov-line-n> *ptr Pt-List)
    
    # Keep ref - change rel
    (pt> *ptr Pair 'b)    
    
    (upd-tree)
    (mov-⇲ (c-lns> *ptr))
        
    (top> Pt-Pair)
    (draw> *ptr)))
    
