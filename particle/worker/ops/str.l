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
      # Replace on X pair
      (if (lay-y?> (lup-pt (get *ptr 'pair)))
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
        List (list Str)
        Pt-List (gen-point List 'x NIL T))
    
    (when (get Pt-Cdr 'line)
      (mov-line Pt-Cdr Pt-List)
      (lns> *ptr))
    (set-ln-n> *ptr Pt-List)
  
    (con> Pt-Pair List)
    (con> Pt-List (get Pt-Cdr 'any) Pt-Cdr T)
          
    (lay-*start) 
    (mov-⇲ (c-lns> *ptr))
    
    (pt> *ptr List 'b Pt-Cdr)
    
    (top> Pt-Cdr)
    (draw> *ptr)))
        
        
(de ins-a-atom (Str)

  #(println 'ins-a-atom)
  
  (let (Pair (get *ptr 'pair)
        Pt-Pair (lup-pt Pair)
        Pt-Car (get Pt-Pair 'a)
        List (list Str)
        Pt-List (gen-point List 'x NIL T)
        Pt-Cdr (get Pt-List 'b))
    
    ~(assert (get Pt-Car 'line))
    (mov-line Pt-Car Pt-List)
    (lns> *ptr)
    (set-ln-n> *ptr Pt-List)
    
    (set> Pt-Pair List Pt-List T)
    
    (pt> *ptr List 'b Pt-Cdr)
  
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
    (top> Pt-Cdr)
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
  
  (let (Pair (get *ptr 'ref)
        Pt-Pair (lup-pt Pair)
        Pre (if (get *ptr 'pair)
                @
                (find-back> *ptr (get *ptr 'reg)))
        Pt-Pre (lup-pt Pre)
        List (list Str)
        Pt-List (gen-point List 'x NIL T))
    
    # Insert before current object
    (con> Pt-Pre List)
    (con> Pt-List Pair Pt-Pair T)
    
    # Mov line
    (when (get Pt-Pair 'line)
      (mov-line Pt-Pair Pt-List)
      (lns> *ptr))
    (set-ln-n> *ptr Pt-List)
                
    # ref does not change
                
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
    (top> Pt-Pair)
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
  
  (let (Pair (get *ptr 'ref)
        Pt-Pair (lup-pt Pair)
        Pre (find-back> *ptr (get *ptr 'reg))
        Pt-Pre (lup-pt Pre)
        List (list Str)
        Pt-List (gen-point List 'x NIL T))
    
    (con> Pt-Pre List Pt-List)
    (con> Pt-List Pair Pt-Pair T)
    
    (if (get Pt-Pre 'line)
        (put Pt-Pre 'line 'n Pt-List)
        (put Pt-Pre 'line (new '(+Line) NIL Pt-Pair)))
        
    (put Pt-List 'line (new '(+Line) Pt-Pre Pt-Pair))

    (when (get Pt-Pre 'exp)
      (put Pt-Pre 'line 'n NIL)
      (put Pt-List 'line 'p NIL))
    
    (put Pt-Pair 'line 'p Pt-List)
    
    (pt> *ptr List 'b)
    
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
        
    # Pop cur line = Pt-Pair
    # Push new list
    (lns> *ptr)
    (lns> *ptr Pt-List)
    
    (top> Pt-List)
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
  
  (let (Pair (get *ptr 'ref)
        Pt-Pair (lup-pt Pair)
        Pre (find-back> *ptr (get *ptr 'reg))
        Pt-Pre (lup-pt Pre)
        List (list Str)
        Pt-List (gen-point List 'x NIL T))
    
    # handle X parent elsewhere
    ~(assert (lay-y?> Pt-Pre))
    
    (set> Pt-Pre List Pt-List)
    (con> Pt-List Pair Pt-Pair T)
        
    (put Pt-List 'line (new '(+Line) NIL Pt-Pair))
    
    (put Pt-Pair 'line 'p Pt-List)
        
    (pt> *ptr List 'a)
        
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
    
    # Pop cur line = Pt-Pair
    # Push new list
    (lns> *ptr)
    (lns> *ptr Pt-List)
    
    (top> Pt-List)
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
  
  (let (Pair (get *ptr 'ref)
        Pt-Pair (lup-pt Pair)
        Pre (find-back> *ptr (get *ptr 'reg))
        Pt-Pre (lup-pt Pre)
        List (list Str)
        Pt-List (gen-point List 'x NIL T))
    
    (set> Pt-Pre List Pt-List)
    (con> Pt-List Pair Pt-Pair T)
        
    # Mov line
    (when (get Pt-Pair 'line)
      (mov-line Pt-Pair Pt-List)
      (lns> *ptr))
    (set-ln-n> *ptr Pt-List)
    
    # Keep ref - change rel
    (pt> *ptr Pair 'b)    
    
    (lay-*start)
    (mov-⇲ (c-lns> *ptr))
        
    (top> Pt-Pair)
    (draw> *ptr)))
    
