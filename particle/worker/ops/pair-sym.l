(de make-pair-str (Data Lay)

  (let (Ref (get *ptr 'b)
        Po-Fn 
          (if *on-car
              (let Po (gen-point Data Lay NIL T)
                (if (is-end?> Ref)
                    (cons (repl-cdr Ref Po) repl-cdr-post)
                    (cons (repl-car Ref Po) repl-car-post)))
              (let Po (gen-point (list Data) Lay (cons NIL T) T)
                (if (is-car?> Ref)
                    (cons (ins-pair-car Ref Po) ins-pair-car-post)
                    (cons (ins-pair-cdr Ref Po) ins-pair-cdr-post)))))
                    
    (upd-tree)
    (mov-⇲ *line)
    
    ((cdr Po-Fn) (car Po-Fn))
    (lay-ptr *ptr)
    
    (car Po-Fn)))

    
(de repl-car-post (Po) 
  (set-ptr *ptr (sub> (get Po 'c 'b))))
  
  
(de repl-cdr-post (Po)
  (set-ptr *ptr (sub> Po)))
  
  
(de ins-pair-car-post (Po)
  (println Po (get (car Po) 'data) (get (car Po) 'b))
  (set-ptr *ptr (if (cdr Po)
                        (sup> (car Po))
                        (top> (get (car Po) 'b)))))
  
  
(de ins-pair-cdr-post (Po) 
  (set-ptr *ptr (top> (get Po 'b))))
  

(de repl-cdr (Ref Po)
  #{
              *
    [ ]  [ ]  NIL
     A    B
     
    ->
    
    [ ]  [ ]  T
     A    B    
  
    Or
    
              *
    [ ]  [ ]  NIL
     A    B
     
    ->
    
    [ ]  [ ]  [ ]
     A    B    C  
  }#

  (println 'repl-cdr)
  
  (let (C (get Ref 'c)
        Cb (get Ref 'b))

    (b@ C Po Cb)

    (when (get Ref 'line)
      (mov-line Ref Po))
    
    # Cdr is still used so don't del
    (del> Ref T NIL)

    Po))

    
(de repl-car (Ref Po)
  #{
    Replace car of y pair
        
    ---
        
    [C]  [P]  [B]
    a    b    c
         *

    
    [ ]  [ ]  [ ]
    a    x    c
         *

    ---
    
    [Ccc]
    a    
    
    [Cc]  [C]  [ ]  [ ]
               x    c
               *        

  }#

  (println 'repl-car)
  
  (let (Pair (get Ref 'c)
        C (get Pair 'c))

    (a> Pair Po)
    
    (cond ((of-cdr?> C Pair)
           (b> C Pair))
          ((of-car?> C Pair)
           (a> C Pair))
          (T
           (quit "repl-car: invalid pair")))
          
    (del> Ref T T)
        
    Po))  

    
(de ins-pair-cdr (Ref Po)
  #{
    Insert after ptr/pair (rel to Ref 'b 'c)
    or
    Insert before ptr/pair
    
          *
    [ ]  [ ]  ...
    a    c    

    ->
    
               *
    [ ]  [ ]  [ ]  ...
    a    b    c                 
    
    ---
    
    Special case: Insert before ptr/list
    
    [ ]  [ ]
    a    b
    
    *
    [Y]  [ ]  NIL
         c
         
    NIL
    
    - Normally Po would be right of B which is awkward,
    so ins Po as newline before Y
    - Or start newline and move Y to newline
    
  }#
  
  (println 'ins-pair-cdr)
  
  (let (Pair (get Ref 'c))

    (b@ Pair Po Ref)
    
    # Make line as necessary if car is a list
    (if (and (has-pair?> Ref)
             (is-pair? Ref))
        (make-line Ref)
        (when (get Ref 'line)
          (mov-line Ref Po)))

    Po))
    

(de ins-pair-car (Ref Po)

  #{
  
    Insert item before ptr

    [X]
    
          *
    [Y]  [ ] ...
          X
    NIL
    
    ->
    
    [X]
    
               *
    [Y]  [ ]  [ ] ...
          Z    X
    NIL    
    
    -------------------
    
          *
    [Y]  [Y]  ...
              
         NIL
    NIL
    
    ->
          *
    [Y]  [ ]
         a
         
         [Y]  ...
              
         NIL
         
    NIL
    
  }#
    
  (println 'ins-pair-car)
    
  (let (Pair (get Ref 'c)
        Is-Ref-Pair-of-Pair (and (is-pair? Pair)
                                 (has-pair?> Ref)))

    (a> Pair Po)                                 
                                 
    (b> Po Ref)
                  
    # assert Ref has a line
                  
    (mov-line Ref Po)        
          
    (when Is-Ref-Pair-of-Pair
      # Create additional line for Ref
      # Ins after current line (Po)
      (put Ref 'line (new '(+Line)
                          Po
                          (get Ref 'b)
                          (if (get *line 'line 'l) @ *line)))
      (put *line 'line 'n Ref))
        
    (cons Po Is-Ref-Pair-of-Pair)))
    

#########################################
    
    
(de repl-list-str (Sta End Data Skip-Upd)

  # Replace list/line of strings with single string
  
  (let (Prv (get Sta 'c)
        Po (gen-point (list Data) 'x NIL T))
    
    # If car of pair
    (if (of-car?> Prv Sta)
        (a> Prv Po)
        (b> Prv Po))

    # Po <-> End'b
    (let (Eb (get End 'b)
          Pb (get Po 'b))
      (when Eb
        (when (get Pb 'line) (mov-line Eb Pb))
        (del> Pb T T)
        (b> Po Eb)))
    
    (when (get Sta 'line)
      (mov-line Sta Po))
    
    # Inclusive
    (del-rx Sta End)
    
    (set-ptr *ptr (get Po 'b))
    
    # Later, move this group to undo/cmd list
    (unless Skip-Upd
      (upd-tree)
      (mov-⇲ *line)        
      
      # Put Ptr above new point
      (mov-cur-abv (get Po 'b)))))


(de repl-str-list (Ref List Skip-Upd)

  # Replace single string with list/line of strings
  
  (let (Sta (get Ref 'c)
        End (get Ref 'b)
        Prv NIL)
  
    # Add list, item by item
    # Do car, then cdr

    (let (Data (car List)
          Po (gen-point (list Data) 'x (cons NIL T) T))
          
      (if (of-car?> Sta Ref)
          (a> Sta Po)
          (b> Sta Po))
  
      (mov-line Ref Po)
  
      (setq Prv Po))
    
    (for Data (cdr List)
    
      (let Po (gen-point (list Data) 'x (cons NIL T) T)
        
        (b> Prv Po)
        
        (setq Prv Po)))

    # Con last Po to existing cdr
    (b> Prv End)        
    
    # Del Ref except Cdr
    (del-car> Ref)    
    
    (set-ptr *ptr End)
    
    (unless Skip-Upd
      (upd-tree)
      (mov-⇲ *line)        
              
      # Put Ptr above new point
      (mov-cur-abv End))))

      
(de build-str (Start)
  # Go prv until Sym, non-Str or newline
  (let (P Start
        B NIL
        D NIL)
        
    (while (and (has-str?> P)
                (not (is-sym? P))
                (not (of-car?> P B))
                (not (get P 'line)))
      
      (push 'D (get P 'a 'data))
      
      (setq B P)
      (setq P (get P 'c)))
    
    (if (and (has-str?> P)
             (not (is-sym? P))
             (not (of-car?> P B)))
      (push 'D (get P 'a 'data))
      (setq P (get P 'b)))
    
    (list P D)))

    
(de build-str-sp (Start)
  # Go prv until sp, non-str or line
  (let (P Start
        B NIL
        D NIL)
        
    (while (and (has-str?> P)
                (not (= (get P 'a 'data) " ")) # what about multiple spaces?
                (not (is-sym? P))
                (not (of-car?> P B))
                (not (get P 'line)))
      
      (push 'D (get P 'a 'data))
      
      (setq B P)
      (setq P (get P 'c)))
    
    (if (and (has-str?> P)
             (not (= (get P 'a 'data) " ")) # what about multiple spaces?
             (not (is-sym? P))
             (not (of-car?> P B)))
      (push 'D (get P 'a 'data))
      (setq P (get P 'b)))
    
    (list P D)))
      
