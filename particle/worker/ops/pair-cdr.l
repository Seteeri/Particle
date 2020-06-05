(de make-list-cdr (Ref)
  (println 'make-list-cdr)
    
  (cond ((and (lay-x?> Ref)
              (is-pair? Ref))
         (make-list-cdr-x Ref))
  
        # Same as case 4
        ((and (lay-y?> Ref)
              (is-pair? Ref))
         (make-list-car-ya Ref))
        
        (T # Atom
         (make-list-cdr-x Ref))))

# make list with pair
(de make-list-car-a-atom-2 ()

  #{
              
        [Cc]  
        NIL

         
        [X/C]  Cb
        NIL
        *
    ->
               
        [Cc]  
          A   
          
        [Po-Pair]  [Po-List]   
                    NIL
                    *
              
        Cb
  }#  
  
  #{
                      
        [Cc]  [C]  [Cb]  NIL
        NIL   NIL  NIL
               *
              
    ->
               
        [Cc]  
        NIL  

              
        [Y/P]  [C]  NIL
               NIL  
               *
        
        [Cb] NIL
        NIL
        
        Poss Cars:
        1. New list with Car
        2. Car itself
           * Same Pair with Y layout
           * Cdr has line also
        3. NIL
        
  }#   

  # Handle NIL
  
  (let (Tgt (ref> *ptr)
        Po-Tgt (lup-pt Tgt)
        Pair (find-back> *ptr (rel> *ptr))
        Po-Pair (lup-pt Pair))
    
    (println "Car/Pair: " Tgt Pair (get Po-Pair 'a))
  
    # otherwise set-ref will attempt uncol on ref
    # pass flag for this...
    (put *ptr 'ref)  
    # put NIL otherwise set will del new point
    (put Po-Pair 'a)
    (del> Po-Tgt)
    
    (let (List (list Tgt)
          Po-List (gen-point List 'x NIL)
          Ln (c-ln> *ptr)
          Ln-N (get Ln 'line 'n)
          Cdr (cdr Pair)
          Po-Cdr (if Cdr (lup-pt Cdr) (get Po-Pair 'b))
          Push-Pair (not (get Po-Pair 'line)))
    
      # Repl tgt with list with tgt list
      (set> Po-Pair List Po-List)
      
      # Set layout for Pair
      (set-lay> Po-Pair 'y)
      
      (if (get Po-Pair 'line)
          (put @ 'n Po-Cdr)      
          (ins-line-aft Ln Po-Pair))
      
      # Create Line for Cdr
      (if (get Po-Cdr 'line)
          (put @ 'p Po-Pair)
          (ins-line-aft Po-Pair Po-Cdr))
          
      # Create line for List
      (put Po-List 'line (new '(+Line)))
      
      (upd-tree)
      (mov-⇲ Ln)
            
      # Pt to list
      (set-ref> *ptr List 'a)
            
      # Push cell and car
      (when Push-Pair
        (ln> *ptr Po-Pair))
      (ln> *ptr Po-List)
      
      (top> Po-List)
      (draw> *ptr))))         
         
# make list with list
(de make-list-cdr-x (Ref)
  #{
        In constrast to make-list-car,
        put cdr in same list also
      
              *
        [C]  [R]  [ ]  [ ]  NIL
         A    B    C    D

        ->
        
        [C]
              *
        [P]  [R]  [ ]  [ ] NIL
              B    C    D
        
        [Pb:NIL]
        
        
        ---

              *
        [C]  [R]  [ ]  
         x    x    x
         
        [ ]  [ ]  [ ]  NIL
         x    x    x

         ->
         
        [C]
              *
        [P]  [R]  [ ]  [ ] NIL
              x    x    x
         
             [ ]  [ ]  [ ]  NIL
              x    x    x
        
        [Pb:NIL]
        
        - D line 'l changes from C -> R, and so does every subsequent list
          - mov-line handles this
         
  }#
  
  (let (C (get Ref 'c)
        Po (gen-point (cons) 'x (cons T NIL) T)
        Pb (get Po 'b))

    (b> C Po)
    (a> Po Ref)        

    (set-lay> Po 'y)

    # With current line, upd 'l for all subseq lines to Ref
    (let L *line
      (while L
        (put L 'line 'l Ref)
        (setq L (get L 'n))))
    (put *line 'line 'n Po)
    
    (if (is-sym? C)
        
        (prog
        
          (put Po 'line (new '(+Line) NIL Pb NIL))
          
          (put Pb 'line (new '(+Line) Po NIL Po))
              
          (put Ref 'line (new '(+Line))))
          
        (prog # Pair
                              
          (put Po 'line (new '(+Line) C Pb *line))
          
          (put Pb 'line (new '(+Line) Po NIL (get Po 'line 'l)))
              
          (put Ref 'line (new '(+Line)))))
        
    (upd-tree)
    (mov-⇲ (car (get *ptr 'ln)))
    
    (setq *line Ref)
    (set-ptr-ref Ref)
    (top> Ref)
    (lay-ptr *ptr)
                        
    (println 'make-list-cdr-y)

    Po))
  
