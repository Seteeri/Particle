(de make-list-cdr ()
  (println 'make-list-cdr)

  (make-list-cdr-x))       
         
         
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
  
