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
    (mov-⇲ *line)
    
    (setq *line Ref)
    (set-ptr *ptr Ref)
    (top> Ref)
    (lay-ptr *ptr)
                        
    (println 'make-list-cdr-y)

    Po))
  
