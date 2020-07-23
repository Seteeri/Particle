(class +Pointer)

#{
(dm find-back-ref> (Reg Ref Pln)
  # Implement custom search to do one-pass
  (let (Res NIL
        Ref (if Ref @ (: atom)))
        
    #(println 'find-back "Search for cell: " Ref)
    
    (for Line (: lns)
    
      # Handle non-pair like exp sym
    
      (let Any (get Line 'any)
        (setq Res (if (= Reg 'a) 
                      (memq Ref Any)
                      (if (sym? Any)
                          (when (= (val Any) Ref) Any)
                          (prior Ref Any))))
        #(println 'find-back "Search line: " Any)
        #(println 'find-back "  Found: " Res)
        )
        
      (T (and Res
              # Occurs for Y-Pair Car = Atom
              # else cont search
              (not (= Res Ref)))))
    (if Res
        Res
        (unless Pln
          (get (cadr (: lns)) 'any)))))
      
      
(dm find-back-any> (Ref Pln)
  (if (find-back-ref> This 'a Ref Pln)
      (list @ 'a)
      (list (find-back-ref> This 'b Ref Pln) 'b)))
}#


(dm trace-back> (Ref Reg)

  # Trace backwards line-by-line
  # Pass fn instead of 'a/b?
  
  (let Res NIL
  
    (for Line (: lns)
      
      (let Any (get Line 'any)
        (setq Res (if (= Reg 'a)
                      (memq Ref Any)
                      (if (sym? Any)
                          (when (= (val Any) Ref)
                            Any)
                          (prior Ref Any))))
        #(println 'trace-back> 'Trace: Any)
        #(println 'trace-back> 'Found: Res)
        )
        
      # Handle when Y-Pair Car = Atom
      (T Res))
    
    Res))


(dm get-ref> (Any Reg)
  
  (unless Any (setq Any (: any)))
  (unless Reg (setq Reg (: reg)))

  (if Reg
      (list Any
            (if (pair Any)
                (if (= Reg 'a)
                    (car Any)
                    (cdr Any))
                (val Any)))

      (list NIL
            Any)))
                

(dm get-ref-pt> (Any Reg)
  
  (unless Any (setq Any (: any)))
  (unless Reg (setq Reg (: reg)))
  
  # If =, then root
    
  (if (= Any Reg)
          
      (list NIL (lup-pt Any))
      
      (let (Pt-Pair (lup-pt Any)
            Ref (if (pair Any)
                    (if (= Reg 'a)
                        (car Any)
                        (cdr Any))
                    (val Any))
            Pt-Ref (if Ref
                       (lup-pt Ref)
                       (get Pt-Pair Reg)))
        (list Pt-Pair Pt-Ref))))

    
# Combine these funcs

      
(dm refs-1> ()

  # Return ref - 1
  
  (let ((Pt-B Pt-C) (get-ref-pt> This)
        C     (get Pt-C 'any)
        Reg-C (: reg)
        B     (get Pt-B 'any))

    (list
      C Pt-C Reg-C
      B Pt-B)))


(dm refs-2> ()

  #   A r B r C 

  # Return ref - 2
  
  (let ((Pt-B Pt-C) (get-ref-pt> This)
        C     (get Pt-C 'any)
        Reg-C (: reg)
        B     (get Pt-B 'any)
        Reg-B NIL
        A     NIL
        Pt-A  NIL)
    
    # Handle T?
    (cond
      ((trace-back> This B 'b)
       (setq A @
             Reg-B 'b))

      ((trace-back> This B 'a)
       (setq A @
             Reg-B 'a)))
             
    #~(assert A) 
    #(println (trace-back> This B 'a))
    #(prinl)
    #(println (trace-back> This B 'b))
    
    (setq Pt-A (lup-pt A))
        
    (list
      C Pt-C Reg-C
      B Pt-B Reg-B
      A Pt-A)))
