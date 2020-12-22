(class +Pointer)
(dm T ()

  (=: any)
  (=: reg)
  
  (=: lns)
  
  # repr
  (=: po))

(dm lns> (X) (if X (push (:: lns) X) (pop (:: lns))))
(dm c-lns> () (car (: lns)))
                
                
(dm stor-col> (Pt)
  (put Pt 'col (get-first-col> Pt)))
  
  
(dm col> (Pt Draw)
  (stor-col> This Pt)
  # Set color but don't draw
  (if Draw
     (col> Pt *selb-yellow 1.0)
     (col-verts> Pt *selb-yellow 1.0)))

  
(dm uncol> (Pt Drw)
  (let ((R G B A NIL) (get Pt 'col))
    (if R
        (if Drw
            (col> Pt (list R G B) A)
            (col-verts> Pt (list R G B) A))
        (prin "WARNING: uncol> Missing col: ")
        (println Pt (get Pt 'any)))))


(dm draw> ()
  #(lay-pt-start '*ptr '*ptr T T))
  (lay-pt-start (get (: pt) 'any) 
                (get (: pt) 'any)
                T T))

(dm top-ref> ()
  (let ((Pt-Pair Pt-Reg) (get-ref-pt> This))
    (if (or (and (isa '+Atom Pt-Reg)
                 (or (reg-b?> This)
                     (lay-y?> Pt-Pair)))
            (and (isa '+Pair Pt-Reg)))
        (top> Pt-Reg)
        (when Pt-Pair # for init sym
          (top> Pt-Pair)))))
          

(dm clr> ()
  (=: any)
  (=: atom))

  
(dm alt-reg> ()
  (=: reg (if (= (: reg) 'a) 'b 'a)))
             
             
(dm pt> (Any Reg Draw Col)
  #{

    TODO: Rename to ?
  
    Store color in pointer or in object
    -> To support multi pointers, store in +Point
    
    Col = force set color
    
  }#
  
  # Uncolor current ref
  # it ref new=old, pass on color
  (let ((Pt-Pair Pt-Reg) (get-ref-pt> This 
                                      (: any)
                                      (: reg))
        (New-Pair New-Reg) (get-ref-pt> This
                                        Any
                                        Reg))

    (when NIL
      (prinl)
      (println 'pt> Pt-Pair Pt-Reg (get Pt-Reg 'any))
      (println 'pt> New-Pair New-Reg (get New-Reg 'any)))
    
    (when (or (not (= New-Reg Pt-Reg)) Col)
                                        
      (when Pt-Reg
        (uncol> This Pt-Reg Draw))

      (col> This New-Reg Draw)))
  
  (=: any Any)
  (=: reg Reg))


##################
# MOV BELOW WHERE?

(de ptr-ls-first (No-Draw)

  (let (Pt-Pair (let (P (prop *ptr 'lns)
                      L NIL)
                  (loop (setq L (pop P))
                    (NIL (get L 'line 'p)))
                  (push P L) # Return it
                  L)
        Pair (get Pt-Pair 'any)
        (Back Reg) (find-back-any> *ptr Pair T)
        Pt-Back (lup-pt Back))
    
    (when (sym? Back)
      (setq Reg 'b))
    
    (pt> *ptr Pair Reg)
    
    (unless No-Draw
      (lay-*start)
      (mov-⇲ (c-lns> *ptr))
      (top> Pt-Pair)
      (draw> *ptr))))
      
      
(de ptr-ls-last (No-Draw)

  # If jumping forward, what about lines?
  #
  # If traverse lines, linear complexity
  #
  # When moving back, have to search list
  # list -> ... -> cur-line
  
  (let Ln (c-lns> *ptr)
  
    (loop
      (NIL (get Ln 'line 'n))
      (setq Ln (get Ln 'line 'n))
      (lns> *ptr Ln))
      
    (let Pair (get Ln 'any)    
      
      (loop 
        (NIL (pair (cdr Pair)))
        (setq Pair (cdr Pair)))
      
      (let (Pt-Pair (lup-pt Pair)
            Pt-Cdr (get Pt-Pair 'b))
        
        (unless (= (get *ptr 'atom) Pt-Cdr)
          
          (pt> *ptr Pair 'b Pt-Cdr T)
          
          (set-ln-n> *ptr Pt-Cdr)
          
          (unless No-Draw
            (lay-*start)
            (mov-⇲ (c-lns> *ptr))
            (top> Pt-Cdr)
            (draw> *ptr)))))))
