(class +Pointer)
(dm T ()

  (=: ref)
  (=: col) # save old color  
  # on pair: rel to prv, 'a 'b or NIL for root
  # on atom: rel to super
  (=: reg) 
  # on atom: contains super
  (=: pair)
  
  # track lines
  (=: lns)
  
  # representation of Pointer
  (=: po))

(dm lay-x?> ()
  (if (: pair)
      (lay-x?> (: ref))
      (lay-x?> (lup-pt (: ref)))))
      
(dm lay-y?> ()
  (if (: pair)
      (lay-y?> (: ref))
      (lay-y?> (lup-pt (: ref)))))

(dm lns> (X) (if X (push (:: lns) X) (pop (:: lns))))
(dm c-lns> () (car (: lns)))

(dm reg-a?> () (= (: reg) 'a))
(dm reg-b?> () (= (: reg) 'b))

(dm a-nil?>  ()
  (and (reg-a?> This) (: pair)))
  
(dm a-pair?> ()
  (and (reg-a?> This) (pair (: ref))))
  
(dm a-atom?> ()
  (and (reg-a?> This) (atom (: ref))))

(dm b-nil?>  ()
  (and (reg-b?> This) (: pair)))
  
(dm b-pair?> ()
  (and (reg-b?> This) (pair (: ref))))
  
(dm b-atom?> ()
  (and (reg-b?> This) (atom (: ref))))

(dm atom?> (or (a-atom?> This) (b-atom?> This)))
(dm pair?> (or (a-pair?> This) (b-pair?> This)))

################

(dm clr> ()
  (=: pair)
  (=: ref))

(dm alt-reg> ()
  (=: reg (if (= (: reg) 'a) 'b 'a)))

  
(dm top-ref> ()   
  (let Pt-Pair (if (: pair) (lup-pt @) (lup-pt (: ref)))
    (top> (if (: pair)
              (if (or (and (lay-x?> Pt-Pair)
                            (= (: reg) 'b))
                      (lay-y?> Pt-Pair))
                  (: ref)
                  Pt-Pair)
              Pt-Pair))))

              
(dm draw> ()
  #(lay-pt-start '*ptr '*ptr T T))
  (lay-pt-start (get (: pt) 'any) 
                (get (: pt) 'any)
                T T))

  
(dm stor-col> (Pt)
  (=: col (get-first-col> Pt)))
  
  
(dm col> (Pt Drw)
  (stor-col> This Pt)
  # Set color but don't draw
  (if Drw
     (col> Pt *selb-yellow 1.0)
     (col-verts> Pt *selb-yellow 1.0)))

  
(dm uncol> (Pt Drw)
  (let ((R G B A) (: col))
    (if Drw
        (col> Pt (list R G B) A)
        (col-verts> Pt (list R G B) A))))

    
(dm pt> (Tgt Reg Po Drw)
  #{
  
    Any T
    - Set Tgt
    - Set Reg to prv
    
    NIL
    - Set Reg of Po to Cell
    - Set Tgt to Cell data
    - Set Po to Po of NIL
    
  }#
        
  # Uncolor current ref
  (unless (= Po (: ref))
    (let? Po (if (: pair)
                (get (lup-pt (: pair)) (: reg))
                (lup-pt (: ref)))
      (uncol> This Po Drw)))
          
        
  # Should store Point also?
  
  # Poss don't store particle for atoms
  # Only reg is needed
  # lup-pt -> get 'a/'b (do this in refs>)
  
  (let Pt-Tgt (lup-pt Tgt)
    
    # Set ref
    (if Po
          
        (prog        
          # Pass if same (when changing pair)
          (unless (= Po (: ref))
            (col> This Po Drw))
          (=: ref Po)
          (=: pair Tgt))
            
        (prog          
          (col> This (lup-pt Tgt) Drw)
          (=: ref Tgt)
          (=: pair))))
        
  (=: reg Reg))

      
######################################


(dm pt-pair> (Reg Drw)
  # Determine tgt
  (let (Tgt (if (= Reg 'a)
                (car (: ref))
                (cdr (: ref)))
        Pt-Top (if (atom Tgt)
                   (pt-pair-atom> This Reg)
                   (pt-pair-pair> This Reg Tgt)))

    (set-ln-n> This Pt-Top)
                   
    (when Drw
      (top> Pt-Top)
      (draw> This))))
              
              
(dm pt-pair-atom> (Reg)
  
  (let (Pt-Pair (lup-pt (: ref))
        Lay-Y (lay-y?> Pt-Pair)
        Pt-Atom (get Pt-Pair Reg))
  
    (pt> This (: ref) Reg Pt-Atom T)

    (if (or (= Reg 'b)
            (and (= Reg 'a)
                  Lay-Y))
        Pt-Atom
        Pt-Pair)))


(dm pt-pair-pair> (Reg Tgt)
  
  (pt> This Tgt Reg NIL T)
  
  (lup-pt Tgt))
  
  
##################


(dm pt-sym> (Reg Drw)
  # Determine tgt
  (let Pt-Tgt (if (val (: ref))
                  (pt-sym-pair> This Reg)
                  (pt-sym-nil> This Reg))
    (set-ln-n> This Pt-Tgt)
    (when Drw
      (top> Pt-Tgt)
      (draw> This))))

          
(dm pt-sym-nil> (Reg)
    
  (prog1
    
    (get (lup-pt (: ref)) Reg)
    
    (pt> This (: ref) Reg @ T)))
    
  
(dm pt-sym-pair> (Reg)
    
  (let Tgt (val (: ref))
  
    (pt> This Tgt Reg NIL T)
    
    (lup-pt Tgt)))
    
    
###############################


(dm set-ln-p> (Pt-Ref Pt-Tgt)    

  # If new has line, set it
  # else if old has line but new doesn't = mov prv line

  (if (get Pt-Tgt 'line)
      (unless (= (c-lns> This) Pt-Tgt)
        #(lns> This Pt-Tgt)
        (lns> This)
        )
      (when (get Pt-Ref 'line)
        (lns> This))))


(dm set-ln-n> (Pt-Tgt)
  (when (get Pt-Tgt 'line) # Tgt has line
    (unless (= (c-lns> This) Pt-Tgt)
      (lns> This Pt-Tgt))))
      
      
(dm find-back> (Reg Ref Pln)
  # Implement custom search to do one-pass
  (let (Res NIL
        Ref (if Ref @ (: ref)))
        
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
  (if (find-back> This 'a Ref Pln)
      (list @ 'a)
      (list (find-back> This 'b Ref Pln) 'b)))

      
(dm pt-bwd> (Reg Draw)

  (let ((C Pt-C Reg-C
         B Pt-B Reg-B
         A Pt-A) (refs-2> *ptr))
    
    (set-ln-p> This
               (if (: pair)
                   (: ref)
                   (lup-pt (: ref)))
               Pt-B)
    
    (pt> This B Reg-B NIL T)
    
    (when Draw
      (top> Pt-B)
      (draw> This))))
            

(dm pt-fwd> (Reg Drw)

  (cond

    ((or (a-nil?> This)
         (b-nil?> This)))
  
    ((or (a-pair?> This)
         (b-pair?> This))
      (pt-pair> This Reg Drw))
    
    ((or (a-atom?> This)
         (b-atom?> This))
      (pt-sym> This Reg Drw))))


(dm pre> (Drw) (pt-bwd> This 'b Drw))
(dm nex> (Drw) (pt-fwd> This 'b Drw))
(dm sup> (Drw) (pt-bwd> This 'a Drw))
(dm sub> (Drw) (pt-fwd> This 'a Drw))


(dm refs-2> ()

  # Create macro?
  # (with-ptr ... )

  # Return points: ref, back, back,
  # and regs for each
  
  (if (: pair)
  
      (let (C     (get (: ref) 'any)
            Pt-C  (: ref)
            B     (: pair)
            Pt-B  (lup-pt B)
            Reg-C (if (== (car B) C) 'a 'b)
            (A Reg-B) (find-back-any> This B T)
            Pt-A  (lup-pt A))
            
        (list
          C Pt-C Reg-C
          B Pt-B Reg-B
          A Pt-A))

      (let (C     (: ref)
            Pt-C  (lup-pt C)
            (B Reg-C) (find-back-any> This C T)
            Pt-B  (lup-pt B)
            (A Reg-B) (find-back-any> This B T)
            Pt-A  (lup-pt A))
            
        (list
          C Pt-C Reg-C
          B Pt-B Reg-B
          A Pt-A))))       

(dm refs-1> ()

  # Create macro?
  # (with-ptr ... )

  # Return points: ref, back, back,
  # and regs for each
  
  (if (: pair)
  
      (let (C     (get (: ref) 'any)
            Pt-C  (: ref)
            B     (: pair)
            Pt-B  (lup-pt B)
            Reg-C (if (== (car B) C) 'a 'b))
            
        (list
          C Pt-C Reg-C
          B Pt-B))

      (let (C     (: ref)
            Pt-C  (lup-pt C)
            (B Reg-C) (find-back-any> This C T)
            Pt-B  (lup-pt B))
            
        (list
          C Pt-C Reg-C
          B Pt-B))))
          
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
        
        (unless (= (get *ptr 'ref) Pt-Cdr)
          
          (pt> *ptr Pair 'b Pt-Cdr T)
          
          (set-ln-n> *ptr Pt-Cdr)
          
          (unless No-Draw
            (lay-*start)
            (mov-⇲ (c-lns> *ptr))
            (top> Pt-Cdr)
            (draw> *ptr)))))))
