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
  
(dm reg-a?> () (= (: reg) 'a))
(dm reg-b?> () (= (: reg) 'b))

(dm lns> (X) (if X (push (:: lns) X) (pop (:: lns))))
(dm c-lns> () (car (: lns)))

################


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
  (let? Po (if (: pair)
               (get (lup-pt (: pair)) (: reg))
               (lup-pt (: ref)))
    (uncol> This Po Drw))
          
        
  # Should store Point also
  
  (let Po-Tgt (lup-pt Tgt)
      
    # Set ref
    (if Po
          
        (prog        
          (col> This Po Drw)
          (=: ref Po)
          (=: pair Tgt))
            
        (prog          
          (col> This (lup-pt Tgt) Drw)
          (=: ref Tgt)
          (=: pair))))
        
  (=: reg Reg))
  

#############################

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
  
      
######################################


(dm pt-pair> (Reg Drw)
  # Determine tgt
  (let (Tgt (if (= Reg 'a)
                (car (: ref))
                (cdr (: ref)))
        Po-Top (if (atom Tgt)
                   (pt-pair-atom> This Reg)
                   (pt-pair-pair> This Reg Tgt)))

    (mov-line-n> This Po-Top)
                   
    (when Drw
      (top> Po-Top)
      (draw> This))))
              
              
(dm pt-pair-atom> (Reg)
  
  (let (Po-Pair (lup-pt (: ref))
        Lay-Y (lay-y?> Po-Pair)
        Po-Atom (get Po-Pair Reg))
  
    (pt> This (: ref) Reg Po-Atom T)

    (if (or (= Reg 'b)
            (and (= Reg 'a)
                  Lay-Y))
        Po-Atom
        Po-Pair)))


(dm pt-pair-pair> (Reg Tgt)
  
  (pt> This Tgt Reg NIL T)
  
  (lup-pt Tgt))
  
  
##################


(dm pt-sym> (Reg Drw)
  # Determine tgt
  (let Po-Tgt (if (val (: ref))
                  (pt-sym-pair> This Reg)
                  (pt-sym-nil> This Reg))
    (mov-line-n> This Po-Tgt)
    (when Drw
      (top> Po-Tgt)
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


(dm mov-line-p> (Po-Ref Po-Tgt)    

  # If new has line, set it
  # else if old has line but new doesn't = mov prv line

  (if (get Po-Tgt 'line)
      (unless (= (c-lns> This) Po-Tgt)
        #(lns> This Po-Tgt)
        (lns> This)
        )
      (when (get Po-Ref 'line)
        (lns> This))))


(dm mov-line-n> (Po-Tgt)
  (when (get Po-Tgt 'line) # Tgt has line
    (unless (= (c-lns> This) Po-Tgt)
      (lns> This Po-Tgt))))
      
      
(dm find-back> (Reg Ref Pln)
  # Implement custom search to do one-pass
  (let (Res NIL
        Ref (if Ref @ (: ref)))
    #(println 'find-back "Search for cell: " Ref (type Ref))
    (for Line (: lns)
      (let Dat (get Line 'any)
        (setq Res (if (= Reg 'a) 
                      (memq Ref Dat)
                      (prior Ref Dat)))    
        #(println 'find-back "Searched line: " Dat)
        #(println 'find-back "Found: " Res)
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
      @
      (find-back> This 'b Ref Pln)))
      
      
(dm back> (Reg Drw)

  (let (Back (if (: pair)
                 @
                 (find-back> This Reg))
        Po-Back (lup-pt Back))
    
    # Find Reg of back to its back
    (let (Find-Car (find-back> This 'a Back T)
          Find-Cdr (find-back> This 'b Back T))      
      #(prin "back> BACK: ") (println Back Find-Car Find-Cdr)
      (setq Reg (if Find-Car 'a 'b))
      #(prin "back> WARN: Unable to find back-2 for ")
      #(println Back)
      )
    
    # Cur -> New
    (mov-line-p> This
                 (if (: pair)
                     (: ref)
                     (lup-pt (: ref)))
                 Po-Back)     
                 
    (pt> This Back Reg NIL T)
    
    (when Drw
      (top> Po-Back)
      (draw> This)))
      
  #(prin "back> PTR: ") (println (: ref) (: pair) (: reg))
  #(prin "back> LN: ") (println (car (: lns)) (get (car (: lns)) 'any))
  )


(dm forw> (Reg Drw)

  (cond

    ((or (a-nil?> This)
         (b-nil?> This))
     T)
  
    ((or (a-pair?> This)
         (b-pair?> This))
      #(println 'forw> "A/B PAIR")
      (pt-pair> This Reg Drw))
    
    ((or (a-atom?> This)
         (b-atom?> This))
      #(println 'forw> "A/B ATOM")
      (pt-sym> This Reg Drw))
            
    (T
      # NIL - do nothing
      ))
      
  #(println 'forw> "PTR: " (: ref) (: pair))
  #(println 'forw> "LN: " (car (: lns)) (get (car (: lns)) 'any))
  )


(dm pre> (Drw) (back> This 'b Drw))
(dm nex> (Drw) (forw> This 'b Drw))
(dm sup> (Drw) (back> This 'a Drw))
(dm sub> (Drw) (forw> This 'a Drw))
