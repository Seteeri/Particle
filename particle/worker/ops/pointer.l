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
  (=: ln)
  
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
  
(dm rel-a?> () (= (: reg) 'a))
(dm rel-b?> () (= (: reg) 'b))

(dm ln> (X) (if X (push (:: ln) X) (pop (:: ln))))
(dm c-ln> () (car (: ln)))

################


(dm draw> ()
  #(lay-pt-start '*ptr '*ptr T T))
  (lay-pt-start (get (: po) 'any) 
                (get (: po) 'any)
                T T))

  
(dm stor-col> (Pt)
  (=: col (get-first-col> Pt)))
  
  
(dm col> (Pt Draw)
  (stor-col> This Pt)
  # Set color but don't draw
  (if Draw
     (col> Pt *selb-yellow 1.0)
     (col-verts> Pt *selb-yellow 1.0)))

  
(dm uncol> (Pt Draw)
  (let ((R G B A) (: col))
    (if Draw
        (col> Pt (list R G B) A)
        (col-verts> Pt (list R G B) A))))

    
(dm pt> (Tgt Rel Po Draw)
  #{
  
    Any T
    - Set Tgt
    - Set Rel to prv
    
    NIL
    - Set Rel of Po to Cell
    - Set Tgt to Cell data
    - Set Po to Po of NIL
    
  }#
    
  # Uncolor current ref
  (let? Po (if (: pair)
               (get (lup-pt (: pair)) (: reg))
               (lup-pt (: ref)))
    (uncol> This Po Draw))
          
        
  # Should store Point also
  
  (let Po-Tgt (lup-pt Tgt)
      
    # Set ref
    (if Po
          
        (prog        
          (col> This Po Draw)
          (=: ref Po)
          (=: pair Tgt))
            
        (prog          
          (col> This (lup-pt Tgt) Draw)
          (=: ref Tgt)
          (=: pair))))
        
  (=: reg Rel))
  

#############################

(dm a-nil?>  ()
  (and (rel-a?> This) (: pair)))
  
(dm a-pair?> ()
  (and (rel-a?> This) (pair (: ref))))
  
(dm a-atom?> ()
  (and (rel-a?> This) (atom (: ref))))

(dm b-nil?>  ()
  (and (rel-b?> This) (: pair)))
  
(dm b-pair?> ()
  (and (rel-b?> This) (pair (: ref))))
  
(dm b-atom?> ()
  (and (rel-b?> This) (atom (: ref))))

(dm atom?> (or (a-atom?> This) (b-atom?> This)))
(dm pair?> (or (a-pair?> This) (b-pair?> This)))
  
      
######################################


(dm pt-pair> (Rel Draw)
  # Determine tgt
  (let (Tgt (if (= Rel 'a)
                (car (: ref))
                (cdr (: ref)))
        Po-Top (if (atom Tgt)
                   (pt-pair-atom> This Rel)
                   (pt-pair-pair> This Rel Tgt)))

    (mov-line-n> This Po-Top)
                   
    (when Draw
      (top> Po-Top)
      (draw> This))))
              
              
(dm pt-pair-atom> (Rel)
  
  (let (Po-Pair (lup-pt (: ref))
        Lay-Y (lay-y?> Po-Pair)
        Po-Atom (get Po-Pair Rel))
  
    (pt> This (: ref) Rel Po-Atom T)

    (if (or (= Rel 'b)
            (and (= Rel 'a)
                  Lay-Y))
        Po-Atom
        Po-Pair)))


(dm pt-pair-pair> (Rel Tgt)
  
  (pt> This Tgt Rel NIL T)
  
  (lup-pt Tgt))
  
  
##################


(dm pt-sym> (Rel Draw)
  # Determine tgt
  (let Po-Tgt (if (val (: ref))
                  (pt-sym-pair> This Rel)
                  (pt-sym-nil> This Rel))
    (mov-line-n> This Po-Tgt)
    (when Draw
      (top> Po-Tgt)
      (draw> This))))

          
(dm pt-sym-nil> (Rel)
    
  (prog1
    
    (get (lup-pt (: ref)) Rel)
    
    (pt> This (: ref) Rel @ T)))
    
  
(dm pt-sym-pair> (Rel)
    
  (let Tgt (val (: ref))
  
    (pt> This Tgt Rel NIL T)
    
    (lup-pt Tgt)))
    
    
###############################


(dm mov-line-p> (Po-Ref Po-Tgt)    

  # If new has line, set it
  # else if old has line but new doesn't = mov prv line

  (if (get Po-Tgt 'line)
      (unless (= (c-ln> This) Po-Tgt)
        #(ln> This Po-Tgt)
        (ln> This)
        )
      (when (get Po-Ref 'line)
        (ln> This))))


(dm mov-line-n> (Po-Tgt)
  (when (get Po-Tgt 'line) # Tgt has line
    (unless (= (c-ln> This) Po-Tgt)
      (ln> This Po-Tgt))))
      
      
(dm find-back> (Rel Ref Pln)
  # Implement custom search to do one-pass
  (let (Res NIL
        Ref (if Ref @ (: ref)))
    #(println 'find-back "Search for cell: " Ref (type Ref))
    (for Line (: ln)
      (let Dat (get Line 'any)
        (setq Res (if (= Rel 'a) 
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
          (get (cadr (: ln)) 'any)))))
      
      
(dm find-back-any> (Ref Pln)
  (if (find-back> This 'a Ref Pln)
      @
      (find-back> This 'b Ref Pln)))
      
      
(dm back> (Rel Draw)

  (let (Back (if (: pair)
                 @
                 (find-back> This Rel))
        Po-Back (lup-pt Back))
    
    # Find Rel of back to its back
    (let (Find-Car (find-back> This 'a Back T)
          Find-Cdr (find-back> This 'b Back T))      
      #(prin "back> BACK: ") (println Back Find-Car Find-Cdr)
      (setq Rel (if Find-Car 'a 'b))
      #(prin "back> WARN: Unable to find back-2 for ")
      #(println Back)
      )
    
    # Cur -> New
    (mov-line-p> This
                 (if (: pair)
                     (: ref)
                     (lup-pt (: ref)))
                 Po-Back)     
                 
    (pt> This Back Rel NIL T)
    
    (when Draw
      (top> Po-Back)
      (draw> This)))
      
  #(prin "back> PTR: ") (println (: ref) (: pair) (: reg))
  #(prin "back> LN: ") (println (car (: ln)) (get (car (: ln)) 'any))
  )


(dm forw> (Rel Draw)

  (cond

    ((or (a-nil?> This)
         (b-nil?> This))
     T)
  
    ((or (a-pair?> This)
         (b-pair?> This))
      #(println 'forw> "A/B PAIR")
      (pt-pair> This Rel Draw))
    
    ((or (a-atom?> This)
         (b-atom?> This))
      #(println 'forw> "A/B ATOM")
      (pt-sym> This Rel Draw))
            
    (T
      # NIL - do nothing
      ))
      
  #(println 'forw> "PTR: " (: ref) (: pair))
  #(println 'forw> "LN: " (car (: ln)) (get (car (: ln)) 'any))
  )


(dm pre> (Draw) (back> This 'b Draw))
(dm nex> (Draw) (forw> This 'b Draw))
(dm sup> (Draw) (back> This 'a Draw))
(dm sub> (Draw) (forw> This 'a Draw))
