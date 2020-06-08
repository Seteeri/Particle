#{

  Ref stored in property list of object
  so draw property cell

  Would be nice to draw entire property list
  on a single line
  
}#

(class +Pointer)
(dm T ()

  (=: ref)
  (=: col) # save old color  
  (=: rel) # rel to prv, 'a 'b or NIL for root
  # when on atom, contains super cell
  (=: cell)
  
  # trav stacks
  #(=: pre)
  #(=: sup)
  (=: ln)
    
  (=: po))

(dm lay-x?> ()
  (if (: cell)
      (lay-x?> (: ref))
      (lay-x?> (lup-pt (: ref)))))
      
(dm lay-y?> ()
  (if (: cell)
      (lay-y?> (: ref))
      (lay-y?> (lup-pt (: ref)))))
  
(dm rel-a?> () (= (: rel) 'a))
(dm rel-b?> () (= (: rel) 'b))

(dm ref> (R) (if R (=: ref R) (: ref)))
(dm cell> (N) (if N (=: cell N) (: cell)))
(dm rel> (S) (if S (=: rel S) (: rel)))    
(dm ln> (X) (if X (push (:: ln) X) (pop (:: ln))))
(dm c-ln> () (car (: ln)))

################


(dm draw> ()
  (lay-pt-start '*ptr '*ptr T T))

  
(dm set-col> (P)
  (=: col (get-first-col> P)))
  
  
(dm col> (P)
  (set-col> This P)
  # Get color from class or inst
  (col> P *selb-yellow 1.0))

  
(dm uncol> (P)
  (let ((R G B A) (: col))
    (col> P (list R G B) A)))

    
(dm set-ref> (Tgt Rel Po)
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
  (let? Po (if (: cell)
               (get (lup-pt (: cell)) (: rel))
               (lup-pt (: ref)))
    (uncol> This Po))
          
        
  # Should store Point also
  
  (let Po-Tgt (lup-pt Tgt)
      
    # Set ref
    (if Po
          
        (prog        
          (col> This Po)
          (=: ref Po)
          (=: cell Tgt))
            
        (prog          
          (col> This (lup-pt Tgt))
          (=: ref Tgt)
          (=: cell))))
        
  (=: rel Rel))
  

#############################

(dm a-nil?>  ()
  (and (rel-a?> This) (: cell)))
  
(dm a-pair?> ()
  (and (rel-a?> This) (pair (: ref))))
  
(dm a-atom?> ()
  (and (rel-a?> This) (atom (: ref))))

(dm b-nil?>  ()
  (and (rel-b?> This) (: cell)))
  
(dm b-pair?> ()
  (and (rel-b?> This) (pair (: ref))))
  
(dm b-atom?> ()
  (and (rel-b?> This) (atom (: ref))))

(dm atom?> (or (a-atom?> This) (b-atom?> This)))
(dm pair?> (or (a-pair?> This) (b-pair?> This)))
  
      
######################################


(dm pt-pair> (Fn-Push Rel)
  # Determine tgt
  (let (Po-Ref (lup-pt (: ref))
        Po-Atom (get Po-Ref Rel))        
    (if Po-Atom
        (pt-pair-nil> This Po-Atom Rel Fn-Push)
        # determine if pair or not
        (let Tgt (if (= Rel 'a)
                     (car (: ref))
                     (cdr (: ref)))
          (if (atom Tgt)
              (pt-pair-atom> This Rel Fn-Push)
              (pt-pair-pair> This Rel Fn-Push))))))

              
(dm pt-pair-atom-2> (Po-Tgt Rel Fn-Push)
  
  (mov-line-n> This Po-Tgt)
  (set-ref> This (: ref) Rel Po-Tgt)
    
  (when (or (= Rel 'b)
            (and (= Rel 'a)
                 (lay-y?> (lup-pt (: ref)))))
    (top> Po-Tgt)
    (draw> This)))
              
            
(dm pt-pair-nil> (Po-Tgt Rel Fn-Push)
      
  # Push val to stack
  #(Fn-Push This (: ref))
  
  (mov-line-n> This Po-Tgt)
  (set-ref> This (: ref) Rel Po-Tgt)
    
  (when (= Rel 'b)
    (top> Po-Tgt)
    (draw> This)))
            

(dm pt-pair-atom> (Rel Fn-Push)
    
  (println 'pt-pair-atom)
    
  (let (Po-Ref (lup-pt (: ref))
        Tgt (if (= Rel 'a)
                (car (: ref))
                (cdr (: ref)))
        Po-Tgt (lup-pt Tgt))

    # Push val to stack
    #(Fn-Push This (: ref))
        
    (mov-line-n> This Po-Tgt)
    (set-ref> This Tgt Rel)
    
    (when (= Rel 'b)
      (top> Po-Tgt)
      (draw> This))))
  
  
(dm pt-pair-pair> (Rel Fn-Push)
  
  (println 'pt-pair-pair)
  
  (let (Po-Ref (lup-pt (: ref))
        Tgt (if (= Rel 'a)
                (car (: ref))
                (cdr (: ref)))
        Po-Tgt (lup-pt Tgt))

    # Push val to stack
    #(Fn-Push This (: ref))
    
    (mov-line-n> This Po-Tgt)
    (set-ref> This Tgt Rel)
      
    (top> Po-Tgt)
    (draw> This)))
  
  
##################


(dm pt-sym> (Fn-Push Rel)
  # Determine tgt
  (let (Ref (: ref)
        Val (val Ref))
    (if Val
        (pt-sym-pair> This Rel Fn-Push)
        (pt-sym-nil> This Rel Fn-Push)))        
  (draw> This))

          
(dm pt-sym-nil> (Rel Fn-Push)
    
  #(Fn-Push Ptr (: ref))

  (let Po-Tgt (get (lup-pt (: ref)) Rel)
  
    (mov-line-n> This Po-Tgt)
    (set-ref> This (: ref) Rel Po-Tgt)
    
    (top> Po-Tgt)))
  
  
(dm pt-sym-pair> (Rel Fn-Push)
    
  (let (Po-Val (lup-pt (: ref))
        Tgt (val (: ref))
        Po-Tgt (lup-pt Tgt))
  
    # Push val to stack
    #(Fn-Push Ptr (: ref))
        
    (mov-line-n> This Po-Tgt)
    (set-ref> This Tgt Rel)
    
    (top> Po-Tgt)))    
    
    
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
      
      
# Pass ref
(dm find-back> (Rel Ref)
  (let (Res NIL
        Ref (if Ref @ (: ref)))
    (println 'find-back "Search for cell: " Ref (type Ref))
    (for Line (: ln)
      (let Dat (get Line 'data)
        (setq Res (if (= Rel 'a) 
                      (memq Ref Dat)
                      (prior Ref Dat)))    
        (println 'find-back "Searched line: " Dat)
        (println 'find-back "Found: " Res))
      (T (and Res
              # Occurs for Y-Pair Car = Atom
              # else cont search
              (not (= Res Ref)))))
    (if Res
        Res
        (get (cadr (: ln)) 'data))))
      
      
(dm back> (Fn-Pop Rel No-Lay)

  #(println 'back> "LN LIST: " (: ln))

  (let (Back (if (: cell)
                 @
                 (find-back> This Rel))
        Po-Back (lup-pt Back))
    
    # Cur -> New
    (mov-line-p> This
                 (if (: cell)
                     (: ref)
                     (lup-pt (: ref)))
                 Po-Back)
    (set-ref> This Back Rel)
    
    (unless No-Lay
      (top> Po-Back)
      (draw> This)))
      
  (println 'back> "PTR: " (: ref) (: cell))
  (println 'back> "LN: " (car (: ln)) (get (car (: ln)) 'data)))


(dm forw> (Fn-Push Rel)

  (cond

    ((or (a-nil?> This)
         (b-nil?> This))
     T)
  
    ((or (a-pair?> This)
         (b-pair?> This))
      (println 'forw> "A/B PAIR")
      (pt-pair> This Fn-Push Rel))
    
    ((or (a-atom?> This)
         (b-atom?> This))
      (println 'forw> "A/B ATOM")
      (pt-sym> This Fn-Push Rel))
            
    (T
      # NIL - do nothing
      ))
      
  (println 'forw> "PTR: " (: ref) (: cell))
  (println 'forw> "LN: " (car (: ln)) (get (car (: ln)) 'data)))


(dm pre> ()
  (println 'pre>)
  (back> This 'p-pre> 'b))

(dm nex> ()
  (println 'nex>)
  (forw> This 'p-pre> 'b))

(dm sup> (No-Lay)
  (println 'sup>)
  (back> This 'p-sup> 'a No-Lay))

(dm sub> ()
  (println 'sub>)
  (forw> This 'p-sup> 'a))
