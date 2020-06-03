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

  # rel to super cell
  # 'a 'b or NIL for root
  (=: rel)

  # trav stacks
  (=: pre)
  (=: sup)
  (=: ln)
  
  # when on nil, contains super cell
  (=: nil)
  
  (=: po))

(dm lay-x?> () (lay-x?> (: po)))
(dm lay-y?> () (lay-y?> (: po)))
  
(dm rel-a?> () (= (: rel) 'a))
(dm rel-b?> () (= (: rel) 'b))

(dm ref> (R) (if R (=: ref R) (: ref)))
(dm nil> (N) (if N (=: nil N) (: nil)))
(dm rel> (S) (if S (=: rel S) (: rel)))    

(dm c-ln> () (car (: ln)))

# rename to car/cdr or a/b                  
(dm p-pre> (X) (if X (push (:: pre) X) (pop (:: pre))))
(dm p-sup> (X) (if X (push (:: sup) X) (pop (:: sup))))

################


(dm set-col> (P)
  (=: col (get-first-col> P)))
  
  
(dm col> (P)
  (set-col> This P)
  # Get color from class or inst
  (col> P *selb-yellow 1.0))

  
(dm uncol> (P)
  (let ((R G B A) (: col))
    (col> P (list R G B) A)))


(dm draw> ()
  (lay-pt-start '*ptr '*ptr T T))

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
  (if (: nil)
      (let? Po-Tgt (get (lup-pt (: nil)) (: rel))
        (uncol> This Po-Tgt))
      (let? Po (lup-pt (: ref))
        (uncol> This Po)))
  
  # Should store Point also
  
  (let Po-Tgt (lup-pt Tgt)
      
    # Set ref
    (if Po
          
        (prog        
          (col> This Po)
          (=: ref Po)
          (=: nil Tgt))
            
        (prog          
          (col> This (lup-pt Tgt))
          (=: ref Tgt)
          (=: nil))))
        
  (=: rel Rel))
  

#############################

(de ptr-nil?    (Ref) (not Ref))
(de ptr-pair?   (Ref) (pair Ref))
(de ptr-atom?   (Ref) (atom Ref))
(de ptr-atom-t? (Ref) (and Ref (atom Ref)))
(de ptr-term?   (Sym) (or
                        (and 
                          (rel-a?> (get Sym 'ptr))
                          (atom?> (val Sym)))
                        (and
                          (rel-b?> (get Sym 'ptr))
                          (atom?> (val Sym)))))


(dm a-nil?>  ()
  (and (rel-a?> This) (: nil)))
  
(dm a-pair?> ()
  (and (rel-a?> This) (pair (: ref))))
  
(dm a-atom?> ()
  (and (rel-a?> This) (atom (: ref))))

(dm b-nil?>  ()
  (and (rel-b?> This) (: nil)))
  
(dm b-pair?> ()
  (and (rel-b?> This) (pair (: ref))))
  
(dm b-atom?> ()
  (and (rel-b?> This) (atom (: ref))))
      
#######################################################                 
  
(dm mov-line-p> (Po-Ref Po-Tgt)    

  # If new has line, set it
  # else if old has line but new doesn't = mov prv line

  (let Line 
        (if (get Po-Tgt 'line)
            (unless (= (car (: ln)) Po-Tgt)
              (push (:: ln) Po-Tgt))
            (when (get Po-Ref 'line)
              (pop (:: ln))))
    #{
    (push (:: ln) 
          (if (get *line 'line 'p)
              @
              *line))
    }#
    
    T))


(dm mov-line-n> (Po-Tgt)
  (when (get Po-Tgt 'line) # Tgt has line
    (unless (= (c-ln> This) Po-Tgt)
      (push (:: ln) Po-Tgt))))
      
      
(dm find-back> (Rel)
  (let (Line (get (c-ln> This) 'data)
        Res (if (= Rel 'a) 
                (memq (: ref) Line)
                (prior (: ref) Line)))
    (println 'find-back "Search for cell: " (: ref) (type (: ref)))
    (println 'find-back "Search in line: " Line)
    (println 'find-back "Found: " Res)
    
    (if Res @ (get (cadr (: ln)) 'data))))
      
      
(dm back> (Fn-Pop Rel No-Lay)

  #(println 'back> "LN LIST: " (: ln))

  (let (Back (if (: nil)
                 @
                 (find-back> This Rel))
        Po-Back (lup-pt Back))
        
    (mov-line-p> This Po-Ref Po-Back)
    (set-ref> This Back Rel)
    
    (unless No-Lay
      (top> Po-Back)
      (draw> This)))
      
  (println 'back> "PTR: " (: ref) (: nil))
  (println 'back> "LN: " (car (: ln)) (get (car (: ln)) 'data)))


(dm forw> (Fn-Push Rel)

  (cond

    ((or (a-nil?> This)
         (b-nil?> This))
     T)
  
    ((a-pair?> This)
      (println 'forw> "A PAIR")
      (pt-pair> This Fn-Push Rel))
    
    ((b-pair?> This)
      (println 'forw> "B PAIR")
      (pt-pair> This Fn-Push Rel))    
    
    ((a-atom?> This) # Num or Sym - Sym relevant
      (println 'forw> "A ATOM")
      (pt-sym> This Fn-Push Rel))  
  
    ((b-atom?> This) # Num or Sym - Sym relevant
      (println 'forw> "B ATOM")
      (pt-sym> This Fn-Push Rel))  
            
    (T
      # NIL - do nothing
      ))
      
  (println 'forw> "PTR: " (: ref) (: nil))
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

  
#################
    

(dm pt-pair> (Fn-Push Rel)
  # Determine tgt
  (let (Po-Ref (lup-pt (: ref))
        Po-Tgt (get Po-Ref Rel))
    (if Po-Tgt
        (pt-pair-nil> This Po-Ref Po-Tgt Rel Fn-Push)
        # determine if pair or not
        (let Tgt (if (= Rel 'a)
                     (car (: ref))
                     (cdr (: ref)))
          (if (atom Tgt)
              (pt-pair-atom> This Rel Fn-Push)
              (pt-pair-pair> This Rel Fn-Push))))))

            
(dm pt-pair-nil> (Po-Ref Po-Tgt Rel Fn-Push)
      
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
  
  
############################

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
    
    
####################
  
      
(de pt-sup-nxt-sub ()      
  # Only works if ptr on car already
  (pt-sup T)
  (pt-nxt T)
  (pt-sub))
