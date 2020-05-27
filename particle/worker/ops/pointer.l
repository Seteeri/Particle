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
  # list trav stack?
  
  # when on nil, contains super cell
  (=: nil)
  
  (=: po))

(dm lay-x?> () (lay-x?> (: po)))
(dm lay-y?> () (lay-y?> (: po)))
  
(dm rel-a?> () (= (: rel) 'a))
(dm rel-b?> () (= (: rel) 'b))

(dm set-col> (P)
  (=: col (get-first-col> P)))
  
  
(dm col> (P)
  (set-col> This P)
  # Get color from class or inst
  (col> P *selb-yellow 1.0))

  
(dm uncol> (P)
  (let ((R G B A) (: col))
    (col> P (list R G B) A)))

    
# rename to car/cdr or a/b                  
(dm p-pre> (X) (if X (push (:: pre) X) (pop (:: pre))))
(dm p-sup> (X) (if X (push (:: sup) X) (pop (:: sup))))

(dm rel> (S) (if S (=: rel S) (: rel)))    

(dm draw> ()
  (lay-pt-start '*ptr '*ptr T T))

(dm ref> (Tgt Rel Po)
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
  
  # Set ref
  (if Po
                  
      (prog
        (col> This Po)
        (=: ref Po)
        (=: nil Tgt))
          
      (prog  
        (col> This (lup-pt Tgt))
        (=: ref Tgt)
        (=: nil)))
        
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
  (and (rel-a?> This) (: ref) (: nil)))
  
(dm a-pair?> ()
  (and (rel-a?> This) (is-pair? (: po))))
  
(dm a-atom?> ()
  (and (rel-a?> This) (is-atom? (: po))))

(dm b-nil?>  ()
  (and (rel-b?> This) (: ref) (: nil)))
  
(dm b-pair?> ()
  (and (rel-b?> This) (pair (: ref))))
  
(dm b-atom?> ()
  (and (rel-b?> This) (atom (: ref))))

(de ptr-nil (Sym) (get Sym 'ptr 'nil))
(de ptr-rel (Sym) (get Sym 'ptr 'rel))

(de ptr-car-pre (Sym) (car (get Sym 'ptr 'pre)))
(de ptr-car-sup (Sym) (car (get Sym 'ptr 'sup)))


(de ptr-ln (Sym Ln) 
  (if Ln
      (put Sym 'ptr 'ln Ln)
      (get Sym 'ptr 'ln)))
      
      
(de ptr-ls (Sym Ls)
  (if Ls
      (put Sym 'ptr 'ls Ls)
      (get Sym 'ptr 'ls)))

      
#######################################################                  


(de find-back (Sym Rel)
  (let (List (if (get *line 'line 'l 'data) @ (get *line 'data))
        Cell-Pr (if (= Rel 'a) 
                    (memq (val Sym) List)
                    (prior (val Sym) List)))
    (println 'find-back "BACK : " (val Sym) List " = " Cell-Pr)))


(de mov-line-p (Po-Ref Po-Tgt)    

  (if (get Po-Tgt 'line)
      
      # Tgt has line
      (ptr-ln '*ptr (setq *line Po-Tgt))
      
      # Ref has line but tgt doesn't = mov prv line
      (when (get Po-Ref 'line)
        (ptr-ln '*ptr 
          (setq *line (if (get *line 'line 'p) @ *line))))))

    
(dm back> (Fn-Pop Rel No-Lay)

  (let? Back (Fn-Pop This)

    (let (Po-Back (lup-pt Back)
          Po-Ref (lup-pt (: ref)))
          
      #(find-back Ptr Rel)
      
      #(println Po-Back (get Po-Back 'data))
      
      (mov-line-p Po-Ref Po-Back)
          
      (ref> This Back Rel)
      
      (unless No-Lay
        (top> Po-Back)
        (draw> This)))))
  

(dm forw> (Fn-Push Rel)

  (cond

    ((or (a-nil?> This)
         (b-nil?> This))
     T)
  
    ((a-pair?> This)
      (println 'forw> "A PAIR")
      (pt-pair This Fn-Push Rel))
    
    ((b-pair?> This)
      (println 'forw> "B PAIR")
      (pt-pair> This Fn-Push Rel))    
    
    ((a-atom?> This) # Num or Sym - Sym relevant
      (println 'forw> "A ATOM")
      (pt-sym This Fn-Push Rel))  
  
    ((b-atom?> This) # Num or Sym - Sym relevant
      (println 'forw> "B ATOM")
      (pt-sym This Fn-Push Rel))  
            
    (T
      # NIL - do nothing
      )))


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

(de mov-line-n (Po-Tgt)
  (when (get Po-Tgt 'line) # Tgt has line
    (setq *line Po-Tgt)))
    

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
  (Fn-Push This (: ref))

  (mov-line-n Po-Tgt)
  
  (ref> This (: ref) Rel Po-Tgt)
    
  (when (= Rel 'b)
    (top> Po-Tgt)
    (draw> This)))
            

(dm pt-pair-atom> (Rel Fn-Push)
    
  (println 'pt-pair-atom)
    
  (let (Po-Val (lup-pt (: ref))
        Val (val (: ref))
        Tgt (if (= Rel 'a)
                (car Val)
                (cdr Val))
        Po-Tgt (lup-pt Tgt))

    # Push val to stack
    (Fn-Push This Val)
    
    (mov-line-n Po-Tgt)
    
    # No mov ptr
    (ref> This Po-Tgt Rel)
    
    (when (= Rel 'b)
      (top> Po-Tgt)
      (draw> This))))
  
  
(dm pt-pair-pair> (Rel Fn-Push)
  
  (println 'pt-pair-pair)
  
  (let (Po-Val (val Ptr)
        Val (get Po-Val 'data)
        Tgt (if (= Rel 'a)
                (car Val)
                (cdr Val))
        Po-Tgt (lup-pt Tgt))

    # Push val to stack
    (Fn-Push This Val)
    
    (mov-line-n Po-Tgt)
    
    (ref> This Po-Tgt Rel)
      
    (top> Po-Tgt)
                  
    (draw> This)))
  
  
############################

(de pt-sym (Ptr Fn-Push Rel)
  # Determine tgt
  (let (Ref (get Ptr 'ref)
        Val (val Ref))
    (if Val
        (pt-sym-pair Ptr Ref Rel Fn-Push)
        (pt-sym-nil Ptr Ref Rel Fn-Push)))        
  (draw> Ptr))

          
(de pt-sym-nil (Ptr Ref Rel Fn-Push)
    
  (Fn-Push Ptr Ref)

  (let Po-Tgt (get (lup-pt Ref) Rel)
    (mov-line-n Po-Tgt)
    (ref> Ptr Ref Rel Po-Tgt)
    (top> Po-Tgt)))
  
  
(de pt-sym-pair (Ptr Po-Ref Rel Fn-Push)
    
  (let (Po-Val (val Ptr)
        Val (get Po-Val 'data)
        Tgt (val Val)
        Po-Tgt (lup-pt Tgt))
  
    # Push val to stack
    (Fn-Push Ptr Po-Val)
        
    (mov-line-n Po-Tgt)
    
    (ref> Ptr Tgt Rel)
    
    (top> Po-Tgt)))
    
    
####################
  
      
(de pt-sup-nxt-sub ()      
  # Only works if ptr on car already
  (pt-sup T)
  (pt-nxt T)
  (pt-sub))
