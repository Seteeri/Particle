#{

  Test ops:

    * Run cmds
    * Output pos'
    * Verify data
    * Verify pos'

  Create NIL tests
  - replace cmd-make-char with NIL    
  Create tests for Sym, besides Pair
  Create tests for pointer

  Check ptr lines
  
  Test ptr and nil before data
  Test lst cmd before str cmd
  
}#


(de props-part (X)
  # Checking ori/lay should be sufficient...
  (with X
    (list (type X)
          (: any)
          (: lay)
          (when (: a) T)
          (when (: b) T)
          (when (: c) T)
          (: ori)
          (: dirt)
          # verts
          (: dims)
          (when (: line)
            (list (when (get @ 'p) T)
                  (when (get @ 'n) T))))))
    

(de comp-part (X I)
  (let (A (props-part X)
        E (= A I))
    #{
    (unless E
      (mapc '((A I) (println (= A I) A I))
            A
            I))
    }#
    E))
    
    
(de get-dfs-struct ()
  # build flattened list of origins
  # depth-first search
  # order/struct should match
  
  # poss verify all props, incl line
  # note, property list is arbitrary order
  # sort than do =
  
  
  (let (O ()
        C (list *start))
        
    (while (setq X (pop 'C))
              
      (if (isa '(+Point) X)
      
          (push 'O (props-part X))
          
          (let (Px (lup-pt X)
                A (car X)
                B (cdr X))
            (push 'O (props-part Px))
            (push 'C
              (if (atom B) (get Px 'b) B)
              (if (atom A) (get Px 'a) A)))))
    (flip O)))


(de ver-dfs-struct (I)
  
  (let (O ()
        C (list *start))
        
    (while (setq X (pop 'C))
              
      (if (isa '(+Point) X)
      
          (push 'O (comp-part X (pop 'I)))
          
          (let (Px (lup-pt X)
                A (car X)
                B (cdr X))
            (push 'O (comp-part Px (pop 'I)))
            (push 'C
              (if (atom B) (get Px 'b) B)
              (if (atom A) (get Px 'a) A)))))
    O))

    
(de ver-start (Nm I)
  (let (P (not I)
        I (if I @ (get-dfs-struct))
        O (ver-dfs-struct I))
        
    (when P
      (pretty I))
        
    (if (full O)
        (prinl Nm ": Pass")
        (prog
          (prinl Nm ": Fail")
          (println O)))))
          
          
# POINTER

(de run-tests ()

  (alt-reg-x-atom-a) #(del-start-val Pt-Sta T)

  #(del-x-atom-a)     #(del-start-val Pt-Sta T)
  
  #{
  (del-x-atom-b)     (del-start-val Pt-Sta T)
  (del-y-atom-a)     (del-start-val Pt-Sta T)
  (del-y-atom-a-reg) (del-start-val Pt-Sta T)
  (del-y-atom-b-reg) (del-start-val Pt-Sta T)
  (del-y-atom-a-nil) (del-start-val Pt-Sta T)
  (del-y-atom-b-nil) (del-start-val Pt-Sta T)
  }#
  
  )
  
(de run-tests-2 ()
  
  (lst-x-atom-b) (del-start-val Pt-Sta T)        
  (lst-x-atom-a) (del-start-val Pt-Sta T)        

  (lst-y-atom-b) (del-start-val Pt-Sta T)
  (lst-y-atom-a) (del-start-val Pt-Sta T)

  (str-x-atom-b) (del-start-val Pt-Sta T)
  (str-x-atom-a) (del-start-val Pt-Sta T)

  (str-y-atom-b) (del-start-val Pt-Sta T)
  (str-y-atom-a) (del-start-val Pt-Sta T)

  (del-x-atom-a) (del-start-val Pt-Sta T)
  (del-x-atom-b) (del-start-val Pt-Sta T)
  (del-y-atom-a) (del-start-val Pt-Sta T)
  (del-y-atom-b) (del-start-val Pt-Sta T)
    
  # Pair
  
  (str-x-pair-x-b) (del-start-val Pt-Sta T)
  (str-x-pair-y-b) (del-start-val Pt-Sta T)

  (str-y-pair-x-b) (del-start-val Pt-Sta T)
  (str-y-pair-y-b) (del-start-val Pt-Sta T)

  (str-y-pair-x-a) (del-start-val Pt-Sta T)        
  (str-y-pair-y-a) (del-start-val Pt-Sta T)
  
  (lst-x-pair-x-b) (del-start-val Pt-Sta T)
  (lst-x-pair-y-b) (del-start-val Pt-Sta T)

  (lst-y-pair-x-b) (del-start-val Pt-Sta T)
  (lst-y-pair-y-b) (del-start-val Pt-Sta T)
  
  (lst-y-pair-x-a) (del-start-val Pt-Sta T)
  (lst-y-pair-y-a) (del-start-val Pt-Sta T)
  
  (bsp-x-atom-b) (del-start-val Pt-Sta T)
  (bsp-y-atom-a) (del-start-val Pt-Sta T)
  (bsp-y-atom-b) (del-start-val Pt-Sta T)
  
  (bsp-x-pair-x-b) (del-start-val Pt-Sta T)
  (bsp-x-pair-y-b) (del-start-val Pt-Sta T)
  
  (bsp-y-pair-x-a) (del-start-val Pt-Sta T)
  (bsp-y-pair-x-b) (del-start-val Pt-Sta T)
  (bsp-y-pair-y-a) (del-start-val Pt-Sta T)
  (bsp-y-pair-y-b) (del-start-val Pt-Sta T))
    
  
