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
        (prinl Nm ": Test passed")
        (prog
          (prinl Nm ": Test failed")
          (println O)))))
          
          
# POINTER

  
(de run-test ()
  (run-atom)
  (run-pair))
