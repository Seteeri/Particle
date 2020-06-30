#{

  Create tests for exp sym later
  Create tests for pointer mov

  Verify color (first vert)
  ...and pointer?
  Verify *start also
  
  Screenshot window for each test
  for visual verification
  
  Refactor so auto update standards:
  * ver
  * std
  
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
        O (ver-dfs-struct I)
        F (full O))
    (if F
        (prinl "# Pass")
        (prog
          (prin "# Fail: ") (println O)))
    (pp (any Nm))
    (prinl)
    F))

          
# POINTER

(de run-tests ()

  # Read test files, run any fns

  (run-tests-dir "atom")
  (run-tests-dir "pair"))


(de run-tests-dir (Nm)
  (let Dir-In (pack "particle/test/" Nm)
    (for File (dir Dir-In)
      (let (Path-Out (pack "tests/" Nm "/" File)
            Fns (in (pack Dir-In "/" File)
                  (str (till NIL T))))
        (out NIL (prinl Path-Out))
        (out Path-Out
          (for Fn Fns
            (out NIL (prin "Test " (cadr Fn) "..."))
            (let F ((val (cadr Fn)))
              (del-start-val Pt-Sta T)
              (out NIL (print F) (prinl)))))))))
