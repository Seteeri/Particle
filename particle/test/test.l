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
        O (ver-dfs-struct I))
        
    (when P
      (pretty I))
        
    (if (full O)
        (prinl Nm ": Pass")
        (prog
          (prin Nm ": ")
          (println O)))))
          
          
# POINTER

(de run-tests-2 ()

  (when T

    (run-tests)

    T))

  
(de run-tests ()

  (test-atom)
  (test-pair))
  
  
(de test-atom ()

  (lst-x-atom-b) (del-start-val Pt-Sta T)
  (lst-x-atom-a) (del-start-val Pt-Sta T)        

  (lst-y-atom-b) (del-start-val Pt-Sta T)
  (lst-y-atom-a) (del-start-val Pt-Sta T)

  (str-x-atom-b) (del-start-val Pt-Sta T)
  (str-x-atom-a) (del-start-val Pt-Sta T)

  (str-y-atom-b) (del-start-val Pt-Sta T)
  (str-y-atom-a) (del-start-val Pt-Sta T)

  #(bsp-atom-x-x-a) (del-start-val Pt-Sta T)
  (bsp-atom-x-x-b) (del-start-val Pt-Sta T)
  (bsp-atom-x-y-a) (del-start-val Pt-Sta T)
  (bsp-atom-x-y-b) (del-start-val Pt-Sta T)
  (bsp-atom-y-y-a) (del-start-val Pt-Sta T)
  (bsp-atom-y-y-b) (del-start-val Pt-Sta T)
  
  (alt-reg-x-atom-a) (del-start-val Pt-Sta T)
  (alt-reg-x-atom-b) (del-start-val Pt-Sta T)
  
  (alt-reg-y-atom-a) (del-start-val Pt-Sta T)
  (alt-reg-y-atom-b) (del-start-val Pt-Sta T)  
  
  (del-x-atom-a)     (del-start-val Pt-Sta T)
  (del-x-atom-b)     (del-start-val Pt-Sta T)
  (del-x-atom-b-nil) (del-start-val Pt-Sta T)
                     
  (del-y-atom-a)     (del-start-val Pt-Sta T)
  (del-y-atom-a-nil) (del-start-val Pt-Sta T)
  (del-y-atom-b)     (del-start-val Pt-Sta T)
  (del-y-atom-b-nil) (del-start-val Pt-Sta T))
  
  
(de test-pair ()
  
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
  
  (bsp-pair-xb-x) (del-start-val Pt-Sta T)
  (bsp-pair-xb-y) (del-start-val Pt-Sta T)
  
  (bsp-pair-ya-x) (del-start-val Pt-Sta T)
  (bsp-pair-ya-y) (del-start-val Pt-Sta T)
  (bsp-pair-yb-x) (del-start-val Pt-Sta T)
  (bsp-pair-yb-y) (del-start-val Pt-Sta T)
  
  (alt-reg-x-pair-a-a)  (del-start-val Pt-Sta T)
  (alt-reg-x-pair-a-px) (del-start-val Pt-Sta T)
  (alt-reg-x-pair-a-py) (del-start-val Pt-Sta T)

  (alt-reg-y-pair-a-a)  (del-start-val Pt-Sta T)
  
  (alt-reg-y-pair-a-px) (del-start-val Pt-Sta T)
  (alt-reg-y-pair-a-py) (del-start-val Pt-Sta T)
  (alt-reg-y-pair-px-a) (del-start-val Pt-Sta T)
  (alt-reg-y-pair-py-a) (del-start-val Pt-Sta T)
  
  (alt-reg-y-pair-px-px) (del-start-val Pt-Sta T)
  (alt-reg-y-pair-px-py) (del-start-val Pt-Sta T)
  (alt-reg-y-pair-py-py) (del-start-val Pt-Sta T)
  (alt-reg-y-pair-py-px) (del-start-val Pt-Sta T)
  
  (del-pair-xb-x-x) (del-start-val Pt-Sta T)
  (del-pair-xb-x-y) (del-start-val Pt-Sta T)
  (del-pair-xb-x-a) (del-start-val Pt-Sta T)
                    
  (del-pair-xb-y-x) (del-start-val Pt-Sta T)
  (del-pair-xb-y-y) (del-start-val Pt-Sta T)
  (del-pair-xb-y-a) (del-start-val Pt-Sta T)
                    
  (del-pair-ya-x-x) (del-start-val Pt-Sta T)
  (del-pair-ya-x-y) (del-start-val Pt-Sta T)
  (del-pair-ya-x-a) (del-start-val Pt-Sta T)
                    
  (del-pair-ya-y-x) (del-start-val Pt-Sta T)
  (del-pair-ya-y-y) (del-start-val Pt-Sta T)
  (del-pair-ya-y-a) (del-start-val Pt-Sta T)
                    
  (del-pair-yb-x-x) (del-start-val Pt-Sta T)
  (del-pair-yb-x-y) (del-start-val Pt-Sta T)
  (del-pair-yb-x-a) (del-start-val Pt-Sta T)
  (del-pair-yb-y-x) (del-start-val Pt-Sta T)
  (del-pair-yb-y-y) (del-start-val Pt-Sta T)
  (del-pair-yb-y-a) (del-start-val Pt-Sta T))  
    
  
