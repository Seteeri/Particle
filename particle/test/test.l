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

(de run-tests ()

  # Finish checking each

  #{
  
    lst-x-atom-b: Pass
    lst-x-atom-a: Pass
    lst-y-atom-b: Pass
    lst-y-atom-a: Pass
    str-x-atom-b: Pass
    str-x-atom-a: Pass
    str-y-atom-b: Pass
    str-y-atom-a: Pass
    alt-reg-x-atom-a: Pass
    alt-reg-x-atom-b: Pass
    alt-reg-y-atom-a: Pass
    alt-reg-y-atom-b: Pass
    del-x-atom-a: Pass
    del-x-atom-b: Pass
    del-x-atom-b-nil: Pass
    del-y-atom-a: Pass
    del-y-atom-a-nil: Pass
    del-y-atom-b: Pass
    del-y-atom-b-nil: Pass
    str-x-pair-x-b: Pass
    str-x-pair-y-b: (T T NIL T NIL T NIL)
    str-y-pair-x-b: Pass
    str-y-pair-y-b: (T T NIL T NIL T NIL)
    str-y-pair-x-a: (T T T NIL T NIL T)
    str-y-pair-y-a: (T T T NIL T NIL NIL)
    lst-x-pair-x-b: Pass
    lst-x-pair-y-b: Pass
    lst-y-pair-x-b: Pass
    lst-y-pair-y-b: Pass
    lst-y-pair-x-a: Pass
    lst-y-pair-y-a: Pass
    bsp-x-atom-b: Pass
    bsp-y-atom-a: Pass
    bsp-y-atom-b: Pass
    bsp-x-pair-x-b: Pass
    bsp-x-pair-y-b: Pass
    bsp-y-pair-x-a: Pass
    bsp-y-pair-x-b: Pass
    bsp-y-pair-y-a: Pass
    bsp-y-pair-y-b: Pass
  
  }#
  
  (str-x-pair-y-b)
  
  (when NIL

  (alt-reg-x-pair-a-a) (call 'sleep 5) (del-start-val Pt-Sta T)
  (alt-reg-x-pair-a-px) (call 'sleep 5) (del-start-val Pt-Sta T)
  # (alt-reg-x-pair-a-py)

  (alt-reg-y-pair-a-a) (call 'sleep 5) (del-start-val Pt-Sta T)
  
  (alt-reg-y-pair-a-px) (call 'sleep 5) (del-start-val Pt-Sta T)
  (alt-reg-y-pair-a-py) (call 'sleep 5) (del-start-val Pt-Sta T)
  (alt-reg-y-pair-px-a) (call 'sleep 5) (del-start-val Pt-Sta T)
  (alt-reg-y-pair-py-a) (call 'sleep 5) (del-start-val Pt-Sta T)
  
  (alt-reg-y-pair-px-px) (call 'sleep 5) (del-start-val Pt-Sta T)
  (alt-reg-y-pair-px-py) (call 'sleep 5) (del-start-val Pt-Sta T)
  (alt-reg-y-pair-py-py) (call 'sleep 5) (del-start-val Pt-Sta T)
  (alt-reg-y-pair-py-px) (call 'sleep 5) (del-start-val Pt-Sta T)

  ))

  
(de run-tests-2 ()
  
  (lst-x-atom-b) (del-start-val Pt-Sta T)        
  (lst-x-atom-a) (del-start-val Pt-Sta T)        

  (lst-y-atom-b) (del-start-val Pt-Sta T)
  (lst-y-atom-a) (del-start-val Pt-Sta T)

  (str-x-atom-b) (del-start-val Pt-Sta T)
  (str-x-atom-a) (del-start-val Pt-Sta T)

  (str-y-atom-b) (del-start-val Pt-Sta T)
  (str-y-atom-a) (del-start-val Pt-Sta T)

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
  (del-y-atom-b-nil) (del-start-val Pt-Sta T)
  
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
    
  
