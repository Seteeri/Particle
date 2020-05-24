#{

  # Test gen-part
  
  '(("1" "Content"
      ("1.1" "Content"
      ("1.1.1" "Content")
      ("1.1.1" "Content"
        ("1.1.1.1" "Content")
        ("1.1.1.2" "Content")))))
        
  '(((((NIL))))))
  
  '(((1) 2) 3))
  
  '(((1 2) 3 4) 5 6))
  
  '((1 2) (3 4) (5 6)))      
  
  '(1 (2))
  
}#


(def '*gen-line)
(def '*gen-list)
(def '*gen-out) 
  
(de gen-part (Any Lay Skips Nrl)
  # TODO: Handle circular lists...use another dot at the end
  # Lay is not used
  # Skip is only for root
  
  (off *gen-line
       *gen-list
       *gen-out)

  (if (pair Any)         
      (gen-pair Any Skips Nrl)
      (setq *gen-out 
            (gen-atom Any)))
      
  *gen-out)
  
  
(de gen-pair (Root Skips Nrl)

  # Iterative pre order traversal

  # Draw pair
  # Draw cars, deepest first
  # Draw cdr, deepest first
  
  (when Nrl (push '*gen-list NIL))
  
  (let (In (list (list Root NIL NIL))
        X NIL)
    
    (while (setq X (pop 'In))
            
      (let ((Any C P) X
            N (if (pair Any)                 
                  (gen-pair-xy Any
                                (if (pair (car Any))
                                    'y
                                    'x))
                  (gen-atom Any)))
      
        #(println Any N (pair (car (get P 'data))))
        #(prinl)
      
        (unless *gen-out
          (setq *gen-out N))
      
        # Connect to prv
        (when P
          (if (= C 'a)
              (put P 'a N)
              (put P 'b N))
          (put N 'c P))      
      
        # Color pairs
        (when (and (pair Any) (pair (car Any)))
          (col-verts> N (cadr *cols-pair) 1.0))          
      
        # Create line if needed
        (cond ((not C) # Root
                (unless Nrl
                  (put N 'line
                        (new '(+Line))))
                (push '*gen-list N)
                #(prinn "Line Root: " N Any *gen-list) (prinl)
                )
              
              ((or (and (pair Any) (pair (car Any)))
                   (and (pair (get P 'data)) (pair (car (get P 'data)))))
                # Is a pair itself with a car pair
                # or parent is a pair with a car pair
                (if (= C 'a)
                    (prog
                      (put N 'line (new '(+Line)))
                      (push '*gen-list N)
                      #(println "Line Car: " N Any (car *gen-list)) (prinl)
                      )
                    (prog
                      (put N 'line
                          (new '(+Line)
                                P
                                NIL
                                (car *gen-list)))
                      (put P 'line 'n N)
                      #(println "Line Cdr: " N Any (car *gen-list))
                      #(println (get N 'line 'l))
                      #(prinl)
                      ))))

                      
        (when (is-end?> N)
          (pop '*gen-list)
          #(println "Pop list: ") (prinl)
          )

        (when (pair Any)
        
          (unless (cdr Skip)
            (let B (cdr Any)
              (push 'In (list B 'b N))))
        
          (unless (car Skip)
            (let A (car Any)
              (push 'In (list A 'a N)))))))))
            
            
(de gen-pair-xy (Any Lay)
  (let (Col (if (= Lay 'x) (car *cols-pair) (cadr *cols-pair))
        Ch (if (= Lay 'x) (car *ch-pair-def) (cadr *ch-pair-def))
        (Verts-Pair Wid-Pair) (make-vert-pair Ch Col)
        Pair (new '(+Pair)
                  Any
                  NIL NIL # next loop will set
                  Verts-Pair
                  Lay))
      (put Pair 'dims (list Wid-Pair (meta '(+Vertex) 'adv-vert-2)))
      Pair))
            
            
(de gen-atom (Any)
  (if (num? Any)
      (gen-num Any)
      (if Any
          (gen-sym Any)
          (gen-nil Any))))


(de gen-num (Any)
  (let ((Verts Wid) (make-vert-atm Any (get-color-type Any))
        Part (new '(+Num)
                  Any
                  NIL NIL
                  Verts
                  'x))
    (put Part 'dims (list Wid (meta '(+Vertex) 'adv-vert-2)))
    Part))
    
    
(de gen-nil (Any)
  (let ((Verts Wid) (make-vert-atm Any (get-color-type Any) (caddr *ch-pair-def))
        Part (new '(+Nil)
                  Any
                  NIL NIL
                  Verts
                  'x))
    (put Part 'dims (list Wid (meta '(+Vertex) 'adv-vert-2)))
    Part))
    

(de gen-sym (Any)
  (let ((Verts Wid) (make-vert-atm Any (get-color-type Any))
        Cls (cond ((box? Any) '(+Box))
                  ((str? Any) '(+Str))
                  ((ext? Any) '(+Ext))
                  ((sym? Any) '(+Int)))
        Part (new Cls
                  Any
                  NIL NIL
                  Verts
                  'x))
    (put Part 'dims (list Wid (meta '(+Vertex) 'adv-vert-2)))
    Part))
    
    
# Expand sym
(de gen-sym-full (Any Lay Skips Char-Pair Col)

  # Technically, name is a number cell

  # Use name for str since it exclude quotes
  # Name any will fail for box, use 'sym
  
  # If lay=y, name should have line
  
  (unless Col (setq Col (get-color-type Any)))
  
  (let (Name (if (str? Any) (name Any) (sym Any))
        Val (val Any)
        Dr (if (= Val Any) NIL Val)
        (Verts-Car Wid-Car) (make-vert-atm Name Col)
        Car (unless (car Skips)
              # Num type but content is a string
              (new '(+Num)
                    Name
                    NIL NIL
                    Verts-Car
                    Lay))
        Cdr (when (and (not (cdr Skips))
                       (not (= Val Any)))
              (gen-part Dr 'x NIL))
        (Verts-Pair Wid-Pair) (make-vert-pair Char-Pair Col (= Val Any))
        Pair (new (if (str? Any) '(+Str) '(+Int)) # Color as a symbol
                       Any
                       Car Cdr
                       Verts-Pair
                       Lay))
                                              
    # Set dims
    (put Car 'dims (list Wid-Car (meta '(+Vertex) 'adv-vert-2)))
    (put Pair 'dims (list Wid-Pair (meta '(+Vertex) 'adv-vert-2)))
                      
    # If CAR/CDR, is a list, set C to pair

    (when Car
      (put Car 'c Pair))

    (when (and Cdr
               (not (= Val Any)))
      (put Cdr 'c Pair))
      
    # Put line in Cdr
    (unless (get Cdr 'line)
      (put Cdr 'line
        (new '(+Line) NIL NIL NIL Wid-Car)))
      
    Pair))
