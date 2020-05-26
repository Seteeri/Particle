#{

  # Test gen-point
  
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
  
(de gen-point (Any Lay Skips Nrl)
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
            Is-Any-Pair (pair Any)
            N (if Is-Any-Pair             
                  (gen-pair-xy Any
                                (if (pair (car Any))
                                    'y
                                    'x))
                  (gen-atom Any)))
      
        # Color pairs
        (when (and Is-Any-Pair
                   (pair (car Any)))
          (col-verts> N (cadr (get '+Vertex 'cols-pair)) 1.0))      
      
        #(println "GEN: " Any)
      
        (unless *gen-out
          (setq *gen-out N))

        # Con NIL
        (when (and (not Any) P)
          (put P (if (= C 'a) @ 'b) N))
          
        # Create lines
        (cond 
          ((not C) # Root
           (unless Nrl
             (put N 'line
                   (new '(+Line))))
           (push '*gen-list N))
              
              
          # Is a pair itself with a car pair
          # or parent is a pair with a car pair              
          ((or (and Is-Any-Pair (pair (car Any)))
                (and (pair (get P 'data)) (pair (car (get P 'data)))))
            (if (= C 'a)
                (prog
                  (put N 'line (new '(+Line)))
                  (push '*gen-list N))
                (prog
                  (put N 'line
                      (new '(+Line)
                            P
                            NIL
                            (car *gen-list)))
                  (put P 'line 'n N)))))

        # End of list
        (when (and (= C 'b)
                   (not Is-Any-Pair))
          (pop '*gen-list))

        (when Is-Any-Pair
        
          (unless (cdr Skips)
            (let B (cdr Any)
              (push 'In (list B 'b N))))
        
          (unless (car Skips)
            (let A (car Any)
              (push 'In (list A 'a N)))))))))
    
            
(de gen-pair-xy (Any Lay)
  (let (Col (if (= Lay 'x) (car (get '+Vertex 'cols-pair)) (cadr (get '+Vertex 'cols-pair)))
        Ch (if (= Lay 'x) (car (get '+Vertex 'chs-pair)) (cadr (get '+Vertex 'chs-pair)))
        (Verts-Pair Wid-Pair) (make-vert-pair Ch Col)
        Pair (new '(+Pair)
                  Any
                  NIL NIL # next loop will set
                  Verts-Pair
                  Lay))
      (put Pair 'dims (list Wid-Pair (meta '(+Vertex) 'adv-vert-2)))
      (idx-pt Pair)
      Pair))
            
            
(de gen-atom (Any)
  (let Atm (if (num? Any)
               (gen-num Any)
               (if Any
                   (gen-sym Any)
                   (gen-nil Any)))
    (when Any
      (idx-pt Atm))
    Atm))


(de gen-num (Any)
  (let ((Verts Wid) (make-vert-atm Any (get-col-type Any))
        Po (new '(+Num)
                  Any
                  NIL NIL
                  Verts
                  'x))
    (put Po 'dims (list Wid (meta '(+Vertex) 'adv-vert-2)))
    Po))
    
    
(de gen-nil (Any)
  (let ((Verts Wid) (make-vert-atm Any (get-col-type Any) (caddr (get '+Vertex 'chs-pair)))
        Po (new '(+Nil)
                  Any
                  NIL NIL
                  Verts
                  'x))
    (put Po 'dims (list Wid (meta '(+Vertex) 'adv-vert-2)))
    Po))
    

(de gen-sym (Any)
  (let ((Verts Wid) (make-vert-atm Any (get-col-type Any))
        Cls (cond ((box? Any) '(+Box))
                  ((str? Any) '(+Str))
                  ((ext? Any) '(+Ext))
                  ((sym? Any) '(+Int)))
        Po (new Cls
                  Any
                  NIL NIL
                  Verts
                  'x))
    (put Po 'dims (list Wid (meta '(+Vertex) 'adv-vert-2)))
    Po))
    
    
# Expand sym
(de gen-sym-full (Any Lay Gen-A Gen-B Ch-Pair Col)

  # Technically, name is a number cell

  # Use name for str since it exclude quotes
  # Name any will fail for box, use 'sym
  
  # If lay=y, name should have line
  
  # If node exists, add it
  # Else create it,  
        
  (unless Col (setq Col (get-col-type Any)))
    
  (let (Name (if (str? Any) (name Any) (sym Any))
        Val (val Any)
        Dr (if (= Val Any) NIL Val)
        (Verts-Car Wid-Car) (make-vert-atm Name Col)
        Car (when Gen-A
              # Num type but content is a string
              (new '(+Num)
                    Name
                    NIL NIL
                    Verts-Car
                    Lay))
        Cdr (when (and Gen-B
                       (not (= Val Any)))
              (gen-point Dr 'x NIL))
        (Verts-Pair Wid-Pair) (make-vert-pair Ch-Pair Col (= Val Any))
        Pair (new (if (str? Any) '(+Str) '(+Int)) # Color as a symbol
                      Any
                      Car Cdr
                      Verts-Pair
                      Lay))
                            
    # Set dims
    (put Car 'dims (list Wid-Car (meta '(+Vertex) 'adv-vert-2)))
    (put Pair 'dims (list Wid-Pair (meta '(+Vertex) 'adv-vert-2)))
                            
    # Put line in Cdr
    (unless (get Cdr 'line)
      (put Cdr 'line
        (new '(+Line) NIL NIL NIL Wid-Car)))
        
    # Put line in symbol also
    (put Pair 'line (new '(+Line)))
    
    # Skip Car and Cdr
    (put Pair 'a)
    (put Pair 'b)
    (idx-pt Pair)
    (put Pair 'a Car)
    (put Pair 'b Cdr)
    
    Pair))
    
    
##############


(de h64 (X)
  (native PATH-H64S "h64s" 'N X))
  
    
(de idx-pt (P)

  (with P

    ~(assert (: data))
  
    (let Node (lup-node (: data))
    
      # If node: add to list
      # else: create it
      (if Node
      
          # Create list if needed
          (prog
            (if (lst? (cdr Node))
              (push (cdr Node) P)
              (con Node (list P)))
            (when T (println 'idx-pt 'Exists! (h64 (adr (: data))) P (type P) (depth *points) *cnt-p)))

          (prog
            (idx-node P (get P 'data))
            (when (: a) (idx-node (: a) (get (: a) 'data)))
            (when (: b) (idx-node (: b) (get (: b) 'data))))))))

  
(de idx-node (X D)
  (let (A (adr D)
        H (h64 A))
    (idx '*points (cons H X) T)
    (inc '*cnt-p)
    (when T (println 'idx-node H A D X (depth *points) *cnt-p))))
  
  
(de lup-node (D)
  (lup *points (h64 (adr D))))
  
  
(de lup-pt (D)
  (cdr (lup *points (h64 (adr D)))))


(de del-node (D)
  (idx *points (h64 (adr D)) NIL))
