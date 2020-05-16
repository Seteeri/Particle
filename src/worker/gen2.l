#{

  # Test gen2-part
  
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

(def '*gen2-line)
(def '*gen2-list)
(def '*gen2-out)

(de gen2-part-x (Any Skips Nrl)
  # TODO: Handle circular lists...use another dot at the end
  
  # Skip is only for root
  
  (off *gen2-line
       *gen2-list
       *gen2-out)

  (if (pair Any)         
      (gen2-pair-x Any Skips Nrl)
      (setq *gen2-out 
            (gen2-atom Any)))
      
  *gen2-out)
  
  
(de gen2-part (Any Lay Skips Nrl)
  # TODO: Handle circular lists...use another dot at the end
  
  # Skip is only for root
  
  (off *gen2-line
       *gen2-list
       *gen2-out)

  (if (pair Any)         
      (gen2-pair Any Skips Nrl)
      (setq *gen2-out 
            (gen2-atom Any)))
      
  *gen2-out)  
  

(de gen2-pair-x (Root Skips Nrl)

  # Iterative pre order traversal

  # Draw pair
  # Draw cars, deepest first
  # Draw cdr, deepest first
  
  (when Nrl (push '*gen2-list NIL))
  
  (let (In (list (list Root NIL NIL))
        X NIL)
    
    (while (setq X (pop 'In))
            
      (let ((Any C P) X
            N (if (pair Any)                 
                  (gen2-pair-xy Any 'x)
                  (gen2-atom Any)))
      
        #(println Any N (pair (car (get P 'data))))
        #(prinl)
      
        (unless *gen2-out
          (setq *gen2-out N))
      
        # Connect to prv
        (when P
          (if (= C 'a)
              (put P 'a N)
              (put P 'b N))
          (put N 'c P))      
      
        # Create line if needed
        (cond ((not C) # Root
                (unless Nrl
                  (put N 'line
                        (new '(+Line))))
                (push '*gen2-list N)
                (println "Line Root: " N Any *gen2-list) (prinl)
                )
              
              ((or (and (pair Any) (pair (car Any)))
                   (and (pair (get P 'data)) (pair (car (get P 'data)))))
                # Is a pair itself with a car pair
                # or parent is a pair with a car pair
                (if (= C 'a)
                    (prog
                      (put N 'line (new '(+Line)))
                      (push '*gen2-list N)
                      (println "Line Car: " N Any *gen2-list) (prinl)
                      ))))
                      
        (when (and (not Any)
                   (= C 'b))
          (pop '*gen2-list)
          (println "Finish list: " )
          #(prinl)
          )

        (when (pair Any)
        
          (unless (cdr Skip)
            (let B (cdr Any)
              (push 'In (list B 'b N))))
        
          (unless (car Skip)
            (let A (car Any)
              (push 'In (list A 'a N)))))))))  
  
  
(de gen2-pair (Root Skips Nrl)

  # Iterative pre order traversal

  # Draw pair
  # Draw cars, deepest first
  # Draw cdr, deepest first
  
  (when Nrl (push '*gen2-list NIL))
  
  (let (In (list (list Root NIL NIL))
        X NIL)
    
    (while (setq X (pop 'In))
            
      (let ((Any C P) X
            N (if (pair Any)                 
                  (gen2-pair-xy Any
                                (if (pair (car Any))
                                    'y
                                    'x))
                  (gen2-atom Any)))
      
        #(println Any N (pair (car (get P 'data))))
        #(prinl)
      
        (unless *gen2-out
          (setq *gen2-out N))
      
        # Connect to prv
        (when P
          (if (= C 'a)
              (put P 'a N)
              (put P 'b N))
          (put N 'c P))      
      
        # Create line if needed
        (cond ((not C) # Root
                (unless Nrl
                  (put N 'line
                        (new '(+Line))))
                (push '*gen2-list N)
                #(println "Line Root: " N Any *gen2-list) (prinl)
                )
              
              ((or (and (pair Any) (pair (car Any)))
                   (and (pair (get P 'data)) (pair (car (get P 'data)))))
                # Is a pair itself with a car pair
                # or parent is a pair with a car pair
                (if (= C 'a)
                    (prog
                      (put N 'line (new '(+Line)))
                      (push '*gen2-list N)
                      #(println "Line Car: " N Any *gen2-list) (prinl)
                      )
                    (prog
                      (put N 'line
                          (new '(+Line)
                                P
                                NIL
                                (car *gen2-list)))
                      (put P 'line 'n N)
                      #(println "Line Cdr: " N Any *gen2-list)
                      #(println (get N 'line 'l))
                      #(prinl)
                      ))))
                      
        (when (and (not Any)
                   (= C 'b))
          (pop '*gen2-list)
          #(println "Finish list: " )
          #(prinl)
          )

        (when (pair Any)
        
          (unless (cdr Skip)
            (let B (cdr Any)
              (push 'In (list B 'b N))))
        
          (unless (car Skip)
            (let A (car Any)
              (push 'In (list A 'a N)))))))))
            
            
(de gen2-pair-xy (Any Lay)
  (let (Col (if (= Lay 'x) (car *pair-col-def) (cadr *pair-col-def))
        (Verts-Pair Wid-Pair) (make-vert-pair NIL Col)
        Pair (new '(+Pair)
                  Any
                  NIL NIL # next loop will set
                  Verts-Pair
                  Lay))
      (put Pair 'dims (list Wid-Pair (meta '(+Vertex) 'adv-vert-2)))
      Pair))
            
            
(de gen2-atom (Any)
  (if (num? Any)
      (gen2-num Any)
      (if Any
          (gen2-sym Any)
          (gen2-nil Any))))


(de gen2-num (Any)
  (let ((Verts Wid) (make-vert-atom Any (get-color-type Any))
        Part (new '(+Num)
                  Any
                  NIL NIL
                  Verts
                  'x))
    (put Part 'dims (list Wid (meta '(+Vertex) 'adv-vert-2)))
    Part))
    
    
(de gen2-nil (Any)
  (let ((Verts Wid) (make-vert-atom Any (get-color-type Any))
        Part (new '(+Nil)
                  Any
                  NIL NIL
                  Verts
                  'x))
    (put Part 'dims (list Wid (meta '(+Vertex) 'adv-vert-2)))
    Part))
    

(de gen2-sym (Any)
  (let ((Verts Wid) (make-vert-atom Any (get-color-type Any))
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
(de gen2-sym-full (Any Lay Skips Char-Pair Col)

  # Technically, name is a number cell

  # Use name for str since it exclude quotes
  # Name any will fail for box, use 'sym
  
  # If lay=y, name should have line
  
  (unless Col (setq Col (get-color-type Any)))
  
  (let (Name (if (str? Any) (name Any) (sym Any))
        Val (val Any)
        Dr (if (= Val Any) NIL Val)
        (Verts-Car Wid-Car) (make-vert-atom Name Col)
        Car (unless (car Skips)
              # Num type but content is a string
              (new '(+Num)
                    Name
                    NIL NIL
                    Verts-Car
                    Lay))
        Cdr (when (and (not (cdr Skips))
                       (not (= Val Any)))
              (gen2-part Dr 'x NIL T))
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
            
    Pair))
