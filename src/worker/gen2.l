#{
    (prinl "START")
    
    #(gen2-part '(((((NIL))))))
    #(gen2-part '(((1) 2) 3))
    #(gen2-part '(((1 2) 3 4) 5 6))
    #(gen2-part '((1 2) (3 4) (5 6)))
    
    (let L (last *gen2-out)
      (println "RESULT: " L)
      (println "RESULT: " (get L 'data))
      
      # Remove line from root
      (view (getl (get L 'line)))
      
      (lay-part-start L L))
    
    (prinl "DONE")
}#

(def '*gen2-line)
(def '*gen2-list)
(def '*gen2-out)

(de gen2-part (Any)
  # TODO: Handle circular lists...use another dot at the end
  
  (off *gen2-line
       *gen2-list
       *gen2-out)

  (if (pair Any)
                         
      (gen2-pair Any)
      
      (gen2-atom Any))
      
  *gen2-out)
  

(de gen2-pair (Root)

  # Iterative pre order traversal

  # Draw pair
  # Draw cars, deepest first
  # Draw cdr, deepest first
  
  (let (In (list (list Root 'a NIL))
        X NIL)
    
    (while (setq X (pop 'In))
            
      (let ((Any C P) X
            N (if (pair Any)                 
                  (gen2-pair-xy Any
                                (if (pair (car Any))
                                    'y
                                    'x))
                  (gen2-atom Any)))
      
        (unless *gen2-out
          (setq *gen2-out N))
      
        # Connect to prv
        (when P
          (if (= C 'a)
              (put P 'a N)
              (put P 'b N))
          (put N 'c P))      
      
        # Create line if needed
        (cond ((or (and (pair (car (get P 'data)))
                        (= C 'a))
                   (not P)) # Root
                (put N 'line
                      (new '(+Line)))
                (push '*gen2-list *gen2-line)
                (setq *gen2-line N)
                (println "Line Car/Root: " N Any (car *gen2-list)) (prinl))
                
              ((and (pair (get P 'data))
                    (pair (car (get P 'data))))
                (put N 'line
                     (new '(+Line)
                          P
                          NIL
                          (pop '*gen2-list)))
                (put P 'line 'n N)
                (setq *gen2-line N)
                (println "Line Cdr: " N Any) (prinl)))
          
                                
        # Push right first
        (when (pair Any)
        
          (let B (cdr Any)
            (push 'In (list B 'b N)))
        
          (let A (car Any)
            (push 'In (list A 'a N))))))))
            
            
(de gen2-pair-xy (Any Lay)
  (let (Col (cadr *pair-col-def)
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
(de gen2-sym-full (Any Lay)

  # Technically, name is a number cell

  # Use name for str since it exclude quotes
  # Name any will fail for box, use 'sym
  
  # If lay=y, name should have line
  
  (unless Col (setq Col (get-color-type Any)))
  
  (let (Name (if (str? Any) (name Any) (sym Any))
        Val (val Any)
        Dr (if (= Val Any) NIL Val)
        (Verts-Car Wid-Car) (make-vert-atom Name Col)
        Car (new '(+Num) # Num type but content is a string
                 Name
                 NIL NIL
                 Verts-Car
                 Lay)
        Cdr (when (not (= Val Any))
              (gen2-part Dr))
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

      (put Car 'c Pair)

      (when (not (= Val Any))
        (put Cdr 'c Pair))
              
      Pair))
