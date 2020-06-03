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
  
  # Lay is either 'X 'Y or T (auto)
  
  # Skip is only for root
  
  (off *gen-line
       *gen-list
       *gen-out)

  (if (pair Any)         
      (gen-pair Any Lay Skips Nrl)
      (setq *gen-out 
            (gen-atom Any)))
   
  *gen-out)
  
  
(de gen-pair (Root
              Lay
              Skips
              Nrl)

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
            N (if (or (= Lay 'x) (= Lay 'y))
                  (if Is-Any-Pair             
                      (gen-pair-xy Any
                                    (if (pair (car Any))
                                        'y
                                        'x))
                      (gen-atom Any))
                  (if Is-Any-Pair             
                      (gen-pair-xy Any
                                    (if (pair (car Any))
                                        'y
                                        'x))
                      (gen-atom Any))))
      
        # Color pairs
        (when (and Is-Any-Pair
                   (pair (car Any)))
          (col-verts> N (cadr (get '+Vertex 'cols-pair)) 1.0))      
      
        #(println 'gen-pair "GEN: " Any)
      
        (unless *gen-out
          (setq *gen-out N))

        # Con NIL or terminal atoms
        # Only con Car when NIL
        (when (or (and (= C 'a)
                       (not Is-Any-Pair)
                       (not Any))
                  (and (= C 'b) 
                       (not Is-Any-Pair)))
          (put P
              (if (= C 'a)
                   'a
                   'b)
              N)
          (put N 'c P))
        
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

        #(println 'gen-pair "..." P (get P 'a) (get P 'b))
          
        (when Is-Any-Pair
        
          (unless (cdr Skips)
            (let B (cdr Any)
              (push 'In (list B 'b N))))
        
          (unless (car Skips)
            (let A (car Any)
              (push 'In (list A 'a N)))))))))
    
            
(de gen-pair-xy (Any Lay)
  (let Pair (new '(+Pair)
              Any
              Lay)
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
  (new '(+Num)
       Any
       'x))


(de gen-sym (Any)
  (new (cond 
        ((box? Any) '(+Box))
        ((str? Any) '(+Str))
        ((ext? Any) '(+Ext))
        ((sym? Any) '(+Int)))
    Any
    'x))

    
(de gen-nil (Any)
  (new '(+Nil)
        Any
        'x))
    
    
(de gen-sym-exp (Any
                 Lay
                 Gen-A
                 Gen-B
                 Ch-Pair
                 Col)
  #{
  
          Symbol
          |
          V
    +-----+-----+                                +----------+---------+
    |  |  | VAL |                                |'hgfedcba'|'onmlkji'|
    +--+--+-----+                                +----------+---------+
        | tail                                       ^
        |                                            |
        V                                            | name
        +-----+-----+     +-----+-----+     +-----+--+--+
        |  |  |  ---+---> | KEY |  ---+---> |  |  |  |  |
        +--+--+-----+     +-----+-----+     +--+--+-----+
          |                                   |
          V                                   V
          +-----+-----+                       +-----+-----+
          | VAL | KEY |                       | VAL | KEY |
          +-----+-----+                       +-----+-----+  
  
    Name is a num ptr

    Use name for str since it exclude quotes (vs sym)
    Name any will fail for box, use 'sym
    
    If lay=y, name/car should have line
    
    Exp:
    NIL = name (one cell)
    1   = name + val
    2   = name + val + props
    
    Syms/Objs:
    
    [X]  CLS-LST
    
    [Y] [X] V1...
    
         K1

    [Y] [X] V2...
    
         K2
         
    NAME
        
  }#
        
  (unless Col (setq Col (get-col-type Any)))
    
  (let (Name (if (and (str? Any) (not (box? Any)))
                 (name Any)
                 (sym Any))
        Val (val Any)
        Car (when Gen-A
              (new '(+Num)
                  Name
                  Lay
                  NIL
                  NIL NIL
                  Col))
        Cdr (when (and Gen-B
                       (not (= Val Any)))
              (gen-point (if (= Val Any) NIL Val) 'x NIL))
        Pair (new (if (str? Any) # Color as a symbol
                      '(+Str)
                      '(+Int))
                  Any
                  Lay
                  Car Cdr                  
                  T))
                  
    (with Pair
      (let ((Verts W) 
              (make-vert-pair Ch-Pair Col (= Val Any)))
        (=: verts Verts)
        (=: dims (list W
                       (meta '(+Vertex) 'adv-vert-2)))))
                      
    # Put line in Cdr
    (unless (get Cdr 'line)
      (put Cdr 'line
        (new '(+Line) NIL NIL NIL Wid-Car)))
        b
    # Put line in symbol also
    (put Pair 'line (new '(+Line)))
    
    # Skip Car and Cdr
    (put Pair 'a)
    (put Pair 'b)
    (idx-pt Pair)
    (put Pair 'a Car)
    (put Pair 'b Cdr)
    
    Pair))    
    
    
(de gen-ptr-full (Any
                  Ch-Pair
                  Col)
  #{
  
          Symbol
          |
          V
    +-----+-----+                                +----------+---------+
    |  |  | VAL |                                |'hgfedcba'|'onmlkji'|
    +--+--+-----+                                +----------+---------+
        | tail                                       ^
        |                                            |
        V                                            | name
        +-----+-----+     +-----+-----+     +-----+--+--+
        |  |  |  ---+---> | KEY |  ---+---> |  |  |  |  |
        +--+--+-----+     +-----+-----+     +--+--+-----+
          |                                   |
          V                                   V
          +-----+-----+                       +-----+-----+
          | VAL | KEY |                       | VAL | KEY |
          +-----+-----+                       +-----+-----+  
    
    Sym->Obj:
    
    [Y] NAME
    
    [X] CLS-LST
    
    [X]    [X]    [X] $
    
    [Y] K  [X] V  [X] V
     V      K      K
    MAIN    
    
    or just Obj:
    
    [X]                 CLS-LST
    
    [X]    [X]    [X] $
    
    [Y] K  [X] V  [X] V
     V      K      K

    condense:
    
    [X]                       CLS-LST
     .      .       .      $
    [Y] ref . time  . date
     V      K       K    
     
    * Condense Y spacing to make room
      * Possible to expand by shrinking font
      * Simulate LOD?
    
    * Lay props x orientation
      * Make Ref prop Y for distinctness and 
      other props X
      
    * Rev puts $ first which is the "name"
      * Techinically, it is null char but
      use name for consistency with symbols
    
    * Add arrows for the pairs to show the rev dir
    
    * Only one '*ptr so draw obj directly
      * To change ptrs: new +Pointer, set ptr to it
      * Wherever *ptr is draw, it'll show the $
        * $ can be referenced in main
        
    * K/V is yellow
    
  }#
        
  # gen-point should pass whether to expand symbols
        
  (unless Col (setq Col (get-col-type Any)))
    
  (let (Name (sym Any)
        Po-Name (new '(+Num)
                  Name
                  Lay
                  NIL NIL
                  NIL
                  Col)
        Po-Cdr (gen-point (val Any) 'x NIL)
        Props (getl Any)
        Po-Car (gen-point Props 'x NIL)
        Po-Pair (new '(+Box)
                  Any
                  'x
                  Po-Car Po-Cdr
                  T))
    
    (with Po-Pair
      (let ((Verts W)
              (make-vert-pair Ch-Pair Col))
        (=: verts Verts)
        (=: dims (list W
                      (meta '(+Vertex) 'adv-vert-2)))))
    
    # Set to X
    (for (P Props P (cdr P))
      (set-lay> (lup-pt P) 'x))
        
    # Attach name to end of plist
    (let (Last (tail 1 Props)
          Po-Last (lup-pt Last))
      (con> Po-Last Name Po-Name))
                  
    (idx-pt Po-Name)        
    (idx-pt Po-Pair)                  

    # Lines
            
    Po-Pair))
    
    
(de gen-ptr (Any
             Ch-Pair
             Col)
        
  (unless Col (setq Col (get-col-type Any)))
    
  (let (Name (if (and (str? Any) (not (box? Any)))
                 (name Any)
                 (sym Any))
        Val (val Any)
        Car (new '(+Num)
              Name
              Lay
              NIL
              NIL NIL
              Col)
        Cdr (gen-point (get Val 'ref) 'x NIL)
        Pair (new (if (str? Any) # Color as a symbol
                      '(+Str)
                      '(+Int))
                  Any
                  Lay
                  Car Cdr                  
                  T))
                  
    (with Pair
      (let ((Verts W) 
              (make-vert-pair Ch-Pair Col (= Val Any)))
        (=: verts Verts)
        (=: dims (list W
                       (meta '(+Vertex) 'adv-vert-2)))))
                      
    # Put line in Cdr
    (unless (get Cdr 'line)
      (put Cdr 'line
        (new '(+Line) NIL NIL NIL Wid-Car)))
        b
    # Put line in symbol also
    (put Pair 'line (new '(+Line)))
    
    # Skip Car and Cdr
    (put Pair 'a)
    (put Pair 'b)
    (idx-pt Pair)
    (put Pair 'a Car)
    (put Pair 'b Cdr)
    
    Pair))
