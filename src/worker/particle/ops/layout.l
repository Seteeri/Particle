# Refactor these into methods

(def '*cur-x)
(def '*cur-y)
(def '*line-lay)
(def '*y-lay)

(de upd-cur ()

  #{
  
   O
    [X] CDR|
    CAR    |
    -------+

   O
    [X] CAR|
    CDR    |
    -------+          
  }#
  
  (setq *cur-x (if *cur-x
                  (max *cur-x (get *cur 1))
                  (get *cur 1)))
  (setq *cur-y (if *cur-y
                   (min *cur-y (get *cur 2))
                   (get *cur 2))))

(de upd-dims (Part)
  (with Part
    (let Dims (list (abs (- *cur-x (get (: ori) 1)))
                    (abs (- *cur-y (get (: ori) 2))))
      (=: dims Dims)
      (when (: line)
        (put (: line) 'dims Dims)))))


(de upd-dims-2 (Part)
  (with Part
    (let Dims (list (abs (- *cur-x (get (: ori) 1)))
                    (abs (- *cur-y (get (: ori) 2))))
      (=: dims Dims))))


(de upd-line-m (Part)
  # Only set on min/max
  (with Part
    (when (: line)
      (let (X (abs (- *cur-x (get (: ori) 1)))
            Y (abs (- *cur-y (get (: ori) 2)))
            D (list (max (car (get (: line) 'dims)) X)
                    (max (cadr (get (: line) 'dims)) Y)))
          (put (: line) 'dims D)))))


(de lay-part-start (Part Line Skip)
  (setq *cur-x NIL
        *cur-y NIL
        *line-lay Line)
  (lay-part Part Line Skip))


(de lay-part (Part Line Skip)
  
  #{
    Only apply skip to immediate Part
    'skip-car
    'skip-cdr
    'skip-car-cdr
  }#
  
  (if (isa '(+Pair) Part)
  
      (lay-pair Part Line Skip)
      
      (lay-atom Part Line Skip)))


(de lay-pair (Part Line Skip)

  # Iterative pre order traversal

  # Draw pair
  # Draw cars, deepest first
  # Draw cdr, deepest first

  # Push root
  (let L ()
    
    (push 'L (cons Part 'a))
    
    # Proccess root
    (while (setq Part (pop 'L))
      
      (let (Type (cdr Part)
            Part (car Part))
        
        (when NIL
          (println 'lay-pair 
                   Part
                   (get Part 'data)
                   '...
                   (if (= Type 'a) 'Car 'Cdr)
                   'of (get Part 'c) (get Part 'c 'data)))
                 
        (if (= (get Part 'lay) 'x)
            (lay-pair-x Part Type (caar L))
            (prog
              (lay-pair-y Part Type (caar L))))
    
        (unless (= Skip 'skip-car-cdr)
            
          # Push right first
          (let B (get Part 'b)
            (if (= Skip 'skip-cdr)
                (setq Skip NIL)
                (unless (or (= B Part) (not B))
                  (push 'L (cons B 'b)))))
        
          (let A (get Part 'a)
            (if (= Skip 'skip-car)
                (setq Skip NIL)
                (unless (or (= A Part) (not A))
                  (push 'L (cons A 'a))))))))))


(de lay-pair-x (Part Type Nxt)

  #(println 'lay-pair-x "X: " Part (get Part 'data) Type)

  (when (= Type 'b)
  
    # For non-newline Cdr:
    # X = bnds of Car
    # Y = origin of Pair

    #(println 'lay-pair-x "Mov cur x to *cur-x: " (format (get *cur 1) *Scl))
    #(println 'lay-pair-x "Mov cur y to origin: " (format (get *cur 2) *Scl))
    
    # Newline will override these
    (adv-cur 2.0)
    (set-cur-y (get Part 'c 'ori 2)))
      
  (lay-x> Part)

  (upd-cur)
  
  # Update on NIL or line
  (when (isa '(+Nil) Part)
    (when (get *line-lay 'line 'l)
      (println 'lay-pair-x "Update line: "
              (get *line-lay 'line 'l)
              (get *line-lay 'line 'l 'data))    
      (upd-dims (get *line-lay 'line 'l)))
    (upd-dims *line-lay)))


(de lay-pair-y (Part Type Nxt)

  #(println 'lay-pair-y "Y: " Part (get Part 'data) Type)

  (when (= Type 'b)
    # For non-newline Cdr:
    # X = origin of Pair
    # Y = bnds of Car
    
    # Newline will override...

    (if (= (get Part 'c 'lay) 'x)
        (prog
          # X should be in place, else get from cur bnds
          (set-cur-x *cur-x) (adv-cur)
          (set-cur-y (get Part 'c 'ori 2)))

        (prog
          (set-cur-x (get Part 'c 'ori 1))
          # Y should be in place, else get from cur bnds
          (set-cur-y *cur-y) (nl-cur 3.0))))
    
  (lay-y> Part)

  (upd-cur)
  
  (when (isa '(+Nil) Part)
    (when (get *line-lay 'line 'l)
      (println 'lay-pair-y "Update line: "
              (get *line-lay 'line 'l)
              (get *line-lay 'line 'l 'data))    
      (upd-dims (get *line-lay 'line 'l)))
    (upd-dims *line-lay)))

  
(de lay-atom (Part Line Skip)
    
  (if (or (isa '+Num Part)
          (isa '+Nil Part))
      
      (prog

        # Move Pair first so below to get correct pos
        # mov> does not use draw fn so it does not use *cur
        (setq *cur (mov> Part *cur))
        #(adv-cur)

        (upd-cur)

        Part)

      (prog
  
        # Should not call lay pair but rather the method
        # lay-sym, lay-str
        (lay-pair Part Line Skip))))


(de force-lay (Part Lay)
  (with Part
    (=: lay Lay)
    (when (and (: a)
                (not (= (: a) Part)))
      (force-lay (: a) Lay))
    (when (and (: b)
               (not (= (: b) Part)))
      (force-lay (: b) Lay))))
              

(de swap-lay (Tgt)
  (force-lay Tgt 
             (if (= (get Tgt 'lay) 'x) 'y 'x))
        
  # Note this causes bounds change also
        
  (mov-cur-part Tgt 'start)
  # Opt: force layout
  (lay-part Tgt (get *main 'ori)))
