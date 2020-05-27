#{
                      cell [PARTICLE]
                        |
            +-----------+-----------+
            |           |           |
         Number       Symbol       Pair
                        |
                        |
   +--------+-----------+-----------+
   |        |           |           |
  NIL   Internal    Transient    External
}#


(class +Pair +Point)

(dm T (Any
       Lay
       A B)
  (super Any
         Lay
         A B)
  (let (Ch (if (= Lay 'x) 
               (car (get '+Vertex 'chs-pair)) 
               (cadr (get '+Vertex 'chs-pair)))
        Col (if (= Lay 'x)
                (car (get '+Vertex 'cols-pair)) 
                (cadr (get '+Vertex 'cols-pair)))
        (Verts W) (make-vert-pair Ch Col))
    (=: verts Verts)
    (=: dims (list W
                  (meta '(+Vertex) 'adv-vert-2)))))
  
(dm lay> ()
  (super)
  #(prin "  ") (println '+Pair.lay> (: a) (: b) (: data))
  (cons
    (let B (cdr (: data))
      (list (if B @ NIL)
            'b
            This))
    (let A (car (: data))
      (list (if A @ NIL)
            'a
            This))))
  
(dm lup-car> () (lup-pt (car (: data))))
(dm lup-cdr> () (lup-pt (cdr (: data))))
  
  
(dm prv> ()
  (when (: c)
    (if *ptr-on-car
        (sub> (: c))
        (prog
          (when (get (: c) 'line)
            (println 'prv> "Set line: " (: c) (get (: c) 'data))
            (setq *line (: c)))
          # If Y layout, must be newline
          (when (lay-y?> This)
            (when (get *line 'line 'p)
              (setq *line @)
              (println 'prv> "Set line: " @ (get @ 'data))))
          (top> (: c))))))
         
          
(dm nxt> ()
  (if (: b) # NIL
      (nil
        (when (get (: b) 'line)
          (println 'nxt> "Set line: " (: b) NIL)
          (setq *line (: b)))      
        (top> (: b)))
      (let? B (lup-cdr> This)
        (if *ptr-on-car
            (sub> B)
            (prog
              (when (get B 'line)
                (println 'nxt> "Set line: " (: b) (get (: b) 'data))
                (setq *line B))
              (top> B))))))


(dm sup> ()
  (if (is-car?> This)
      (prog
        ~(assert (get (: c) 'line))
        (println 'sup> "Set line: " (: c) (get (: c) 'data))
        (setq *line (: c))
        (top> (: c)))
        
      (top> This)))


(dm sub> ()
    
  (cond ((has-pair?> This)
         ~(assert (get (: a) 'line))
         (println 'sub> "Set line: " (: a) (get (: a) 'data))
         (setq *line (: a))
           
          # If X lay, mov to Car
          # If Y lay, stay Cdr          
          (if (lay-x?> This)
              (prog
                (set-on-car T)
                (sub> (: a)))
              (top> (: a))))
        
        ((not (car (: data))) # NIL
         (when (get (: a) 'line)
           (println 'sub> "Set line: " (: a) (get (: a) 'data))
           (setq *line (: a)))
         # If X lay, mov to Car
         # If Y lay, stay Cdr
         (if (lay-x?> This)
             NIL
             (top> (: a))))
        
        (T
          (let Po-Car (lup-car> This)
            # Keep ptr @ pos
            (top> This)
            (sub> Po-Car)))))

            
(dm con> (Dat Po-Dat Del)

  # Del prv
  # NIL is not ref'd in tree so pass
  (if (: b)
      (prog
        (del> (: b))
        (=: b))
      (when Del
        (del> (lup-pt (cdr (: data))))))
      
  (con (: data) Dat)
  
  # If Po-Dat is an atom
  (when Po-Dat
    (unless (is-pair? Po-Dat)
      (put This 'b Po-Dat)
      (put Po-Dat 'c This))))

      
(dm set> (Dat Po-Dat Del)

  # Del prv
  # NIL is not ref'd in tree so pass
  (if (: a)
      (prog
        (del> (: a))
        (=: a))
      (when Del
        (del> (lup-pt (car (: data))))))
      
  (set (: data) Dat)
  
  # If Po-Dat is an atom
  (when Po-Dat
    (unless (is-pair? Po-Dat)
      (put This 'a Po-Dat)
      (put Po-Dat 'c This))))
