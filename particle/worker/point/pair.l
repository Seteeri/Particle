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
  #(prin "  ") (println '+Pair.lay> (: a) (: b) (: any))
  (cons
    (let B (cdr (: any))
      (list (if B @ NIL)
            'b
            This))
    (let A (car (: any))
      (list (if A @ NIL)
            'a
            This))))
  
(dm lup-car> () (lup-pt (car (: any))))
(dm lup-cdr> () (lup-pt (cdr (: any))))
  
  
(dm prv> ()
  (when (: c)
    (if *ptr-on-car
        (sub> (: c))
        (prog
          (when (get (: c) 'line)
            (println 'prv> "Set line: " (: c) (get (: c) 'any))
            (setq *line (: c)))
          # If Y layout, must be newline
          (when (lay-y?> This)
            (when (get *line 'line 'p)
              (setq *line @)
              (println 'prv> "Set line: " @ (get @ 'any))))
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
                (println 'nxt> "Set line: " (: b) (get (: b) 'any))
                (setq *line B))
              (top> B))))))


(dm sup> ()
  (if (is-car?> This)
      (prog
        ~(assert (get (: c) 'line))
        (println 'sup> "Set line: " (: c) (get (: c) 'any))
        (setq *line (: c))
        (top> (: c)))
        
      (top> This)))


(dm sub> ()
    
  (cond ((has-pair?> This)
         ~(assert (get (: a) 'line))
         (println 'sub> "Set line: " (: a) (get (: a) 'any))
         (setq *line (: a))
           
          # If X lay, mov to Car
          # If Y lay, stay Cdr          
          (if (lay-x?> This)
              (prog
                (set-on-car T)
                (sub> (: a)))
              (top> (: a))))
        
        ((not (car (: any))) # NIL
         (when (get (: a) 'line)
           (println 'sub> "Set line: " (: a) (get (: a) 'any))
           (setq *line (: a)))
         # If X lay, mov to Car
         # If Y lay, stay Cdr
         (if (lay-x?> This)
             NIL
             (top> (: a))))
        
        (T
          (let Pt-Car (lup-car> This)
            # Keep ptr @ pos
            (top> This)
            (sub> Pt-Car)))))

            
(dm con> (Dat Pt-Dat Del)

  # Del prv
  # NIL is not ref'd in tree so pass
  (if (: b)
      (prog
        (del> (: b))
        (=: b))
      (when Del
        (del> (lup-pt (cdr (: any))))))
      
  (con (: any) Dat)
  
  # If Pt-Dat is an atom
  (when Pt-Dat
    (unless (is-pair? Pt-Dat)
      (put This 'b Pt-Dat)
      (put Pt-Dat 'c This))))

      
(dm set> (Dat Pt-Dat Del)

  # Del prv
  # NIL is not ref'd in tree so pass
  (if (: a)
      (prog
        (del> (: a))
        (=: a))
      (when Del
        (del> (lup-pt (car (: any))))))
      
  (set (: any) Dat)
  
  # If Pt-Dat is an atom
  (when Pt-Dat
    (unless (is-pair? Pt-Dat)
      (=: a Pt-Dat)
      (put Pt-Dat 'c This))))
