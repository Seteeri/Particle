#{

                      cell
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

            
(dm con> (Any Pt-Any Del)

  (when Del
    (if (: b)
        (prog
          (del> (: b))
          (=: b))
        (prog
          # Print warning if non-existent
          (del> (lup-pt (cdr (: any))))
          ~(assert (not (: b))))))
      
  (con (: any) Any)
  
  (if (atom Any)
      (prog
        (=: b Pt-Any)
        (put Pt-Any 'c This))
      (=: b)))

      
(dm set> (Any Pt-Any Del)
      
  (when Del
    (if (: a)
        (prog
          (del> (: a))
          (=: a))
        (prog
          # Print warning if non-existent
          (del> (lup-pt (car (: any))))
          ~(assert (not (: a))))))
      
  (set (: any) Any)
  
  (if (atom Any)
      (prog
        (=: a Pt-Any)
        (put Pt-Any 'c This))
      (=: a)))


#{
(dm con-pair-str> (Str)
  (if (reg-a?> *ptr)  
      (if (lay-x?> *ptr)
          (con-a-pair-x Str)
          (con-a-pair-y Str)))
      (if (lay-x?> *ptr)
          (con-b-pair-x Str)
          (con-b-pair-y Str)))
}#
      
##############################

(dm con-2> (Any Pt-Any Del Ln)

  (when Ln  
    (if (: line)
        (prog
          (put @ 'n Pt-Any)
          (put Pt-Any 'line 'p This))
        (let Ln (c-lns> *ptr)
          (put Pt-Any 'line 'p Ln)
          (put Ln 'line 'n Pt-Any))))

  (when Del
    (if (: b)
        (prog
          (del> (: b))
          (=: b))
        (prog
          # Print warning if non-existent
          (del> (lup-pt (cdr (: any))))
          ~(assert (not (: b))))))
      
  (con (: any) Any)
  
  (if (atom Any)
      (prog
        (=: b Pt-Any)
        (put Pt-Any 'c This))
      (=: b)))

      
(dm set-2> (Any Pt-Any Del Ln)
  
  # Y pair requires a line for its car
  # Opts for Ln
  # * mov-old-new = mov line from old
  # * mov-pair-new = 
  # * new = create new
  # * nil = use existing/do nothing
  
  (cond
    
    ((= Ln 'mov)
      (let (Car (if (: a)
                    (get @ 'any)
                    (car (: any)))
            Pt-Car (if (: a)
                       @
                       (lup-pt (car (: any)))))
        (when (get Pt-Car 'line)
          (mov-line Pt-Car Pt-Any)
          (lns> *ptr)))
      (set-ln-n> *ptr Pt-Ls))
    
    ((= Ln 'new)
      # For Y Pair
      (put Pt-Any 'line (new '(+Line)))))

  (when Del
    (if (: a)
        (prog
          (del> (: a))
          (=: a))
        (prog
          # Print warning if non-existent
          (del> (lup-pt (car (: any))))
          ~(assert (not (: a))))))
    
  (set (: any) Any)
  
  (if (atom Any)
      (prog
        (=: a Pt-Any)
        (put Pt-Any 'c This))
      (=: a)))
