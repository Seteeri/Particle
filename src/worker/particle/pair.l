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


(class +Pair +Particle)

(dm T (Data
       A
       B
       Verts
       Lay)

  (super Data A B Verts Lay)
  
  (=: line))

(dm prv> ()
  (when (: c)
    (if *on-car
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
          (mov-cur-abv (: c))
          (: c)))))

          
(dm nxt> ()  
  (when (: b)
    (if *on-car
        (sub> (: b))
        (prog
          (when (get (: b) 'line)
            (println 'nxt> "Set line: " (: b) (get (: b) 'data))
            (setq *line (: b)))
          (mov-cur-abv (: b))
          (: b)))))


(dm sup> ()
  (if (is-car?> This)
      (prog
        ~(assert (get (: c) 'line))
        (println 'sup> "Set line: " (: c) (get (: c) 'data))
        (setq *line (: c))
        (set-on-car)
        (mov-cur-abv (: c))
        (: c))
        
      (prog
        (mov-cur-abv This)
        This)))


(dm sub> ()
  (cond ((isa '(+Pair) (: a))
  
         ~(assert (get (: a) 'line))
        (println 'sub> "Set line: " (: a) (get (: a) 'data))
        (setq *line (: a))
           
         # If X lay, mov to Car
         # If Y lay, stay Cdr          
         (if (lay-x?> This)
             (prog
               (set-on-car T)
               (sub> (: a)))
             (prog
               (set-on-car)
               (mov-cur-abv (: a))
               (: a))))
        
        ((isa '(+Nil) (: a))
         (println 'sub> "Set line: " (: a) (get (: a) 'data))
         (setq *line (: a))
         # If X lay, mov to Car
         # If Y lay, stay Cdr          
         (if (lay-x?> This)
             (prog
               (set-on-car T)
               (sub> (: a)))
             (prog
               (set-on-car)
               (mov-cur-abv (: a))
               (: a))))         
        
        (T
         (sub> (: a)))))

  
(class +Line)
(dm T (P N L D)
  (=: p P)
  (=: n N)
  (=: l L)
  (=: dims (if D @ (list 0.0 0.0))))
