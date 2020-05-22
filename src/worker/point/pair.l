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
          (top> (: c))))))

          
(dm nxt> ()  
  (when (: b)
    (if *on-car
        (sub> (: b))
        (prog
          (when (get (: b) 'line)
            (println 'nxt> "Set line: " (: b) (get (: b) 'data))
            (setq *line (: b)))
          (top> (: b))))))


(dm sup> ()
  (if (is-car?> This)
      (prog
        ~(assert (get (: c) 'line))
        (println 'sup> "Set line: " (: c) (get (: c) 'data))
        (setq *line (: c))
        (top> (: c)))
        
      (top> This)))


(dm sub> ()
  
  ~(assert (: a))
  
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
             (top> (: a))))
        
        ((isa '(+Nil) (: a))
         (println 'sub> "Set line: " (: a) (get (: a) 'data))
         (setq *line (: a))
         # If X lay, mov to Car
         # If Y lay, stay Cdr          
         (if (lay-x?> This)
             (prog
               (set-on-car T)
               (sub> (: a)))
              (top> (: a))))
        
        (T
         (sub> (: a)))))

  
(class +Line)
(dm T (P N L D)
  (=: p P)
  (=: n N)
  (=: l L)
  (=: dims (if D @ (list 0.0 0.0))))
