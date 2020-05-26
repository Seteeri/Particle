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

  
(dm lay> ()
  (super)
  (cons
    (let B (cdr (: data))
      (list (if B @ NIL)
            'b
            This))
    (let A (car (: data))
      (list (if A @ NIL)
            'a
            This))))
  
  
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
        
        ((has-nil?> This)
         (when (get (: a) 'line)
           (println 'sub> "Set line: " (: a) (get (: a) 'data))
           (setq *line (: a)))
         # If X lay, mov to Car
         # If Y lay, stay Cdr
         (if (lay-x?> This)
             (prog
               (set-on-car T)
               (sub> (: a)))
              (top> (: a))))
        
        (T
         (sub> (: a)))))
