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
        (prog # These depend on layout...
          (mov-cur-abv (: c))
          (: c)))))

(dm nxt> ()  
  (when (: b)
    (if *on-car
        (sub> (: b))
        (prog
          (mov-cur-abv (: b))
          (: b)))))

(dm sup> ()
  (if (= (get (: c) 'a) This)
      (prog
        (println 'sup> "Mov ptr -> super-line/list" (: c) (get (: c) 'data))
        (setq *on-car NIL
              *main (: c))
        (let? Ln (get (: line) 'p)
          (setq *line Ln))
        (mov-cur-abv (: c))
        (: c))

      (if (: c)
          (sup> (: c))
          (prog
            (mov-cur-abv This)
            This))))


(dm sub> ()
  (if (isa '(+Pair) (: a))
      # Line change: superlist -> sublist
      (prog
        (println 'sub> "Mov ptr -> sub-line/list" (: a) (get (: a) 'data))
        (setq *main (: a))
        (let? Ln (get (: line) 'n)
          (setq *line Ln))
        # Here can either move Pair or Car
        # Default is to follow Car             
        #(setq *on-car)
        #(mov-cur-abv (: a))
        (if *on-car
            (sub> (: a))
            (mov-cur-abv (: a))))

      # Should always be a Car
      (when (: a)
        (sub> (: a)))))


(class +Line)
(dm T (P N Dims Base)
  (=: p P)
  (=: n N)
  (=: dims (if Dims Dims (list 0.0 0.0)))
  (=: base Base)) # either 'line or 'prv  
