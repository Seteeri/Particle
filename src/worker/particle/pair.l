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
          (when (get (: line) 'p)
            (println 'prv> "Mov ptr -> super-line/list" (get (: line) 'p) (get (: line) 'p 'data))
            (setq *line (get (: line) 'p)))
          (mov-cur-abv (: c))
          (: c)))))

(dm nxt> ()  
  (when (: b)
    (if *on-car
        (sub> (: b))
        (prog
          (when (get (: b) 'line)
            (println 'nxt> "Mov ptr -> sub-line/list" (: b) (get (: b) 'data))
            (setq *line (: b)))
          (mov-cur-abv (: b))
          (: b)))))


(dm sup> ()
  (if (= (get (: c) 'a) This)
      # Exclude Y layout Pair -> X layout Car
      # *main?
      (prog
        #(let? Ln (get (: line) 'p)
        #  (println 'sup> "Mov ptr -> super-line/list" Ln (get Ln 'data))        
        #  (setq *line Ln))
        (setq *on-car NIL)        
        (mov-cur-abv (: c))
        (: c))

      (if (: c)
          (sup> (: c))
          (prog
            (mov-cur-abv This)
            This))))


(dm sub> ()
  (if (isa '(+Pair) (: a))
      # Exclude Y layout Pair -> X layout Car
      (prog
        #(let? Ln (get (: line) 'n)
        #  (println 'sub> "Mov ptr -> sub-line/list" Ln (get Ln 'data))
        #  (setq *line Ln))
        # Either move Pair or Car; default follows Car             
        (if *on-car
            (sub> (: a))
            (prog
              (mov-cur-abv (: a))
              (: a))))

      # Should always be a Car
      (when (: a)
        (sub> (: a)))))


(dm lay-x> ()
  
  (check-nl> This)

  (setq *cur (mov> This *cur))
  (upd-cur)
  
  (pos-cur-for-car-x> This))
  

(dm lay-y> ()
  
  (check-nl> This)

  (setq *cur (mov> This *cur))
  (upd-cur)
  
  (pos-cur-for-car-y> This))

#########################

(dm pos-cur-for-car-x> ()
  # Pos cur for car
  (setq *cur (: ori))
    
  # Move below or use its Y dims - origin is same as dims for single line
  # This assumes Pair is always single line with dot or 2x dot
  (nl-cur))

  
(dm pos-cur-for-car-y> ()
  # Pos cur for car
  (setq *cur (: ori))
    
  # Use X dims
  (set-cur-x (get (: ori) 1))
  (adv-cur 2.0))


(dm check-nl> ()
  (when (get (: line) 'p)
    # When newline detected, must save line dims
    # Don't update cursor...
    (upd-dims *line-lay)
    (println 'lay-y> "Update line:" *line-lay (get *line-lay 'data))  
  
    #(println 'lay> "Detected line:" This (get This 'data))
    #(println 'lay> "Detected line prv:" (: line) (get (: line) 'p 'data))
    
    (align-cur-to-line This)
        
    (setq *cur-x (get *cur 1) *cur-y (get *cur 2))        
        
    (setq *line-lay This)))

  
(class +Line)
(dm T (P N Dims Base)
  (=: p P)
  (=: n N)
  (=: dims (if Dims Dims (list 0.0 0.0)))
  (=: base Base)) # either 'line or 'prv  
