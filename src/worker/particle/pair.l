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
            (println 'prv> "Mov ptr -> super-line/list" (: c) (get (: c) 'data))
            (setq *line (: c)))
          # If Y layout, must be newline
          (when (= (: lay) 'y)
            (when (get *line 'line 'p)
              (setq *line @)
              (println 'prv> "Mov ptr -> super-line/list" @ (get @ 'data))))
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
      # *start?
      (prog
        #(let? Ln (get (: c) 'line)
        #  (println 'sup> "Mov ptr -> super-line/list" Ln (get Ln 'data))        
        #  (setq *line (: c)))
        (set-on-car)
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
        (when (get (: a) 'line)
          (println 'sub> "Mov ptr -> sub-line/list" (: a) (get (: a) 'data))
          (setq *line (: a)))
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

  (mov> This)
  (upd-cur)
  
  (pos-cur-for-car-x> This))
  

(dm lay-y> ()
  
  (check-nl> This)
  
  (mov> This) 
  (upd-cur)
  
  (pos-cur-for-car-y> This))

#########################

(dm pos-cur-for-car-x> ()
  # Pos cur for car
  (setq *cur (: ori))
    
  # Move below or use its Y dims - origin is same as dims for single line
  # This assumes Pair is always single line with dot or 2x dot
  (adj-cur-y))

  
(dm pos-cur-for-car-y> ()
  # Pos cur for car
  (setq *cur (: ori))
    
  # Use X dims
  (mov-cur-x (get (: ori) 1))
  (adj-cur-x 2.0))

  
(class +Line)
(dm T (P N L D)
  (=: p P)
  (=: n N)
  (=: l L)
  (=: dims (if D @ (list 0.0 0.0))))
