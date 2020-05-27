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


(class +Atom +Point)

 
(dm prv> ()
  (when (: c) (prv> @)))


(dm nxt> ()
  (if (: b) # NIL
      (nil
        (when (get (: b) 'line)
          (println 'nxt> "Set line: " (: b) NIL)
          (setq *line (: b)))            
        (top> (: b)))
      (let B (lup-pt (val (: data)))
        (if B
            (if *ptr-on-car
                (sub> B)
                (top> B))
            (when (: c)
              (nxt> @))))))


(dm sup> ()
  (quit "IMPLEMENT SUP>"))


#  Do nothing for now
(dm sub> ())
