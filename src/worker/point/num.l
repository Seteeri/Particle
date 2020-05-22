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


(class +Num +Point)


(dm prv> ()
  (when (: c) (prv> @)))


(dm nxt> ()
  # If no b, do c
  (if (: b)
      (if *on-car
          (sub> (: b))
          (prog
            (top> (: b))))
      (when (: c)
        (nxt> @))))


(dm sup> ()
  (when (: c) (top> @)))


(dm sub> ()
  (unless *on-car (set-on-car T))
  (if (: a) # T=expanded
      (prog
        (mov-cur-ori @)
        (adj-cur-y)
        @)
      (prog
        (mov-cur-ori This)
        (adj-cur-y)
        This)))
