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
  (when (: c)
    (prv> (: c))))


(dm nxt> ()
  (when (: c)
    (nxt> (: c))))


(dm sup> ()
  (when (: c)
    (set-on-car)
    (mov-cur-abv (: c))
    (: c)))


(dm sub> ()
  (unless *on-car (set-on-car T))
  (if (: a)
      (mov-cur-ori (: a))
      (mov-cur-ori This))
  (adj-cur-y)
  This)
