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


(class +Num +Particle)

(dm lay-x> ()
  (setq *cur (mov> This *cur))
  (upd-cur)
  This)

(dm lay-y> ()
  (setq *cur (mov> This *cur))
  (upd-cur)
  This)
