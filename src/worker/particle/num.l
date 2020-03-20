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

(dm T (Data
       A
       B
       Verts
       Lay)

  (super Data A B Verts Lay))
