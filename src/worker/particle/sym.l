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

  
(class +Sym +Particle)

(dm T (Data
       A
       B
       Verts
       Lay)

  (super Data A B Verts Lay))


(class +Str +Particle)

(dm T (Data
       A
       B
       Verts
       Lay)

  (super Data A B Verts Lay))
