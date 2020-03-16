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
       Verts)

  (super Data A B Verts))  
