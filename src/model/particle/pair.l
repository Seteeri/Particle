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
       Verts)

  (super Data A B Verts)
  
  (=: lay NIL)
  (=: len 1))


(dm cons> (B) (=: b B))
