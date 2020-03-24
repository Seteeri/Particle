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


(class +Str +Sym)

(dm T (Data
       A
       B
       Verts
       Lay)

  (super Data A B Verts Lay))


(class +Nil +Sym)

(dm T (Data
       A
       B
       Verts
       Lay)

  (super Data A B Verts Lay))

# +Int
# +Ext
# +Str (= Transient)
# +Box (= Transient)
