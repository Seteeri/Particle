#{

                      cell
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

# +Int
# +Ext
# +Str (Transient)
# +Box (Transient-Anonymous)
#   Note, (str? (box)) = T
# +Nil

# CAR empty for Anon syms (CAR is actually number 0)

# These assume Ptrs not expanded

(class +Sym +Atom)

(dm T (Any
       Lay
       A B
       Exp)
  (super Any
         Lay
         A B)
  (if Exp
      T
      (let ((Verts W)
              (make-vert-atm Any (get-col-type Any)))
        (=: verts Verts)
        (=: dims (list W
                      (meta '(+Vertex) 'adv-vert-2)))))
  (=: exp Exp))
