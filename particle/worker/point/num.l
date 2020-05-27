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


(class +Num +Atom)

(dm T (Any
       Lay
       A B
       Exp
       Col)
  (super Any
         Lay
         A B)
  
  (unless Col (setq Col (get-col-type Any)))         
  
  (let ((Verts W) 
          (make-vert-atm Any Col))
    (=: verts Verts)
    (=: dims (list W
                  (meta '(+Vertex) 'adv-vert-2))))
  (=: exp Exp))

(dm lay> ()
  (lay-cur> This)
  NIL)
