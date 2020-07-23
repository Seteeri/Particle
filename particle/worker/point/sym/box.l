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

(class +Box +Sym)

(dm lay> ()
  # Draw This
  # Determine if expanded to draw parts...
  (super)
  
  (when (: exp)
    (cons
      (list NIL
            'b
            This)
      (list NIL
            'a
            This))))
