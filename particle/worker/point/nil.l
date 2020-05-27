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


(class +Nil +Sym)

(dm T (Any
       Lay
       A B
       Exp)
  (super Any
         Lay
         A B)
  (let ((Verts W) 
          (make-vert-atm Any
                         (get-col-type Any)
                         (caddr (get '+Vertex 'chs-pair))))
    (=: verts Verts)
    (=: dims (list W
                  (meta '(+Vertex) 'adv-vert-2))))
  (=: exp Exp))

(dm prv> ()
  (if (is-car?> This)
      (prv> (: c))
      (when (: c)
        (when (get (: c) 'line)
          (println 'prv> "Mov ptr -> super-line/list" (: c) (get (: c) 'data))
          (setq *line (: c)))
        (if *ptr-on-car
            (sub> (: c))
            (top> (: c))))))


(dm nxt> ()
  (if (is-car?> This)
      (nxt> (: c))
      (if *ptr-on-car
          (sub> This)
          (top> This))))


(dm sup> ()
  (prog1
    # 4 POS' : X-A, X-B, Y-A, Y-B
    # Each pos can be car or cdr
    # Cdr only for X-B, Y-B
    #{
    (if *ptr-on-car
        (if (and (lay-x?> (: c))
                 (is-car?> This))
            (prog (mov-cur-abv (: c)) (: c))
            (prog (mov-cur-abv This)  This))
        (prog (mov-cur-abv (: c)) (: c)))
    }#
    
    (if (and *ptr-on-car
             (or (lay-y?> (: c))
                 (is-cdr?> This)))
        (prog (mov-cur-abv This)  This)
        (prog (mov-cur-abv (: c)) (: c)))
    
    (set-on-car)))


(dm sub> ()
  (setq *cur (: ori))
  (adj-cur-y)
  This)

  
(dm lay> ()
  (lay-cur> This)
  NIL)
