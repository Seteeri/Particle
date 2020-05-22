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


(class +Atom +Point)


(dm exp?>
  # When expanded, 'c will be a symbol and its car will be caller
  (and (bel-sym?> This) (is-car?> This)))


(dm upd-line> ()
  # End NIL
  (when (is-cdr?> This)
    (upd-dims> *line-lay)
    # Update line dims with entire dims
    # Poss to get 1st line dims by sub 2nd ln from 1st ln
    (when (get *line-lay 'line 'l)
      (upd-dims> (get *line-lay 'line 'l)))))


(dm lay-x> ()
  (check-nl> This)
  (mov> This)
  (upd-cur)
  (pos-cur-for-car-x> This)
  (upd-line> This)
  This)
  

(dm lay-y> ()
  (check-nl> This)
  (mov> This) 
  (upd-cur)
  (pos-cur-for-car-y> This)
  (upd-line> This)
  This)


# These 4 are same as +Sym  
  
(dm prv> ()
  (when (: c) (prv> @)))


(dm nxt> ()
  # If no b, do c
  (if (: b)
      (if *on-car
          (sub> (: b))
          (prog
            (top> (: b))))
      (when (: c)
        (nxt> @))))


(dm sup> ()
  (when (: c) (top> @)))


(dm sub> ()
  (unless *on-car (set-on-car T))
  (if (: a) # T=expanded
      (prog
        (mov-cur-ori @)
        (adj-cur-y)
        @)
      (prog
        (mov-cur-ori This)
        (adj-cur-y)
        This)))   
