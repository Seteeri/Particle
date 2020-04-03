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

# +Int
# +Ext
# +Str (= Transient)
# +Box (= Transient)

# CAR empty for Anon syms (CAR is actually number 0)

######################
(class +Sym +Particle)

(dm lay-x> () 
  (setq *cur (mov> This *cur))
  (upd-cur)
  
  # Pos cur for car
  (setq *cur (: ori))
    
  # Move below or use its Y dims - origin is same as dims for single line
  # This assumes Pair is always single line with dot or 2x dot
  (nl-cur))

#################
(class +Box +Sym)


#################
(class +Ext +Sym)


#################
(class +Int +Sym)


(dm prv> ()
  (when (: c)
    (prv> (: c))))


(dm nxt> ()
  # Should always have a 'b
  (when (: b)
    (if *on-car
        (sub> (: b))
        (prog
          (setq *on-car)
          (mov-cur-abv (: b))
          (: b)))))


(dm sup> ()
  (if (: c)
      (sup> (: c))
      (prog
        (setq *on-car)
        (mov-cur-abv This)
        This)))


(dm sub> ()
  (setq *on-car T
        *cur (get (: a) 'ori))
  (nl-cur)
  This)


#################
(class +Str +Sym)


(dm prv> ()
  (when (: c)
    (prv> (: c))))


(dm nxt> ()
  (when (: c)
    (nxt> (: c))))


(dm sup> ()
  (when (: c)
    (setq *on-car)
    (mov-cur-abv (: c))
    (: c)))


(dm sub> ()
  (setq *on-car T)
  (mov-cur-ori (: a))
  (nl-cur)
  This)


#################
(class +Nil +Sym)


(dm prv> ()
  (if (= (get (: c) 'a) This)
      (prv> (: c))
      (when (: c)
        (if *on-car
            (sub> (: c))
            (prog
              (mov-cur-abv (: c))
              (: c))))))


(dm nxt> ()
  (if (= (get (: c) 'a) This)
      (nxt> (: c))
      # Move to start of line cdr
      (let? Ln-CB (get *line 'c)
        (println 'nxt> "Mov ptr -> super-line/list" Ln-CB (get Ln-CB 'data))
        (if *on-car
            (sub> Ln-CB)
            (sup> Ln-CB)))))


(dm sup> ()
  (if (= (get (: c) 'a) This)
      (prog
        (setq *on-car)
        (mov-cur-abv (: c))
        (: c))
      (prog
        (setq *on-car)
        (mov-cur-abv This)
        This)))


(dm sub> ()
  (setq *on-car T
        *cur (: ori))
  (nl-cur)
  This)


(dm lay-x> ()
  (setq *cur (mov> This *cur))
  (upd-cur)
  This)
