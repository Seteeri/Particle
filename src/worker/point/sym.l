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
# +Str (Transient)
# +Box (Transient-Anonymous)
#   Note, (str? (box)) = T
# +Nil

# CAR empty for Anon syms (CAR is actually number 0)

######################
(class +Sym +Point)

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
          (set-on-car)
          (mov-cur-abv (: b))
          (: b)))))


(dm sup> ()
  (if (: c)
      (sup> (: c))
      (prog
        (set-on-car)
        (mov-cur-abv This)
        This)))


(dm sub> ()
  (unless *on-car (set-on-car T))
  (setq *cur (get (: a) 'ori))
  (adj-cur-y)
  This)


#################
(class +Str +Sym)

# This class represents either, the name
# or the pair when expanded (name/car is +Num)

(dm prv> ()
  (when (: c)
    (prv> (: c))))


(dm nxt> ()
  (when (: c)
    (nxt> (: c))))


(dm sup> ()
  (when (: c)
    (set-on-car)
    (mov-cur-abv (: c))
    (: c)))


(dm sub> ()
  (set-on-car T)
  (if (: a)
      (mov-cur-ori (: a))
      (mov-cur-ori This))
  (adj-cur-y)
  This)
  
  
(dm mov-verts> ()

  # Handle newline and tab

  # Must update cur bnds since 
  # originally computes based on final pos  
    
  (let ((X Y) *cur
        X-B X
        Y-B Y)
  
    (for Vert (: verts)
  
      (with Vert
        
        (when (: dirt)
        
          (=: pos *cur)
          (upd-mod> Vert)
          (req-send *ipc Vert)
          
          (off (:: dirt)))
      
        (if (or (= (: val) "^M")
                (= (: val) "^J"))
            (setq X-B (max X-B (car *cur))
                  Y-B (min Y-B (cadr *cur))
                  *cur (list X (- (cadr *cur) (meta '(+Vertex) 'adv-vert-2))))
            (setq *cur (list (+ (car *cur) (meta '(+Vertex) 'adv-vert)) (cadr *cur))))))
      
    (=: ori (ori> This))

    # Mov cur back one adv
    (setq *cur (list (max X-B (- (car *cur) (meta '(+Vertex) 'adv-vert)))
                     (min Y-B (cadr *cur))))))
  

#################
(class +Nil +Sym)


(dm prv> ()
  (if (= (get (: c) 'a) This)
      (prv> (: c))
      (when (: c)
        (when (get (: c) 'line)
          (println 'prv> "Mov ptr -> super-line/list" (: c) (get (: c) 'data))
          (setq *line (: c)))
        (if *on-car
            (sub> (: c))
            (prog
              (mov-cur-abv (: c))
              (: c))))))


(dm nxt> ()
  (if (= (get (: c) 'a) This)
      (nxt> (: c))
      (if *on-car
          (sub> This)
          (prog
            (set-on-car)
            (mov-cur-abv This)
            This))))


(dm sup> ()
  (prog1
    # 4 POS' : X-A, X-B, Y-A, Y-B
    # Each pos can be car or cdr
    # Cdr only for X-B, Y-B
    #{
    (if *on-car
        (if (and (lay-x?> (: c))
                 (is-car?> This))
            (prog (mov-cur-abv (: c)) (: c))
            (prog (mov-cur-abv This)  This))
        (prog (mov-cur-abv (: c)) (: c)))
    }#
    
    (if (and *on-car
             (or (lay-y?> (: c))
                 (is-cdr?> This)))
        (prog (mov-cur-abv This)  This)
        (prog (mov-cur-abv (: c)) (: c)))
    
    (set-on-car)))


(dm sub> ()
  (unless *on-car (set-on-car T))
  (setq *cur (: ori))
  (adj-cur-y)
  This)


(dm lay-x> ()
  (check-nl> This)
  (mov> This)
  (upd-cur)
  (upd-line> This)
  This)


(dm lay-y> ()
  (check-nl> This)
  (mov> This)
  (upd-cur)
  (upd-line> This)
  This)


(dm upd-line> ()
  # Standalone NIL
  (when (= (get (: c) 'b) This)
    (upd-dims *line-lay)
    (when (and (get *line-lay 'line 'l) T)
      #(println 'lay-pair-x "Update line: "
      #        (get *line-lay 'line 'l)
      #        (get *line-lay 'line 'l 'data))    
      (upd-dims (get *line-lay 'line 'l)))))
