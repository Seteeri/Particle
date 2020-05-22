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

# These assume Syms not expanded

(class +Sym +Atom)
 
(class +Str +Sym) 
  
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
  (if (is-car?> This)
      (prv> (: c))
      (when (: c)
        (when (get (: c) 'line)
          (println 'prv> "Mov ptr -> super-line/list" (: c) (get (: c) 'data))
          (setq *line (: c)))
        (if *on-car
            (sub> (: c))
            (top> (: c))))))


(dm nxt> ()
  (if (is-car?> This)
      (nxt> (: c))
      (if *on-car
          (sub> This)
          (top> This))))


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
  # Similar to +Atom, except no pos-cur-*
  (check-nl> This)
  (mov> This)
  (upd-cur)
  (upd-line> This)
  This)


(dm lay-y> ()
  # Similar to +Atom, except no pos-cur-*
  (check-nl> This)
  (mov> This)
  (upd-cur)
  (upd-line> This)
  This)

  
(class +Box +Sym)
(class +Ext +Sym)
(class +Int +Sym)      
