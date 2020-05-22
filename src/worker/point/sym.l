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

(class +Sym +Point)


(dm exp?>
  # When expanded, 'c will be a symbol and its car will be caller
  (and (isa '(+Sym) (: c)) (= (get (: c) 'a) This)))
  
  
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
  (when (is-cdr?> This)
    (upd-dims *line-lay)
    (when (and (get *line-lay 'line 'l) T)
      #(println 'lay-pair-x "Update line: "
      #        (get *line-lay 'line 'l)
      #        (get *line-lay 'line 'l 'data))    
      (upd-dims (get *line-lay 'line 'l)))))

      
(class +Box +Sym)
(class +Ext +Sym)
(class +Int +Sym)      
