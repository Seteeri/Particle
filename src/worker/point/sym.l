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

(class +Box +Sym)
(class +Ext +Sym)
(class +Int +Sym)      
