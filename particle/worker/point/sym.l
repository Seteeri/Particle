#{
                      cell [PARTICLE]
                        |
            +-----------+-----------+
            |           |           |
         Number       Ptrbol       Pair
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

(class +Ext +Sym)

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

(class +Int +Sym)  

# Draw Car/Name after main
  
(dm lay> ()

  # Draw This
  (super)

  (when (: exp) 
    (cons
      (unless (= (: b) T)
        (list (if (: b) NIL (val (: data)))
              'b
              This))
      (list NIL
            'a
            This))))
  
        
(dm lup-a> () (: a))
(dm lup-b> ()
  (if (: b)
      (: b)
      (super)))


(dm con> (Dat Po-Dat Del)

  # Del prv
  # NIL is not ref'd in tree so pass
  (if (: b)
      (prog
        (del> (: b))
        (=: b))
      (when Del
        (del> (lup-pt (val (: data))))))
    
  (set (: data) Dat)
  
  (when Po-Dat
    (put Po-Dat 'c This)))

      
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

                     
(dm lay> ()
  (lay-cur> This)
  # Handle when value is not self
  NIL)                     

  
(dm sub> ()
  (set-ptr-ref (: data))
  (: data))
  
  
(dm nxt> () (: data))
