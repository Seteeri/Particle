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

(class +Ext +Sym)

(class +Box +Sym)

(dm lay> ()

  # Draw This
  (super)

  # Draw car (name)
  # Car is only ref'd from here, not in tree
  (lay> (: a))
      
  # Return next item, cdr
  (let B (val (: data))
  
    # If NIL, only ref'd here
    (cons NIL
          (list (if B @ NIL)
                'b
                This))))

(class +Int +Sym)  

# Draw Car/Name after main
  
(dm lay> ()

  # Draw This
  (super)

  # Draw car (name)
  # Car is only ref'd from here, not in tree
  (lay> (: a))
      
  # Return next item, cdr
  (let B (val (: data))
  
    # If NIL, only ref'd here
    (cons NIL
          (list (if B @ NIL)
                'b
                This))))
  
        
(dm lup-a> () (: a))
(dm lup-b> ()
  (if (: b)
      (super)
      (: b)))


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
