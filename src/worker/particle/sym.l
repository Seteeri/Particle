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
(class +Sym +Particle)

(dm lay-x> () 
  (setq *cur (mov> This *cur))
  (upd-cur)
  
  (pos-cur-for-car-x> This))


(dm lay-y> () 
  (setq *cur (mov> This *cur))
  (upd-cur)
    
  (pos-cur-for-car-y> This))
  
  
(dm pos-cur-for-car-x> ()
  # Pos cur for car
  (setq *cur (: ori))
    
  # Move below or use its Y dims - origin is same as dims for single line
  # This assumes Pair is always single line with dot or 2x dot
  (nl-cur))

  
(dm pos-cur-for-car-y> ()
  # Pos cur for car
  (setq *cur (: ori))
    
  # Use X dims
  (set-cur-x (get (: ori) 1))
  (adv-cur 2.0))


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
    (setq *on-car)
    (mov-cur-abv (: c))
    (: c)))


(dm sub> ()
  (setq *on-car T)
  (if (: a)
      (mov-cur-ori (: a))
      (mov-cur-ori This))
  (nl-cur)
  This)
  
  
(dm mov-verts> (Pos)

  # Handle newline and tab

  # Must update cur bnds since 
  # originally computes based on final pos  
    
  (let ((X Y) Pos
        X-B X
        Y-B Y)
  
    (for Vert (: verts)
  
      (with Vert
        (=: pos Pos)
        (upd-gly> Vert))
        
      (upd-mod-mat> Vert)
      (cmd-upd-vert *ipc Vert)
      
      (if (or (= (get Vert 'val) "^M")
              (= (get Vert 'val) "^J"))      
          (setq X-B (max X-B (car Pos))
                Y-B (min Y-B (cadr Pos))
                Pos (list X (- (cadr Pos) *adv-vert-2)))
          (setq Pos (list (+ (car Pos) *adv-vert) (cadr Pos)))))
  
    (setq Pos (list (- (car Pos) *adv-vert)
                    (cadr Pos)))
    
    (=: ori (ori> This))

    (list (max X-B (car Pos)) (min Y-B (cadr Pos)))))
  

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
            (setq *on-car)
            (mov-cur-abv This)
            This))))
            
#{
      # Move to start of line cdr
      (let? Ln-CB (get *line 'c)
        (println 'nxt> "Mov ptr -> super-line/list" Ln-CB (get Ln-CB 'data))
        (if *on-car
            (sub> Ln-CB)
            (sup> Ln-CB)))))
}#

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
  (check-nl> This)
  (setq *cur (mov> This *cur))
  (upd-cur)
  (upd-line> This)
  This)


(dm lay-y> ()
  (check-nl> This)
  (setq *cur (mov> This *cur))
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
