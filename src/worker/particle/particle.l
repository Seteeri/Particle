#{

  (c (a b))
                     [a|*]    [b|*]    (SYMS)  a b
                       |        |
          [c|.]      [.|.] -> [.|/]    (CONS)  (a b)
            |        |
  Abst:   [.|.]  ->  [.|/]                     ((a b))

           .          .
          /_\        /_\
           |          |

  Conc:    P    ->    .
                      |
                      a...

  (struct (>> -4 (adr *l)) '((B . 8) (B . 8)))

  Parts reference data so deleting parts will del the ref data (provided no
  other data references it) so if the visual representation is gone, so will 
  the data (be GC'd).
}#


(class +Particle)
(dm T (Data
       A
       B
       Verts
       Lay)

  (=: data Data)
  (=: a A)
  (=: b B)
  (=: c) # Prev
  (=: verts Verts)
  (=: lay Lay)

  (=: ori)
  #{
   use l b r t?

   ymax = line height
   xmin = origin
   +---+
   |   |
   O---+ xmax/ymin
         can go below
  }#
  # or create layout obj storing dims for x and y
  (=: dims)

  # list of tags; always str; order matters like when searching
  # consider first tag, the name
  # when linking, def show date, tags, or show nm or raw location
  (=: tg)

  # date/time
  (=: dat (date))
  (=: tim (time))
  
  # whether to redraw
  (=: dirt T))


(dm first-vert> () (car (: verts)))
(dm last-vert> () (last (: verts)))

(dm lay-x?> () (= (: lay) 'x))
(dm lay-y?> () (= (: lay) 'y))

(dm b> (Cdr)

  (=: b Cdr)
  (put Cdr 'c This)
  
  # Remember, *start's 'data is always the symbol itself
  # To get its val, is to get it's 'b
  (when (pair (: data))
    (con (car (:: data)) (get Cdr 'data))))


(dm a> (Car)

  (=: a Car)
  (put Car 'c This)
  
  (=: data (cons (get Car 'data)
                 (cdr (: data)))))  
    
    
(dm c-a> ()
  (let (C This
        A (: a))
    (while (= (get C 'a) A)
      (a> C A)
      (setq A C
            C (get C 'c)))
    (when (= (get C 'b) A)
      (b> C A))))
    

(dm del> (A B)

  #{
    Render will render *max-verts
    * Poss send cmd to render to adjust instance count
      * verts is LIFO - always keeps instances contiguous
  }#
  
  (for Vert (: verts)
    (send-msg-rend (pack "(zv " (get Vert 'off-gl) '")"))
    (push '*vert-offs (get Vert 'off-gl))
    (dec '*cnt-v))

  (when (and A (: a)) (del> (: a) A B))

  (when (and B (: b)) (del> (: b) A B))

  #(println 'del> This (: data))
  
  This)


(dm del-car> ()
  (when (: a) (del> @ T T))
  (del> This))
                  
  
(dm mov-verts> ()

  (for Vert (: verts)
  
    (with Vert
  
      (when (: dirt)
        
        (=: pos *cur)      
        (upd-mod> Vert)    
        (req-send *ipc Vert)
        
        (off (:: dirt))))

    (setq *cur (list (+ (car *cur) (meta '(+Vertex) 'adv-vert))
                    (cadr *cur))))
                                        
  (=: ori (ori> This))
    
  (setq *cur (list (- (car *cur) (meta '(+Vertex) 'adv-vert))
                   (cadr *cur))))

  
(dm mov> ()

  # Check first vert
  # If changed, change rest
  # else, do nothing
  
  #{
  (let? Vert (car (: verts))
        
    (if (= (ori> (get Vert 'met)
                 (get Vert 'pos))
           Pos)
    
        # Set to pos of last vert (mul by length)
        (setq Pos (list (+ (car Pos)
                            (*/ (meta '(+Vertex) 'adv-vert) (num-to-fl (- (length (: verts)) 1)) 1.0))
                        (cadr Pos)))
    
        (setq Pos (mov-verts> This Pos))))
  }#
  
  (mov-verts> This))
  
  
# Rename off -> rel
(dm mov-off> (Off P)
  # Calc tgt pos
  (let Pos (: ori)
    (setq *cur (list (+ (get Pos 1) (get Off 1))
                     (+ (get Pos 2) (get Off 2))
                     (+ (get Pos 3) (get Off 3))))
    (mov> This)

    (when (and (car P) (: a)) (mov-off> (: a) Off P))
    (when (and (cdr P) (: b)) (mov-off> (: b) Off P))))


(dm mov-part> (B P)

  (let (Pos     (: ori)
        Pos-Tgt (get B 'ori)
        Off (list (- (get Pos-Tgt 1) (get Pos 1))
                  (- (get Pos-Tgt 2) (get Pos 2))
                  (- (get Pos-Tgt 3) (get Pos 3))))

    (setq *cur Pos-Tgt)
    (mov> This)

    # Apply offset to car/cdr
    (when (and (car P) (: a)) (mov-off> (: a) Off P))
    (when (and (cdr P) (: b)) (mov-off> (: b) Off P))))


(dm mov-part-aft> (B P)
  # Make offset version instead of static function?

  (mov-cur-vert (last-vert> B) T)
  (adv-cur 2.0)

  (let Pos (: ori)
    (mov-off> This
              (list (- (get *cur 1) (get Pos 1))
                    (- (get *cur 2) (get Pos 2))
                    (- (get *cur 3) (get Pos 3)))
              P)))


(dm mov-part-abv> (B P)
  # Make offset version instead of static function?

  (mov-cur-vert (first-vert> B) T)
  (nl-cur -1.0)

  (let Pos (: ori)
    (mov-off> This
              (list (- (get *cur 1) (get Pos 1))
                    (- (get *cur 2) (get Pos 2))
                    (- (get *cur 3) (get Pos 3)))
              P)))


(dm mov-part-bel> (B P)
  # Make offset version instead of static function?

  (mov-cur-vert (first-vert> B) T)
  (nl-cur)

  (let Pos (: ori)
    (mov-off> This
              (list (- (get *cur 1) (get Pos 1))
                    (- (get *cur 2) (get Pos 2))
                    (- (get *cur 3) (get Pos 3)))
              P)))


# Verts
# Most of these don't use any properties from part
# so should move to a vert class or make normal function

(dm ori> ()
  (let? Vert (car (: verts))
    (with Vert
      (ori> (: met)
            (: pos)))))


(dm col> (Col)
  (let ((R G B) Col)
    (for Vert (: verts)
      (put Vert 'rgba (list R G B 1.0
                            R G B 1.0
                            R G B 1.0
                            R G B 1.0))
      (upd-mod-mat> Vert)
      (req-send *ipc Vert))))


(dm bnds> (Car-Skip Cdr-Skip)

  (let ((Xa Ya) (when (and (: a) 
                           (not (= (: a) This))
                           (not Car-Skip))
                  (bnds> (: a)))
        (Xb Yb) (when (and (: b)
                           (not (= (: b) This))
                           (not Cdr-Skip))
                  (bnds> (: b)))
        (X Y) (with (last-vert> This)
                  (ori> (: met)
                        (: pos))))

    #(println 'calc-bounds This (: data) (format X *Scl) (format Y *Scl))

    # If neither Car or Cdr, use cons
    (list (max (if Xa Xa X) (if Xb Xb X) X)
          (min (if Ya Ya Y) (if Yb Yb Y) Y))))


(dm check-nl> ()
  (when (get (: line))
  
    (when (get (: line) 'p)
    
      # Set bnds of current line
      (upd-dims *line-lay)
    
      # Reset max x
      # Caller will call upd-cur after this
      (setq *bnds-x)
    
      (if (lay-y?> (: c)) # or auto for pairs
          (align-cur-to-line-y This)
          (align-cur-to-line-x This)))
          
    (setq *line-lay This)))


# Debug


(dm trace> (Cnt)
  (let (D ()
        X (: b))
    (loop
      (push 'D (get X 'data))
      (println (car D) "->" (get X 'b))
      (NIL (get X 'b))
      (setq X (get X 'b)))
    D))

(dm trace-2> (Cnt)
  (let (S ()
        X (: b))
    (loop
      (push 'S X)
      (println X "->" (get X 'b))
      (NIL (get X 'b))
      (setq X (get X 'b)))
    S))
