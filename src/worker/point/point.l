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


(class +Point)

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

(de is-pair? (P) (isa '(+Pair) P))
(de is-str?  (P) (isa '(+Str)  P))
(de is-sym?  (P) (isa '(+Sym)  P))
(de is-nil?  (P) (isa '(+Nil)  P))

(dm bel-pair?> () (isa '(+Pair) (: c)))
(dm bel-sym?>  () (isa '(+Sym)  (: c)))
(dm bel-str?>  () (isa '(+Str)  (: c)))
(dm bel-nil?>  () (isa '(+Nil)  (: c)))

(dm has-pair?> () (isa '(+Pair) (: a)))
(dm has-sym?>  () (isa '(+Sym)  (: a)))
(dm has-str?>  () (isa '(+Str)  (: a)))
(dm has-nil?>  () (isa '(+Nil)  (: a)))

(dm of-car?> (A) (= (: a) A))
(dm of-cdr?> (B) (= (: b) B))

(dm is-car?> () (= (get (: c) 'a) This))
(dm is-cdr?> () (= (get (: c) 'b) This))
(dm is-end?> () (and (is-cdr?> This)
                     (not (is-pair? This))))

(dm lay-x?> () (= (: lay) 'x))
(dm lay-y?> () (= (: lay) 'y))

(dm lay> (L)

  # Set color on change unless it is a ptr

  # Ptr uses put instead of this
  # TODO: '(+Ptr +Sym)
        
  (let Col (if (= L 'x)
               (car *pair-col-def)
               (cadr *pair-col-def))
    (col> This Col 1.0))
    
  (=: lay L))

  
(de b@ @
  (let (A (next)
        B NIL)
    (while (setq B (next))
      (b> A B)
      (setq A B))))


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

  (set (car (:: data)) (get Car 'data)))

    
# Unused
(de c-a (S)
  (let (C S
        A (get S 'a))
    (while (= (get C 'a) A)
      (a> C A)
      (setq A C
            C (get C 'c)))
    (when (= (get C 'b) A)
      (b> C A))))
    

(dm top> ()
  (when *on-car (set-on-car))
  (mov-cur-abv This)
  This)
 
  
(dm pos-cur-for-car-x> ()
  # Pos cur for car
  (setq *cur (: ori))
    
  # Move below or use its Y dims - origin is same as dims for single line
  # This assumes Pair is always single line with dot or 2x dot
  (adj-cur-y))

  
(dm pos-cur-for-car-y> ()
  # Pos cur for car
  (setq *cur (: ori))
    
  # Use X dims
  (mov-cur-x (get (: ori) 1))
  (adj-cur-x 2.0))
  

(dm lay-cur> ()
  (upd-line-pre> This)
  (mov> This) # *cur will be at last vert pos
  (upd-cur)
  (upd-line-post> This))

  
(dm lay-x> ()
  (lay-cur> This)
  (pos-cur-for-car-x> This)
  This)
  

(dm lay-y> ()
  (lay-cur> This)
  (pos-cur-for-car-y> This)
  This)


# Track each line
# Track each list
  
  
(dm upd-line-pre> ()
  #{
  
    Cur is at extends of recently drawn item
    
    If next item has line:
    * Update bnds for recent line
    * Reset x bnds
    
    Then pos cur for next item dep on lay
    
    Update cur line
  
  }#
  
  (when (: line)
  
    (when (get (: line) 'p)
    
      # Set bnds of current line
      (upd-dims> *line-lay)
      # Cache end
      (put (get *line-lay 'line 'e) This)
    
      # Reset max x
      #(println 'upd-line-pre "Reset *bnds-x: " (format *bnds-x *Scl) " | "
      #  (: data))
      (off *bnds-x)
    
      (if (lay-y?> (: c)) # or auto for pairs
          (align-cur-to-line-y This)
          (align-cur-to-line-x This)))
          
    (setq *line-lay This)))
    
    
(dm upd-line-post> ()
  #{
  
    After drawing cell, if item is end of list
    update cur line dims
    
  }#
  
  (when (is-end?> This)
        
    # Upd current line
    (upd-dims> *line-lay)
    # Cache end atom
    (put (get *line-lay 'line 'e) This)
    
    # Update list dims with entire dims
    # Poss to get 1st line dims by sub 2nd ln from 1st ln
    (when (get *line-lay 'line 'l)
      # (upd-dims> @)
      # Cache end atom
      # Can be used to get the y min, but not max x
      #(println "Update list dims: " (car (get @ 'data)))
      (put (get @ 'line 'dims 'e) This))))
      

(dm upd-dims> ()
  #{
    +Pair: Car bnds
    +Sym: Car+Cdr bnds
    +Num: Car+Cdr bnds
  }#
  
  (let Dims (list (abs (- *bnds-x (car  (: ori))))
                  (abs (- *bnds-y (cadr (: ori)))))
    (=: dims Dims)
    #(when (: line) (upd-dims> @ (: ori)))))
    (when (: line) (put @ 'dims Dims))))
    
    
##############

  
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

  # If first vert changed, change rest
  # else, do nothing
  
  (let? Vert (car (: verts))

    # Set to pos of last vert (mul by length)
    (if (= (: ori) *cur) 
        (list (+ (car *cur)
                  (*/ (meta '(+Vertex) 'adv-vert)
                      (num-to-fl (- (length (: verts)) 1))
                      1.0))
              (cadr *cur)))
    (mov-verts> This)))
  
  #(mov-verts> This))
  
  
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
  (adj-cur-x 2.0)

  (let Pos (: ori)
    (mov-off> This
              (list (- (get *cur 1) (get Pos 1))
                    (- (get *cur 2) (get Pos 2))
                    (- (get *cur 3) (get Pos 3)))
              P)))


(dm mov-part-abv> (B P)
  # Make offset version instead of static function?

  (mov-cur-vert (first-vert> B) T)
  (adj-cur-y -1.0)

  (let Pos (: ori)
    (mov-off> This
              (list (- (get *cur 1) (get Pos 1))
                    (- (get *cur 2) (get Pos 2))
                    (- (get *cur 3) (get Pos 3)))
              P)))


(dm mov-part-bel> (B P)
  # Make offset version instead of static function?

  (mov-cur-vert (first-vert> B) T)
  (adj-cur-y)

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

            
(dm col> (Rgb A P)
  (let ((R G B) Rgb)
    (for Vert (: verts)
      (put Vert 'rgba (list R G B A
                            R G B A
                            R G B A
                            R G B A))
      (req-send *ipc Vert)))
  (when P
    (when (: a) (col> @ Rgb A P))
    (when (: b) (col> @ Rgb A P))))


(dm col-verts> (Rgb A P)
  (let ((R G B) Rgb)
    (for Vert (: verts)
      (put Vert 'rgba (list R G B A
                            R G B A
                            R G B A
                            R G B A))))
  (when P
    (when (: a) (col-verts> @ Rgb A P))
    (when (: b) (col-verts> @ Rgb A P))))

                            
(dm col-a> (F P)
  (for Vert (: verts)
    (with Vert
      (=: rgba
          (place 4
            (place 8
              (place 12
                (place 16 (: rgba) F) F) F) F)))
    (req-send *ipc Vert))
  (when P
    (when (: a) (col-a> @ F P))
    (when (: b) (col-a> @ F P))))

    
# debug, unused
(dm bnds> (A B)

  (let ((Xa Ya) (when (and (: a) 
                           (not (= (: a) This))
                           (not A))
                  (bnds> (: a)))
        (Xb Yb) (when (and (: b)
                           (not (= (: b) This))
                           (not B))
                  (bnds> (: b)))
        (X Y) (with (last-vert> This)
                  (ori> (: met)
                        (: pos))))

    #(println 'calc-bounds This (: data) (format X *Scl) (format Y *Scl))

    # If neither Car or Cdr, use cons
    (list (max (if Xa Xa X) (if Xb Xb X) X)
          (min (if Ya Ya Y) (if Yb Yb Y) Y))))


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
