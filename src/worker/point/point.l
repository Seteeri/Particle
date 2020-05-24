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
(de is-int?  (P) (isa '(+Int)  P))
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

(dm has-car?> () (: a))
(dm has-cdr?> () (: b))

(dm of-car?> (A) (= (: a) A))
(dm of-cdr?> (B) (= (: b) B))

(dm is-car?> () (= (get (: c) 'a) This))
(dm is-cdr?> () (= (get (: c) 'b) This))
(dm is-end?> () (and (is-cdr?> This)
                     (not (is-pair? This))))
                     
(dm lay-x?> () (= (: lay) 'x))
(dm lay-y?> () (= (: lay) 'y))

(dm lay> (L)
  
  #{
    
    Set color and glyph
    
    For +Pair only?

    Ptr uses put instead of this
    TODO: '(+Ptr +Sym)
    
  }#
  
  (unless (= (: lay) L)
  
    (let (Col (if (= L 'x)
                  (car *cols-pair)
                  (cadr *cols-pair))
          Ch (if (= L 'x)
                 (car *ch-pair-def)
                 (cadr *ch-pair-def)))
      # Change vert
      (del> (pop (:: verts)))
      (push (:: verts) (make-vert-glyph Ch Col)))
      
      #(col> This Col 1.0)
      
    (=: lay L)))

  
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
  #{
  
    Move below or use Pair dims
    
    Assumes Pair is always single line with dot or 2x dot
    
    Otherwise must use dims of Pair
    
  }#

  (setq *cur (: ori))    

  (if (or (is-pair? (: a))
          (and (is-int? (: a)) (not (: b))))
    (adj-cur-y (+ 1.0 *sublist-space-y))
    (adj-cur-y)))

  
(dm pos-cur-for-car-y> ()
  (setq *cur (: ori))
    
  (adj-cur-x *cdr-space-x))
  

(dm lay-cur> ()
  (when NIL
    (println 'lay-cur> This
              "@ Cur: " (format (get *cur 1) *Scl)  
                        (format (get *cur 2) *Scl)))

  (when (get (: line) 'p) (align-cur-line This))
  
  (mov> This) # *cur will be at last vert pos
  (when NIL
    (println 'lay-cur> This
              "+ Cur: " (format (get *cur 1) *Scl)  
                        (format (get *cur 2) *Scl)))
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
    
  
(dm upd-line-post> ()
  #{
  
    Update line and update list
    
    Item properties:
    * Has line start
    * Has line
    * Is end
  
    List will always have Line
  
  }#

  (upd-dims> This)         
  
  (if (: line)
      
      (prog
        (if (get (: line) 'p) # Sub line
            (prog
              (off *bnds-x) # Reset line bnds x
              (upd-cur)
              (upd-dims-2> This *cur))
            (prog
              # Start of line/list
              (println 'upd-line-post> "Push list: " This (car (: data)))
              (push '*list-lay
                (list This # NIL T
                      (list NIL
                            T
                            0.0)))))
        (setq *line-lay This))
        
      (prog
        # Middle or end
        # Set dims of cur line
        (upd-dims> *line-lay)))
         
  # List Stuff
      
  # Update list head
  (when (caar *list-lay)
    # Update list dims from cur
    (upd-lst-dims)
    #(upd-dims-2> @ (cadar *list-lay))
    #(println 'upd-list-lay (caar *list-lay)
    #  (format (car (cadar *list-lay)) *Scl)
    #  (format (cadr (cadar *list-lay)) *Scl))
    )
    
  # On end or Y-Pair.car=NIL: pop/upd list
  (when (or (is-end?> This)
            (and (: c)
                 (lay-y?> (: c))
                 (is-nil? This)))
    
    #(println 'upd-line-post> (is-end?> This) This (car (: data)))
        
    # Cache ends
    (put (get (caar *list-lay) 'line) 'e This)
    (put (get *line-lay 'line) 'e This)
        
    # Pop and upd list
    (pop-list-lay)))


(dm upd-dims> ()
  #{
    +Pair: Car bnds
    +Sym: Car+Cdr bnds
    +Num: Car+Cdr bnds
  }#
  
  (let Dims (list (abs (- *bnds-x (car  (: ori))))
                  (abs (- *bnds-y (cadr (: ori)))))
  
    #(println 'upd-dims> This Dims)
    
    ~(assert (car Dims))
                  
    (=: dims Dims)
    (when (: line)
      (put @ 'dims Dims))))
    
    
(dm upd-dims-2> (Bnds)
  #{
    +Pair: Car bnds
    +Sym: Car+Cdr bnds
    +Num: Car+Cdr bnds
  }#
    
  (let Dims (list (abs (- (car  Bnds) (car  (: ori))))
                  (abs (- (cadr Bnds) (cadr (: ori)))))
    
    #(println 'upd-dims-2> This Dims)                  
    
    ~(assert (car Dims))
                  
    (=: dims Dims)
    (when (: line)
      (put @ 'dims Dims))))
    
    
##############

  
(dm del> (A B)

  #{
    Render will render *max-verts
    * Poss send cmd to render to adjust instance count
      * verts is LIFO - always keeps instances contiguous
  }#
    
  (for Vert (: verts) (del> Vert))
    
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
  
  #{
  (let? Vert (car (: verts))

    # Set to pos of last vert (mul by length)
    (if (= (: ori) *cur) 
        (list (+ (car *cur)
                  (*/ (meta '(+Vertex) 'adv-vert)
                      (num-to-fl (- (length (: verts)) 1))
                      1.0))
              (cadr *cur))
        (mov-verts> This))))
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
