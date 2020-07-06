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

  Points reference data so deleting points will del the ref data (provided no
  other data references it) so if the visual representation is gone, so will 
  the data (be GC'd).
}#

#{
   use l b r t?

   ymax = line height
   xmin = origin
   +---+
   |   |
   O---+ xmax/ymin
         can go below
}#

(class +Point)

(dm T (Any
       Lay
       A B)
       
  (=: any Any)
    
  (=: lay Lay)  
  (=: a A)
  (=: b B)
  (=: c)  

  (=: ori)  
  
  # whether to redraw
  (=: dirt T))


(dm first-vert> () (car (: verts)))
(dm last-vert> () (last (: verts)))

(de is-pair? (P) (isa '(+Pair) P))
(de is-str?  (P) (isa '(+Str)  P))
(de is-int?  (P) (isa '(+Int)  P))
(de is-sym?  (P) (isa '(+Sym)  P))
(de is-nil?  (P) (isa '(+Nil)  P))
(de is-atom? (P) (not (isa '(+Pair)  P)))

(dm bel-pair?> () (isa '(+Pair) (: c)))
(dm bel-sym?>  () (isa '(+Sym)  (: c)))
(dm bel-str?>  () (isa '(+Str)  (: c)))
(dm bel-nil?>  () (isa '(+Nil)  (: c)))

(dm bel-x?>  () (lay-x?> (: c)))
(dm bel-y?>  () (lay-y?> (: c)))

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

(dm set-lay> (L Reuse-Col)
  
  #{
    
    Set color and glyph
    
  }#
  
  (unless (= (: lay) L)
  
    (let (Col (if Reuse-Col
                  (get-first-col> This)
                  (if (= L 'x)
                      (car (get '+Vertex 'cols-pair))
                      (cadr (get '+Vertex 'cols-pair))))
          Ch (if (= L 'x)
                 (car (get '+Vertex 'chs-pair))
                 (cadr (get '+Vertex 'chs-pair))))
      # Change vert
      (del> (pop (:: verts)))
      (push (:: verts) (make-vert-glyph Ch Col)))
      
      #(col> This Col 1.0)
      
    (=: lay L)))

#{
(de b@ @
  (let (A (next)
        B NIL)
    (while (setq B (next))
      (b> A B)
      (setq A B))))


(dm b> (Cdr)
      
  (=: b Cdr)
  (put Cdr 'c This)
  
  # Remember, *start's 'any is always the symbol itself
  # To get its val, is to get it's 'b
  (when (pair (: any))
    (con (car (:: data)) (get Cdr 'any))))


(dm a> (Car)

  (=: a Car)
  (put Car 'c This)

  (set (car (:: data)) (get Car 'any)))
}#

(dm lup-a> ()
  (if (: a)
      @
      (lup-pt (car (: any)))))

      
(dm lup-b> ()
  (when (: b)
    (lup-pt (get (: b) 'any))))
  

(dm top> ()
  (mov-cur-abv This)
  (: any))
 
 
(dm pos-cur-cdr> ()

  (if (lay-x?> This)

      # If Pair get from Car
      # If Sym (exp) get from Val        

      (let (Car (lup-a> This)
            Val (when Car (lup-b> Car))
            X (if (is-sym? This)
                  (if Val # Get from Cdr else Car
                      (+ (get Val 'ori 1) (get Val 'dims 1))
                      (+ (get Car 'ori 1) (get Car 'dims 1)))
                  (if Val # Get from Cdr else Car
                      (+ (get Val 'ori 1) (get Val 'dims 1))
                      (+ (get Car 'ori 1) (get Car 'dims 1)))))
                      
        (mov-cur X (cadr (: ori)))
        (adj-cur-x 2.0))
      
      (prog
        # Pos under
        (mov-cur (car (: ori))
                 (- (cadr (: ori)) (cadr (: dims))))
        (adj-cur-y (+ 1.0 *sublist-space-y)))))
 

(dm pos-cur-car> ()
  (if (lay-x?> This)
      (pos-cur-x-car> This)
      (pos-cur-y-car> This)))
  
  
(dm pos-cur-x-car> ()
  #{
  
    X lay = car is -Y
    
  }#
  
  #(setq *cur (: ori))    

  (mov-cur (car (: ori))
           (cadr (: ori)))
           #(- (cadr (: ori)) (cadr (: dims))))
  
  # Double space if car has line
  (adj-cur-y 
    (let A (lup-a> This)
      (when (get A 'line)
        (+ 1.0 *sublist-space-y)))))

  
(dm pos-cur-y-car> ()
  #{
  
    Y lay = car is +X
    
  }#
  
  #(setq *cur (: ori))
  
  (mov-cur (+ (car (: ori)) (car (: dims)))
           (cadr (: ori)))
    
  (adj-cur-x *cdr-space-x))
  

(dm lay-cur> ()

  (when (get (: line) 'p)
    (align-cur-line This))

  # After mov>, *cur will be at last vert pos
  # Use it to set the dims
  
  (mov> This)
  
  (upd-dims-lay *dims-lay *cur)
  (upd-dims> This *dims-lay)
  
  (if (: line)
      # Check start
      (upd-line-start> This)
      # Middle or end, set dims of cur line
      (upd-dims> *line-lay *dims-lay))

  # Update list head from cur
  (when (caar *list-lay)
    (upd-dims-lay (cadr (car *list-lay)) *cur))
      
  (upd-list-end> This))


(dm lay> ()
  (lay-cur> This)
  (pos-cur-car> This))
  

(dm upd-line-start> ()
            
  (if (get (: line) 'p) # Sub line
  
      (prog
      
        (upd-dims> This *cur)
      
        # Reset x dims
        (set *dims-lay))
              
      # Push new inner list
      (push '*list-lay
        (list This # Particle for List
              (list NIL # Bnds
                    T
                    NIL))))
                      
  (setq *line-lay This))
        
  
(dm upd-list-end> ()
        
  # Pop on end of list:
  # * X-Pair cdr atom
  # * Y-Pair car atom
  (when (or (and (= (get (: c) 'b) This)
                 (not (is-pair? This)))
            (and (= (get (: c) 'a) This)
                 (not (is-pair? This))
                 (lay-y?> (: c))))
    
    (when (lay-y?> (: c))
      ~(assert (get This 'line)))
    
    # Cache ends
    #(put (caar *list-lay) 'line 'e This)
    #(put *line-lay 'line 'e This)
    
    (let? Ls-In (pop '*list-lay)
        
      (let (Ls-Out (car *list-lay)
            Dims-I (cadr Ls-In)
            Dims-O (cadr Ls-Out))
      
        (when (car Ls-Out)
          (upd-dims-lay Dims-O Dims-I))
      
        (upd-dims> (car Ls-In) Dims-I)))))

    
(dm upd-dims> (Cur)
  #{
    +Pair: Car bnds
    +Sym: Car+Cdr bnds
    +Num: Car+Cdr bnds
  }#
    
  (let Dims (list (abs (- (car  Cur) (car  (: ori))))
                  (abs (- (cadr Cur) (cadr (: ori)))))
    
    # Should always be updated?
                  
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
  
  #(println '+Point.del> "Delete: " This (type This))  
  
  (for Vert (: verts) (del> Vert))
  
  # del from index
  (del-node (: any))
  
  (when A
    #(println (type This) (: a) (lup-pt (car (: any))))
    (del> (if (: a) @ (lup-pt (car (: any))))
          T T))
  (when B
    (del> (if (: b) @ (lup-pt (cdr (: any))))
          T T))
    
  This)


(dm del-car> ()
  (if (: a)
      (prog
        (del> (: a))
        (=: a))
      (let? Po (lup-pt (car (: any)))
        (del> Po)
        (=: a))))
                  
  
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


(dm mov-to-pt> (B P)

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


(dm mov/+x> (B P)
  # Make offset version instead of static function?

  (mov-cur-vert (last-vert> B) T)
  (adj-cur-x 2.0)

  (let Pos (: ori)
    (mov-off> This
              (list (- (get *cur 1) (get Pos 1))
                    (- (get *cur 2) (get Pos 2))
                    (- (get *cur 3) (get Pos 3)))
              P)))


(dm mov/+y> (B P)
  # Make offset version instead of static function?

  (mov-cur-vert (first-vert> B) T)
  (adj-cur-y -1.0)

  (let Pos (: ori)
    (mov-off> This
              (list (- (get *cur 1) (get Pos 1))
                    (- (get *cur 2) (get Pos 2))
                    (- (get *cur 3) (get Pos 3)))
              P)))


(dm mov/-y> (B P)
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
# Most of these don't use any properties from point
# so should move to a vert class or make normal function

(dm get-first-col> ()
  (get (car (: verts)) 'rgba))

(dm repl-verts> (Str)
  # Save color, del verts, add new verts
  
  (let Col (get (car (: verts)) 'rgba)
    
    (for Vert (: verts) (del> Vert))
    
    (=: verts (make-vert-repr (chop Str) Col))))

    
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

    #(println 'calc-bounds This (: any) (format X *Scl) (format Y *Scl))

    # If neither Car or Cdr, use cons
    (list (max (if Xa Xa X) (if Xb Xb X) X)
          (min (if Ya Ya Y) (if Yb Yb Y) Y))))
