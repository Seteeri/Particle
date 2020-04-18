#{

  (c (a b))
                     [a|*]    [b|*]    (SYMS)  a b
                       |        |
          [c|.]      [.|.] -> [.|/]    (CONS)  (a b)
            |        |
  Abst:   [.|.]  ->  [.|/]                     ((a b))

           ^          ^
          / \        / \
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
  #(=: dims-a)
  #(=: dims-b)

  # list of tags; always str; order matters like when searching
  # consider first tag, the name
  # when linking, def show date, tags, or show nm or raw location
  (=: tg)

  # date/time
  (=: dat (date))
  (=: tim (time)))

(dm has-car-pair> () (isa '(+Pair) (: a)))
(dm is-nil-or-nil-car> () (or (isa '+Nil This) (isa '+Nil (: a))))

(dm first-vert> () (car (: verts)))
(dm last-vert> () (last (: verts)))


(dm b> (Cdr)

  (=: b Cdr)
  (put Cdr 'c This)
  
  # Remember, *main's 'data is always the symbol itself
  # To get its val, is to get it's 'b
  (when (pair (: data))
    (con (car (:: data)) (get Cdr 'data))))


(dm a> (Car)

  (=: a Car)
  (put Car 'c This)
  
  (=: data (cons (get Car 'data)
                 (cdr (: data)))))  
    
    
(dm con-cars-c> ()
  (let (Pa This
        Prv (: a))
    (while (= (get Pa 'a) Prv)
      (put Pa 'a Prv)
      (put Pa 'data (cons (get Prv 'data)
                          (cdr (get Pa 'data))))
      (setq Prv Pa
            Pa (get Pa 'c)))
    (when (= (get Pa 'b) Prv)
      (b> Pa Prv))))
    

(dm del> (Car Cdr)

  #{
    Currently, render will attempt to render all verts (*max-verts)
    -> Send cmd to render to adjust max
    - instanceCount is number of instances from beginning of array
    *verts is LIFO, thus will always keep instances pack from beginning of array
    -> Maintain vert cnt, instanceCount = size - used

    However, if del from arbitrary point then must zero it
    and instanceCount remains the same
    After del, vert returns to queue
    and next vert will reuse it

    Makes most sense to simply zero vert
    and not worry about instanceCount
    as frag will be discarded...fix later
  }#

  (let Sock-Render (ipc~get-fd> *ipc "RENDER")
    (for Vert (: verts)
      (send-msg *ipc
                Sock-Render
                (pack "(zv " (get Vert 'off-gl) '")"))
      (push '*verts Vert)
      (dec '*cnt-v)))

  (when (and Car (: a))
    (del> (: a) Car Cdr))

  (when (and Cdr (: b))
    (del> (: b) Car Cdr))

  #(println 'del> This (: data))

  # Clear links and flags?
  #(=: a)
  #(=: b)
  #(=: c)
  #(=: line) # Remove from p/n
  #(=: data)
  )


(dm mov> (Pos)

  # Check first vert
  # If changed, change rest
  # else, do nothing

  (let? Vert (car (: verts))
    (unless (= Pos (get Vert 'pos))
      (for Vert (: verts)
        (with Vert
          # Same pos, don't bother
          (unless (= Pos (: pos))
            (=: pos Pos)
            (upd-gly> Vert (char (: chr))))
            (upd-mod-mat> Vert)
            (cmd-upd-vert *ipc Vert))

        # adv pos
        (setq Pos (place 1
                         Pos
                         (+ (car Pos) *adv-vert))))))

  (=: ori (origin> This))
  
  Pos)


# Rename off -> rel
(dm mov-off> (Off P)
  # Calc tgt pos
  (let (Pos (: ori)
        Pos-Tgt (list (+ (get Pos 1) (get Off 1))
                      (+ (get Pos 2) (get Off 2))
                      (+ (get Pos 3) (get Off 3))))

    (mov> This Pos-Tgt)

    (when (and (car P) (: a)) (mov-off> (: a) Off P))
    (when (and (cdr P) (: b)) (mov-off> (: b) Off P))))


(dm mov-part> (B P)

  (let (Pos     (: ori)
        Pos-Tgt (get B 'ori)
        Off (list (- (get Pos-Tgt 1) (get Pos 1))
                  (- (get Pos-Tgt 2) (get Pos 2))
                  (- (get Pos-Tgt 3) (get Pos 3))))

    (mov> This Pos-Tgt)

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

(dm origin> ()
  (let? Vert (car (: verts))
    (with Vert
      (origin-from-bearing> (get *metrics (: chr))
               (: pos)
               *sca-vert))))


(dm col> (Col)
  (let ((R G B) Col)
    (for Vert (: verts)
      (put Vert 'rgba (list R G B 1.0
                            R G B 1.0
                            R G B 1.0
                            R G B 1.0))
      (upd-mod-mat> Vert)
      (cmd-upd-vert *ipc Vert))))


(dm bounds> (Car-Skip Cdr-Skip)

  (let ((Xa Ya) (when (and (: a) 
                           (not (= (: a) This))
                           (not Car-Skip))
                  (bounds> (: a)))
        (Xb Yb) (when (and (: b)
                           (not (= (: b) This))
                           (not Cdr-Skip))
                  (bounds> (: b)))
        (X Y) (with (last-vert> This)
                  (origin-from-bearing> (get *metrics (: chr))
                           (: pos)
                           *sca-vert)))

    #(println 'calc-bounds This (: data) (format X *Scl) (format Y *Scl))

    # If neither Car or Cdr, use cons
    (list (max (if Xa Xa X) (if Xb Xb X) X)
          (min (if Ya Ya Y) (if Yb Yb Y) Y))))


(dm check-nl> ()
  (when (get (: line))
  
    (when (get (: line) 'p)
    
      (if (= (get (: c) 'lay) 'y)
          (align-cur-to-line-y This)
          (align-cur-to-line-x This)))
          
    (setq *line-lay This)))


######################
      
(de con-all (Parts)

  (let Prv (car Parts)
  
    (for Part (cdr Parts)     
        
      # Prv -> Part
      (b> Prv Part)
      
      # Do this in con
      (put Part 'c Prv)
      
      (setq Prv Part))))

      
# con-cdr
# move to con-cdr?
(de con-aft (Ref Part Line-Skip)

  #{

    Insert x after a, taking pos of next:

    *0
    [ ]  [ ]  NIL
    a    b

           *0
    [ ]  [ ]  [ ]  NIL
    a    x    b

    Newline case:

    *0
    [ ]
    a

    [ ] - NIL
    b


    [ ]
    a

    *0
    [ ] - [ ] - NIL
    x     b

    Possible to not make newline so x is on same line as a
    
  }#

  (let Cdr (get Ref 'b)

    (con-all (list Ref Part Cdr))

    # If Cdr is a new line, xfer it to new Part
    (when (and (get Cdr 'line)
               (not Line-Skip))
      (xchg-nl Cdr Part))))


(de repl-pair (Ref Part Del-Skip)

  #{

    Replace b with x

          *0
    [ ]   [ ]  [ ]  NIL
    a     b    c

          *0
    [ ]   [ ]  [ ]
    a     x    c

  }#

  (let (Cdr (get Ref 'b)
        Prv (get Ref 'c))

    (con-all (list Prv Part Cdr))

    # If Ref is a new line, xfer it to new Part
    (when (get Ref 'line)
      (xchg-nl Ref Part))

    (unless (val 'Del-Skip)
      (del> Ref T NIL))))


(de xchg-nl (Src Dst)

  (let (Nl (get Src 'line)
        Prv (get Nl 'p)
        Nxt (get Nl 'n))

    (when Prv
      (put (get Prv 'line) 'n Dst))

    (when Nxt
      (put (get Nxt 'line) 'p Dst))
      
    # Update children's lists when Src is NIL NIL
    (when (and (not (get Nl 'p)) (not (get Nl 'l)))
      (let (Child (get Nl 'n)
            Visited NIL)
        (while (and Child
                    (not (member Child Visited)))
          (println 'xchg-nl "Set list: " Child (get Child 'data))
          (push 'Visited Child)
          (put (get Child 'line) 'l Dst)
          (setq Child (get Child 'line 'n))
          (when (member Child Visited)
            (println 'xchg-nl "WARNING! Cycle detected: " Child (get Child 'data))))))

    (put Dst 'line Nl))

  (put Src 'line NIL)
  (println 'xchg-nl "Exchange line: " Src (get Src 'data) " -> " Dst (get Dst 'data))

  (setq *line Dst))


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


######
# Line

# Used only by delete
(dm adj-line-to-part> (Ref)
  (let (Ori (: ori)
        Bnds (bounds> Ref NIL T)
        Dims (list (abs (- (get Bnds 1) (get Ori 1)))
                   (abs (- (get Bnds 2) (get Ori 2)))))
    (with (: line)
      (let ((X Y) (: dims))
        (=: dims Dims)
        (list (- (car Dims) X)
              (- (cadr Dims) Y))))))


(dm inc-line-by-part-x> (Part)
  # Calculate width of new Part plus adv
  # Bnds - Origin
  (let (Ori (get Part 'ori)
        Bnds (bounds> Part NIL T)
        Dx (abs (+ (- (get Bnds 1) (get Ori 1))
              *adv-vert-2)))
            
    (with (: line)
      (=: dims (list (+ (car (: dims)) Dx)
                     (cadr (: dims)))))

    Dx))


(dm dec-line-by-part-x> (End)
  # Add op to add/sub
  (let (Bnds-B (bounds> End NIL T)
        Ori (: ori)
        Dims (get (: line) 'dims)
        Bnds-Ln (list (+ (get Ori 1) (get Dims 1))
                      (- (get Ori 2) (get Dims 2)))
        Dx (abs (- (get Bnds-Ln 1) (get Bnds-B 1))))
        
    (with (: line)
      (=: dims (list (- (car Dims) Dx)
                     (cadr Dims))))

    Dx))


(dm min-line-by-part-y> (Part)
  # If new bnds smaller, use it
  (let (Ori (cadr (: ori))
        Dims (get (: line) 'dims)
        Bnds-Ln (- Ori (get Dims 2))
        Bnds-B (cadr (bounds> Part NIL T))
        Dy (abs (if (< Bnds-B Bnds-Ln)
                    (- Bnds-B Ori)
                    (cadr Dims))))

    (with (: line)
      (=: dims (list (car Dims)
                     Dy)))

    Dy))


(dm calc-dims-nl> ()
  # Calculate until NIL data
  
  (let (Part This
        Ori (: ori)
        (X Y) (if (has-car-pair> This)
                  Ori
                  (bounds> This NIL T)))
        
    (loop
        
      (let ((Xb Yb) (if (has-car-pair> Part) 
                        Ori
                        (bounds> Part NIL T)))
        (setq X (max X Xb)
              Y (min Y Yb)))

      (setq Part (get Part 'b))

      (T (or (not Part)
             (get Part 'line))))
    
    (list (abs (- X (car Ori)))
          (abs (- Y (cadr Ori))))))
