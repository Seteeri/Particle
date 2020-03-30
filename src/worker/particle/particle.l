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

  # Store origin also?

  (=: data Data)
  (=: a A)
  (=: b B)
  (=: c) # Prev
  (=: verts Verts)
  (=: lay Lay)
  (=: nl)

  #{
   use l b r t?

   ymax = line height
   xmin = origin
   +---+
   |   |
   O---+ xmax/ymin
         can go below
  }#
  (=: dims)

  # list of tags; always str; order matters like when searching
  # consider first tag, the name
  # when linking, def show date, tags, or show nm or raw location
  (=: tg)

  # date/time
  (=: dat (date))
  (=: tim (time)))

(dm has-car-pair> () (= (type (: a)) '(+Pair)))

(dm last-vert-pos> (Cnt) (get (last (: verts)) 'pos Cnt))
(dm first-vert> () (car (: verts)))
(dm last-vert> () (last (: verts)))

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

  (let (Sock-Render (ipc~get-fd> *ipc "RENDER")
        Verts (: verts))
    (for Vert Verts
      (send-msg *ipc
                Sock-Render
                (pack "(zv " (get Vert 'off-gl) '")"))
      (push '*verts Vert)
      (dec '*cnt-v)))

  (when (and Car (: a))
    (del> (: a) Car Cdr))

  (when (and Cdr (: b))
    (del> (: b) Car Cdr))

  (println 'cmd-del "Deleted Particle:" This (: data))

  # Clear links and flags
  (=: a)
  (=: b)
  (=: c)
  (=: nl)
  (=: data))

# List specific?

(dm last> ()

  (let Last (: b)

    (loop

      (NIL (get Last 'b))
      (println "last>" Last (get Last 'data))
      (setq Last (get Last 'b)))

    Last))


(dm last-1> ()

  (let Last This
    (loop
      # Stop at second to last
      (NIL (get Last 'b 'b))
      (setq Last (get Last 'b)))
    Last))


(dm tail> (Cnt)

  (let (I (: b)
        Ta ())

    # End loop when NIL
    (while (setq I (get I 'b))
      (push 'Ta I))

    (flip (head Cnt Ta))))


(dm mov> (Pos)

  (for Vert (: verts)

    (with Vert
      (=: pos Pos)
      (update-glyph> Vert (char (: chr))))
    (upd-mdl-mtrx> Vert)
    (cmd-upd-vert *ipc Vert)

    # adv pos
    (setq Pos (place 1
                     Pos
                     (+ (car Pos) *adv-vert)))))


# Rename off -> rel
(dm mov-off> (Off P)
  # Calc tgt pos
  (let (Pos (origin> This)
        Pos-Tgt (list (+ (get Pos 1) (get Off 1))
                      (+ (get Pos 2) (get Off 2))
                      (+ (get Pos 3) (get Off 3))))

    (mov> This Pos-Tgt)

    (when (and (car P) (: a)) (mov-off> (: a) Off P))
    (when (and (cdr P) (: b)) (mov-off> (: b) Off P))))


(dm mov-part> (B P)

  (let (Pos     (origin> This)
        Pos-Tgt (origin> B)
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

  (let Pos (origin> This)
    (mov-off> This
              (list (- (get *cur 1) (get Pos 1))
                    (- (get *cur 2) (get Pos 2))
                    (- (get *cur 3) (get Pos 3)))
              P)))


(dm mov-part-abv> (B P)
  # Make offset version instead of static function?

  (mov-cur-vert (first-vert> B) T)
  (nl-cur -1.0)

  (let Pos (origin> This)
    (mov-off> This
              (list (- (get *cur 1) (get Pos 1))
                    (- (get *cur 2) (get Pos 2))
                    (- (get *cur 3) (get Pos 3)))
              P)))


(dm mov-part-bel> (B P)
  # Make offset version instead of static function?

  (mov-cur-vert (first-vert> B) T)
  (nl-cur)

  (let Pos (origin> This)
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

      (origin> (get *metrics (: chr))
                   (: pos)
                   *sca-vert))))


(dm col> (Col)
  (let ((R G B) Col)
    (for Vert (: verts)
      (put Vert 'rgba (list R G B 1.0
                            R G B 1.0
                            R G B 1.0
                            R G B 1.0))
      (upd-mdl-mtrx> Vert)
      (cmd-upd-vert *ipc Vert))))


(dm bounds> (Car-Skip Cdr-Skip)

  (let ((Xa Ya) (when (and (: a) (not Car-Skip)) (bounds> (: a)))
        (Xb Yb) (when (and (: b) (not Cdr-Skip)) (bounds> (: b)))
        (X Y) (with (last-vert> This)
                  (origin> (get *metrics (: chr))
                               (: pos)
                               *sca-vert)))

    #(println 'calc-bounds This (: data) (format X *Scl) (format Y *Scl))

    # If neither Car or Cdr, use cons
    (list (max (if Xa Xa X) (if Xb Xb X) X)
          (min (if Ya Ya Y) (if Yb Yb Y) Y))))

# Line

(dm adj-line-to-part> (Ref)
  (let (Ori (origin> This)
        Bnds (bounds> Ref NIL T)
        Dims (list (- (get Bnds 1) (get Ori 1))
                   (- (get Bnds 2) (get Ori 2))))
    (with (: nl)
      (let ((X Y) (: dims))
        (=: dims Dims)
        (list (- (car Dims) X)
              (- (cadr Dims) Y))))))


(dm inc-line-by-part-x> (Part)
  # Calculate width of new Part plus adv
  # Bnds - Origin
  (let (Ori (origin> Part)
        Bnds (bounds> Part NIL T)
        Dx (+ (- (get Bnds 1) (get Ori 1))
              *adv-vert-2))
            
    (with (: nl)
      (=: dims (list (+ (car (: dims)) Dx)
                     (cadr (: dims)))))

    Dx))


(dm dec-line-by-part-x> (End)
  # Add op to add/sub
  (let (Bnds-B (bounds> End NIL T)
        Ori (origin> This)
        Dims (get (: nl) 'dims)
        Bnds-Ln (list (+ (get Ori 1) (get Dims 1))
                      (- (get Ori 2) (get Dims 2)))
        Dx (- (get Bnds-Ln 1) (get Bnds-B 1)))
        
    (with (: nl)
      (=: dims (list (- (car Dims) Dx)
                     (cadr Dims))))

    Dx))


(dm upd-line-by-part-y> (Part)
  # If new bnds smaller, use it
  (let (Ori (cadr (origin> This))
        Dims (get (: nl) 'dims)
        Bnds-Ln (- Ori (get Dims 2))
        Bnds-B (cadr (bounds> Part NIL T))
        Dy (abs (if (< Bnds-B Bnds-Ln)
                    (- Bnds-B Ori)
                    (cadr Dims))))

    (with (: nl)
      (=: dims (list (car Dims)
                     Dy)))

    Dy))


(dm calc-dims-nl> ()
  (let (Part This
        Ori (origin> This)
        (X Y) (bounds> This NIL T))
    (loop
      (let ((Xb Yb) (bounds> Part NIL T))
        (setq X (max X Xb)
              Y (min Y Yb)))
      (println Part (get Part 'data))
      (setq Part (get Part 'b))
      (T (or (not Part) (get Part 'nl))))
    (list (- X (car Ori)) (abs (- Y (cadr Ori))))))


(dm mov-line-p> ()
  (let? P (get (: nl) 'p)
    (setq *line P)))


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
