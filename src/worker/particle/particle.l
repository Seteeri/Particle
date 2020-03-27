#{

  Goal is to generate a particle (representation for each cell encountered)

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

  Parts reference data so deleting parts will del the ref data and thus it can
  be GC so if the visual representation is gone, so will the data

  The visual representation must have the same structure as the underlying
  data, in other words, the underlying data are cons cells aka linked lists, so
  the visual representation has the same props plus additional props since they
  are compound cells (instances of a class)

  ###################################

  Create different display levels:

  - Expand lists
  - Expand symbols and numbers

}#

(de sym-int? (Any)
  (and (sym? Any)
       (not (str? Any)) (not (box? Any)) (not (ext? Any))
       (not (= Any NIL))))


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
  
  (=: dims)
  
  # list of tags; always str; order matters like when searching
  # consider first tag, the name
  # when linking, def show date, tags, or show nm or raw location
  (=: tg)
    
  # date/time
  (=: dat (date))
  (=: tim (time)))


(dm con> (B) (=: b B))

(dm val> () (: b))

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
      (NIL (get (get Last 'b) 'b))
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
      (=: pos (copy Pos))
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
  (let (Pos (get-origin> This)
        Pos-Tgt (list (+ (get Pos 1) (get Off 1))
                      (+ (get Pos 2) (get Off 2))
                      (+ (get Pos 3) (get Off 3))))

    (mov> This Pos-Tgt)

    (when (and (car P) (: a)) (mov-off> (: a) Off P))
    (when (and (cdr P) (: b)) (mov-off> (: b) Off P))))


(dm mov-part> (B P)

  (let (Pos     (get-origin> This)
        Pos-Tgt (get-origin> B)
        Off (list (- (get Pos-Tgt 1) (get Pos 1))
                  (- (get Pos-Tgt 2) (get Pos 2))
                  (- (get Pos-Tgt 3) (get Pos 3))))
    
    (mov> This Pos-Tgt)
    
    # Apply offset to car/cdr
    (when (and (car P) (: a)) (mov-off> (: a) Off P))
    (when (and (cdr P) (: b)) (mov-off> (: b) Off P))))


(dm mov-part-aft> (B P)
  # Make offset version instead of static function?
  
  (mov-cur-vert (last (get B 'verts)) T)
  (adv-cur 2.0)
  
  (let Pos (get-origin> This)
    (mov-off> This
              (list (- (get *cur 1) (get Pos 1))
                    (- (get *cur 2) (get Pos 2))
                    (- (get *cur 3) (get Pos 3)))
              P)))


(dm mov-part-abv> (B P)
  # Make offset version instead of static function?
  
  (mov-cur-vert (car (get B 'verts)) T)
  (nl-cur -1.0)

  (let Pos (get-origin> This)
    (mov-off> This
              (list (- (get *cur 1) (get Pos 1))
                    (- (get *cur 2) (get Pos 2))
                    (- (get *cur 3) (get Pos 3)))
              P)))


(dm mov-part-bel> (B P)
  # Make offset version instead of static function?
  
  (mov-cur-vert (car (get B 'verts)) T)
  (nl-cur)
  
  (let Pos (get-origin> This)
    (mov-off> This
              (list (- (get *cur 1) (get Pos 1))
                    (- (get *cur 2) (get Pos 2))
                    (- (get *cur 3) (get Pos 3)))
              P)))
    

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
    
  # Clear links and flags
  (=: a)
  (=: b)
  (=: c)  
  (=: nl))    


# Verts
# Most of these don't use any properties from part
# so should move to a vert class or make normal function

(dm get-origin> ()
    
  (when (: verts)
  
    (let (Vert (car (: verts))
          Pos (get Vert 'pos)
          (X Y) (get-origin> (get *metrics (get Vert 'chr))
                             Pos
                             *sca-vert))

      (list X Y (get Pos 3)))))
  

(dm col> (Col)
  (let ((R G B) Col)
    (for Vert (: verts)
      (put Vert 'rgba (list R G B 1.0
                            R G B 1.0
                            R G B 1.0
                            R G B 1.0))
      (upd-mdl-mtrx> Vert)
      (cmd-upd-vert *ipc Vert))))


(dm last-vert-pos> (Cnt)
  (get (get (last (: verts)) 'pos) Cnt))


(dm first-vert> ()
  (car (: verts)))

  
(dm last-vert> ()
  (last (: verts)))


(dm calc-bounds> (Car-Skip Cdr-Skip)

  # Calc abs bounds of CAR and CDR
  # Store this?

  (let ((Xa Ya) (when (and (: a) (not Car-Skip)) (calc-bounds> (: a)))
        (Xb Yb) (when (and (: b) (not Cdr-Skip)) (calc-bounds> (: b)))
        (X Y) (with (last-vert> This)
                  (get-origin> (get *metrics (: chr))
                               (: pos)
                               *sca-vert)))

    # If neither Car or Cdr, use cons
    (list (max (if Xa Xa X) (if Xb Xb X) X)
          (min (if Ya Ya Y) (if Yb Yb Y) Y))))


(dm has-car-pair> () (= (type (: a)) '(+Pair)))

          
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
