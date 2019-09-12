# bg:fg/text = base03:base0

(def '*col-base-03 (list (*/ 1.0 0.0   255.0)   (*/ 1.0 43.0  255.0)  (*/ 1.0 54.0  255.0)))
(def '*col-base-02 (list (*/ 1.0 0.0   255.0)   (*/ 1.0 54.0  255.0)  (*/ 1.0 66.0  255.0)))
(def '*col-base-01 (list (*/ 1.0 88.0  255.0)   (*/ 1.0 110.0 255.0)  (*/ 1.0 117.0 255.0)))
(def '*col-base-00 (list (*/ 1.0 101.0 255.0)   (*/ 1.0 123.0 255.0)  (*/ 1.0 131.0 255.0)))

(def '*col-base-0 (list (*/ 1.0 131.0 255.0) (*/ 1.0 148.0 255.0) (*/ 1.0 150.0 255.0)))
(def '*col-base-1 (list (*/ 1.0 147.0 255.0) (*/ 1.0 161.0 255.0) (*/ 1.0 161.0 255.0)))
(def '*col-base-2 (list (*/ 1.0 238.0 255.0) (*/ 1.0 232.0 255.0) (*/ 1.0 213.0 255.0)))
(def '*col-base-3 (list (*/ 1.0 253.0 255.0) (*/ 1.0 246.0 255.0) (*/ 1.0 227.0 255.0)))

(def '*col-violet  (list (*/ 1.0 108.0 255.0) (*/ 1.0 113.0  255.0) (*/ 1.0 196.0 255.0)))
(def '*col-blue    (list (*/ 1.0 38.0  255.0) (*/ 1.0 139.0  255.0) (*/ 1.0 210.0 255.0)))
(def '*col-cyan    (list (*/ 1.0 42.0  255.0) (*/ 1.0 161.0  255.0) (*/ 1.0 152.0 255.0)))
(def '*col-green   (list (*/ 1.0 133.0 255.0) (*/ 1.0 153.0  255.0) (*/ 1.0 0.0   255.0)))
(def '*col-yellow  (list (*/ 1.0 181.0 255.0) (*/ 1.0 137.0  255.0) (*/ 1.0 0.0   255.0)))

(def '*col-orange  (list (*/ 1.0 203.0 255.0) (*/ 1.0 75.0   255.0) (*/ 1.0 22.0  255.0)))
(def '*col-red     (list (*/ 1.0 220.0 255.0) (*/ 1.0 50.0   255.0) (*/ 1.0 47.0  255.0)))
(def '*col-magenta (list (*/ 1.0 211.0 255.0) (*/ 1.0 54.0 255.0) (*/ 1.0 130.0 255.0)))


(de get-color-type (Any)
  #{

    Str and Delimeters are base-01/base-0

    Violet = SYM
    Blue = NUM
    Cyan = CUR
    Green = CONS
    Yellow = NIL/()/'''' and T
    - make () same as delimeter?
    - and "" same as str?
    Orange = BOX
    Red = PTR
    Magenta = EXT

  }#
  
  # Adjust brightness for time

  (cond ((= Any NIL) *col-yellow)
        ((lst? Any)
         (if (lst? (cdr Any))
             *col-base-0
             *col-green)) # pair
        (T
         (cond ((= Any T)   *col-yellow)
               ((num? Any)  *col-blue)
               ((box? Any)  *col-orange)
               ((ext? Any)  *col-magenta)
               ((str? Any)  *col-base-01)
               ((sym? Any)  *col-violet)))))


(de gen-particles (Any)
  # Circ: 1 2 3 .
  # Improper: 1 2 3 . 4

  #{
    (list 1 2 3 (4 5 (6)))

    P1
    - 1
    - P2 = (2 3)
    - [1]

    P2
    - 2
    - P3 = (3)
    - [2]

    P3
    - 3
    - PL
    - [3]

    PL
    - (4 5 (6))
    - P4
    - [( )]

    # Close token can only be determined after trav list
    
  }#

  # NIL also a lst so place before to treat as sym

  (cond ((= Any NIL) (gen-atom Any T))
        ((lst? Any)
          # If CDR list then list else cons
          (if (lst? (cdr Any))
            (gen-list Any)
            (gen-pair Any)))
        (T           (gen-atom Any T))))


(de gen-list (Any)
  (if (circ? Any)
      (nil
        (prinl "gen-cells: circ list")
        (gen-atom Any))

      (let (Open  (draw-chars "(" *col-base-0 NIL)
            Parts (gen-list-parts Any)
            First (car Parts))
        (new '(+Particle)
             Any
             First
             (push (draw-chars ")" *col-base-0 T) Open)))))


(de gen-list-parts (Any)
  (let (List ()
        Prev NIL)
    # Connect each particle
    # 1 -> 2 -> 3
    (for A Any
      (adv-ptr)
      (let Part (gen-particles A)
        (push 'List Part)
        (when Prev
          (put Prev 'next Part))
        (setq Prev Part)))
    (flip List)))


(de gen-pair (Any)
  (let (Open  (draw-chars "(" (get-color-type Any) T)
        Car   (gen-particles (car Any))
        Dummy (adv-ptr)
        Dot   (draw-chars "." (get-color-type Any) T)
        Dummy (adv-ptr)
        Cdr   (gen-particles (cdr Any))
        Close (draw-chars ")" (get-color-type Any) T))
    (new '(+Particle)
         Any
         (cons Car Cdr)
         (append Open Dot Close))))


(de gen-atom (Any Adv)

  (let (Col (get-color-type Any)
        Verts (cond ((= Any NIL) (draw-sym Any Col Adv))
                    ((num? Any)  (draw-num Any Col Adv))
                    ((box? Any)  (draw-sym Any Col Adv))
                    ((str? Any)  (draw-str Any Col Adv))
                    (T           (draw-sym Any Col Adv))))
    (new '(+Particle) Any NIL Verts)))


################################################################################

# Batch these

(de draw-str (Str Col Adv)

  (let Verts ()

    (for C (chop (pack "\"" Str "\""))
      (push 'Verts (draw-glyph C Col))
      (when Adv (adv-ptr)))

    (flip Verts)))


(de draw-num (Num Col Adv)

  (let Verts ()

    (for C (chop (format Num))
      (push 'Verts (draw-glyph C Col))
      (when Adv (adv-ptr)))

    # Inc byte spacing depending on length

    (flip Verts)))


(de draw-sym (Sym Col Adv)

  (let Verts ()

    (for C (chop (sym Sym))
      (push 'Verts (draw-glyph C Col))
      (when Adv (adv-ptr)))

    # Inc byte spacing depending on length

    (flip Verts)))


(de draw-chars (Chars Col Adv)
  # Similar to str without quotes

  (let Verts ()

    (for C (chop Chars)
      (push 'Verts (draw-glyph C Col))
      (when Adv (adv-ptr)))

    (flip Verts)))


(de draw-glyph (Char Col)

  (let Vert (pop '*verts)

    # Reset vertex
    # Pos = baseline = X,0,0
    (with Vert
      (=: pos (get *vert-ptr 'pos))
      (=: rgba (list (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0)))

    (update-glyph> Vert Char)

    (update-model-matrix> Vert)

    (cmd-update-vert *ipc Vert)

    (inc '*cnt-v 1)

    Vert))
