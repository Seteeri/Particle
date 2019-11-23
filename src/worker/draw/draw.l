(def '*space-line (+ *adv-vert *adv-vert))

# bg:fg/text = base03:base0

(def '*col-base-03 (list (*/ 1.0 0.0   255.0) (*/ 1.0 43.0  255.0) (*/ 1.0 54.0  255.0)))
(def '*col-base-02 (list (*/ 1.0 0.0   255.0) (*/ 1.0 54.0  255.0) (*/ 1.0 66.0  255.0)))
(def '*col-base-01 (list (*/ 1.0 88.0  255.0) (*/ 1.0 110.0 255.0) (*/ 1.0 117.0 255.0)))
(def '*col-base-00 (list (*/ 1.0 101.0 255.0) (*/ 1.0 123.0 255.0) (*/ 1.0 131.0 255.0)))

(def '*col-base-0  (list (*/ 1.0 131.0 255.0) (*/ 1.0 148.0 255.0) (*/ 1.0 150.0 255.0)))
(def '*col-base-1  (list (*/ 1.0 147.0 255.0) (*/ 1.0 161.0 255.0) (*/ 1.0 161.0 255.0)))
(def '*col-base-2  (list (*/ 1.0 238.0 255.0) (*/ 1.0 232.0 255.0) (*/ 1.0 213.0 255.0)))
(def '*col-base-3  (list (*/ 1.0 253.0 255.0) (*/ 1.0 246.0 255.0) (*/ 1.0 227.0 255.0)))

(def '*col-violet  (list (*/ 1.0 108.0 255.0) (*/ 1.0 113.0  255.0) (*/ 1.0 196.0 255.0)))
(def '*col-blue    (list (*/ 1.0 38.0  255.0) (*/ 1.0 139.0  255.0) (*/ 1.0 210.0 255.0)))
(def '*col-cyan    (list (*/ 1.0 42.0  255.0) (*/ 1.0 161.0  255.0) (*/ 1.0 152.0 255.0)))
(def '*col-green   (list (*/ 1.0 133.0 255.0) (*/ 1.0 153.0  255.0) (*/ 1.0 0.0   255.0)))
(def '*col-yellow  (list (*/ 1.0 181.0 255.0) (*/ 1.0 137.0  255.0) (*/ 1.0 0.0   255.0)))

(def '*col-orange  (list (*/ 1.0 203.0 255.0) (*/ 1.0 75.0 255.0) (*/ 1.0 22.0  255.0)))
(def '*col-red     (list (*/ 1.0 220.0 255.0) (*/ 1.0 50.0 255.0) (*/ 1.0 47.0  255.0)))
(def '*col-magenta (list (*/ 1.0 211.0 255.0) (*/ 1.0 54.0 255.0) (*/ 1.0 130.0 255.0)))

(de get-color-type (Any)
  #{

    Str and Delimeters are base-01/base-0

    Violet = SYM
    Blue = NUM
    Cyan =
    - or since cursor is a symbol...
    Green = CONS
    Yellow = NIL/()/'''' and T
    - make () same as delimeter?
    - and "" same as str?
    Orange = BOX
    Red = PTR
    Magenta = EXT

  }#

  # Adjust brightness for time

  (cond ((== Any NIL) *col-yellow)
        ((lst? Any)
         (if (lst? (cdr Any))
             *col-base-0
             *col-green)) # pair
        (T
         (cond ((== Any T)  *col-yellow)
               ((num? Any)  *col-blue)
               ((box? Any)  *col-orange)
               ((ext? Any)  *col-magenta)
               ((str? Any)  *col-base-01)
               ((sym? Any)  *col-violet)))))


#{
  * Lists/cons are fully expanded, however atoms (num/syms) are condensed
    * NIL
      * Both simultaneously a list/symbol
      * Data structure is 4 cons cells
      * Also condensed -> NIL or #### (ptr)

  * Lists:   expanded
  * Symbols: condensed, draw CDR (no show property list etc)
  * Nums:    condensed, draw proper num (not chain of cells)
}#

#{
(let ((Adr-Car Adr-Cdr) (unpack-ptrs Any))
  (prin "Adr: ") (prinl (decode-ptr Adr))
  (prin "Car: ") (prinl Adr-Car)
  (prin "Cdr: ") (prinl Adr-Cdr))
}#

# Determine type of data
# NIL also a lst so place before to treat NIL as sym

#{
  
  (let A 1
    T
    (let (A 1
          B 2)
      T))

  (prog
    1
    2
    3)

  (for I 10
    T)
    
  (def '*foo 'bar)
  
  (1 2 . 3)
  (4 5 6)
}#


(de draw-any (Any Depth X-Root)
  
  (if (pair Any)
      (if (lst? (cdr Any))
          (draw-any-list Any
                         Depth
                         X-Root)
          (draw-pair Any
                     Depth
                     X-Root))
      (draw-atom Any T)))


(de draw-any-list (Any Depth X-Root)
  (let Car (car Any)
    (cond
      ((= Car 'let) (draw-let Any Depth X-Root))  
      ((or (= Car 'with)
           (= Car 'when)
           (= Car 'unless)
           (= Car 'while)) (draw-with Any Depth X-Root))             
      ((= Car 'prog) (draw-prog Any Depth X-Root))  
      ((= Car 'for) (draw-for Any Depth X-Root))      
      ((= Car 'cond) (draw-cond-2 Any Depth X-Root))
      ((= Car 'def) (draw-list-row-dot Any Depth X-Root))
      ((= Car 'de) (draw-de Any Depth X-Root))
      ((or (= Car '=)
           (= Car '==)
           (= Car 'cons)) (draw-list-row-dot Any Depth X-Root))
      ((num? Car) (draw-list-row-dot Any Depth X-Root))
      (T (draw-list-col2-dot Any Depth X-Root)))))


(de draw-list-col2-dot (List Depth X-Root)

  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr 2.0)

  (let ((X Y) (get *vert-ptr 'pos))
    (draw-list-col2 List (inc 'Depth 1.0) X-Root)))


(de draw-list-col2 (List Depth X-Root)
  
  #{
    Draw first two items, then remainder aligned below second item
    Default method to draw forms
    Exceptions are in forms
  }#

  (let? Any (car List)
    (draw-any Any Depth X-Root)
    (adv-ptr))

  # Rem will align to 2nd item
    
  (let ((X Y) (get *vert-ptr 'pos))

    (let? Any (cadr List)
      (draw-any Any Depth X-Root))

    (let? Rem (nth List 3)
      (set-x-ptr-2 X)
      (nl-ptr)
      (draw-list-y Rem Depth X))))


(de draw-list-col-dot (List Depth X-Root)

  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr 2.0)

  (let ((X Y) (get *vert-ptr 'pos))
    (draw-list-col List Depth X Y)))


# TODO: Rename set-x-ptr-2 -> x-ptr

(de draw-list-col (List Depth X-Root)
  
  (let ((X Y) (get *vert-ptr 'pos))
    (draw-list-y List Depth X)))
    

(de draw-list-row-dot (List Depth X-Root)

  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr 2.0)

  (let ((X Y) (get *vert-ptr 'pos))
    (draw-list-row List Depth X Y)))


(de draw-list-row (List Depth X-Root)

  (let ((X Y) (get *vert-ptr 'pos))
    (draw-list-x List Depth X Y)))


#################################

# These assume caller has drawn dot so X/Y-Root is rel to dot

# Pass Root as a coord

(de draw-list-y (List Depth X-Root)

  (let Rem List
  
    (while (pair Rem)
      
      # Only draw cons if not NIL and not a LST/Cons
      # Aka this is an improper list
      (if (not (lst? (cdr Rem)))
          (draw-pair Rem Depth X-Root)
          (draw-any (car Rem) Depth X-Root))
      
      (when (setq Rem (cdr Rem))
        (set-x-ptr-2 X-Root)
        (nl-ptr)))))


(de draw-list-x (List Depth X-Root Y-Root)
  
  # Pair vs Lst?
  # (pair NIL) = NIL
  # (lst? NIL) = T

  (let Rem List
  
    (while (pair Rem)
      
      # Only draw cons if not NIL and not a LST/Cons
      # Aka this is an improper list
      (if (not (lst? (cdr Rem)))
          (draw-pair Rem Depth X-Root)
          (draw-any (car Rem) Depth X-Root))
      
      (when (setq Rem (cdr Rem))
        (set-y-ptr-2 Y-Root)
        (adv-ptr)))))


####################

(de draw-atom (Atom)
  (let Col (get-color-type Atom)
    (cond ((num? Atom) (draw-num Atom Col T))
          ((str? Atom) (draw-str Atom Col T))
          (T           (draw-sym Atom Col T)))))
          

(de draw-atom-2 (Atom)

  (draw-chars Atom (get-color-type Atom) T))


(de draw-pair (Pair Depth X-Root)
  #{
    Draw pair as car . cdr

    car . cdr

    . a b c . d . = (a b (c . d) .)

    Or no space?

    . a . b . = ((a . b) .)
    vs
    . a.b .

    -> Still distinguishable - requires only visual-spatial processing instead of
    abstract reasoning

    Syms - colored diff
    Strings - color diff and/or quotes
    List - (a b .) denotes circular list; only valid at end of list; so if . is
             by itself it denotes a circular list; an empty list is NIL or () or ""

      . a
        b
        .
  }#

  # TODO: Pass space flag to draw-any

  (draw-any (car Pair) Depth X-Root)

  (draw-chars (char 183) *col-green T)

  (draw-any (cdr Pair) Depth X-Root)

  T)

################################################################################


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
