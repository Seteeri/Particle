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

(def '*space-line (+ *adv-vert *adv-vert))

(de decode-ptr  (Any) (>> -4 (adr Any)))
(de unpack-ptrs (Any) (struct (decode-ptr Any) '((B . 8) (B . 8))))
(de list? (Any) (and (lst? Any) (not (= Any NIL))))

(class +Particle)
(dm T (Data
       Next
       Verts)

  (=: data Data)
  (=: next Next)
  (=: verts Verts))


(de del-particle (Part)
  # Pass adv?

  (let (Sock-Render (ipc~get-fd> *ipc "RENDER")
        Verts (get Part 'verts))
    (for Vert Verts
      (send-msg *ipc
                Sock-Render
                (pack "(zv " (get Vert 'off-gl) '")"))
      (push '*verts Vert))

    # Push ptr back n vertices
    (adv-ptr (num-to-fl (- (length Verts)))))

  # Only chase ptrs (recurse) if list
  (when (and (lst? (get Part 'data))
             (not (= NIL (get Part 'data))))
    (let? Next (get Part 'next)
      (del-particle Next))))


(de draw-any (Any Depth X-Root)

  #{
    * Use box-ptr diagrams

    * Lists/cons are fully expanded, however atoms (num/syms) are condensed
      * NIL
        * Both simultaneously a list/symbol
        * Data structure is 4 cons cells
        * Also condensed -> NIL or #### (ptr)

    * Lists:   expanded
    * Symbols: condensed, draw CDR (no show property list etc)
    * Nums:    condensed, draw proper num (not chain of cells)

    * To draw sublists in list, must know number of sublists in
    subsequent sublists to calculate the depth (-Y), two pass
      * For multiprocess, when sublist encountered pass to process
  }#

  #{
  (let ((Adr-Car Adr-Cdr) (unpack-ptrs Any))
    (prin "Adr: ") (prinl (decode-ptr Adr))
    (prin "Car: ") (prinl Adr-Car)
    (prin "Cdr: ") (prinl Adr-Cdr))
  }#

  # Determine type of data
  # NIL also a lst so place before to treat NIL as sym

  (cond ((== Any NIL) (draw-sym Any (get-color-type Any) T))
        # If CDR list then list else cons
        ((lst? Any)   (if (lst? (cdr Any))
                          (draw-any-list Any
                                         Depth
                                         X-Root)
                          (draw-pair Any
                                     Depth
                                     X-Root)))
        (T            (draw-atom Any T))))


(de draw-any-list (Any Depth X-Root)
  (cond
    ((= (car Any) 'prog) (draw-prog Any Depth X-Root))
    ((= (car Any) 'cond) (draw-cond-2 Any Depth X-Root))
    ((= (car Any) 'de) (draw-de Any Depth X-Root))
    ((= (car Any) 'let) (draw-let Any Depth X-Root))    
    ((or (= (car Any) 'with)
         (= (car Any) 'when)
         (= (car Any) 'unless)) (draw-with Any Depth X-Root))    
    ((or (= (car Any) '=)
         (= (car Any) '==)
         (= (car Any) 'cons)) (draw-list-row-dot Any Depth X-Root))
    (T (draw-list-col2-dot Any Depth X-Root))))


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
    (while (car Rem)
      
      (draw-any (car Rem) Depth X-Root)
      
      (when (setq Rem (cdr Rem))
        (set-x-ptr-2 X-Root)
        (nl-ptr)))))


(de draw-list-x (List Depth X-Root Y-Root)

  (let Rem List
    (while (car Rem)
      
      (draw-any (car Rem) Depth X-Root)
      
      (when (setq Rem (cdr Rem))
        (set-y-ptr-2 Y-Root)
        (adv-ptr)))))


####################

(de draw-atom (Atom)

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

(de gen-particles (Any)
  #{
    Circ: 1 2 3 .
    Improper: 1 2 3 . 4
  }#

  # NIL also a lst so place before to treat NIL as sym

  (cond ((== Any NIL) (gen-atom Any T))
        ((lst? Any)
          # If CDR list then list else cons
          (if (lst? (cdr Any))
              (gen-list Any)
              (gen-pair Any)))
        (T (gen-atom Any T))))


(de gen-list (Any)
  (if (circ? Any)
      (nil
        (prinl "gen-cells: circ list")
        (gen-atom NIL))

      #{
        * Open parenthesis belongs to first object (in repr opposite)
        * Close parenthesis belongs to last object (in repr opposite)
        * So upon adding a new object, must move parenthesis vertex to new object

        * If one object, then both parenthesis are part of vertices
        * Draw parenthesis into position, then attach to respective
        atom
      }#

      (let (Open  (draw-chars "(" *col-base-0 NIL)
            Parts (gen-list-parts Any)
            Close (draw-chars ")" *col-base-0 T))

          # Attach both parenthesis to first atom
          (if (=1 (length Any))
              T
              T))))

#{
      (let (Open  (draw-chars "(" *col-base-0 NIL)
            Parts (gen-list-parts Any)
            First (car Parts))
        (new '(+Particle)
             Any
             First
             (conc (draw-chars ")" *col-base-0 T) Open)))))
}#


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
        Verts (cond ((== Any NIL) (draw-sym Any Col Adv))
                    ((num? Any)  (draw-num Any Col Adv))
                    ((box? Any)  (draw-sym Any Col Adv))
                    ((str? Any)  (draw-str Any Col Adv))
                    (T           (draw-sym Any Col Adv))))

    (new '(+Particle) Any NIL Verts)))


(de change-col-particle (Part Col)
  (for Vert (get Part 'verts)
    (with Vert
      (=: rgba (list (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0)))
    (update-model-matrix> Vert)
    (cmd-update-vert *ipc Vert)))
