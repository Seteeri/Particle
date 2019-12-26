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

  PARTICLE CAR/CDR = list of particles or particle

  0
  .   P(P)     / P(a b)
  .   P(.a .b) / NIL

  1
  .a  (a b) / P(b)
  .b  b (b) / NIL  [or B itself]

  2
  a   a / a
  b   b / b

  Problem: Cannot differentiate symbol from pair except through ptr encoding
  -> WAIT! Sym? and other functions use pointer encoding...

  Hmm, poss later implement traversal in asm/picoasm

  TODO: Rewrite this using pointer traversal and combine lessons from first
  iteration and second iteration. Every cell encountered, create a particle
  and have it reference the cell. Adr can be used for ptr/num <-> sym/pair. Num
  themselves can be determined by ptr encoding.

  Unlike first iteration, particle has CAR/CDR so it mimics the cons cell/pair
  but also possesses other capabilities. Ideally, these would be implemented
  directly into the interpreter, but hoisting the structure/functionality
  above the interpreter (i.e. at the lisp level) trades accessibility with
  performance.

  Particle itself is a symbol so still using symbolic computation.

  #############################################

  To get the value is to get the car of the car

  Parts reference data so deleting parts will del the ref data and thus it can
  be GC so if the visual representation is gone, so will the data

  The visual representation must have the same structure as the underlying
  data, in other words, the underlying data are cons cells aka linked lists, so
  the visual representation has the same props plus additional props since they
  are compound cells (instances of a class)

  The visual interface is still composed of the underlying data and has the
  same structure so it is homoiconic.

  ###################################

  Create different display levels:

  - Expand lists
  - Expand symbols and numbers

  Default is to expand all of them

  List of chars/nums: draw horizontally (check first item)
  List of defined forms: draw specifically
  List of any: draw two columns (assumes fn calls)

}#


(de decode-ptr  (Any) (>> -4 (adr Any)))
(de unpack-ptrs (Any) (struct (decode-ptr Any) '((B . 8) (B . 8))))
(de list? (Any) (and (lst? Any) (not (= Any NIL))))


(de change-col-particle (Part Col)
  (for Vert (get Part 'verts)
    (with Vert
      (=: rgba (list (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0
                     (get Col 1) (get Col 2) (get Col 3) 1.0)))
    (update-model-matrix> Vert)
    (cmd-update-vert *ipc Vert)))


(class +Particle)
(dm T (Data
       A
       B
       Verts)

  (=: data Data)
  (=: a A)
  (=: b B)
  (=: verts Verts)

  (=: layout NIL)
  (=: end    NIL))


(dm cons> (B)
  # Cons B unto A (this)
  (=: b B))


(dm last-part> ()
  # Make nth version also:
  # (nth> *part-ptr 1)
  (let Last (: b)
    (loop
      #(with Last (println Last (: data) (: a) (: b)))
      (NIL (get (get Last 'b) 'data))
      (setq Last (get Last 'b)))
    # Should return itself?
    Last))


(dm last-part-2> ()
  (let Last (: b)
    (loop
      (with Last (println Last (: data) (: a) (: b)))
      (NIL (get (get (get Last 'b) 'b) 'data))
      (setq Last (get Last 'b)))
    Last))


(de del-particle (Part Car Cdr)

  (let (Sock-Render (ipc~get-fd> *ipc "RENDER")
        Verts (get Part 'verts))
    (for Vert Verts
      (send-msg *ipc
                Sock-Render
                (pack "(zv " (get Vert 'off-gl) '")"))
      (push '*verts Vert)
      (dec '*cnt-v))

    # Push ptr back n vertices
    #(adv-ptr (num-to-fl (- (length Verts))))
    )
    
  (with Part
    (when (and Car (: a))
      (del-particle (: a) Car Cdr))
    (when (and Cdr (: b))
      (del-particle (: b) Car Cdr))))


(de gen-part (Any Depth Root Layout Car Cdr)
  # TODO:
  # Handle circular lists - done?
  # Add hooks for special layout depending on car of list
  
  # Maybe layout particles after gen?
  
  # Root is supposed to be root position of parent list
  
  (if (pair Any)
      (if (lst? (cdr Any))
          # Specific forms override layout
          (cond ((= (car Any) 'let)
                 (gen-let Any Depth (get *vert-ptr 'pos) Layout))
                (T
                 (Layout Any Depth (get *vert-ptr 'pos) Layout)))
          (Layout Any Depth (get *vert-ptr 'pos) Layout))
      (gen-atom Any T)))


(de gen-atom (Any Adv)

  # If full expansion wanted, go further in gen-num/str/sym etc.

  (let (Col (get-color-type Any)
        Verts (cond ((num? Any)  (draw-num Any Col Adv))
                    ((str? Any)  (draw-str Any Col Adv))
                    (T           (draw-sym Any Col Adv))))
    (new '(+Particle)
          Any
          NIL
          NIL
          Verts)))


#{

  Default to X layout because of strings
  - Can we mix layouts? Will it confuse the user?
  
  CAR can be either dir; top-bottom, left-right
  
  Place dot middle when vertical?
  -> No, destroys grid layout
  
  X:

    CAR·CDR -> CAR·CDR -> NIL
    |          |
    1          2

  Y:
  
    CAR·CDR - 1
        |
    +---+
    |
    CAR·CDR - 2
        |
    +---+
    |
    NIL
    
  Finally layouts can be defined for specific forms, e.g. let, if, cond, etc.
  
}#


(de draw-cons-x (Ar Dr Ptr-Ar Ptr-Dr)
  (let (Vert-Ar (if (num? Ar)
                    (draw-chars (format Ar) (get-color-type Ar) T)
                    (draw-chars "[" *col-base-0 T)) # (char 187)
        Dot   (if (num? Dr)
                  (draw-chars (char 183) (get-color-type Any) T)
                  (draw-dot T))
        Vert-Dr (if (num? Dr)
                    (draw-chars (format Dr) (get-color-type Dr) T)
                    (draw-chars "]" *col-base-0 T))) # (char 166)
    (list Vert-Ar Dot Vert-Dr)))


(de gen-pair-x-h (Any Depth Root Layout Car Cdr)

  # Draw-cons
  # Draw-ptrs

  (let (Ar (car Any)
        Dr (cdr Any)
        Ptr (>> -4 (adr Any))
        (Ptr-Ar Ptr-Dr) (struct Ptr '(N . 2))
        Tail-Ar (pack (tail 4 (chop (bin Ptr-Ar))))
        Tail-Dr (pack (tail 4 (chop (bin Ptr-Dr))))
        (X Y) (get *vert-ptr 'pos))

    (let ((Vert-Ar Dot Vert-Dr) (draw-cons-x Ar Dr Tail-Ar Tail-Dr)
          X-Cdr (get (get *vert-ptr 'pos) 1)
          
          Pa-Ar (when (and (not (num? Ar))
                           (not Car))
                  # Move underneath
                  (nl-ptr)
                  (set-x-ptr-2 X)
                  (gen-part Ar Depth Root gen-pair-x-h))

          Pa-Dr (when (and (not (num? Dr))
                           (not Cdr))
                  # If cursor short then must adv
                  (when (<= (get (get *vert-ptr 'pos) 1) X-Cdr)
                    (set-x-ptr-2 X-Cdr))
                  (adv-ptr)
                  (set-y-ptr-2 Y)
                  (gen-part Dr Depth Root gen-pair-x-h)))
      
      (new '(+Particle)
           Any
           Pa-Ar Pa-Dr
           (append Vert-Ar Dot Vert-Dr)))))
           

(de gen-pair-y-h (Any Depth Root Layout Car Cdr)

  # TODO:
  # Offset cells properly

  (let (Ar (car Any)
        Dr (cdr Any)
        Ptr (>> -4 (adr Any))
        (Ptr-Ar Ptr-Dr) (struct Ptr '(N . 2))
        Tail-Ar (pack (tail 4 (chop (bin Ptr-Ar))))
        Tail-Dr (pack (tail 4 (chop (bin Ptr-Dr))))
        (X Y) (get *vert-ptr 'pos))

    (let ((Vert-Ar Dot Vert-Dr) (draw-cons-x Ar Dr Tail-Ar Tail-Dr)
          Dummy (adv-ptr)
          Pa-Ar (when (and (not (num? Ar))
                           (not Car))
                  (gen-part Ar Depth Root gen-pair-y-h))
          Pa-Dr (when (and (not (num? Dr))
                           (not Cdr))
                  # For cons pair, use colored dot
                  # Position under like a list
                  (set-x-ptr-2 X)
                  (nl-ptr)
                  (gen-part Dr Depth Root gen-pair-y-h)))
      
      (new '(+Particle)
           Any
           Pa-Ar Pa-Dr
           (append Vert-Ar Dot Vert-Dr)))))


(de gen-let (List Depth Root Layout)
  
  # Must determine width of local vars for prg
  
  (Layout List Depth (get *vert-ptr 'pos) Layout NIL T)
  
  (adv-ptr)

  # Manually link above to below

  (gen-let-list (if (lst? (get List 2))
                    (nth List 2)
                    (cdr List))
                Depth
                Root
                Layout)
  
  (set-x-ptr-2 (car Root))
  (nl-ptr)
  
  # Manually link above to below
  
  (gen-pair-y-h (if (lst? (get List 2))
                    (nth List 3)
                    (nth List 4))
                Depth
                (get *vert-ptr 'pos)
                gen-pair-y-h))


(de gen-let-list (List Depth Root Layout)

  # Generate two at a time
  # Pass option to limit gen-any search depth?

  (Layout List Depth (get *vert-ptr 'pos) Layout T T)
  
  (let ((X Y) (get *vert-ptr 'pos)
        R (car List))

    (while R

      (setq A (car R)
            B (cadr R))

      (let D (+ Depth 1.0)
        (Layout R Depth (get *vert-ptr 'pos) Layout NIL T)
        (adv-ptr)
        (Layout (cdr R) Depth (get *vert-ptr 'pos) Layout NIL (when (cddr R) T)))

      (when (setq R (cddr R))
        (nl-ptr)
        (set-x-ptr-2 X)))))


(de gen-let-var (List Depth Root Layout)

  (Layout List Depth (get *vert-ptr 'pos) Layout NIL T)
  (adv-ptr)
  (Layout (cdr List) Depth (get *vert-ptr 'pos) Layout NIL T))


################################################################################
################################################################################


(de gen-list-col2 (List Depth X-Root Y-Root)

  # Link particles
  # List = (P 1 2 3 4)
  # Parts + Out = (4 3 2) + (1 P)
  # Out = (4 3 2 1 P)
  # (flip Out) = (P 1 2 3 4)

  (let (Rem List
        Out ()
        Prev NIL)

    (let? Any (car List)
      (let Part (gen-part Any Depth X-Root)
        (push 'Out Part)
        (setq Prev Part))
      (adv-ptr))

    (let ((X Y) (get *vert-ptr 'pos))

      (let? Any (cadr List)
        (let Part (gen-part Any Depth X-Root)
          (push 'Out Part)
          (cons> Prev Part)
          (setq Prev Part)))

      (let? Rem (nth List 3)
        (set-x-ptr-2 X)
        (nl-ptr)
        (let Parts (gen-list-y Rem Depth X)
          (cons> (car Out) (last Parts))
          (setq Out (append Parts Out)))))

    (flip Out)))


(de gen-list-line (List Depth X-Root Y-Root)

  # Pass roots as single pair

  (let (Rem List
        Out ()
        Prev NIL)

    (while (pair Rem)

      (let Part (if (lst? (cdr Rem))
                    (gen-part (car Rem) Depth X-Root)
                    (gen-pair Rem Depth X-Root))
        (push 'Out Part)
        (when Prev (cons> Prev Part))
        (setq Prev Part))

      (when (setq Rem (cdr Rem))
        (fn X-Root Y-Root)))

    Out))


(de gen-part-2 (Any Depth X-Root)

  #{
    CONS:      ...0000
    SYM:       ...1000
    BIGNUM:    ...S100
    SHORTNUM:  ...S010

    STR: stored as nums, shortnum takes entire PTR, bignum in CAR + CDR ptr

    S= pos:0 neg:1
  }#

  #(setq Any (1 2 3))

  (let (Ptr (>> -4 (adr Any))
        (Ptr-Car Ptr-Cdr) (struct Ptr '(N . 2))
        Bin-Ptr             (bin Ptr)
        Tail-Ptr            (pack (tail 4 (chop Bin-Ptr)))
        Bin-Car             (bin Ptr-Car)
        Bin-Cdr             (bin Ptr-Cdr)
        Tail-Car            (pack (tail 4 (chop Bin-Car)))
        Tail-Cdr            (pack (tail 4 (chop Bin-Cdr))))

    (println Bin-Car Bin-Cdr)

    # Draw this cell, then call fn depending on contents

    (let (Car   (draw-chars (format Ptr-Car) *col-base-0 T)
          #Dummy (adv-ptr)
          Dot   (draw-chars "." *col-base-0 T)
          #Dummy (adv-ptr)
          Cdr   (draw-chars (format Ptr-Cdr) *col-base-0 T))
      (adv-ptr)
      (new '(+Particle)
           Any
           Car
           Cdr
           Dot))

    (cond ((and (= Tail-Ptr "0000") (cdr Any))
           # Either a cons cell or list (if CDR is another cons)
           (println "(gen-part-2)" (cdr Any))
           (gen-part-2 (cdr Any) (+1 Depth) X-Root))

          ((= Tail-Ptr "1000")
           # Includes sym and str
           # Need not pursue further
           (cond ((= Ptr Ptr-Cdr)   (println "(draw-str)"))
                 (T)))

          (T
           (let Chop (chop Tail-Ptr)
             # Need not pursue further
             (cond ((= (pack (tail 3 Chop)) "100")
                    (println "(draw-big)"))

                   ((= (pack (tail 2 Chop)) "10")
                    (println "(draw-short)"))))))))

