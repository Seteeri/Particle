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
    (upd-mdl-mtrx> Vert)
    (cmd-upd-vert *ipc Vert)))


(class +Particle)
(dm T (Data
       A
       B
       Verts)

  (=: data Data)
  (=: a A)
  (=: b B)
  (=: verts Verts))


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


(dm get-origin> ()
  (when (: verts)
    (let (Vert (car (: verts))
          Pos (get Vert 'pos)
          (X Y) (get-origin> (get *metrics (get Vert 'chr))
                             Pos
                             *sca-vert))
      (list X Y (get Pos 3)))))
  

###############################


(de del-particle (Part Car Cdr)

  (let (Sock-Render (ipc~get-fd> *ipc "RENDER")
        Verts (get Part 'verts))
    (for Vert Verts
      (send-msg *ipc
                Sock-Render
                (pack "(zv " (get Vert 'off-gl) '")"))
      (push '*verts Vert)
      (dec '*cnt-v)))
    
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
  
  (if (pair Any)
  
      (if (lst? (cdr Any))
      
          # Specific forms override layout
          (cond ((= (car Any) 'let)
                 (gen-let Any Depth Root Layout))
                (T
                 (Layout Any Depth Root Layout)))
                 
          (Layout Any Depth Root Layout))
      
      (gen-atom Any Depth Root Layout T)))


(de gen-atom (Any Depth Root Layout Adv)

  # Pass flag to draw sym val
  
  (let Part (new '(+Particle)
                  Any
                  NIL
                  NIL
                  (draw-atom Any
                            (get-color-type Any)
                            Adv))

    # Handle internal symbol excl. T - draw the value (CDR)
    # Transient sym's value is itself...        
    (when (and (sym? Any)
               (not (str? Any)) (not (box? Any)) (not (ext? Any))
               (not (= Any NIL)))
      (put Part 'b (gen-sym-int Any Depth Root Layout)))
      
    Part))


(de gen-sym-int (Any Depth Root Layout)
  # Draw CDR
  # Opt: Ignore NIL?
        
  (adv-cursor)
  (gen-part (val Any) Depth Root gen-pair-x-h))


(de draw-cons-x (Ar Dr Ptr-Ar Ptr-Dr)
  (let (Vert-Ar (if (num? Ar)
                    (draw-chars (format Ar) (get-color-type Ar) T)
                    (draw-chars "[" *col-base-0 T)) # (char 187)
        Dot   (list (draw-dot T))
        Vert-Dr (if (num? Dr)
                    (draw-chars (format Dr) (get-color-type Dr) T)
                    (draw-chars "]" *col-base-0 T))) # (char 166)
    (list Vert-Ar Dot Vert-Dr)))


(de gen-pair-x-h (Any Depth Root Layout Car Cdr)
  #{
    Root is baseline of cons
    so for a list Root would be coordinates of first cons
  }#

  (let (Ar (car Any)
        Dr (cdr Any)
        Ptr (>> -4 (adr Any))
        (Ptr-Ar Ptr-Dr) (struct Ptr '(N . 2))
        Tail-Ar (pack (tail 4 (chop (bin Ptr-Ar))))
        Tail-Dr (pack (tail 4 (chop (bin Ptr-Dr))))
        (X Y) (let Pos *pos-cursor
                (if Pos Pos Root)))

    (let ((Vert-Ar Dot Vert-Dr) (draw-cons-x Ar Dr Tail-Ar Tail-Dr)
          X-Cdr (get *pos-cursor 1)
          
          Pa-Ar (when (and (not (num? Ar))
                           (not Car))
                  # Move below, align left of cons
                  (nl-cursor) (set-cur-x X)
                  (gen-part Ar Depth Root gen-pair-x-h))

          Pa-Dr (when (and (not (num? Dr))
                           (not Cdr))
                  # If cursor short then must adv
                  (when (<= (get *pos-cursor 1) X-Cdr)
                    (set-cur-x X-Cdr))
                  (adv-cursor)
                  (set-ptr-y Y)
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
        (X Y) *pos-cursor)

    (let ((Vert-Ar Dot Vert-Dr) (draw-cons-x Ar Dr Tail-Ar Tail-Dr)
          Dummy (adv-cursor)
          Pa-Ar (when (and (not (num? Ar))
                           (not Car))
                  (gen-part Ar Depth Root gen-pair-y-h))
          Pa-Dr (when (and (not (num? Dr))
                           (not Cdr))
                  # For cons pair, use colored dot
                  # Position under like a list
                  (set-cur-x X)
                  (nl-cursor)
                  (gen-part Dr Depth Root gen-pair-y-h)))
      
      (new '(+Particle)
           Any
           Pa-Ar Pa-Dr
           (append Vert-Ar Dot Vert-Dr)))))


(de gen-let (List Depth Root Layout)
  
  # Must determine width of local vars for prg
  
  (Layout List Depth *pos-cursor Layout NIL T)
  
  (adv-cursor)

  # Manually link above to below

  (gen-let-list (if (lst? (get List 2))
                    (nth List 2)
                    (cdr List))
                Depth
                Root
                Layout)
  
  (set-cur-x (car Root))
  (nl-cursor)
  
  # Manually link above to below
  
  (gen-pair-y-h (if (lst? (get List 2))
                    (nth List 3)
                    (nth List 4))
                Depth
                *pos-cursor
                gen-pair-y-h))


(de gen-let-list (List Depth Root Layout)

  # Generate two at a time
  # Pass option to limit gen-any search depth?

  (Layout List Depth *pos-cursor Layout T T)
  
  (let ((X Y) *pos-cursor
        R (car List))

    (while R

      (setq A (car R)
            B (cadr R))

      (let D (+ Depth 1.0)
        (Layout R Depth *pos-cursor Layout NIL T)
        (adv-cursor)
        (Layout (cdr R) Depth *pos-cursor Layout NIL (when (cddr R) T)))

      (when (setq R (cddr R))
        (nl-cursor)
        (set-cur-x X)))))


(de gen-let-var (List Depth Root Layout)

  (Layout List Depth *pos-cursor Layout NIL T)
  (adv-cursor)
  (Layout (cdr List) Depth *pos-cursor Layout NIL T))


################################################################################
################################################################################

  #{
               3 2 1 |
    CONS:      0 0 0 0
    SYM:       1 0 0 0
    BIGNUM:    S 1 0 0
    SHORTNUM:  S 0 1 0

    STR: stored as nums
    NUM: shortnum takes entire PTR, bignum in CAR + CDR ptr
    S= pos:0 neg:1
  
    1. *col-yellow  
    2. *col-orange
    3. *col-red
    
    4. *col-magenta    
    5. *col-violet
    6. *col-blue
    7. *col-cyan
    8. *col-green

    Cell
    * Num - 3
      1 Ptr (builtins) = *col-base-01 or content tone
      2 Big (interpret whole) = *col-green
      3 Short = *col-cyan
    * Sym - 2
      4 Internal - blue
      4 Transient
      4 External
      5 NIL - violet    
    6 Pair/List (Cell) - magenta
      
    * Accents are *Col
  }#

# Draw ptrs
(de draw-cons-ptr (Ptr-Ar Col-Ar Ptr-Dr Col-Dr)
  (list (draw-chars Ptr-Ar Col-Ar T)
        (draw-dot T)
        (draw-chars Ptr-Dr Col-Dr T)))


(de get-col-ptr (Ptr)    

  (let ((B1 B2 B3 B4) (cdr (flip (tail 4 (chop (bin Ptr))))))
    #(println B1 B2 B3 B4)
    (cond ((= "1" B1) *col-cyan)
          ((= "1" B2) *col-green)
          ((= "1" B3) *col-violet)
          ((= "0" B3) *col-magenta)
          (T *col-base-01))))


(de get-type-ptr (Ptr)

  # Built-ins will typically be shorts; bits are arbitrary
   
  (let ((B1 B2 B3 B4) (cdr (flip (tail 4 (chop (bin Ptr))))))
    #(println B1 B2 B3 B4)
    (cond ((= "1" B1) 'short)
          ((= "1" B2) 'big)
          ((= "1" B3) 'sym)
          ((= "0" B3) 'cons)
          (T 'ptr))))


(de gen-cell (Any Type Root)
  (cond ((= Type 'big)   (gen-big Any Root))
        ((= Type 'sym)   (gen-sym Any Root))
        ((= Type 'cons)  (gen-pair Any Root))))


(de gen-ptr-sym (Ptr Root)

  #{
  
    INTERNAL:
    CAR = Cons (property list+name), Short/Big (name)
    CDR = Any
    
    ANONYMOUS:
    CAR = 0 (long = 2)
    CDR = Any
    
  }#

  (let ((Ptr-Ar Ptr-Dr) (struct Ptr '(N . 2))
        Type-Ar         (get-type-ptr Ptr-Ar)
        Type-Dr         (get-type-ptr Ptr-Dr)
        (X Y)           *pos-cursor
        
        blah (println Ptr Ptr-Ar Ptr-Dr)
        blah (println Ptr Type-Ar Type-Dr)

        Col-Ar (cond ((= Ptr-Ar 4380504) *col-yellow)
                     ((= Ptr-Ar 4380648) *col-red)
                     (T (get-col-ptr Ptr-Ar)))
        Col-Dr (cond ((= Ptr-Dr 4380504) *col-yellow)
                     ((= Ptr-Dr 4380648) *col-red)
                     (T (get-col-ptr Ptr-Dr)))
        (Vert-Ar Dot Vert-Dr) (draw-cons-ptr (pack (tail 4 (chop (bin Ptr-Ar)))) Col-Ar
                                             (pack (tail 4 (chop (bin Ptr-Dr)))) Col-Dr)

        X-Cdr (get *pos-cursor 1)

        Pa-Ar (unless (or (= Ptr-Ar 4380504)
                          (= Ptr-Ar 4380648))
                # Move below, align left of cons
                (nl-cursor) (set-cur-x X)    
                (gen-cell (cond ((= Type-Ar 'sym) (- Ptr-Ar 8))
                                ((= Type-Ar 'big) (- Ptr-Ar 4))
                                (T Ptr-Ar))
                          Type-Ar
                          Root))
                          
        Pa-Dr (unless (or (= Ptr-Dr 4380504)
                          (= Ptr-Dr 4380648)
                          (= Ptr-Dr (+ Ptr 8)))
                 # If cursor short then must adv
                 (when (<= (get *pos-cursor 1) X-Cdr)
                   (set-cur-x X-Cdr))
                 (adv-cursor)
                 (set-ptr-y Y)
                 (gen-cell (cond ((= Type-Dr 'sym) (- Ptr-Dr 8))
                                 ((= Type-Dr 'big) (- Ptr-Dr 4))
                                 (T Ptr-Dr))
                            Type-Dr
                            Root)))
    (new '(+Particle)
         Ptr
         Pa-Ar Pa-Dr
         (append Vert-Ar Dot Vert-Dr))))


(de gen-ptr-big (Ptr Root)
  
  (println 'gen-big)

  (let ((Ptr-Ar Ptr-Dr) (struct Ptr '(N . 2))
        Type-Ar         'ptr # always
        Type-Dr         (get-type-ptr Ptr-Dr) # bignum or short
        (X Y)           *pos-cursor

        blah (println Ptr Ptr-Ar Ptr-Dr)
        blah (println Ptr Type-Ar Type-Dr)
        
        Col-Ar *col-base-01
        Col-Dr (cond ((= Ptr-Dr 4380504) *col-yellow)
                     ((= Ptr-Dr 4380648) *col-red)
                     (T (get-col-ptr Ptr-Dr)))
        (Vert-Ar Dot Vert-Dr) (draw-cons-ptr (pack (tail 4 (chop (bin Ptr-Ar)))) Col-Ar
                                             (pack (tail 4 (chop (bin Ptr-Dr)))) Col-Dr)

        X-Cdr (get *pos-cursor 1)

        # Never draw...unless wanted...
        Pa-Ar NIL
                          
        Pa-Dr (unless (or (= Ptr-Dr 4380504)
                          (= Ptr-Dr 4380648))
                 # If cursor short then must adv
                 (when (<= (get *pos-cursor 1) X-Cdr)
                   (set-cur-x X-Cdr))
                 (adv-cursor)
                 (set-ptr-y Y)
                 (gen-cell (cond ((= Type-Dr 'sym) (- Ptr-Dr 8))
                                 ((= Type-Dr 'big) (- Ptr-Dr 4))
                                 (T Ptr-Dr))
                            Type-Dr
                            Root)))
    (new '(+Particle)
         Ptr
         Pa-Ar Pa-Dr
         (append Vert-Ar Dot Vert-Dr))))


(de gen-ptr-pair (Ptr Root)
  #{
  }#

  (println 'gen-pair)

  (let ((Ptr-Ar Ptr-Dr) (struct Ptr '(N . 2))
        Type-Ar         (get-type-ptr Ptr-Ar)
        Type-Dr         (get-type-ptr Ptr-Dr)
        (X Y)           *pos-cursor
        
        blah (println Ptr Ptr-Ar Ptr-Dr)
        blah (println Ptr (bin Ptr-Ar) (bin Ptr-Dr))        
        blah (println Ptr Type-Ar Type-Dr)

        Col-Ar (cond ((= Ptr-Ar 4380504) *col-yellow)
                     ((= Ptr-Ar 4380648) *col-red)
                     (T (get-col-ptr Ptr-Ar)))
        Col-Dr (cond ((= Ptr-Dr 4380504) *col-yellow)
                     ((= Ptr-Dr 4380648) *col-red)
                     (T (get-col-ptr Ptr-Dr)))
        (Vert-Ar Dot Vert-Dr) (draw-cons-ptr (pack (tail 4 (chop (bin Ptr-Ar)))) Col-Ar
                                             (pack (tail 4 (chop (bin Ptr-Dr)))) Col-Dr)

        X-Cdr (get *pos-cursor 1)

        Pa-Ar (unless (or (= Ptr-Ar 4380504)
                          (= Ptr-Ar 4380648))
                # Move below, align left of cons
                (nl-cursor) (set-cur-x X)
                (gen-cell (cond ((= Type-Ar 'sym) (- Ptr-Ar 8))
                                ((= Type-Ar 'big) (- Ptr-Ar 4))
                                (T Ptr-Ar))
                          Type-Ar
                          Root))
                          
        Pa-Dr (unless (or (= Ptr-Dr 4380504)
                          (= Ptr-Dr 4380648))
                 # If cursor short then must adv
                 (when (<= (get *pos-cursor 1) X-Cdr)
                   (set-cur-x X-Cdr))
                 (adv-cursor)
                 (set-ptr-y Y)
                 (gen-cell (cond ((= Type-Dr 'sym) (- Ptr-Dr 8))
                                 ((= Type-Dr 'big) (- Ptr-Dr 4))
                                 (T Ptr-Dr))
                            Type-Dr
                            Root)))
    (new '(+Particle)
         Ptr
         Pa-Ar Pa-Dr
         (append Vert-Ar Dot Vert-Dr))))
