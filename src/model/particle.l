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


(de draw-any (Any Depth)

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
                          (cond 
                            ((= (car Any) 'let)
                              (draw-let Any
                                        Depth
                                        (get (get *vert-ptr 'pos) 1)))
                            (T
                              (draw-list-dot Any
                                             Depth
                                             (get (get *vert-ptr 'pos) 1))))
                          (draw-pair Any)))
        (T            (draw-atom Any T))))


(de draw-list-dot (List Depth X-Root)

  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr)
  
  (draw-list List (+ Depth 1.0) (get (get *vert-ptr 'pos) 1)))

      
(de draw-list (List Depth X-Root)

  (inc 'Depth 1.0)

  (for Any List
    (set-x-ptr-2 X-Root)
    # Default draw vertically
    (nl-ptr)
    (draw-any Any Depth X-Root)))


(de draw-atom (Any) (draw-chars Any (get-color-type Any) T))
(de draw-pair (Any) T)


(de draw-let (List Depth X-Root)

  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr)
  
  (draw-atom (car List))
  (adv-ptr)

  # If second atom is a list:
  #   draw pairs
  #   draw two atoms
  
  # Must determine width of local vars for prg
  
  (if (lst? (get List 2))
      (draw-let-f1 List Depth)
      (draw-let-f2 List Depth X-Root)))


(de draw-let-f1 (List Depth)
  (let ((X Y) (get *vert-ptr 'pos))
    # Loop two at a time:
    (let R (get List 2)
      (until (not R)
        (setq A (car R))
        (setq B (cadr R))
        (setq R (cddr R))
        (draw-any A (+ Depth 1.0)) (adv-ptr)
        (draw-any B (+ Depth 1.0))
        (when R
          (nl-ptr)
          (set-x-ptr-2 X))))

    (set-y-ptr-2 Y)
    # Draw list will produce newline before drawing
    (nl-ptr -1.0)
    (adv-ptr 6.0)
    (draw-list (nth List 3) (+ Depth 1.0) (get (get *vert-ptr 'pos) 1))))


(de draw-let-f2 (List Depth X-Root)
  # Same as above...except drawing only 1 pair
  (draw-any (get List 2)) # a
  (adv-ptr)
  (draw-any (get List 3)) # (+ 1 2)
  (adv-ptr)
          
  # Newline, Return feed, Indent two spaces
  (set-x-ptr-2 (+ X-Root (*/ Depth *adv-vert 1.0))) (adv-ptr 2.0)
  
  # Remainder can be either list or atom
  # NTH/CDR will return a list although it is not the start of a list but
  # the continuation/remainder of one
  (draw-list (nth List 4) (+ Depth 1.0) (get (get *vert-ptr 'pos) 1)))


(de draw-cond (Item Depth) T)

  #{
    Layout depends on data structure...
  
    Symbol Functions:
    * de - first three items on same line
    * let - 1st/2nd item on same line; 2nd item is a list or atom...
    * prog/with - cdr on different lines
    
    Control Flow:
    * if/when/unless
    * loop/for/while/do
    * cond/case - like prog...
    * and/or/not - separate line for each
    
    Function Calls:
    * prin* - vertically align arguments after string
    * fn - (+ 1 2 3) = (+ 1
                          2
                          3)
      * poss add option to split after specific length like 2-3 etc
    
    Other:
    * Strings - newline or line break
    
    Default to drawing horizontally for lists
    
    Specific formatting for specific structures allow for faster
    readability, in other words, fast identification of blocks of code
    or in this case, lists
  }#

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
