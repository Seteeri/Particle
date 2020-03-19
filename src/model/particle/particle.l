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

(de sym-int? (Any)
  (and (sym? Any)
       (not (str? Any)) (not (box? Any)) (not (ext? Any))
       (not (= Any NIL))))

# Call this Cell instead?
(class +Particle)
(dm T (Data
       A
       B
       Verts)
  
  # Store as cons?
  # (=: ab (a . b))
  (=: a A)
  (=: b B)
  
  (=: verts Verts)
  (=: ori)

  # for list, track length
  # (cons Data Length=Num/NIL)
  (=: data Data)
  
  # list of tags; always str; order matters like when searching
  (=: tg)
  
  # when linking, def show date, tags, or show nm or raw location
  (=: nm)
  
  # date/time
  (=: dat (date))
  (=: tim (time))
  
  (=: lay 'y))


(dm cons> (B) (=: b B))

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
              (list (- (get *pos-cursor 1) (get Pos 1))
                    (- (get *pos-cursor 2) (get Pos 2))
                    (- (get *pos-cursor 3) (get Pos 3)))
              P)))


(dm mov-part-abv> (B P)
  # Make offset version instead of static function?
  
  (mov-cur-vert (car (get B 'verts)) T)
  (nl-cur -1.0)

  (let Pos (get-origin> This)
    (mov-off> This
              (list (- (get *pos-cursor 1) (get Pos 1))
                    (- (get *pos-cursor 2) (get Pos 2))
                    (- (get *pos-cursor 3) (get Pos 3)))
              P)))


(dm mov-part-bel> (B P)
  # Make offset version instead of static function?
  
  (mov-cur-vert (car (get B 'verts)) T)
  (nl-cur)

  (let Pos (get-origin> This)
    (mov-off> This
              (list (- (get *pos-cursor 1) (get Pos 1))
                    (- (get *pos-cursor 2) (get Pos 2))
                    (- (get *pos-cursor 3) (get Pos 3)))
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
    
  (when (and Car (: a)) (del> (: a) Car Cdr))
  (when (and Cdr (: b)) (del> (: b) Car Cdr)))


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
  (for Vert (: verts)
    (with Vert
      (let ((R G B) Col)
        (=: rgba (list R G B 1.0
                       R G B 1.0
                       R G B 1.0
                       R G B 1.0))))
    (upd-mdl-mtrx> Vert)
    (cmd-upd-vert *ipc Vert)))


(dm last-vert-pos> (Cnt)
  (get (get (last (: verts)) 'pos) Cnt))

(dm first-vert> ()
  (car (: verts)))
  
(dm last-vert> ()
  (last (: verts)))
  
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

###############################

# List fn

(de con-list (Any)
  (let (Lst-Any (list Any)
        Val (val> p0)
        Cdr (get Val 'b)
        Part (gen-part Lst-Any
                       0.0
                       *pos-cursor
                       NIL T))

    # Set cdr of cur-part to new-part
    # [c] -> [n]
    (put Val 'b Part)
    
    # Set cdr of new-part to cdr-part
    # [n] -> [b] -> ...
    (put Part 'b Cdr)
    
    # Connect new part data to list part data
    # Need separate function if not at end
    (with *part-main
      (=: data (conc (: data) Lst-Any)))
      
    Part))


(de repl-part (Any Pre)
  #{
        p0
  [] -> [] -> NIL
  a     NIL

        p0
  []  
  
        p0
  [] -> [] -> NIL
  a     |
        b
  
  }#

  # Replace NIL with List
    
  # Delete particle first
  (del> (val> p0) T T)
  (put Pre 'b) # data still there
  (put p0 'b)

  # Set cursor 2 spaces above ptr
  (nl-cur -2.0)

  (let (Lst-Any (list (list Any))
        Part (gen-part Lst-Any
                       0.0
                       *pos-cursor))

    (with Pre
      (=: b Part)
      (con (car (:: data)) Lst-Any))
        
    # Set new list
    (setq *part-main (get Part 'a))
      
    Part))


###############################


(de gen-part (Any Depth Root Car Cdr)
  # TODO: Handle circular lists...use another dot at the end

  (if (pair Any)
  
      (if (lst? (cdr Any))
      
          # Handle specific forms here
          (cond (T
                 (gen-pair Any Depth Root Car Cdr)))
                 
          (gen-pair Any Depth Root Car Cdr))
      
      (gen-atom Any Depth Root T Car Cdr)))


(de gen-atom (Any Depth Root Adv Car Cdr)

  # Pass flag to draw sym val
  
  # Handle internal symbol excl. T - draw the value (CDR)
  # Transient sym's value is itself...

  # Must do after so drawn after
  
  (let (B (when (and (not Cdr)
                     (sym-int? Any))
            (gen-sym-int Any Depth Root Car Cdr)))
            
    (new (if (num? Any)
             '(+Num)
             (if (str? Any)
                 '(+Str)
                 '(+Sym)))
         Any
         NIL
         B
         (draw-atom Any
                     (get-color-type Any)
                     Adv))))


(de gen-sym-int (Any Depth Root Car Cdr)
  # Opt: Ignore NIL?

  (gen-part (val Any) Depth Root Car Cdr))


(de gen-pair (Any Depth Root Ignore-Car Ignore-Cdr)
  #{
    Root is baseline of cons
    so for a list Root would be coordinates of first cons
  }#

  # TODO: REFACTOR CAR/CDR - FLIP FLAGS?

  (let (Ar (car Any)
        Dr (cdr Any)
        (X Y) (let Pos *pos-cursor
                (if Pos Pos Root)))

    (let ((Vert-Ar Dot Vert-Dr) (draw-cell Ar Dr)
          X-Cdr                 (get *pos-cursor 1)
          Part (new '(+Pair)
                     Any
                    (unless Ignore-Car (gen-part Ar Depth Root))
                    (unless Ignore-Cdr (gen-part Dr Depth Root))
                    (append Vert-Ar Dot Vert-Dr)))
      #{
        Skip the list containing the list so:
          a
          . 1 2 3
          c
      }#
      (when (num? Ar)
        (lay-num-ds Part 'x))

      #(when (str? Ar)
      #  (lay-str-ds Part 'x))
      
      Part)))


(de lay-str-ds (Part Lay)
  # If nested lists, must keep checking until non-pair/list
  
  (with Part
    
    (let (P-Typ (type Part)
          A-Typ (type (: a))
          B-Typ (type (: b)))
  
      (when (and (= P-Typ '(+Pair))
                 (or (= A-Typ '(+Pair))
                     (= A-Typ '(+Str))))
        
        (=: lay Lay)
        (put (: a) 'lay Lay))
      
      (when (= A-Typ '(+Pair)) (lay-num-ds (: a) Lay))
      (when (= B-Typ '(+Pair)) (lay-num-ds (: b) Lay)))))


(de lay-num-ds (Part Lay)
  # If nested lists, must keep checking until non-pair/list
  #
  # Move to caller fn or gen-part...
  
  (with Part
    
    (let (P-Typ (type Part)
          A-Typ (type (: a))
          B-Typ (type (: b)))
  
      (when (and (= P-Typ '(+Pair))
                 (or (= A-Typ '(+Pair))
                     (= A-Typ '(+Num))))
        
        (=: lay Lay)
        (put (: a) 'lay Lay))
      
      (when (= A-Typ '(+Pair)) (lay-num-ds (: a) Lay))
      (when (= B-Typ '(+Pair)) (lay-num-ds (: b) Lay)))))
