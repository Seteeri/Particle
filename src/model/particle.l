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


(class +Particle)
(dm T (Data
       A
       B
       Verts)

  (=: a A)
  (=: b B)
  (=: verts Verts)

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
  
  # author
  (=: aut)
  
  )


(dm cons> (B) (=: b B))
(dm val> () (: b))

(dm last> ()
  
  (let Last (: b)
  
    (loop
    
      (NIL (get Last 'b))
      (println "last>" Last (get Last 'data))
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

  # TODO:
  # * Pass flag whether to move CAR/CDR
  # * Move to another Particle, specify optional adv/nl

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


(dm mov-part> (B Off)
  # TODO:
  # Implement offset either absolute units or adv
  (mov> This (get-origin> B)))
  

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


###############################


(de make-part-list-atom (Any)
  #{
    Procedure:
    1. Get val of p0
      * Initially Val is *part-main
      * So Val is either symbol itself or second to last (before NIL)
        which in either case, the cdr will be NIL      
    2. Get Last (always NIL) and Last-2
    3. Mov cursor to NIL
    4. Generate new particle
    5. Connect new particle to Last and Last-2
       Last-2 -> New -> Last (NIL)
  }#

  # Get value of p0
  # Val is Last-1; Nil is Last
  (let (Val (val> p0)
        Nil (get Val 'b))
  
    #(println "Val'data" (get Val 'data) (adr Val))
    #(println "Nil'data" (get Nil 'data) (adr Nil))
  
    # Mov cursor to NIL first vert pos
    (mov-cursor (car (get Nil 'verts)) T)

    (let Lst-Any (list Any)

      # Gen part for new data; incl CAR/CDR
      (let Part (gen-pair-x-h Lst-Any
                              0.0
                              *pos-cursor
                              gen-pair-x-h
                              NIL T)

        # Connect NIL
        (with Part (=: b Nil))
        
        # Reposition NIL in front of new Part
        (mov-cursor (last (get Part 'verts)) T) (adv-cursor 2.0) (mov> Nil *pos-cursor)

        # Connect prev last to new Particle
        (with Val (=: b Part))
        
        # Point to new Part
        (put p0 'b Part)
        # TODO: Position p0 over new item
        (nl-cursor -1.0) (adv-cursor -4.0) (mov> p0 *pos-cursor))

      # Currently assume this list doesn't change
      # But upon list change, must update that specific list
      (with *part-main
        (=: data (conc (: data) Lst-Any))))))


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
      
      (gen-atom Any Depth Root Layout T Car Cdr)))


(de gen-atom (Any Depth Root Layout Adv Car Cdr)

  # Pass flag to draw sym val
  
  # Handle internal symbol excl. T - draw the value (CDR)
  # Transient sym's value is itself...

  # Must do after so drawn after
  
  (let (V (draw-atom Any
                     (get-color-type Any)
                     Adv)
        B (when (and (not Cdr) (sym-int? Any))
            (gen-sym-int Any Depth Root Layout)))
    (new '(+Particle)
         Any
         NIL
         B
         V)))


(de gen-sym-int (Any Depth Root Layout)
  # Opt: Ignore NIL?
        
  (adv-cursor)
  (gen-part (val Any) Depth Root Layout))


(de draw-cons-x (Ar Dr Ptr-Ar Ptr-Dr)
  (let (Vert-Ar (if (num? Ar)
                    (draw-chars (format Ar) (get-color-type Ar) T)
                    (draw-chars "[" *col-base-0 T)) # (char 187)
        Dot   (list (draw-dot T))
        Vert-Dr (if (num? Dr)
                    (draw-chars (format Dr) (get-color-type Dr) T)
                    (draw-chars "]" *col-base-0 T))) # (char 166)
    (list Vert-Ar Dot Vert-Dr)))


(de gen-pair-x-h (Any Depth Root Layout Ignore-Car Ignore-Cdr)
  #{
    Root is baseline of cons
    so for a list Root would be coordinates of first cons
  }#

  # TODO: REFACTOR CAR/CDR

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
                           (not Ignore-Car))
                  # Move below, align left of cons
                  (nl-cursor) (set-cur-x X)
                  (gen-part Ar Depth Root gen-pair-x-h))

          Pa-Dr (when (and (not (num? Dr))
                           (not Ignore-Cdr))
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
