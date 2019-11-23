#{

  (1 2 3)

  Abst:   [1|.]  ->  [2|.]  ->  [3|.]
           ^          ^          ^
          / \        / \        / \
           |          |          |
  Conc:    1    ->    2    ->    3

  Note, conc is pointing to whatever CAR is pointing to
  Numbers would be copied however

  ((a b) (c d) (e f))

          [a|.]...   [c|.]...   [e|.]...
           |          |          |
  Abst:   [.|.]  ->  [.|.]  ->  [.|.]
           ^          ^          ^
          / \        / \        / \
           |          |          |

  Conc:    . a    ->    . c    ->    . e

  Parts reference data so deleting parts will del the ref data and thus it can
  be GC so if the visual representation is gone, so will the data

  However, this means the visual representation must have the same structure
  as the underlying data, in other words, the underlying data are cons cells
  aka linked lists, so the visual representation has the same props plus
  additional props since they are compound cells (instances of a class)

  The visual interface is still composed of the underlying data and has the
  same structure so it is homoiconic.
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
       Next
       Verts)

  (=: data Data)
  (=: next Next)
  (=: verts Verts))

(dm cons> (B)
  # Cons B unto A (this)
  (=: next B))


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


(de gen-part (Any Depth X-Root)

  # TODO: Handle circular lists and improper lists properly

  (if (pair Any)
      (if (lst? (cdr Any))
          (gen-list-any Any
                        Depth
                        X-Root)
          (gen-pair Any
                    Depth
                    X-Root))
      (gen-atom Any T)))


(de gen-list-any (Any Depth X-Root)

  (if (circ? Any)

      (nil
        (prinl "gen-cells: circ list")
        (gen-atom NIL))

      # Draw dot first
      (let (Verts (draw-chars (char 183) *col-base-0 NIL)
            Adv   (adv-ptr 2.0)
            (X Y) (get *vert-ptr 'pos)
            Parts (gen-list-col2 Any Depth X))
        
        (prinl-info "gen-list-any" (pack "Generate particle (list): " Any))

        (for Pt Parts
          (prinl (adr Pt) ":" (get Pt 'data)))
        
        (new '(+Particle)
             Any
             Parts
             Verts)

        )))


(de gen-list-col2 (List Depth X-Root)

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


(de gen-list-y (List Depth X-Root)

  (let (Rem List
        Out ()
        Prev NIL)

    (while (pair Rem)

      (let Part (if (not (lst? (cdr Rem)))
                    (gen-pair Rem Depth X-Root)
                    (gen-part (car Rem) Depth X-Root))
        (push 'Out Part)
        (when Prev (cons> Prev Part))
        (setq Prev Part))

      (when (setq Rem (cdr Rem))
        (set-x-ptr-2 X-Root)
        (nl-ptr)))

    Out))


###############################


(de gen-atom (Any Adv)

  (prinl-info "gen-atom" (pack "Gen particle (atom): " Any))

  (let (Col (get-color-type Any)
        Verts (cond ((num? Any)  (draw-num Any Col Adv))
                    ((str? Any)  (draw-str Any Col Adv))
                    (T           (draw-sym Any Col Adv))))

    (new '(+Particle)
          Any
          NIL
          Verts)))


(de gen-pair (Any Depth X-Root)
  (let (Car   (gen-part (car Any))
        Dummy (adv-ptr)
        Dot   (draw-chars "." (get-color-type Any) T)
        Dummy (adv-ptr)
        Cdr   (gen-part (cdr Any)))
    (new '(+Particle)
         Any
         (cons Car Cdr)
         Dot)))
