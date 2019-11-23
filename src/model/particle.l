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
