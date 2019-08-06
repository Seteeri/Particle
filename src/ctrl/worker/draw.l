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

# Magenta = unused
# Brightest color = least used element


(de get-color-type (Any)
  # Simplify to num, sym, str/sym and pair
  # Diff pair vs lst? Lst ret T, Pair returns Any
  (cond ((num? Any) *col-violet)
        ((str? Any) *col-blue)
        ((sym? Any) *col-cyan)
        ((pair Any) *col-green)
        ((box? Any) (println "BOX TYPE: IMPLEMENT COLOR!") (bye))))

                  
(de read-string ()
  (let (Verts  ()
        Data  ())
    # Pop particles off timeline - ignore ptr (ptr not in timeline)
    (for Prt *timeline
      (push 'Data (get Prt 'data))
      (for Vert (get Prt 'verts)
        (push 'Verts Vert)))

    (list Verts Data)))


(de pop-string ()
  (let (Verts  ()
        Data  ())
    # Pop particles off timeline - ignore ptr (ptr not in timeline)
    (let Prt NIL
      (while (setq Prt (pop '*timeline))
        (push 'Data (get Prt 'data))
        (for Vert (get Prt 'verts)
          (push 'Verts Vert))))

    (list Verts Data)))


(de draw-str (Str Col)

  (let Verts ()

    (for C (chop Str)

      (let Vert (pop '*verts)

        # Set baseline
        (with Vert
          (=: pos (list (get *pen-v 1) (get *pen-v 2) 0.0))
          (=: rgba (list (get Col 1) (get Col 2) (get Col 3) 1.0
                         (get Col 1) (get Col 2) (get Col 3) 1.0
                         (get Col 1) (get Col 2) (get Col 3) 1.0
                         (get Col 1) (get Col 2) (get Col 3) 1.0)))

        (update-glyph> Vert C)

        (update-model-matrix> Vert)

        (cmd-update-vert *ipc Vert)

        (adv-pen 1.0)

        (inc '*cnt-v)

        (push 'Verts Vert)))

      (flip Verts)))


(de draw-bytes (Bytes)

  (for Byte Bytes

    (draw-num Byte)))


# Refactor this into draw-char
(de draw-num (Num Col)

  (let Str (format Num)

    (for N (chop Str)

      (let Vert (pop '*verts)

        # Reset vertex
        # Pos = baseline = X,0,0
        (with Vert
          (=: pos (list (get *pen-v 1) (get *pen-v 2) 0.0))
          (=: rgba (list (get Col 1) (get Col 2) (get Col 3) 1.0
                         (get Col 1) (get Col 2) (get Col 3) 1.0
                         (get Col 1) (get Col 2) (get Col 3) 1.0
                         (get Col 1) (get Col 2) (get Col 3) 1.0)))

        (update-glyph> Vert N)

        (adv-pen 1.0)

        (update-model-matrix> Vert)

        (cmd-update-vert *ipc Vert)

        (inc '*cnt-v 1)))

      # Inc byte spacing depending on length
      (when T
        (setq *pen-v (list (+ (get *pen-v 1) (* *adv-vert (- 4 (length Str))))
                           (get *pen-v 2)
                           0.0)))

                         T))
