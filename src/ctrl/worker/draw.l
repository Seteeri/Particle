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

  # CAR/CDR are base-0/base-01

  # Adjust brightness to indicate time

  (cond ((num? Any) (prinl-info "get-color-type" "NUM")  *col-violet)
        ((str? Any) (prinl-info "get-color-type" "STR")  *col-blue)
        ((sym? Any) (prinl-info "get-color-type" "SYM")  *col-cyan)
        ((pair Any) (prinl-info "get-color-type" "CONS") *col-green)
        ((box? Any) (prinl-info "get-color-type" "BOX")  *col-yellow)))


#{
  A Cell represents a single cell or CAR/CDR

  Example: For sym, user can delete one cell of sym or all

  This allows user to disassemble and reassemble cells

  DATA
  [.|.] -> [.|.] -> [.|.] -> [.|.]

  REPR
  [.|.]    -> [.|.]    -> [.|.]    -> [.|.]
  |           |           |           |
  [.|v]       [.|v]       [.|v]       [.|v]  (all pushed into timeline)

  (or build same structure from cons/lists? and push into timeline)

  A Cell holds the Data and the Verts
  Example: For sym, Cell must be created for each cell in sym

          Symbol
          |
          V
    +-----+-----+
    |  |  | VAL |
    +--+--+-----+
       | tail
       |
       V
       +-----+-----+     +-----+-----+     +-----+--+--+     +----------+---------+
       |  |  |  ---+---> | KEY |  ---+---> |  |  |  ---+---> |'hgfedcba'|'onmlkji'|
       +--+--+-----+     +-----+-----+     +--+--+-----+     +----------+---------+
          |                                   |
          V                                   V
          +-----+-----+                       +-----+-----+
          | VAL | KEY |                       | VAL | KEY |
          +-----+-----+                       +-----+-----+


                       cell
                        |
            +-----------+-----------+
            |           |           |
         Number       Symbol       Pair
                        |
                        |
   +--------+-----------+-----------+
   |        |           |           |
  NIL   Internal    Transient    External

}#

(def '*pos-start-car)
(def '*pos-end-car)
(def '*pos-start-cdr)
(def '*pos-end-cdr)

(de draw-cell (Adr)

  # Draw CAR/CDR addr
  # Adjust scale for pointers?

  # Draw CAR ref underneath
  # Draw CDR ref to the right

  (let ((Adr-Car Adr-Cdr) (struct Adr '((B . 8) (B . 8)))

        Ptr-Pos-Start     (setq *pos-start-car (get *vert-ptr 'pos))

        Verts-Car         (draw-str (glue " " Adr-Car) *col-base-0)

        Pos-End-Car       (setq *pos-end-car (get *vert-ptr 'pos))

        Dummy             (adv-ptr 2.0)

        Pos-Start-Cdr     (setq *pos-start-cdr (get *vert-ptr 'pos))

        Verts-Cdr         (draw-str (glue " " Adr-Cdr) *col-base-01)

        Ptr-Pos-End       (setq *pos-end-cdr (get *vert-ptr 'pos)))

    (push '*timeline (new '(+Particle) Data (conc Verts-Car Verts-Cdr)))


    (let ((Long-Car Long-Cdr) (struct Adr '(N . 2))
           Bin-Car            (bin Long-Car)
           Tail-Car           (pack (tail 4 (chop Bin-Car)))
           Bin-Cdr            (bin Long-Cdr)
           Tail-Cdr           (pack (tail 4 (chop Bin-Cdr))))

      (prin "Adr: ") (prinl Adr)
      (prin "Car: ") (prinl Long-Car " " Bin-Car " " Tail-Car)
      (prin "Cdr: ") (prinl Long-Cdr " " Bin-Cdr " " Tail-Cdr)

      # ADD RES VERTS TO PARTICLE

      # Move pointer underneath start of Car
      (when (= Tail-Car "0000")
        (nl-ptr 2.0)
        (set-pos-ptr-x (get Ptr-Pos-Start 1)))
      (check-ptr-type Adr Long-Car Tail-Car)

      # Move pointer above end of Cdr
      (when NIL
        (when (= Tail-Cdr "0000")
          (set-pos-ptr-x (get Ptr-Pos-End 1))        
          (set-pos-ptr-y (get Ptr-Pos-End 2))
          (adv-ptr 4.0))
        (check-ptr-type Adr Long-Cdr Tail-Cdr))
      
      )))


(de check-ptr-type (Ptr-Car Ptr Tail-Ptr)

  #{
    CONS:      ...0000
    SYM:       ...1000
    BIGNUM:    ...S100
    SHORTNUM:  ...S010

    STR: stored as nums, shortnum takes entire PTR, bignum in CAR + CDR ptr

    S= pos:0 neg:1
  }#

  (cond ((= Tail-Ptr "0000")
         (println "(draw-cell)")   
         (draw-cell Ptr))

        ((= Tail-Ptr "1000")
          # Ptr is to CDR so -8
          # Watch for NIL and circular ptrs
          # For NIL, draw NIL underneath
         (println "(draw-sym)" Ptr Ptr-Car)
         (cond ((= Ptr 4380488) (draw-nil Ptr-Car Ptr Tail-Ptr))
               ((= Ptr (+ Ptr-Car 8)) (println "str detected"))
               (T (nl-ptr 2.0) (draw-cell (- Ptr 8)))))

        (T
         (cond ((= (pack (tail 3 (chop Tail-Ptr))) "100")
                (println "(draw-big)"))

               ((= (pack (tail 2 (chop Tail-Ptr))) "10")
                (println "(draw-short)")
                (draw-short Ptr-Car Ptr Tail-Ptr))

               (T
                (println "(draw-ptr)"))))))


(de draw-short (Ptr-Car Ptr Tail-Ptr)

  (nl-ptr)
  (set-pos-ptr-x (get *pos-start-car 1))
  
  (draw-str Ptr *col-violet))


(de draw-big () T)


(de draw-ptr () T)


(de draw-nil (Ptr-Car Ptr Tail-Ptr)

  (if (= Ptr Ptr-Car)
    (prog
      (nl-ptr)
      (set-pos-ptr-x (get *pos-start-car 1)))
    (set-pos-ptr-x (get *pos-start-cdr 1)))
  
  (draw-str "NIL" *col-cyan))


################################################################################

# Batch these

(de draw-str (Str Col)

  (let Verts ()

    (for C (chop Str)
      (push 'Verts (draw-glyph C Col))
      (adv-ptr 1.0))

    (flip Verts)))


(de draw-num (Num Col)

  (let Verts ()

    (for N (chop (format Num))
      (push 'Verts (draw-glyph C Col))
      (adv-ptr 1.0))

    # Inc byte spacing depending on length

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