# Really the controller?
(class +Particle)
(dm T (Data
       Verts)
  
  (=: data Data)
  (=: verts Verts))



(class +Particle-2)
(dm T (Data
       Parent
       Children)
  
  (=: data Data)
  (=: parent Parent)
  (=: children Children))

#
  public static void CalculateNodePositions(TreeNodeModel<T> rootNode)
  
      // initialize node x, y, and mod values
      InitializeNodes(rootNode, 0);

      // assign initial X and Mod values for nodes
      CalculateInitialX(rootNode);

      // ensure no node is being drawn off screen
      CheckAllChildrenOnScreen(rootNode);

      // assign final X values to nodes
      CalculateFinalPositions(rootNode, 0);
  
#

(dm calc-x> ()

  (for Child (: children)
    (calc-x Child))

  (cond
    # no children
    ((is-leaf Part)
      # if there is a previous sibling in this set, set X to prevous sibling + designated distance
      (if (not (is-left-most Part))
          (=: x (+ (get (get-prev-sibling Part) 'x) *node-size *sibling-dist))
          # if this is the first node in a set, set X to 0
          (=: x 0)))

    # one child
    ((= (length (: children)) 1)
      # if this is the first node in a set, set it's X value equal to it's child's X value
      (if (is-left-most Part)
          (=: x (get (car (: children)) 'x))
          (prog
            (=: x (+ (get (get-prev-sibling Part) 'x) *node-size *sibling-dist))
            (=: m (- (: x) (get (car (: children)) 'x))))))

    # multiple children
    (T
      (let (ch-l (get-left-most-child Part)
            ch-r (get-right-most-child Part)
            mid  (/ (+ (get ch-l 'x) (get ch-r 'x)) 2))

        (if (is-left-most Part)
            (=: x mid)
            (prog
              (=: x (+ (get (get-prev-sibling Part) 'x) *node-size *sibling-dist))
              (=: m (- (: x) mid)))))))
  
  (when (and (> (length (: children)) 0) (not (is-left-most Part)))
    # Since subtrees can overlap, check for conflicts and shift tree right if needed
    (check-conflicts Part)))


(dm is-leaf> () (: children))


(dm is-left-most> ()
  (if (: parent)
      (= (car (get (: parent) 'children)) This)
      T))


(dm is-right-most ()
  (if (: parent)
      (= (last (get (: parent) 'children)) This)
      T))


(dm get-prev-sibling> ()
  (if (or (not (: parent)) (is-left-most> This))
      NIL
      (let Children (get ( :parent) 'children)
        (get Children (- (index This Children) 1)))))


(dm get-next-sibling> ()
  (if (or (not (: parent)) (is-right-most> This))
      NIL
      (let Children (get ( :parent) 'children)
        (get Children (+ (index This Children) 1)))))


(dm get-left-most-sibling> ()
  (cond ((not (: parent)) NIL)
        ((is-left-most> This) This)
        (T (car (get (: parent) 'children)))))


(dm get-left-most-child> ()
  (car (: children))


(dm get-right-most-child> ()
  (get (: children) (- (length (: children)) 1)))


(dm get-left-contour> (Mod-Sum)
  T)


(dm check-conflicts> ()
  (let (Dist-Min (+ *tree-dist *node-size)
        Val-Shift 0.0
        Node-Contour (get-left-contour> This 0)
        Sibling (get-left-most-sibling> This))
        
    # var nodeContour = new Dictionary<int, float>();
    # GetLeftContour(node, 0, ref nodeContour);

    (while (and Sibling (not (= Sibling This)))

      # var siblingContour = new Dictionary<int, float>();
      # GetRightContour(sibling, 0, ref siblingContour);
      (let Sibling-Contour (get-right-contour> Sibling 0)
        (for (Level (+ (: y) 1)
              (<= Level (min (length Sibling-Contour) (length Node-Contour)))
              (inc Level))
            (let Dist (- (get Node-Contour Level) (get Sibling-Contour Level))
              (when (< (+ Dist Val-Shift) Dist-Min)
                (setq Val-Shift (- Dist-Min Dist))))))

      (when (> shiftValue 0)
        (inc (:: x) Val-Shift)
        (inc (:: m) Val-Shift)
        (center-nodes-between node sibling)
        (setq Val-Shift 0))

      (setq Sibling (get-next-sibling> Sibling)))))


(de center-nodes-between (Node-Left Node-Right)

  (let (Ix-Left  (index Node-Right (get (get Node-Left 'parent) 'children))
        Ix-Right (index Left-Node  (get (get Node-Left 'parent) 'children))
        Num-Nodes-Between (- (- Ix-Right Ix-Left) 1))

    (when (> Num-Nodes-Between 0)
      
      (let (Dist-Between-Nodes (/ (- (get Node-Left 'x) (get Node-Right 'x))
                                  (+ Num-Nodes-Between 1))
            Cnt 1)

        (for (I (+ Ix-Left 1)
              (< I Ix-Right)
              (inc I))
          (let (Middle-Node (get (get (get Node-Left 'parent) 'children) I)
                Desired-X (+ (get Node-Right 'x) (* Dist-Between-Nodes Cnt))
                Offset (- Desired-X (get Middle-Node 'x)))

              (with Middle-Node
                (inc (:: x)   Offset)
                (inc (:: mod) Offset))

              (inc Cnt))))
              
      (check-for-conflicts Node-Left))))


(de check-all-children-on-screen (Node)

  (let (Node-Contour (get-left-contour Node 0)
        Shift-Amount 0.0)
        
    (foreach (var y in nodeContour.Keys)
      (when (< (+ Node-Contour[y] Shift-Amount) 0)
        (setq Shift-Amount (* Node-Contour[y] -1))))

    (when (> Shift-Amount 0.0)
      (with Node
        (inc (:: x) Shift-Amount)
        (inc (:: m) Shift-Amount)))))


(de get-left-contour (Node Mod-Sum Values)

  #{
    if (!values.ContainsKey(node.Y))
        values.Add(node.Y, node.X + modSum);
    else
        values[node.Y] = Math.Min(values[node.Y], node.X + modSum);
  }#
  
  (inc 'Mod-Sum (get Node 'm))

  (for Child (: children)
    (get-left-contour Child Mod-Sum Values)))


(de get-right-contour (Node Mod-Sum Values)

  #{
    if (!values.ContainsKey(node.Y))
        values.Add(node.Y, node.X + modSum);
    else
        values[node.Y] = Math.Max(values[node.Y], node.X + modSum);
  }#
  
  (inc 'Mod-Sum (get Node 'm))

  (for Child (: children)
    (get-right-contour Child Mod-Sum Values)))


(de calc-final-pos (Node Mod-Sum)
  (with Node

    (inc (:: x) Mod-Sum)
    (inc Mod-Sum (: m))

    (for Child (: children)
      (calc-final-pos Child Mod-Sum))

    (if (: children)
      (prog
        (=: width (get (car (order-by-desc (: children) (p => p.Width))) 'width))
        (=: height (get (car (order-by-desc (: children) (p => p.Height))) 'height)))
      (prog
        (=: width (: x))
        (=: height (: y))))))


################################################################################
################################################################################


#{
    # Generate, position, draw cells

    (let (Ptr-Pos-Start (get *vert-ptr 'pos)
          Data-Root (cons (cons 1 2) (cons 3 4))
          Node-Root (gen-particle (>> -4 (adr Data-Root)) NIL))

      (setq *x ())
      (setq *y ())
      (setq *mod ())
      (calc-x> Node-Root)
      
      (pretty *x)
      (pretty *y)
      (pretty *mod)

      (nl-ptr 6.0)
      (set-pos-ptr-x (get Ptr-Pos-Start 1))))
}#


(de cell? (Adr-R Adr)

 (cond ((= Adr 4380488)
        (nil (println "NIL detected")))

       ((or (= Adr-R (+ Adr 8)) (= Adr-R Adr))
        (nil (println "Cycle detected")))

       # Return T if not short num
       (T
         (let (Bin             (bin Adr)
               Tail            (pack (tail 4 (chop Bin))))
            (unless (= (pack (tail 2 (chop Tail))) "10")
              T)))))


(def '*x ())
(def '*y ())
(def '*mod ())
(def '*node-size 1)
(def '*sibling-dist 0.0)
(def '*tree-dist 0.0)

(de gen-particle (Adr-R Parent)
  # LRN
  (let ((Adr-A Adr-D) (struct Adr-R '(N . 2))
         P (new '(+Particle-2) Adr-R Parent NIL))
    (put P 'children (cons (when (cell? Adr-R Adr-A) (gen-particle Adr-A P))
                           (when (cell? Adr-R Adr-D) (gen-particle Adr-D P))))
    P))


################################################################################


(de trav-post-order (Adr-R X)
  # LRN

  (let ((Adr-A Adr-D) (struct Adr-R '(N . 2))
        Cell-A (cell? Adr-R Adr-A)
        Cell-D (cell? Adr-R Adr-D))

    (println Adr-R Adr-A "Traverse left node...")
    (when Cell-A
      (trav-post-order Adr-A 0))

    (println Adr-R Adr-D "Traverse right node...")
    (when Cell-D
      (trav-post-order Adr-D 1))))


(de proc-ptr (Adr-R Adr)

  (let (Bin                 (bin Adr)
        Tail                (pack (tail 4 (chop Bin))))
           
    (cond ((= Tail "0000")
           (println Adr "Detected cons"))

          ((= Tail "1000")
           (println Adr "Detected sym"))

          (T
           (cond ((= (pack (tail 3 (chop Tail))) "100")
                  (println Adr "Detected bignum"))

                 ((= (pack (tail 2 (chop Tail))) "10")
                  (println Adr "Detected short num"))

                 (T
                  (println Adr "Detected ptr")))))))


################################################################################

#{

  DATA/REPR/PTR

  [.|.] -> [.|.] -> [.|.] -> [.|.]
  |        |        |        |
  [.|v] -> [.|v] -> [.|v] -> [.|v]
  |
  *...............................

  or use doubly linked list?
  or use assoc list?

  repr/part struct matches data struct
  so modify repr modify data

  def -> timeline -> data
  remove data from timeline and it will be GC

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


#{

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
}#


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

      T)))




(def '*pos-start-car)
(def '*pos-end-car)
(def '*pos-start-cdr)
(def '*pos-end-cdr)

      