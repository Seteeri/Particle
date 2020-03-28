#{
Based on texture...poss move to render?

Product of X and Y = (*/ X Y 1.0).

: (*/ 32.1 1.056 1.0)  # 32.1 * 1.056 = 33.8976
-> 33898

Quotient of N and D = (*/ 1.0 N D)

: (*/ 1.0 91.8 1.323)  # 91.8 / 1.323 = 69.38775510...
-> 69388
}#

# TODO: Move these out to caller

(de load-file-metrics-data (Code)

  (let Path-Glyph (pack PATH-GLYPHS "/metrics/" Code "-metrics.l")

    #(prinl (usec) " | MODEL | load-file-metrics-data | " Code "-metrics.l ...")
    (eval (any (in Path-Glyph (till NIL T))))))


(de load-metrics (Sca-Glyph)

  (let Metrics ()

    (for Code 255

      #Format path name, read file/code, eval data -> list of data
      (let (Data          (load-file-metrics-data Code)
            Metrics-Glyph (new '(+Metrics) (get Data 4) (get Data 6)))

        # When no bounds max it
        (with Metrics-Glyph
          (unless (: bounds)
            # (* 7.74184810394 2.0) = 15.48369620788
            # l b r t
            (=: bounds (list 0.0 0.0 15.48369620788 15.48369620788))))
          
        (calc-metrics> Metrics-Glyph Sca-Glyph)

        (push 'Metrics Metrics-Glyph)))

    (flip Metrics)))


#Range
#Advance
#Scale

(class +Bounds)
(dm T (L
       B
       R
       T)
  (=: l L)
  (=: b B)
  (=: r R)
  (=: t T))

(class +Metrics)
(dm T (Translate
       Bounds)
  (=: translate Translate)
  (=: bounds Bounds)
  (=: bounds-tex NIL)
  (=: dims NIL)
  (=: ratio-aspect NIL)
  (=: uv NIL))


#Scale and advance the same (adv bc monospaced)
#Merge metrics and data(ppm) into single file
#Calculate other params and save to file so need not do everytime


(dm calc-metrics> (Sca-Glyph)

    # Convert bounds from shape units to pixel units by multiplying by scale
    # Bounds is relative to origin so neg values possible
    (=: bounds (list (*/ (get (: bounds) 1) Sca-Glyph 1.0)
                     (*/ (get (: bounds) 2) Sca-Glyph 1.0)
                     (*/ (get (: bounds) 3) Sca-Glyph 1.0)
                     (*/ (get (: bounds) 4) Sca-Glyph 1.0)))

    # Calculate bounding box for glyph (for drawing the texture)
    # l b r t
    # First, get origin which may have moved to create enough space
    # for distance field, then add bounds
    (=: bounds-tex (list (+ (*/ (get (: translate) 1) Sca-Glyph 1.0) (get (: bounds) 1))
                         (+ (*/ (get (: translate) 2) Sca-Glyph 1.0) (get (: bounds) 2))
                         (+ (*/ (get (: translate) 1) Sca-Glyph 1.0) (get (: bounds) 3))
                         (+ (*/ (get (: translate) 2) Sca-Glyph 1.0) (get (: bounds) 4))))

    # Dims of bbox
    (=: dims (list (- (get (: bounds-tex) 3) (get (: bounds-tex) 1))
                   (- (get (: bounds-tex) 4) (get (: bounds-tex) 2))))

    # AR of bbox
    (=: ratio-aspect (/ (get (: dims) 1)
                        (get (: dims) 2)))

    # l b r t = 1 2 3 4
    # origin (0,0) is considered bottom left corner so (1,1) is top right corner
    #
    # baseline.x - bounds.l
    # baseline.y - bounds.b
    # baseline.x + bounds.r
    # baseline.y + bounds.t
    (=: uv (list (*/ 1.0 (get (: bounds-tex) 3) 96.0)
                 (*/ 1.0 (get (: bounds-tex) 4) 96.0)
                 0.0
                 0.0   #r-t 1,1
                 (*/ 1.0 (get (: bounds-tex) 3) 96.0)
                 (*/ 1.0 (get (: bounds-tex) 2) 96.0)
                 0.0
                 0.0   #r-b 1,0
                 (*/ 1.0 (get (: bounds-tex) 1) 96.0)
                 (*/ 1.0 (get (: bounds-tex) 2) 96.0)
                 0.0
                 0.0   #l-b 0,0
                 (*/ 1.0 (get (: bounds-tex) 1) 96.0)
                 (*/ 1.0 (get (: bounds-tex) 4) 96.0)
                 0.0
                 0.0))) #l-t 0,1


(dm get-pos> (Pos Sca)
  #{
    Glyph has been cropped to its bbox so adjust vertex by bounds
    
    For example, 'g' hangs below baseline, its bottom bounds is negative so
    adding to the y pos will shift glyph downward into proper pos
    
    Thus, baseline can be derived by substracting the glyph bounds from the pos
  }#

  (list (+ (get Pos 1) (*/ (get (: bounds) 1) Sca 1.0))
        (+ (get Pos 2) (*/ (get (: bounds) 2) Sca 1.0))
        0.0))


(dm get-origin> (Pos Sca)
  #{
    Derive origin from Vertex position
    
    get-baseline should have been applied
  }#

  (list (+ (get Pos 1) (- (*/ (get (: bounds) 1) Sca 1.0)))
        (+ (get Pos 2) (- (*/ (get (: bounds) 2) Sca 1.0)))
        (get Pos 3)))


(dm get-sca> (Sca)
  # Dims already scaled by sca-glyph

  #(println (format (*/ (get (: dims) 1) Sca 1.0) *Scl)
  #         (format (*/ (*/ *adv-glyph *sca-glyph 1.0) Sca 1.0) *Scl))

  (list (*/ (get (: dims) 1) Sca 1.0)
        (*/ (get (: dims) 2) Sca 1.0)
        Sca))
