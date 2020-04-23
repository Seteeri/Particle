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
  (let Path-Glyph (pack PATH-GLYPHS
                        "/metrics/"
                        Code "-metrics.l")
    (eval (any (in Path-Glyph (till NIL T))))))


(de load-metrics ()

  (let Metrics ()

    (for Code 255

      #Format path name, read file/code, eval data -> list of data
      (let (Data (load-file-metrics-data Code)
            Metric-Glyph (new '(+Metrics) (get Data 4) (get Data 6)))

        # When no bounds max it
        (with Metric-Glyph
          (unless (: bnds)
            # (* 7.74184810394 2.0) = 15.48369620788
            # l b r t
            #(=: bnds (list 0.0 0.0 15.48369620788 15.48369620788))
            (=: bnds (need 4 0.0))))
          
        (calc-metrics> Metric-Glyph)

        (push 'Metrics Metric-Glyph)))

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


# Per set of glyphs
(class +Metrics)
(dm T (Tra
       Bnds)
  (=: tra Tra)
  (=: bnds Bnds)
  (=: bnds-tex NIL)
  (=: dims NIL)
  (=: ar NIL)
  (=: uv NIL))


# Precalc these when generating metadata

(dm calc-metrics> ()

    # bnds: l b r t

    # sca-glyph * shape units = pixel units
    # Bounds is rel to ori so neg nums possible
    (=: bnds 
        (list (*/ (get (: bnds) 1) *sca-glyph 1.0)
              (*/ (get (: bnds) 2) *sca-glyph 1.0)
              (*/ (get (: bnds) 3) *sca-glyph 1.0)
              (*/ (get (: bnds) 4) *sca-glyph 1.0)))

    # bbox (for tex draw) rel to 
    # 1. Get origin which may have moved during gen to create enough space for df
    # 2. Add bnds
    # 3. Expand by 1.0 since ori=(0,0)
    (=: bnds-tex 
        (list (+ (*/ (car  (: tra)) *sca-glyph 1.0) (get (: bnds) 1) -1.0)
              (+ (*/ (cadr (: tra)) *sca-glyph 1.0) (get (: bnds) 2) -1.0)
              (+ (*/ (car  (: tra)) *sca-glyph 1.0) (get (: bnds) 3) 1.0)
              (+ (*/ (cadr (: tra)) *sca-glyph 1.0) (get (: bnds) 4) 1.0)))
    
    # dims of bbox
    (=: dims (list (- (get (: bnds-tex) 3) (get (: bnds-tex) 1))
                   (- (get (: bnds-tex) 4) (get (: bnds-tex) 2))))

    # AR of bbox
    (=: ar (/ (car  (: dims))
              (cadr (: dims))))

    # l b r t = 1 2 3 4
    # ori=(0,0)=bottom left
    # so (1,1)=top right
    #
    # baseline.x - bounds.l
    # baseline.y - bounds.b
    # baseline.x + bounds.r
    # baseline.y + bounds.t
    (=: uv (list (*/ 1.0 (get (: bnds-tex) 3) (car  *glyph-dims))
                 (*/ 1.0 (get (: bnds-tex) 4) (cadr *glyph-dims))
                 0.0
                 0.0   # r-t 1,1
                 (*/ 1.0 (get (: bnds-tex) 3) (car  *glyph-dims))
                 (*/ 1.0 (get (: bnds-tex) 2) (cadr *glyph-dims))
                 0.0
                 0.0   # r-b 1,0
                 (*/ 1.0 (get (: bnds-tex) 1) (car  *glyph-dims))
                 (*/ 1.0 (get (: bnds-tex) 2) (cadr *glyph-dims))
                 0.0
                 0.0   # l-b 0,0
                 (*/ 1.0 (get (: bnds-tex) 1) (car  *glyph-dims))
                 (*/ 1.0 (get (: bnds-tex) 4) (cadr *glyph-dims))
                 0.0
                 0.0))) # l-t 0,1


(dm pos> (Ori Sca)
  #{
    Pos is the ori
    Actual vert pos is rel to ori
    Adj vert by bnds since glyph has been cropped to bbox/bnds
    
    For example, 'g' hangs below baseline,
    bottom bnds is neg,
    y pos + y bnds (neg), shifts glyph downward into proper pos
  }#

  (list (+ (car  Ori) (*/ (car  (: bnds)) Sca 1.0))
        (+ (cadr Ori) (*/ (cadr (: bnds)) Sca 1.0))
        0.0))


(dm ori> (Pos Sca)
  #{
    Derive ori from pos
  }#

  (list (+ (car  Pos) (- (*/ (car  (: bnds)) Sca 1.0)))
        (+ (cadr Pos) (- (*/ (cadr (: bnds)) Sca 1.0)))
        0.0))


(dm sca> (Sca)
  # Scale based on uv dims, not tex/glyph dims
  (list (*/ (car  (: dims)) Sca 1.0)
        (*/ (cadr (: dims)) Sca 1.0)
        Sca))
