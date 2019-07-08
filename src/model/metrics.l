#{
Based on texture...poss move to render?

Product of X and Y = (*/ X Y 1.0).

: (*/ 32.1 1.056 1.0)  # 32.1 * 1.056 = 33.8976
-> 33898

Quotient of N and D = (*/ 1.0 N D)

: (*/ 1.0 91.8 1.323)  # 91.8 / 1.323 = 69.38775510...
-> 69388
}#

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


(de load-file-metrics-data (Code)

  (let (Path-Glyph (pack PATH-GLYPHS "/metrics/" Code "-metrics.l"))
  
    #(prinl (usec) " | MODEL | load-file-metrics-data | " Code "-metrics.l ...")
    (eval (any (in Path-Glyph (till NIL T))))))


(de load-metrics ()

    (let (Metrics ())

      (for Code 255

        #Format path name, read file/code, eval data -> list of data
        (let (Data          (load-file-metrics-data Code)
              Metrics-Glyph (new '(+Metrics) (get Data 4) (get Data 6)))

          (when (get Metrics-Glyph 'bounds)
            (calc-metrics> Metrics-Glyph))

          (push 'Metrics Metrics-Glyph)))

      (reverse Metrics)))


(dm calc-metrics> ()

    #Convert bounds from shape units to pixel units by multiplying by scale
    (=: bounds (let L NIL
                (for X (: bounds)
                 (push 'L (*/ X *sca-glyph 1.0)))
                (reverse L)))
    
    #Calculate bounds (pixel and absolute values)
    #l b r t    
    (=: bounds-tex (list (+ (*/ (get (: translate) 1) *sca-glyph 1.0) (get (: bounds) 1))
                         (+ (*/ (get (: translate) 2) *sca-glyph 1.0) (get (: bounds) 2))
                         (+ (*/ (get (: translate) 1) *sca-glyph 1.0) (get (: bounds) 3))
                         (+ (*/ (get (: translate) 2) *sca-glyph 1.0) (get (: bounds) 4))))
    
    (=: dims (list (- (get (: bounds-tex) 3) (get (: bounds-tex) 1))
                   (- (get (: bounds-tex) 4) (get (: bounds-tex) 2))))
    
    (=: ratio-aspect (/ (get (: dims) 1)
                        (get (: dims) 2)))

    #dims + baseline coords + bounds -> calculate glyph bounds in abs px coords
    #bounds = l b r t = 1 2 3 4
    #origin (0,0) is considered bottom left corner so (1,1) is top right corner
    #
    #bounds-abs:
    #baseline.x - bounds.l
    #baseline.y - bounds.b
    #baseline.x + bounds.r
    #baseline.y + bounds.t        
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
