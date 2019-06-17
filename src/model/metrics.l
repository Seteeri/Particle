(class +Metrics)

#Range
#Advance
#Scale
(dm T (Translate
       Bounds
       Bounds-texture       
       Dims-glyph
       Ratio-aspect
       Scale-uv
       UV)
    (=: translate Translate)
    (=: bounds Bounds)
    (=: bounds-texture Bounds-texture)
    (=: dims-glyph Dims-glyph)
    (=: ratio-aspect Ratio-aspect)
    (=: scale-uv Scale-uv)
    (=: uv UV))

(class +Bounds)

(dm T (L
       B
       R
       T)
    (=: l L)
    (=: b B)
    (=: r R)
    (=: t T))

#Scale and advance the same (adv bc monospaced)
#Merge metrics and data(ppm) into single file
#Calculate other params and save to file so need not do everytime

(de load-file-metrics-data (Code)
    (let (path-glyph (pack *path-glyphs Code "-metrics.l"))
      ;; (println "Reading " path-glyph " ...")
      (eval (any (in path-glyph (till NIL T))))))
	 
(de load-metrics ()
    (let (metrics ())
      (for Code 255
	   #Format path name, read file/code, eval data -> list of data
	   (let (metrics-file (load-file-metrics-data Code)
	         metrics-glyph (apply 'new '(+Metrics) (nth metrics-file 4) (nth metrics-file 6)))
	     (when (get 'metrics-glyph 'bounds)
	       (calc-metrics-bounds metrics-glyph))
	     (push 'metrics metrics-glyph)))
      #(pretty metrics)
      metrics))

(dm calc-metrics-bounds ()
    #Convert bounds from shape units to pixel units by multiplying by scale
    (=: bounds (let L NIL
		    (for X (=: bounds)
			 (push 'L (* X *scale-glyph)))
		    (reverse L)))
    
    #Calculate bounds (pixel and absolute values)
    #l b r t    
    (=: bounds-texture (list (+ (* (nth (: translate) 1) *scale-glyph) (nth (: bounds) 1))
			     (+ (* (nth (: translate) 2) *scale-glyph) (nth (: bounds) 2))
			     (+ (* (nth (: translate) 1) *scale-glyph) (nth (: bounds) 3))
			     (+ (* (nth (: translate) 2) *scale-glyph) (nth (: bounds) 4))))
    
    (=: dims-glyph (list (- (nth (: bounds-texture) 3) (nth (: bounds-texture) 1))
			 (- (nth (: bounds-texture) 4) (nth (: bounds-texture) 2))))
    
    (=: ratio-aspect (/ (nth (: dims-glyph) 1)
			(nth (: dims-glyph) 2)))

    #dims + baseline coords + bounds -> calculate glyph bounds in abs px coords
    #bounds = l b r t = 1 2 3 4
    #origin (0,0) is considered bottom left corner so (1,1) is top right corner
    #
    #bounds-abs:
    #baseline.x - bounds.l
    #baseline.y - bounds.b
    #baseline.x + bounds.r
    #baseline.y + bounds.t        
    (=: uv (list (/ (nth (: bounds-texture) 3) 96)
		 (/ (nth (: bounds-texture) 4) 96)
		 0.0
		 0.0   #r-t 1,1
		 (/ (nth (: bounds-texture) 3) 96)
		 (/ (nth (: bounds-texture) 2) 96)
		 0.0
		 0.0   #r-b 1,0
		 (/ (nth (: bounds-texture) 1) 96)
		 (/ (nth (: bounds-texture) 2) 96)
		 0.0
		 0.0   #l-b 0,0
		 (/ (nth (: bounds-texture) 1) 96)
		 (/ (nth (: bounds-texture) 4) 96)
		 0.0
		 0.0))) #l-t 0,1
