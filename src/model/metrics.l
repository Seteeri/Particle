#{
Based on texture...poss move to render?
}#

#Range
#Advance
#Scale

(class +Metrics)
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
    (let (Path-glyph (pack PATH-GLYPHS "/metrics/" Code "-metrics.l"))
      #(prinl (usec) " | MODEL | load-file-metrics-data | " Code "-metrics.l ...")
      (eval (any (in Path-glyph (till NIL T))))))

(de load-metrics ()
    (let (Metrics ())
      (for Code 255
	   #Format path name, read file/code, eval data -> list of data
	   (let (Data (load-file-metrics-data Code)
                 Metrics-glyph (apply 'new T '(+Metrics) (car (nth Data 4)) (car (nth Data 6))))
	     (when (get 'Metrics-glyph 'bounds)
	       (calc-metrics-bounds Metrics-glyph))
	     (push 'Metrics Metrics-glyph)))
      #(pretty Metrics)
      Metrics))

(dm calc-metrics-bounds ()
    #Convert bounds from shape units to pixel units by multiplying by scale
    (=: bounds (let L NIL
		    (for X (=: bounds)
			 (push 'L (* X *scale-glyph)))
		    (reverse L)))
    
    #Calculate bounds (pixel and absolute values)
    #l b r t    
    (=: bounds-texture (list (+ (* (car (nth (: translate) 1)) *scale-glyph) (car (nth (: bounds) 1)))
			     (+ (* (car (nth (: translate) 2)) *scale-glyph) (car (nth (: bounds) 2)))
			     (+ (* (car (nth (: translate) 1)) *scale-glyph) (car (nth (: bounds) 3)))
			     (+ (* (car (nth (: translate) 2)) *scale-glyph) (car (nth (: bounds) 4)))))
    
    (=: dims-glyph (list (- (car (nth (: bounds-texture) 3)) (car (nth (: bounds-texture) 1)))
			 (- (car (nth (: bounds-texture) 4)) (car (nth (: bounds-texture) 2)))))
    
    (=: ratio-aspect (/ (car (nth (: dims-glyph) 1))
			(car (nth (: dims-glyph) 2))))

    #dims + baseline coords + bounds -> calculate glyph bounds in abs px coords
    #bounds = l b r t = 1 2 3 4
    #origin (0,0) is considered bottom left corner so (1,1) is top right corner
    #
    #bounds-abs:
    #baseline.x - bounds.l
    #baseline.y - bounds.b
    #baseline.x + bounds.r
    #baseline.y + bounds.t        
    (=: uv (list (/ (car (nth (: bounds-texture) 3)) 96)
		 (/ (car (nth (: bounds-texture) 4)) 96)
		 0.0
		 0.0   #r-t 1,1
		 (/ (car (nth (: bounds-texture) 3)) 96)
		 (/ (car (nth (: bounds-texture) 2)) 96)
		 0.0
		 0.0   #r-b 1,0
		 (/ (car (nth (: bounds-texture) 1)) 96)
		 (/ (car (nth (: bounds-texture) 2)) 96)
		 0.0
		 0.0   #l-b 0,0
		 (/ (car (nth (: bounds-texture) 1)) 96)
		 (/ (car (nth (: bounds-texture) 4)) 96)
		 0.0
		 0.0))) #l-t 0,1
