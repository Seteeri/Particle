#{
- Make class for these so need not destructuring bind?
- Should move to files that both model and render can load...
  - Seperate GL params from MMap params
- These are determined ahead of time
- To be completely dynamic:
  - Free/Alloc GL memory
  - Free/Alloc mmap/memfd
- Technically, all of it can be done dynamically
- Use these as baseline
}#

(def *params-mmap-atomic-counter (list 'atomic-counter-buffer
					"protoform-atomic-counter"
					(* 4 6)  #6 ints/params
					4 -1
					'triple
					0))

(def *params-mmap-draw-indirect (list 'draw-indirect-buffer
				       "protoform-draw-indirect"
				       (* 4 6)  #6 ints/params
				       -1 -1
				       'triple
				       0))

(def *params-mmap-element (list 'element-array-buffer
				 "protoform-element"
				 (* 4 6)  #4 bytes/int * 6 ints or indices
				 -1 -1
				 'triple
				 0))

(def *params-mmap-nodes (list 'shader-storage-buffer
			       "protoform-nodes"
			       (/ 134217728 4)
			       2 3
			       'triple
			       0))

(def *params-mmap-projview (list 'uniform-buffer
				  "protoform-projview"
				  (* (+ 16 16) 4)
				  0 0  #cs-in (cache), vs-in (raster)
				  'triple
				  0))


(def *params-mmap-tex-glyphs (list 'texture-buffer
				    "protoform-texture-glyphs"
				    (/ 134217728 4)
				    -1 -1
				    'triple
				    0
				    'rgba8)) #requires fmt type

(def *params-mmap-vertices (list 'uniform-buffer
				  "protoform-vertices"
				  (* 16 4)
				  1 1
				  'triple
				  0))
