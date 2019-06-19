#Make class for these so need not destructuring bind
#These are here for convenience primarily...

(setq *params-mmap-atomic-counter (list 'atomic-counter-buffer
					"protoform-atomic-counter"
					(* 4 6)  #6 ints/params
					4 -1
					'triple
					0))

(setq *params-mmap-draw-indirect (list 'draw-indirect-buffer
				       "protoform-draw-indirect"
				       (* 4 6)  #6 ints/params
				       -1 -1
				       'triple
				       0))

(setq *params-mmap-element (list 'element-array-buffer
				 "protoform-element"
				 (* 4 6)  #4 bytes/int * 6 ints or indices
				 -1 -1
				 'triple
				 0))

(setq *params-mmap-nodes (list 'shader-storage-buffer
			       "protoform-nodes"
			       (/ 134217728 4)
			       2 3
			       'triple
			       0))

(setq *params-mmap-projview (list 'uniform-buffer
				  "protoform-projview"
				  (* (+ 16 16) 4)
				  0 0  #cs-in (cache), vs-in (raster)
				  'triple
				  0))


(setq *params-mmap-tex-glyphs (list 'texture-buffer
				    "protoform-texture-glyphs"
				    (/ 134217728 4)
				    -1 -1
				    'triple
				    0
				    'rgba8)) #requires fmt type

(setq *params-mmap-vertices (list 'uniform-buffer
				  "protoform-vertices"
				  (* 16 4)
				  1 1
				  'triple
				  0))


(class +Mmap)

(dm T (Path
       FD
       Ptr
       Size)
    (=: path (when Path path))
    (=: fd   (when FD FD))    
    (=: ptr  (when Ptr Ptr))
    (=: size (when Size Size)))


(de init-mmap (Params
	       Create)
    
    (let (Path (car (nth Params 2))
          Size (car (nth Params 3))
          Fd   (memfd-create Path 0))
            
      (when (= Fd -1)
	#Figure out how to get errno...
	(quit "(= memfd-create -1)"))
      
      #Configure the size of the shared memory segment - only on create
      (when Create
	(ftruncate Fd Size))
      
      #Map shm to address space of process
      (let (Ptr (mmap NIL
	              Size
		      (| PROT-READ PROT-WRITE)
		      (| MAP-SHARED)
		      Fd
		      0))
	 
	(when (= Ptr 0) #null ptr
	  (quit "(= mmap -1)"))

	# zero memory?
	(new '(+Mmap) Path Fd Ptr Size))))


(de init-mmap-nodes ()
    (init-mmap *params-mmap-nodes T))


(de init-mmap-atomic-counter ()
    (init-mmap *params-mmap-atomic-counter T))


(de init-mmap-draw-indirect ()
    (let (Mmap (init-mmap *params-mmap-draw-indirect T)
	  Ptr (get Mmap 'ptr))
            
      #(println "FD" (get Mmap 'fd))

      #Use struct function to write to pointer     
      (struct Ptr 'N
	      (6 . 8)
	      (cons *inst-max 8)
	      (0 . 8)
	      (0 . 8)
	      (0 . 8))

      #(println "Read 6 int32" (struct Ptr '(I . 6)))
      
      Mmap))


(de init-mmap-vertices ()
    #{
    ;;top right, bottom right, bottom left, top left
    ;;
    ;;3---0
    ;;| / |
    ;;2---1
    ;;
    ;;ccw: 0 2 1 0 3 2
    }#
    (let (Mmap (init-mmap *params-mmap-vertices T)
	  Ptr (get Mmap 'ptr))
      (struct Ptr 'N (list -1.0  1.0  1.0  0.0  1.0
			         1.0  0.0  0.0  1.0
			         0.0  0.0  0.0  1.0
				 0.0  1.0  0.0  1.0))
      Mmap))


(de init-mmap-element ()
    (let (Mmap (init-mmap *params-mmap-element T)
          Ptr (get Mmap 'ptr))
      (struct Ptr 'N
	      (0 . 8)
	      (2 . 8)
	      (1 . 8)
	      (0 . 8)
	      (3 . 8)
	      (2 . 8))
      Mmap))


(de init-mmap-projview ()
    (let (Mmap (init-mmap *params-mmap-projview T)
          Ptr (get Mmap 'ptr))
      (write-mat-proj-to-shm Mmap *projview)
      (write-mat-view-to-shm Mmap *projview)
      Mmap))


(de init-mmap-tex-glyphs ()
    # To load faster, do bulk memcpy or convert to bytes
    (let (Mmap (init-mmap *params-mmap-tex-glyphs T)
	  Ptr (get Mmap 'ptr)
	  Offset 0)     
      (for Code 255
	   (let (Path-glyph (pack PATH-GLYPHS "/tex/" Code "-data.l"))
	     #(println "Reading " Path-glyph " ...")
	     (any (in Path-glyph (till NIL T)))
	     (struct (+ Ptr Offset) 'N
		     (0 . 8)
		     (2 . 8)
		     (1 . 8)
		     (0 . 8)
		     (3 . 8)
		     (2 . 8))
	     (inc 'Offset)))
      Mmap))
