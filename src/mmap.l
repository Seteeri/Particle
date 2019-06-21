(class +Mmap)

(dm T (Path
       FD
       Ptr
       Size)
    (=: path (when Path path))
    (=: fd   (when FD FD))    
    (=: ptr  (when Ptr Ptr))
    (=: size (when Size Size)))


(de init-mmap (Path
	       Size
	       Create)
    
    (let (Fd   (memfd-create Path 0))
            
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


#{
(de init-mmap-ipc (P)
    (init-mmap (car (nth P 2))
	       (car (nth P 3))
	       T))
}#

(de init-mmap-nodes ()
    (init-mmap (car (nth *params-mmap-nodes 2))
	       (car (nth *params-mmap-nodes 3))
	       T))


(de init-mmap-atomic-counter ()
    (init-mmap (car (nth *params-mmap-atomic 2))
	       (car (nth *params-mmap-counter 3))
	       T))


(de init-mmap-draw-indirect ()
    (let (Mmap (init-mmap
		(car (nth *params-mmap-draw-indirect 2))
		 (car (nth *params-mmap-draw-indirect 3))
		 T)
	  Ptr (get Mmap 'ptr))
            
      #(println "FD" (get Mmap 'fd))

      #Use struct function to write to pointer     
      (struct Ptr 'N
	      (6 . 4)
	      (cons *inst-max 4)
	      (0 . 4)
	      (0 . 4)
	      (0 . 4))

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
    (let (Mmap (init-mmap
		(car (nth *params-mmap-vertices 2))
		 (car (nth *params-mmap-vertices 3))
		 T)
	  Ptr (get Mmap 'ptr))
      (struct Ptr 'N (list -1.0  1.0  1.0  0.0  1.0
			         1.0  0.0  0.0  1.0
			         0.0  0.0  0.0  1.0
				 0.0  1.0  0.0  1.0))
      Mmap))


(de init-mmap-element ()
    (let (Mmap (init-mmap
		(car (nth *params-mmap-element 2))
		 (car (nth *params-mmap-element 3))
		 T)
          Ptr (get Mmap 'ptr))
      (struct Ptr 'N
	      (0 . 4)
	      (2 . 4)
	      (1 . 4)
	      (0 . 4)
	      (3 . 4)
	      (2 . 4))
      Mmap))


(de init-mmap-projview ()
    (let (Mmap (init-mmap
		(car (nth *params-mmap-projview 2))
		 (car (nth *params-mmap-projview 3))
		 T)
          Ptr (get Mmap 'ptr))
      (write-mat-proj-to-shm Mmap *projview)
      (write-mat-view-to-shm Mmap *projview)
      Mmap))


(de init-mmap-tex-glyphs ()
    #{
    - To load faster, do bulk memcpy or convert to bytes
    - Possible Configurations:
      - all ppm, all metrics
      - per ppm+metrics
      - per ppm, per metrics (current)
      - Need separate ppm since data is copied to shm?
        - Or list of lists with each list starting with the name
    }#
    (let (Mmap (init-mmap
		(car (nth *params-mmap-tex-glyphs 2))
		 (car (nth *params-mmap-tex-glyphs 3))
		 T)
	  Ptr (get Mmap 'ptr)
	  Offset 0)     
      (for Code 255
	   (let (Path-glyph (pack PATH-GLYPHS "/tex/" Code "-data.l"))
	     #(println "Reading " Path-glyph " ...")
	     (any (in Path-glyph (till NIL T)))
	     (struct (+ Ptr Offset) 'N
		     (0 . 4)
		     (2 . 4)
		     (1 . 4)
		     (0 . 4)
		     (3 . 4)
		     (2 . 4))
	     (inc 'Offset)))
      Mmap))
