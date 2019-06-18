#Make class for these so need not destructuring bind

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


(setq *params-mmap-texture-glyphs (list 'texture-buffer
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
       Ptr
       FD
       Size)
    (=: path (when Path path))
    (=: ptr  (when Ptr Ptr))
    (=: fd   (when FD fd))
    (=: size (when Size Size)))


(de init-mmap (Path
	       Size
	       Create)
    
    (let (Fd (memfd-create Path 0))
            
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
	
	(new (+Mmap) Path Fd Ptr Size))))


(de init-shm (Params)
      (init-mmap (car (nth Params 2))
		 (car (nth Params 3))
		 T))


(de init-mmap-nodes ()
    (init-shm *params-mmap-nodes))


(de init-mmap-atomic-counter ()
    (init-shm *shm-atomic-counter))


#{
(de init-mmap-draw-indirect ()
    (let (shm (init-shm *shm-draw-indirect))
      (with-slots (ptr size)
	  shm
	(let ((data (make-array 5
				:element-type '(unsigned-byte 32)
				:initial-contents (list 6 *inst-max* 0 0 0))))
	  (dotimes (i (length data))
	    (setf (mem-aref ptr ::uint i)
		  (aref data i)))))
      shm))


(de init-mmap-element ()
    (let (shm (init-shm *shm-element))
      (with-slots (ptr size)
	  shm
	(let ((data (make-array 6
				:element-type '(unsigned-byte 32)
				:initial-contents (list 0 2 1 0 3 2))))
	  (dotimes (i (length data))
	    (setf (mem-aref ptr ::uint i)
		  (aref data i)))))
      shm))


(de init-mmap-projview ()
    (let (shm (init-shm *shm-projview))
      (setf *shm-projview* shm) ; below require this
      (update-mat-proj)
      (update-mat-view)
      (copy-mat-proj-to-shm)
      (copy-mat-view-to-shm)
      shm))


(de init-mmap-texture-glyphs ()

    ;; Could convert lisp data straight to bytes...
    ;; Load into textures for now...

    (let (shm (init-shm *shm-texture-glyphs))
      (with-slots (ptr size)
	  shm
	(loop
	   :for code :from 1 :to 255
	   :with msdf-glyphs-path := (merge-pathnames #P"src-cl/glyphs/" (asdf:system-source-directory :protoform))
	   :with i := 0
	   :for lisp-path := (merge-pathnames (make-pathname :name (format nil "~a-data" (write-to-string code))
							     :type "lisp")
					      msdf-glyphs-path)
	   :do (loop 
		  :for c :across (read-from-string (read-file-string lisp-path))
		  :do (progn
			(setf (mem-aref ptr :unsigned-char i) c)
			(incf i)))))
      shm))


(de init-mmap-vertices ()
    ;; top right, bottom right, bottom left, top left
    ;;
    ;; 3---0
    ;; | / |
    ;; 2---1
    ;;
    ;; ccw: 0 2 1 0 3 2
    (let (shm (init-shm '*shm-vertices))
      (with-slots (ptr size)
	  shm
	(let ((data (make-array (* 4 4)
				:element-type 'single-float
				:initial-contents (list 1.0  1.0  0.0  1.0
							1.0  0.0  0.0  1.0
							0.0  0.0  0.0  1.0
							0.0  1.0  0.0  1.0))))
	  (dotimes (i (length data))
	    (setf (mem-aref ptr :float i)
		  (aref data i)))))
      shm))
}#
