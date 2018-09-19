(in-package :protoform.model)

(defun init-mapping-base (inst-max
			  mapping-base
			  paths)
  (dolist (boa paths)
    (let* ((name (first boa))
	   (path (second boa))
	   (size (third boa))
	   (mmap (init-mmap path
			    size
			    t ; create - replace these types with symbols
			    :data (make-array size
					      :element-type '(unsigned-byte 8)
					      :initial-element (coerce 0 '(unsigned-byte 8))))))
      (setf (gethash name mapping-base) mmap)
      (format t "[init-mapping-base] shm-mmap: ~a, ~a bytes~%" path size))))

;; TODO: Move to external file gltf JSON
(defun init-vector-position (n)
  (make-array (* 4 4 n) :element-type 'single-float
	      ;; top right, bottom right, bottom left, top left
	      ;;
	      ;; 3---0
	      ;; | / |
	      ;; 2---1
	      ;;
	      ;; ccw: 0 2 1 0 3 2
	      :initial-contents (list 1.0  1.0  0.0  1.0
				      1.0  0.0  0.0  1.0
				      0.0  0.0  0.0  1.0
				      0.0  1.0  0.0  1.0)))


(defun set-matrix (ptr-dest matrix-src offset)
  (let ((matrix-arr (marr (mtranspose matrix-src))))
    (dotimes (i 16)
      (setf (mem-aref ptr-dest :float (+ offset i))
	    (aref matrix-arr i)))))

;; Do not draw control characters
;; They stay in the skip list but are not copied

;; Should do char and display char

;; Order only needed for layout calculation

;; ((char-equal chr #\Newline)
;;  ;; After new chr, move down, reset x for next char
;;  ;; Treat it like a space...
;;  (setf (vx3 cursor) 0)
;;  (decf (vy3 cursor) (* 9.375 2.0 scale-glyph)))

(defun serialize-chrs (mapping-base
		       sl-chrs
		       metrics
		       dpi-glyph
		       scale-glyph)

  (let* ((metrics-space (gethash 32 metrics))
	 (uv-space (uv metrics-space))
	 (offset-ptr 0)
	 (cursor (vec3 0.0 0.0 0.0)))
    
    (with-slots (ptr size)
	(gethash "instance" mapping-base)

      (skip-list:doskiplist
       (sl-chr sl-chrs)

       (with-slots (chr
		    model-matrix
		    rgba
		    flags)
	   sl-chr

	 (cond ((char-equal chr #\Tab)
	        t)

	       ;; ascii
	       (t
		(when (and (char-equal chr #\Newline) t)
		  ;; After new chr, move down, reset x for next char
		  ;; Treat it like a space...
		  (setf (vx3 cursor) 0)
		  (decf (vy3 cursor) (* 9.375 2.0 scale-glyph)))
		
		(loop
		   :for c :across (marr (matrix model-matrix))
		   :for c-i :upfrom 0
		   :do (setf (mem-aref ptr :float (+ offset-ptr c-i))
			     c))
		(incf offset-ptr 16)
		
		(loop
		   :for c :across rgba
		   :for c-i :upfrom 0
		   :do (setf (mem-aref ptr :float (+ offset-ptr c-i))
			     c))
		(incf offset-ptr 16)
		
		;; (u v s t) * 4
		(loop
		   :for c :across (if (char-equal chr #\Newline)
				      uv-space
				      (uv (gethash (char-code chr) metrics)))
		   :for c-i :upfrom 0
		   :do (setf (mem-aref ptr :float (+ offset-ptr c-i))
			     c))
		(incf offset-ptr 16)
		
		;; Glyph, Flags, pad, pad
		(setf (mem-aref ptr :int (+ offset-ptr 0)) (- (char-code chr) 32))
		(setf (mem-aref ptr :int (+ offset-ptr 1)) flags)
		(incf offset-ptr 4))))))))
