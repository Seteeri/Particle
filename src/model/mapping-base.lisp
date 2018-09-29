(in-package :protoform.model)

(defun init-mapping-base (mapping-base
			  params-shm)
  (dolist (params params-shm)
    (destructuring-bind (target name path size bind) params
      ;; Is data needed persistently?
      (let ((mmap (init-mmap path
			     size
			     t ; create - replace these types with symbols
			     :data (make-array size
					       :element-type '(unsigned-byte 8)
					       :initial-element (coerce 0 '(unsigned-byte 8))))))
	(setf (gethash name mapping-base) mmap)
	(fmt-model t "init-mapping-base" "shm-mmap: ~a, ~a bytes~%" path size)))))
  
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
