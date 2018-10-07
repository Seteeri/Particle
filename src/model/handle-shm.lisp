(in-package :protoform.model)

(defun init-handle-shm (handles-shm
			params-shm)
  (dolist (params params-shm)
    (destructuring-bind (target name path size bind-cs bind-vs &rest rest) params
      ;; Is data needed persistently?
      (let ((mmap (init-mmap path
			     size
			     t ; create - replace these types with symbols
			     :data (make-array size
					       :element-type '(unsigned-byte 8)
					       :initial-element (coerce 0 '(unsigned-byte 8))))))
	(setf (gethash name handles-shm) mmap)
	(fmt-model t "init-mapping-base" "shm-mmap: ~a, ~a bytes~%" path size)))))

(defun set-matrix (ptr-dest matrix-src offset)
  (let ((matrix-arr (marr (mtranspose matrix-src))))
    (dotimes (i 16)
      (setf (mem-aref ptr-dest :float (+ offset i))
	    (aref matrix-arr i)))))
