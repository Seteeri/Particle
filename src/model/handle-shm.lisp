(in-package :protoform.model)

(defun init-shm ()
  (init-handle-shm (handles-shm *model*)
		   *params-shm*)
  ;; instance and texture is done later
  (copy-projview-to-shm nil)
  (copy-shm-vertices)
  (copy-shm-element)
  (copy-shm-draw-indirect))

(defun init-handle-shm (handles-shm
			params-shm)
  (dolist (params params-shm)
    (destructuring-bind (target
			 name
			 path
			 size
			 bind-cs
			 bind-vs
			 count-buffer
			 flag-copy
			 &rest rest)
	params
      (let ((mmap (init-mmap path
			     size
			     t ; create - replace these types with symbols
			     :data (make-array size
					       :element-type '(unsigned-byte 8)
					       :initial-element (coerce 0 '(unsigned-byte 8))))))
	(setf (gethash name handles-shm) mmap)
	(fmt-model t "init-handle-shm" "~S, ~S bytes~%" path size)))))

(defun clean-up-handles-shm ()
  (loop 
     :for key :being :the :hash-keys :of (handles-shm *model*)
     :using (hash-value mmap)
     :do (cleanup-mmap mmap)))


;; TODO:
;; For below, abstract out the data so can generate dynamically
;; DO same for projview?

(defun copy-shm-vertices ()
  ;; top right, bottom right, bottom left, top left
  ;;
  ;; 3---0
  ;; | / |
  ;; 2---1
  ;;
  ;; ccw: 0 2 1 0 3 2    
  (with-slots (ptr size)
      (gethash "vertices" (handles-shm *model*))
    (let ((data (make-array (* 4 4)
			    :element-type 'single-float
			    :initial-contents (list 1.0  1.0  0.0  1.0
						    1.0  0.0  0.0  1.0
						    0.0  0.0  0.0  1.0
						    0.0  1.0  0.0  1.0))))
      (dotimes (i (length data))
	(setf (mem-aref ptr :float i)
	      (aref data i))))))

(defun copy-shm-element ()
  (with-slots (ptr size)
      (gethash "element" (handles-shm *model*))
    (let ((data (make-array 6
      			    :element-type '(unsigned-byte 32)
      			    :initial-contents (list 0 2 1 0 3 2))))
      (dotimes (i (length data))
	(setf (mem-aref ptr ::uint i)
	      (aref data i))))))

(defun copy-shm-draw-indirect ()
  (with-slots (ptr size)
      (gethash "draw-indirect" (handles-shm *model*))
    (let ((data (make-array 5
      			    :element-type '(unsigned-byte 32)
      			    :initial-contents (list 6 (inst-max *model*) 0 0 0))))
      (dotimes (i (length data))
	(setf (mem-aref ptr ::uint i)
	      (aref data i))))))
