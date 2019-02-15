(in-package :protoform.model)

(defparameter *shm-vertices* nil)

(defparameter *params-vertices-shm* (list :uniform-buffer
					  "/protoform-vertices"
					  (* 16 4)
					  1 1
					  :triple
					  0))

(defun init-shm-vertices ()
  ;; top right, bottom right, bottom left, top left
  ;;
  ;; 3---0
  ;; | / |
  ;; 2---1
  ;;
  ;; ccw: 0 2 1 0 3 2
  (let ((shm (init-shm '*shm-vertices*)))
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
