(in-package :protoform.model)

(defparameter *shm-draw-indirect* nil)

(defparameter *params-draw-indirect-shm* (list :draw-indirect-buffer
					       "/protoform-draw-indirect"
					       (* 4 6)  ; 6 ints/params
					       -1 -1
					       :triple
					       0))

(defun init-shm-draw-indirect ()
  (let ((shm (init-shm '*shm-draw-indirect*)))
    (with-slots (ptr size)
	shm
      (let ((data (make-array 5
      			      :element-type '(unsigned-byte 32)
      			      :initial-contents (list 6 *inst-max* 0 0 0))))
	(dotimes (i (length data))
	  (setf (mem-aref ptr ::uint i)
		(aref data i)))))
    shm))
