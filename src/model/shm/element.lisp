(in-package :protoform.model)

(defparameter *shm-element* nil)

(defparameter *params-element-shm* (list :element-array-buffer
					 "/protoform-element"
					 "/protoform-element"
					 (* 4 6)  ; 4 bytes/int * 6 ints or indices
					 -1 -1
					 :triple
					 0))

(defun init-shm-element ()
  (let ((shm (init-shm '*shm-element*)))
    (with-slots (ptr size)
	shm
      (let ((data (make-array 6
      			      :element-type '(unsigned-byte 32)
      			      :initial-contents (list 0 2 1 0 3 2))))
	(dotimes (i (length data))
	  (setf (mem-aref ptr ::uint i)
		(aref data i)))))
    shm))
