(in-package :protoform.model)

(defparameter *shm-atomic-counter* nil)

(defparameter *params-atomic-counter-shm* (list :atomic-counter-buffer
						"/protoform-atomic-counter"
						(* 4 6)  ; 6 ints/params
						4 -1
						:triple
						0))

(defun init-shm-atomic-counter ()
  (init-shm '*shm-atomic-counter*))
