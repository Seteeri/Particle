(in-package #:protoform.model)

;; https://gamedev.stackexchange.com/questions/48227/smooth-movement-pygame

;; (:export :linear
;; 	 :in-sine :out-sine :in-out-sine
;; 	 :in-cubic :out-cubic :in-out-cubic
;; 	 :in-quad :out-quad :in-out-quad
;; 	 :in-quart :out-quart :in-out-quart
;; 	 :in-quint :out-quint :in-out-quint
;; 	 :in-exp :out-exp :in-out-exp
;; 	 :in-circ :out-circ :in-out-circ
;; 	 :in-elastic :out-elastic :in-out-elastic
;; 	 :in-back :out-back :in-out-back
;; 	 :in-bounce :out-bounce :in-out-bounce)

(defparameter *value-start* 0)
(defparameter *time-start* 0)
(defparameter *time-end* 0)
(defparameter *time-duration* 0)
(defparameter *time-elapsed* 0)
(defparameter *time-run* nil)
(defparameter *fn-anim* nil)

(defparameter *time-last* 0)

(defun move-x ()
  
  (when *time-run*
    
    (let* ((time-now (osicat:get-monotonic-time))
	   (time-delta (- time-now *time-start*)))

      (incf *time-elapsed* time-delta)

      (when nil
	(format t "~4$ { ~4$ } ~4$ (~4$) [~4$] ~%" *time-start* *time-elapsed* *time-end* *time-duration* time-delta)
	(format t "  ~7$~%" (osicat:get-monotonic-time)))

      (with-slots (pos scale-ortho)
      	  *projview*

	;; normalize x/time elapsed by dividing over duration
	;; normalize y by multiplying by displacement
	;; add to begin value to get new value otherwise its just relative displacement from beginning

	(let ((pos-new (+ *value-start*
			  (* (funcall *fn-anim* (/ *time-elapsed* *time-duration*))
      			     -4.0))))
	  ;; (format t "t = ~a, y = ~a~%" (/ *time-elapsed* *time-duration*) (easing:out-exp (/ *time-elapsed* *time-duration*)))
      	  (setf (vx3 pos) pos-new))
	
	t)
      
      (copy-projview-to-shm)
      
      ;; Cap time-delta to ending time
      (when (> *time-elapsed* *time-duration*)
	(fmt-model t "handle-view-sync" "Ending anim~%")
	(setf *time-run* nil)))))
