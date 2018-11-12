(in-package #:protoform.model)

;; Setup anim params class

(defparameter *value-start* 0)
(defparameter *time-start* 0)
(defparameter *time-end* 0)
(defparameter *time-duration* 0)
(defparameter *time-elapsed* 0)
(defparameter *fn-anim* nil)

;; :linear
;; :in-sine :out-sine :in-out-sine
;; :in-cubic :out-cubic :in-out-cubic
;; :in-quad :out-quad :in-out-quad
;; :in-quart :out-quart :in-out-quart
;; :in-quint :out-quint :in-out-quint
;; :in-exp :out-exp :in-out-exp
;; :in-circ :out-circ :in-out-circ
;; :in-elastic :out-elastic :in-out-elastic
;; :in-back :out-back :in-out-back
;; :in-bounce :out-bounce :in-out-bounce

(defun ease-camera-x (seq-event)
      
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

    (update-mat-view)

    (let ((arr-view (marr (mtranspose (mat-view *projview*)))))
      (sb-concurrency:enqueue (list *shm-projview*
				    (pack:pack "<16f"
					       (aref arr-view 0)  (aref arr-view 1)  (aref arr-view 2)  (aref arr-view 3)
					       (aref arr-view 4)  (aref arr-view 5)  (aref arr-view 6)  (aref arr-view 7)
					       (aref arr-view 8)  (aref arr-view 9)  (aref arr-view 10) (aref arr-view 11)
					       (aref arr-view 12) (aref arr-view 13) (aref arr-view 14) (aref arr-view 15))
				    (* 16 4))
			      *queue-view*))
    
    ;; Cap time-delta to ending time
    (if (> *time-elapsed* *time-duration*)
	(progn
	  (fmt-model t "handle-view-sync" "Ending anim~%")
	  (setf *time-run* nil))
	(progn
	  ;; Push to other queue for next frame or anim will complete same frame
	  (sb-concurrency:enqueue (list #'ease-camera-x
	    				seq-event)
	    			  (if (eq *queue-input* *queue-front*)
  				      *queue-back*
  				      *queue-front*))))))
