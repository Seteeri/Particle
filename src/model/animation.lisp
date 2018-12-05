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

(defun ease-camera-x-callback (seq-key ptree queue)
  
  ;; Should not be global!
  (setf *fn-anim* #'easing:in-cubic
	*value-start* (vx3 (pos *projview*))
	*time-start* (osicat:get-monotonic-time)
	*time-end* (+ *time-start* 4) ; (/ frame count fps)
	*time-duration* (- *time-end* *time-start*) ; (/ frame-count fps)
	*time-elapsed* 0.0)

  (fmt-model t "ease-camera-x" "~a~%" seq-key)

  (ptree-fn 'ease-camera-x
	    '()
	    (lambda ()
	      (funcall #'ease-camera-x-2 seq-key))
	    ptree)
  
  (sb-concurrency:enqueue 'ease-camera-x
			  queue))

(defun ease-camera-x-2 (seq-event)
      
  (let* ((time-now (osicat:get-monotonic-time))
	 (time-delta (- time-now *time-start*)))

    (incf *time-elapsed* time-delta)

    (when nil
      (format t "~4$ { ~4$ } ~4$ (~4$) [~4$] ~%"
	      *time-start* *time-elapsed* *time-end* *time-duration* time-delta)
      (format t "  ~7$~%" (osicat:get-monotonic-time)))

    (with-slots (pos scale-ortho)
      	*projview*

      ;; normalize x/time elapsed by dividing over duration
      ;; normalize y by multiplying by displacement
      ;; add to begin value to get new value otherwise its just relative displacement from beginning

      (let ((pos-new (+ *value-start*
			(* (funcall *fn-anim*
				    (/ *time-elapsed* *time-duration*))
      			   -4.0))))
	;; (format t "t = ~a, y = ~a~%" (/ *time-elapsed* *time-duration*) (easing:out-exp (/ *time-elapsed* *time-duration*)))
      	(setf (vx3 pos) pos-new))
      
      t)

    (update-mat-view)
    (enqueue-mat-view)
    
    ;; Cap time-delta to ending time
    (if (> *time-elapsed* *time-duration*)
	(progn
	  (fmt-model t "ease-camera-x" "Ending anim~%")
	  (setf *time-run* nil))
	(progn
	  ;; Push to queue
	  (enqueue-anim seq-event)))))

(defun enqueue-anim (seq-event)
    
  (fmt-model t "enqueue-anim" "~a~%" seq-event)
  
  (sb-concurrency:enqueue (list 'ease-camera-x
				'()
				(lambda ()
				  (funcall #'ease-camera-x-2 seq-event)))
			  *queue-anim*))
