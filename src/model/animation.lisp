(in-package #:protoform.model)

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

(defclass animation ()
  ((fn :accessor fn :initarg :fn :initform nil)
   (value-start :accessor value-start :initarg :value-start :initform nil)
   (time-start :accessor time-start :initarg :time-start :initform nil)
   (time-end :accessor time-end :initarg :time-end :initform nil)
   (time-duration :accessor time-duration :initarg :time-duration :initform nil)
   (time-elapsed :accessor time-elapsed :initarg :time-elapsed :initform nil)))

;; What to do if object is destroyed, this must be destroyed also
;; Which means attach animations to the object (node) so they can be tracked
;; On delete, stop/remove animations and set valid property
;; Give node valid property so anim can check it

(defun ease-camera-x-callback (seq-key ptree queue)
  
  ;; Create animation instance and pass instance continuously
  
  ;; time-end:       (+ *time-start* 4)         ; (/ frame count fps)
  ;; time-duration: (- *time-end* *time-start*) ; (/ frame-count fps)

  (fmt-model t "ease-camera-x" "~a~%" seq-key)
  
  (let ((anim (make-instance 'animation
			     :fn #'easing:in-cubic
			     :value-start (vx3 (pos *projview*))
			     :time-start (osicat:get-monotonic-time)
			     :time-elapsed 0.0)))
    (with-slots (time-start
		 time-end
		 time-duration)
	anim
      (setf time-end (+ time-start 4))
      (setf time-duration (- time-end time-start)))
    
    (ptree-fn 'ease-camera-x
	      '()
	      (lambda ()
		(funcall #'ease-camera-x-2
			 seq-key
			 anim))
	      ptree))
  
  (sb-concurrency:enqueue 'ease-camera-x
			  queue))

(defun ease-camera-x-2 (seq-event anim)

  (with-slots (fn
	       value-start
	       time-start
	       time-end
	       time-duration
	       time-elapsed)
      anim
    
    (let* ((time-now (osicat:get-monotonic-time))
	   (time-delta (- time-now time-start)))
      
      (incf time-elapsed time-delta)
      
      ;; (when nil
      ;; 	(format t "~4$ { ~4$ } ~4$ (~4$) [~4$] ~%"
      ;; 		time-start time-elapsed time-end time-duration time-delta)
      ;; 	(format t "  ~7$~%" (osicat:get-monotonic-time)))
      
      (with-slots (pos scale-ortho)
      	  *projview*
	
	;; normalize x/time elapsed by dividing over duration
	;; normalize y by multiplying by displacement
	;; add to begin value to get new value otherwise its just relative displacement from beginning
	
	(let ((pos-new (+ value-start
			  (* (funcall fn
				      (/ time-elapsed time-duration))
      			     -4.0))))

	  ;; (format t "t = ~a, y = ~a~%"
	  ;;         (/ *time-elapsed* *time-duration*)
	  ;;         (easing:out-exp (/ *time-elapsed* *time-duration*)))
	  
      	  (setf (vx3 pos) pos-new)))
      
      (update-mat-view)
      (enqueue-mat-view)
      
      ;; Cap time-delta to ending time
      (if (> time-elapsed time-duration)
	  (fmt-model t "ease-camera-x" "Ending anim~%")
	  (enqueue-anim seq-event
			anim)))))

(defun enqueue-anim (seq-event anim)
  
  (fmt-model t "enqueue-anim" "~a~%" seq-event)
  
  (sb-concurrency:enqueue (list 'ease-camera-x
				'()
				(lambda ()
				  (funcall #'ease-camera-x-2 seq-event
					   anim)))
			  *queue-anim*))
