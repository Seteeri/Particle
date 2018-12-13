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
  ((object        :accessor object        :initarg :object        :initform nil)
   (slot          :accessor slot          :initarg :slot          :initform nil)
   (fn-easing     :accessor fn-easing     :initarg :fn-easing     :initform nil)
   (fn-new        :accessor fn-new        :initarg :fn-new        :initform nil)
   (value-start   :accessor value-start   :initarg :value-start   :initform nil)
   (value-delta   :accessor value-delta   :initarg :value-delta   :initform nil)
   (time-start    :accessor time-start    :initarg :time-start    :initform nil)
   (time-end      :accessor time-end      :initarg :time-end      :initform nil)
   (time-duration :accessor time-duration :initarg :time-duration :initform 1.0)
   (time-elapsed  :accessor time-elapsed  :initarg :time-elapsed  :initform nil)))

;; time-end:       (+ *time-start* 4)          ; (/ frame count fps)
;; time-duration:  (- *time-end* *time-start*) ; (/ frame-count fps)

;; What to do if object is destroyed, this must be destroyed also
;; Which means attach animations to the object (node) so they can be tracked
;; On delete, stop/remove animations and set valid property
;; Give node valid property so anim can check it

;; If animation already playing, stop existing, then interpolate...
;; Poss issue - same anims/obj in same ptree

;; Per object->slot - can run slots in parallel

(defun run-anim (seq-event anim)

  (with-slots (object
	       slot
	       fn-easing
	       fn-new
	       value-start
	       value-delta
	       time-start
	       time-end
	       time-duration
	       time-elapsed)
      anim

    ;; Set time start here instead of at callback
    (let* ((time-now (if time-start
			 (osicat:get-monotonic-time)
			 (progn
			   (setf time-start (osicat:get-monotonic-time))
			   (setf time-end (+ time-start time-duration))
			   time-start))))
      (setf time-elapsed (- time-now time-start))

      (when nil
      	(format t "{ ~4$ ~4$ ~4$ } (~4$) ~%"
      		time-start time-elapsed time-end time-duration)
      	(format t "  ~7$~%" (osicat:get-monotonic-time))
	t)
      
      (let ((value-new (+ value-start
			  (* (funcall fn-easing
				      (/ time-elapsed time-duration))
      			     value-delta))))
	(fmt-model t "run-anim" "~a -> ~a~%" (slot-value object slot) value-new)
	(funcall fn-new
		 value-new))
      
      ;; Alternative: flag shm as dirty; check during loop
      (update-mat-proj)
      (enqueue-mat-proj)
      
      ;; Cap time-delta to ending time
      (if (> time-elapsed time-duration)
	  (fmt-model t "run-anim" "Ending anim; elapsed time was ~7$~%" time-elapsed)
	  (enqueue-anim seq-event
			anim
			'run-anim-scale
			(lambda ()
			  (funcall #'run-anim
				   seq-event
				   anim)))))))

(defun enqueue-anim (seq-event
		     anim
		     id
		     fn)
  ;; (fmt-model t "enqueue-anim" "~a~%" seq-event)  
  (sb-concurrency:enqueue (list id 
				'()
				fn)
			  *queue-anim*))
