(in-package :protoform.controller)

(defparameter *color-chr-default* (make-array 4
					      :adjustable nil
					      :fill-pointer nil
					      :element-type 'single-float
					      :initial-contents (list  (coerce (/ 131 255) 'single-float)
								       (coerce (/ 148 255) 'single-float)
								       (coerce (/ 155 255) 'single-float)
								       (coerce (/ 255 255) 'single-float))))

; yellow
(defparameter *color-chr-selected* #(1.0 1.0 0.0 1.0
				     1.0 1.0 0.0 1.0
				     1.0 1.0 0.0 1.0
				     1.0 1.0 0.0 1.0))

(defun handle-ascii (controller keysym)
  ;; Do this check when registering
  ;; Ignore zoom: +/-
  (when (or (= keysym 45)
	    (= keysym 61))
    (return-from handle-ascii))
  ;; (format t "[handle-ascii] Insert ~a, ~a~%" keysym (code-char keysym))  
  (request-insert (conn-model controller)
		  keysym
		  nil)
  (request-modify-rgba (conn-model controller)
		       0
		       *color-chr-selected*
		       nil))

(defun handle-enter (controller keysym)
  ;; Implicitly insert at cursor
  (request-insert (conn-model controller)
		  (char-code #\Newline)
		  nil))

(defun handle-space (msdf keysym)
  ;; Implicitly insert at cursor
  (request-insert (conn-model controller)
		  (char-code #\Space)
		  nil)
  (request-modify-rgba (conn-model controller)
		       0
		       *color-chr-selected*
		       nil))

(defun handle-delete (controller keysym)
  ;; Delete at cursor
  ;; Relative to cursor...or pass coords
  (request-delete (conn-model controller)
		  -1
		  nil)
  (request-modify-rgba (conn-model controller)
		       0
		       *color-chr-selected*
		       nil))

(defun handle-backspace (controller keysym)
  ;; Delete behind cursor
  ;; Relative to cursor...or pass coords
  (request-delete (conn-model controller)
		  -2 ; offset-del
		  ;; offset-cursor
		  nil)
  (request-modify-rgba (conn-model controller)
		       0
		       *color-chr-selected*
		       nil))  

;; cardinals

;; modify cursor
;; change color

(defun handle-up (controller keysym)
  t)

(defun handle-down (controller keysym)
  t)

(defun handle-left (controller keysym)
  t)

(defun handle-right (controller keysym)
  t)

;; (defun handle-left (msdf keysym)
;;   (setf (vx2 (cursor msdf))
;; 	(max 1 (- (vx2 (cursor msdf)) 1)))

;; (defun handle-right (msdf keysym)
;;   (incf (vx2 (cursor msdf)))
