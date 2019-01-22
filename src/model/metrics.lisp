(in-package :protoform.model)

(defclass metrics ()
  ((advance
    :accessor advance
    :initarg :advance
    :documentation "")  
   (translate
    :accessor translate
    :initarg :translate
    :initform (vec2 0.0 0.0)
    :documentation "")
   (range
    :accessor range
    :initarg :range
    :documentation "")
   (bounds
    :accessor bounds
    :initarg :bounds
    :initform (make-array 4
			  :adjustable nil
			  :fill-pointer nil
			  :element-type 'single-float
			  :initial-element 0.0)
    :documentation "")
   (scale
    :accessor scale
    :initarg :scale
    :initform 5.8239365
    :documentation "") 
   (dims-glyph ; rename to dim-glyph
    :accessor dims-glyph
    :initarg :dims-glyph
    :documentation "")
   (ratio-aspect
    :accessor ratio-aspect
    :initarg :ratio-aspect
    :documentation "")
   (scale-uv
    :accessor scale-uv
    :initarg :scale-uv
    :documentation "")
   (uv
    :accessor uv
    :initarg :uv
    :documentation "")))

(defun init-metrics ()
  (loop
     :for code :from 1 :to 255
     :with ht-metrics := (make-hash-table :size 255)
     :with msdf-glyphs-path := (merge-pathnames #P"glyphs-msdf/"
						(asdf:system-source-directory :protoform))
     :for lisp-path := (merge-pathnames (make-pathname :name (format nil "~a-metrics" (write-to-string code))
						       :type "lisp")
					msdf-glyphs-path)
     :with scale := 5.8239365 ; taken from script
     :with i := 0
     :do (destructuring-bind (&key code advance translate range (bounds nil))
	     (eval (read-from-string (read-file-string lisp-path)))
	   ;; Invalid if no bounds?
	   (format t "HERE: ~S ~S ~S ~S~%" code advance translate range)
	   (setf (gethash code ht-metrics)
		 (make-instance 'metrics
				:advance advance
				:translate (vec2 (aref translate 0)
						 (aref translate 1))
				:range range
				:bounds (if bounds
					    (calculate-bounds translate bounds scale)
					    #(0.0 0.0 0.0 0.0))
				:scale scale
				:dims-glyph (if bounds
						(calculate-dims (calculate-bounds translate bounds scale))
						(vec2 0.0 0.0))
				:ratio-aspect (if bounds
						  (calculate-ar (calculate-dims (calculate-bounds translate bounds scale)))
						  0.0)
				:uv (if bounds
					(calculate-uv (calculate-bounds translate bounds scale) 96)
					(make-array 16
						    :adjustable nil
						    :fill-pointer nil
						    :element-type 'single-float
						    :initial-contents (list 1.0 1.0 0.0 0.0
									    1.0 0.0 0.0 0.0
									    0.0 0.0 0.0 0.0
									    0.0 1.0 0.0 0.0))))))
     :finally (return ht-metrics)))

(defun calculate-bounds (baseline bounds scale)
  ;; Calculate bounds (pixel and absolute values)
  (let* ((baseline-abs (loop :for x :across baseline :collect (* x scale)))
	 (bounds-rel   (loop :for x :across bounds   :collect (* x scale))))
    ;; l b r t
    (list (+ (nth 0 baseline-abs) (nth 0 bounds-rel))
	  (+ (nth 1 baseline-abs) (nth 1 bounds-rel))
	  (+ (nth 0 baseline-abs) (nth 2 bounds-rel))
	  (+ (nth 1 baseline-abs) (nth 3 bounds-rel)))))

(defun calculate-dims (bounds)
  ;; Assumes bounds is in pixels
  (vec2 (- (nth 2 bounds) (nth 0 bounds))
	(- (nth 3 bounds) (nth 1 bounds))))

(defun calculate-ar (dims)
  (/ (vx2 dims) (vy2 dims)))
  
(defun calculate-uv (bounds dim)
  ;; Assumes square dimensions
  (let* ((uv-bounds (loop :for x :in bounds :collect (/ x dim))))
    (make-array 16
		:adjustable nil
		:fill-pointer nil
		:element-type 'single-float
		:initial-contents (list (nth 2 uv-bounds) (nth 3 uv-bounds) 0.0 0.0   ; r-t (1,1)
					(nth 2 uv-bounds) (nth 1 uv-bounds) 0.0 0.0   ; r-b (1,0)
					(nth 0 uv-bounds) (nth 1 uv-bounds) 0.0 0.0   ; l-b (0,0)
					(nth 0 uv-bounds) (nth 3 uv-bounds) 0.0 0.0)))) ; l-t (0,1)

;; (defun calculate-uv (baseline bounds scale)
;;   ;; Calculate UV coords
;;   ;; - dims + baseline coords + bounds -> calculate glyph bounds in abs px coords
;;   ;; - bounds = l b r t
;;   ;;            0 1 2 3
;;   ;; - origin (0,0) is considered bottom left corner so (1,1) is top right corner
;;   ;; - Shape->Pixels = multiply by 5.8239365
;;   ;;
;;   ;; bounds-abs:
;;   ;; baseline.x - bounds.l
;;   ;; baseline.y - bounds.b
;;   ;; baseline.x + bounds.r
;;   ;; baseline.y + bounds.t  
;;   (let* ((baseline-abs (loop :for x :across baseline :collect (* x scale)))
;; 	 (bounds-rel   (loop :for x :across bounds   :collect (* x scale)))
;; 	 (bounds-abs   (list (+ (nth 0 baseline-abs) (nth 0 bounds-rel)) ;l b r t
;; 	   		     (+ (nth 1 baseline-abs) (nth 1 bounds-rel))
;; 	   		     (+ (nth 0 baseline-abs) (nth 2 bounds-rel))
;; 	   		     (+ (nth 1 baseline-abs) (nth 3 bounds-rel))))
;; 	 (uv-bounds   (loop :for x :in bounds-abs :collect (/ x 96)))
;; 	 (uv          (make-array 16
;; 				  :adjustable nil
;; 				  :fill-pointer nil
;; 				  :element-type 'single-float
;; 				  :initial-contents (list (nth 2 uv-bounds) (nth 3 uv-bounds) 0.0 0.0   ; r-t (1,1)
;; 							  (nth 2 uv-bounds) (nth 1 uv-bounds) 0.0 0.0   ; r-b (1,0)
;; 							  (nth 0 uv-bounds) (nth 1 uv-bounds) 0.0 0.0   ; l-b (0,0)
;; 							  (nth 0 uv-bounds) (nth 3 uv-bounds) 0.0 0.0)))) ; l-t (0,1)
;;     ;; (format t "~S (bounds/UV): ~S~%        ~S~%" code bounds-abs uvs)
;;     uv))
