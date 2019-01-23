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
   (bounds-origin
    :accessor bounds-origin
    :initarg :bounds-origin
    :initform (make-array 4
			  :adjustable nil
			  :fill-pointer nil
			  :element-type 'single-float ; l b r t
			  :initial-contents (list 0.0 0.0 9.375 9.375))
    :documentation "")
   (bounds-texture
    :accessor bounds-texture
    :initarg :bounds-texture
    :initform (make-array 4
			  :adjustable nil
			  :fill-pointer nil
			  :element-type 'single-float
			  :initial-contents (list 0.0 0.0 9.375 9.375))
    :documentation "")
   (scale
    :accessor scale
    :initarg :scale
    :initform +scale-msdf+
    :documentation "") 
   (dims-glyph
    :accessor dims-glyph
    :initarg :dims-glyph
    :initform (vec2 0.0 0.0)
    :documentation "")
   (ratio-aspect ; ratio aspect of dims-glyph
    :accessor ratio-aspect
    :initarg :ratio-aspect
    :initform 0.0
    :documentation "")
   (scale-uv
    :accessor scale-uv
    :initarg :scale-uv
    :documentation "")
   (uv
    :accessor uv
    :initarg :uv
    :initform (make-array 16
			  :adjustable nil
			  :fill-pointer nil
			  :element-type 'single-float
			  :initial-contents (list 1.0 1.0 0.0 0.0
						  1.0 0.0 0.0 0.0
						  0.0 0.0 0.0 0.0
						  0.0 1.0 0.0 0.0))
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
     :with scale := +scale-msdf+ ; taken from script
     :with i := 0
     :do (destructuring-bind (&key code advance translate range (bounds nil))
	     (eval (read-from-string (read-file-string lisp-path)))

	   ;; Invalid if no bounds?
	   ;; Bounds is referred to as bearings by freetype

	   (let ((metrics (make-instance 'metrics
					 :advance advance
					 :translate (vec2 (aref translate 0)
							  (aref translate 1))
					 :range range
					 :scale scale)))

	     (setf (gethash code ht-metrics) metrics)
	     
	     (with-slots (bounds-origin ; rel-to-origin bounds in pixel
			  bounds-texture   ; rel-to-texture bounds in pixel
			  dims-glyph
			  ratio-aspect
			  uv)
		 metrics
	       (when bounds
		 (setf bounds-origin (make-array 4
						 :adjustable nil
						 :fill-pointer nil
						 :element-type 'single-float
						 :initial-contents (loop :for x :across bounds
								      :collect (* x scale))))
		 (setf bounds-texture (calculate-bounds translate bounds scale))
		 (setf dims-glyph (calculate-dims bounds-texture))
		 (setf ratio-aspect (calculate-ar dims-glyph))
		 (setf uv (calculate-uv bounds-texture 96))))))

     :finally (return ht-metrics)))

(defun calculate-bounds (baseline bounds scale)
  ;; Calculate bounds (pixel and absolute values)
  (let* ((baseline-abs (loop :for x :across baseline :collect (* x scale))) ; bounds-shape
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
  ;; - dims + baseline coords + bounds -> calculate glyph bounds in abs px coords
  ;; - bounds = l b r t
  ;;            0 1 2 3
  ;; - origin (0,0) is considered bottom left corner so (1,1) is top right corner
  ;; - Shape->Pixels = multiply by +scale-msdf+
  ;;
  ;; bounds-abs:
  ;; baseline.x - bounds.l
  ;; baseline.y - bounds.b
  ;; baseline.x + bounds.r
  ;; baseline.y + bounds.t  

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
