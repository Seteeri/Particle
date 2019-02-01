(in-package :protoform.model)

(defconstant +size-struct-instance+ 208)
(defconstant +scale-msdf+ 5.8239365)
(defconstant +spaces-tab+ 1)

(defparameter *dpi-glyph* (/ 1 90))
(defparameter *scale-node* 0.008)
(defparameter +linegap+ (* (* 9.375 2)    ;; use advance....
			   +scale-msdf+))  ;; multiplied by scale-node later (per node)

(defparameter *uv-default-node* (list 1.0 1.0  0.0 0.0
			  	      1.0 0.0  0.0 0.0
			  	      0.0 0.0  0.0 0.0
			  	      0.0 1.0  0.0 0.0))

(defparameter *color-default-node* (list  (coerce (/ 131 255) 'single-float)
					  (coerce (/ 148 255) 'single-float)
					  (coerce (/ 155 255) 'single-float)
					  (coerce (/ 255 255) 'single-float)

					  (coerce (/ 131 255) 'single-float)
					  (coerce (/ 148 255) 'single-float)
					  (coerce (/ 155 255) 'single-float)
					  (coerce (/ 255 255) 'single-float)

					  (coerce (/ 131 255) 'single-float)
					  (coerce (/ 148 255) 'single-float)
					  (coerce (/ 155 255) 'single-float)
					  (coerce (/ 255 255) 'single-float)

					  (coerce (/ 131 255) 'single-float)
					  (coerce (/ 148 255) 'single-float)
					  (coerce (/ 155 255) 'single-float)
					  (coerce (/ 255 255) 'single-float)))

(defparameter *data-zero-node* (make-array +size-struct-instance+
					   :adjustable nil
					   :fill-pointer nil
					   :element-type '(unsigned-byte 8)
					   :initial-element (coerce 0 '(unsigned-byte 8))))
;; :data (make-array size
;; 		  :element-type '(unsigned-byte 8)
;; 		  :initial-element (coerce 0 '(unsigned-byte 8)))

(defclass node ()
  ((data :accessor data :initarg :data :initform nil) ; data-glyph/char
   (data-2 :accessor data-2 :initarg :data-2 :initform nil) ; data object
   
   (offset-texel-texture :accessor offset-texel-texture :initarg :offset-texel-texture :initform 0)
   (dims-texture :accessor dims-texture :initarg :dims-texture :initform (vec2 0 0))
   (uv :accessor uv :initform (make-array 16
					  :adjustable nil
					  :fill-pointer nil
					  :element-type 'single-float
					  :initial-contents *uv-default-node*))
   
   (index :accessor index :initarg :index :initform nil)
   (origin :accessor origin :initarg :origin :initform (vec3 0 0 0))
   (model-matrix :accessor model-matrix :initarg :model-matrix :initform (make-instance 'model-matrix))
   (rgba :accessor rgba :initarg :rgba :initform (make-array (* 4 4) ; or use vec4
							     :adjustable nil
							     :fill-pointer nil
							     :element-type 'single-float
							     :initial-contents *color-default-node*))
   (flags :accessor flags :initarg :flags :initform 1)))
