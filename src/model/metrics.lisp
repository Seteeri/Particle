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
     :with msdf-glyphs-path := (merge-pathnames #P"glyphs-msdf/" (asdf:system-source-directory :protoform))
     :for lisp-path := (merge-pathnames (make-pathname :name (format nil "~a-metrics" (write-to-string code))
						       :type "lisp")
					msdf-glyphs-path)
     :with i := 0
     :do (destructuring-bind (&key code advance translate range (bounds nil))
	     (eval (read-from-string (read-file-string lisp-path)))
	   (setf (gethash code ht-metrics)
		 (make-instance 'metrics
				:advance advance
				:translate (vec2 (aref translate 0)
						 (aref translate 1))
				:range range
				:bounds bounds
				:scale 5.8239365
				:dims-glyph (vec2 96 96)
				:ratio-aspect (/ 96 96))))
     :finally (return ht-metrics)))
