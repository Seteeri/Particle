(in-package :protoform.model)

(defclass metrics ()
  ((bounds
    :accessor bounds
    :initform (make-array 4
			  :adjustable nil
			  :fill-pointer nil
			  :element-type 'single-float
			  :initial-element 0.0)
    :documentation "")
   (advance
    :accessor advance
    :documentation "")
   (scale
    :accessor scale
    :initform 5.8239365
    :documentation "")   
   (translate
    :accessor translate
    :initform (vec2 0.0 0.0)
    :documentation "")
   (range
    :accessor range
    :documentation "")
   (dim-glyph ; rename to dim-glyph
    :accessor dim-glyph
    :documentation "")
   (ratio-aspect
    :accessor ratio-aspect
    :documentation "")
   (scale-uv
    :accessor scale-uv
    :documentation "")
   (uv
    :accessor uv
    :initform (make-array 16
			  :adjustable nil
			  :fill-pointer nil
			  :element-type 'single-float
			  :initial-contents (list 0.0 0.0 0.0 0.0
						  0.0 0.0 0.0 0.0
						  0.0 0.0 0.0 0.0
						  0.0 0.0 0.0 0.0))
    :documentation "")))

(defun init-metrics ()
  (let* ((path-glyphs-msdf (merge-pathnames #P"glyphs-msdf/" (asdf:system-source-directory :protoform)))
	 (path-metrics (merge-pathnames (make-pathname :name "metrics")
					path-glyphs-msdf))
	 (metrics (parse-metrics path-metrics)))
    
    (calculate-uvs-metrics metrics)
    
    metrics))

(defun parse-metrics (path)
  (let* ((path-glyphs-msdf (merge-pathnames #P"glyphs-msdf/" (asdf:system-source-directory :protoform)))
	 (path-metrics (merge-pathnames (make-pathname :name "metrics")
					path-glyphs-msdf))
	 (ht-metrics (make-hash-table :size 255))
	 (metrics nil)
	 (char nil))

    (with-open-file (stream path-metrics)

      ;; range is same
      ;; scale is same

      (loop 
         :for line := (read-line stream nil nil)
	 :until (eq line nil)
	 :do (when (> (length line) 0)
	       (let ((char-first (char line 0)))	       
		 (cond ((digit-char-p char-first)
			(setf char line)
			(setf metrics (make-instance 'metrics))
			(setf (gethash (parse-integer line) ht-metrics) metrics))
		       
		       ((char-equal char-first #\b)
			(let ((line-split (str:split-omit-nulls " " line)))
			  (setf (aref (bounds metrics) 0) (coerce (read-from-string (second line-split)) 'single-float))
			  (setf (aref (bounds metrics) 1) (coerce (read-from-string (third line-split)) 'single-float))
			  (setf (aref (bounds metrics) 2) (coerce (read-from-string (fourth line-split)) 'single-float))
			  (setf (aref (bounds metrics) 3) (coerce (read-from-string (fifth line-split)) 'single-float))))
		       
		       ((char-equal char-first #\a)
			(setf (advance metrics) (read-from-string (second (str:split-omit-nulls " " line)))))

		       ((char-equal char-first #\s)
			(setf (scale metrics) (read-from-string (second (str:split-omit-nulls " " line)))))
			;; (when (< (scale metrics) top-max)
			;;   (setf top-max (scale metrics))
			;;   (format t "scale min: ~a~%" top-max))
		       
		       ((char-equal char-first #\t)
			(let ((line-split (str:split-omit-nulls " " line)))
			  (setf (vx2 (translate metrics)) (read-from-string (second line-split)))
			  (setf (vy2 (translate metrics)) (read-from-string (third line-split)))))
		       
		       ((char-equal char-first #\r)
			(setf (range metrics) (read-from-string (second (str:split-omit-nulls " " line))))))))))
    ht-metrics))


(defun calculate-uvs-metrics (metrics)
  (loop
     :for key :being :the :hash-keys :of metrics
     :using (hash-value value)
     :do (with-slots (bounds
		      advance
		      scale
		      translate
		      range
		      dim-glyph
		      uv
		      scale-uv)
	     value

	   (setf dim-glyph (vec2 96 96))
	   
	   (setf ratio-aspect (/ (vx2 dim-glyph)
				 (vy2 dim-glyph)))

	   ;; Override scale
	   ;; scale = 5.8239365
	   (setf scale 5.8239365)
	   
	   (setf scale-uv dim-glyph)

	   (setf (aref uv 0) 1.0)
	   (setf (aref uv 1) 1.0)
	   (setf (aref uv 2) 0.0)
	   (setf (aref uv 3) 0.0)
	   
	   (setf (aref uv 4) 1.0)
	   (setf (aref uv 5) 0.0)
	   (setf (aref uv 6) 0.0)
	   (setf (aref uv 7) 0.0)

	   (setf (aref uv 8) 0.0)
	   (setf (aref uv 9) 0.0)
	   (setf (aref uv 10) 0.0)
	   (setf (aref uv 11) 0.0)
	   
	   (setf (aref uv 12) 0.0)
	   (setf (aref uv 13) 1.0)
	   (setf (aref uv 14) 0.0)
	   (setf (aref uv 15) 0.0))))
