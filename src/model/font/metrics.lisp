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
   (width
    :accessor width
    :documentation "")
   (height
    :accessor height
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
     :do (progn	   
	   (with-slots (bounds
			advance
			scale
			translate
			range
			width
			height
			uv
			scale-uv)
	       value
	     
	     ;; bounds = l b r t

	     (setf width (abs (- (aref bounds 2) (aref bounds 0))))
	     (setf height (abs (- (aref bounds 3) (aref bounds 1))))

	     ;; Expand width back by range
	     (incf width (* 2 range))
	     ;; Expand height by range
	     (incf height (* 2 range))

	     (setf ratio-aspect (/ width height))

	     ;; calc new scale
	     ;; apply when setting model scale
	     ;; should not take into account range?
	     (setf scale-uv (vec2 (/ width (/ 96 5.8239365))
				  (/ height (/ 96 5.8239365))))
	     
	     (let ((origin (vec2 (+ (vx2 translate) (aref bounds 0))
				 (+ (vy2 translate) (aref bounds 1)))))
	       ;; Shift x back by range
	       (decf (vx2 origin) range)
	       ;; Shift y down by range
	       (decf (vy2 origin) range)
	       
	       ;; uv right bottom = t.x + w, t.y
	       ;; uv right top    = t.x + w, t.y + h
	       ;; uv left top     = t.x,     t.y + h
	       ;; uv left bottom  = t.x,     t.y
	       
	       (let* ((right-bottom (vec2 (+ (vx2 origin) width)
					  (+ (vy2 origin) 0)))
		      (right-top (vec2 (+ (vx2 origin) width)
				       (+ (vy2 origin) height)))
		      (left-top (vec2 (+ (vx2 origin) 0)
				      (+ (vy2 origin) height)))
		      (left-bottom (vec2 (+ (vx2 origin) 0)
					 (+ (vy2 origin) 0))))
		 
		 ;; convert shape coords to uv coords (0-1)
		 (nv/ right-bottom (/ 96 5.8239365))
		 (nv/ right-top    (/ 96 5.8239365))
		 (nv/ left-top     (/ 96 5.8239365))
		 (nv/ left-bottom  (/ 96 5.8239365))

		 ;; Flatten
		 (setf (aref uv 0) (vx2 right-bottom))
		 (setf (aref uv 1) (vy2 right-bottom))
		 (setf (aref uv 2) 0.0)
		 (setf (aref uv 3) 0.0)
		 
		 (setf (aref uv 4) (vx2 right-top))
		 (setf (aref uv 5) (vy2 right-top))
		 (setf (aref uv 6) 0.0)
		 (setf (aref uv 7) 0.0)
		 
		 (setf (aref uv 8) (vx2 left-top))
		 (setf (aref uv 9) (vy2 left-top))
		 (setf (aref uv 10) 0.0)
		 (setf (aref uv 11) 0.0)
		 
		 (setf (aref uv 12) (vx2 left-bottom))
		 (setf (aref uv 13) (vy2 left-bottom))
		 (setf (aref uv 14) 0.0)
		 (setf (aref uv 15) 0.0)
		 ))))))
