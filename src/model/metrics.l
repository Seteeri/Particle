(class +Metrics)

(dm T (Advance
       Translate
       Range
       Bounds-origin
       Bounds-texture
       Scale
       Dims-glyph
       Ratio-aspect
       Scale-uv
       UV)
    (=: advance)
    (=: translate)
    (=: range)
    (=: bounds-origin)
    (=: bounds-texture)
    (=: scale)
    (=: dims-glyph)
    (=: ratio-aspect)
    (=: scale-uv)
    (=: uv))

#Scale and advance the same (adv bc monospaced)
#Merge metrics and data(ppm) into single file
#Calculate other params and save to file so need not do everytime

(de load-metrics ()
    (for Code 255

	 #Format path name, Read file/code, Eval data -> Data
	 (let (dir-glyphs "/home/user/quicklisp/local-projects/protoform/src/glyphs/metrics/"
	       path-glyph (pack dir-glyphs Code "-metrics.l"))

	   (println "Reading " path-glyph " ...")
	   (in path-glyph (till NIL T)))
	   #(eval (read))

	 #New metrics
	 (with (setq *metrics (new '(+Metrics)))
	       NIL)
	 
	 NIL))

#{    
(de old ()    
    (loop
     :for code :from 1 :to 255
     :with ht-metrics := (make-hash-table :size 255)
     :with i := 0
     :for path-metrics := (merge-pathnames (make-pathname :name (format nil "~a-metrics" (write-to-string code))
						       :type "lisp")
					path)
     :do (destructuring-bind (&key code advance translate range (bounds nil))
	     (eval (read-from-string (read-file-string path-metrics)))
	   
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

(de calculate-dims (bounds)
  ;; Assumes bounds is in pixels
  (vec2 (- (nth 2 bounds) (nth 0 bounds))
	(- (nth 3 bounds) (nth 1 bounds))))

(de calculate-ar (dims)
  (/ (vx2 dims) (vy2 dims)))
  
(de calculate-uv (bounds dim)
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
}#
