;; Split metrics into individual files
;; bounds
;; advance
;; scale
;; translate
;; range
;; dim-glyph
;; ratio-aspect
;; scale-uv
;; uv

(defun write-plist (plist)
  ;; Plist is assumed backwards so must reverse
  (let* ((path-glyphs-msdf (pathname "/home/user/quicklisp/local-projects/protoform/glyphs-msdf/"))
	 (path (merge-pathnames (make-pathname
				 :name (format nil "~a-metrics" (nth (- (length plist) 2) plist))
				 :type "lisp")
				path-glyphs-msdf)))
    (with-open-file (stream path
			    ;; :external-format charset:iso-8859-1
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)

      (format stream "(list")
      
      (destructuring-bind (&key code advance translate range (bounds nil)) (reverse plist)
	(format stream " :code ~S~%" code)
	(format stream "       :advance ~S~%" advance)
	(format stream "       :translate ~S~%" translate)
	(format stream "       :range ~S~%" range)
	(format stream "       :bounds ~S" bounds))
      
      (format stream ")"))
    path))

(defun parse-metrics ()
  (let* ((path-glyphs-msdf (pathname "/home/user/quicklisp/local-projects/protoform/glyphs-msdf/"))
	 (path-metrics (merge-pathnames (make-pathname :name "metrics")
					path-glyphs-msdf))
	 (ht-metrics (make-hash-table :size 255))
	 (metrics nil))

    ;; (list :code 1
    ;;       :advance 9.375
    ;;       :translate #(7.74184810394 7.74184810394)
    ;;       :range 0.343410337664)
    
    ;; 1
    ;; advance 9.375
    ;; translate 7.74184810394 7.74184810394
    ;; range 0.343410337664   
    
    ;; range/scale is same
    ;; Build a list for each number, then when new number encountered, write list to file and start new list
    (with-open-file (stream path-metrics)
      (loop
	 :named outer
         :for line := (read-line stream nil nil)
	 :until (eq line nil)
	 :with plist := ()
	 :finally (when plist
		    (format t "Wrote file ~a~%" (write-plist plist)))
	 :do (when (> (length line) 0)
	       (let ((char-first (char line 0)))
		 ;; If char is digit then start new file
		 (cond ((digit-char-p char-first)
			(when plist
			  (format t "Wrote file ~a~%" (write-plist plist))
			  (setf plist ()))
			(push :code plist)
			(push (parse-integer line) plist))
					   
		       ((char-equal char-first #\b)
			(let ((line-split (str:split-omit-nulls " " line)))
			  (push :bounds plist)
			  (push (make-array 4 :initial-contents (list (coerce (read-from-string (second line-split)) 'single-float)
								      (coerce (read-from-string (third line-split)) 'single-float)
								      (coerce (read-from-string (fourth line-split)) 'single-float)
								      (coerce (read-from-string (fifth line-split)) 'single-float)))
				plist)))

		       ((char-equal char-first #\a)
			(push :advance plist)
			(push (read-from-string (second (str:split-omit-nulls " " line))) plist))

		       ((char-equal char-first #\s)
			(push :scale plist)
			(push (read-from-string (second (str:split-omit-nulls " " line))) plist))
		       
		       ((char-equal char-first #\t)
			(let ((line-split (str:split-omit-nulls " " line)))
			  (push :translate plist)
			  (push (make-array 2 :initial-contents (list (read-from-string (second line-split))
								      (read-from-string (third line-split))))
				plist)))
		       
		       ((char-equal char-first #\r)
			(push :range plist)
			(push (read-from-string (second (str:split-omit-nulls " " line))) plist)))))))))


(defun calculate-metrics (metrics)
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
