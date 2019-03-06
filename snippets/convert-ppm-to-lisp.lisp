(defun convert-ppm-to-lisp ()
  (loop
     :for code :from 1 :to 255
     :with msdf-glyphs-path := (pathname "/home/user/quicklisp/local-projects/protoform/glyphs-msdf/")
     :for ppm-path := (merge-pathnames (make-pathname :name (write-to-string code)
						      :type "ppm")
				       msdf-glyphs-path)
     :for lisp-path := (merge-pathnames (make-pathname :name (format nil "~a-data" (write-to-string code))
						       :type "lisp")
					msdf-glyphs-path)
     :for bmp := (read-ppm ppm-path)
     :do (progn
	   
    	   (assert (= (length bmp) (* 96 96 4)))

	   (format t "Writing ~S~%" lisp-path)

	   (with-open-file (stream lisp-path
				   ;; :external-format charset:iso-8859-1
				   :direction :output
				   :if-exists :overwrite
				   :if-does-not-exist :create)
	     (format stream "~S" bmp)))))

(defun read-ppm (filepath)
  (let ((bitmap (make-array (* 96 96 4)
			    :fill-pointer 0
			    :element-type '(unsigned-byte 8))))
    (with-open-file (stream filepath)

      ;; Skip first 3 lines
      (dotimes (i 3)
	(read-line stream nil))
      
      ;; OpenGL textures are row-major order = row/column,  e.g.3x2:
      ;; -> -> -> \n
      ;; -> -> ->
      ;; Line 0: (0,0)
      ;; Line 1: (1,0)
      ;; Line 2: (2,0)
      ;; Line 3: (0,1)
      ;; Line 4: (1,1)
      ;; Line 5: (2,1)
      ;; Note, however, OpenGL matrices are column-major order,
      ;; somewhat by historical accident
      
      ;; Each line has 12 space delimited values
      ;; 12 / 3 components per pixel = 4 pixels every line

      (loop 
         :for pixels := (read-line stream nil)
	 :while pixels
	 :do (loop
		:for c :in (str:split-omit-nulls " " pixels)
		:with i := 0
		:do (progn
		      (vector-push (parse-integer c) bitmap)
		      (incf i)
		      (when (= i 3)
			(vector-push 255 bitmap)
			(setf i 0))))))
    bitmap))
