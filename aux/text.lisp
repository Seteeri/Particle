(in-package :protoform.model)

(defun init-glyph-data ()

  ;; Convert ppm to bytes offline for faster loading
  ;; Or turn into lisp data and make a vector

  ;; Load into textures for now...
  
  (with-slots (ptr size)
      (gethash "texture" (handles-shm *model*))
    
    (loop
       :for code :from 32 :to 255
       :with msdf-glyphs-path := (merge-pathnames #P"glyphs-msdf/" (asdf:system-source-directory :protoform))
       :for ppm-path := (merge-pathnames (make-pathname :name (write-to-string code) :type "ppm")
					 msdf-glyphs-path)
       :with i := 0
       :do (let* ((bmp (read-ppm ppm-path)))
	     
    	     (assert (= (length bmp) (* 96 96 4)))

	     (fmt-model t "init-glyph-data" "Loading ~S~%" ppm-path)
	     
	     (loop 
		:for c :across bmp
		:do (progn
		      (setf (mem-aref ptr :unsigned-char i) c)
		      (incf i)))))))

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
