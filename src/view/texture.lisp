(in-package :protoform.view)

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
	 :do (loop :for c :in (str:split-omit-nulls " " pixels)
		:with i = 0
		:do (progn
		      (vector-push (parse-integer c) bitmap)
		      (incf i)
		      (when (= i 3)
			(vector-push 255 bitmap)
			(setf i 0))))))
    bitmap))

(defun init-buffer-texture (program
			    target ; :texture-2d-array
			    name ; "texture-buffer"
			    type ; rgba8 :char
			    size ; * 4
			    count ; * w h
			    binding-layout
			    mapped-persistent
			    &key
			      (buffering 'triple)    ; 'single 'double 'triple, make this required
			      (usage :static-draw) ; dynamic-draw :stream-draw
			      (data nil))
  
  (let* ((buffer (init-buffer-object program
				     target
				     name
				     type
				     size
				     count
				     binding-layout
				     mapped-persistent
				     :buffering buffering
				     :usage usage
				     :data data))	
	 (texture (aref (buffers buffer) 0)))

    ;; https://www.opengl.org/discussion_boards/showthread.php/173917-samplerBuffer-example-needed
    
    ;; Should be bound already from init-buffer-object
    (gl:bind-texture :texture-buffer texture)
    
    (%gl:tex-buffer :texture-buffer :rgba8 texture)

    (loop
       :for code :from 32 :to 255
       :with msdf-glyphs-path := (merge-pathnames #P"glyphs-msdf/" (asdf:system-source-directory :protoform))
       :for ppm-path := (merge-pathnames (make-pathname :name (write-to-string code) :type "ppm")
					 msdf-glyphs-path)
       :with i := 0
       :with ptr := (aref (ptrs-buffer buffer) 0)
       :do (let* ((bmp (read-ppm ppm-path)))
	     
    	     (assert (= (length bmp) (* 96 96 4)))

	     (loop 
	        :for c :across bmp
		:do (progn
		      (setf (mem-aref ptr :unsigned-char i) c)
		      (incf i)))))

    buffer))
