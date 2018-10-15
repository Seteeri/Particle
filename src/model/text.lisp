(in-package :protoform.model)

(defclass texture ()
  ((data :accessor data :initarg :data :initform nil)
   (size :accessor size :initarg :size :initform nil)
   (dim :accessor dim :initarg :dim :initform nil)))

(defun copy-textures-to-shm ()
  (let ((offset 0))
    (with-slots (ptr size)
	(gethash "texture" (handles-shm *model*))
      (loop
	 :for texture :across (textures *model*)
	 :do (progn
	       ;; assert
	       (c-memcpy (inc-pointer ptr offset)
			 (data texture)
			 (size texture))
	       (incf offset (size texture)))))))

(defun init-glyph-data ()

  ;; Could convert lisp data straight to bytes...

  ;; Load into textures for now...
  
  (with-slots (ptr size)
      (gethash "texture" (handles-shm *model*))
    
    (loop
       :for code :from 1 :to 255
       :with msdf-glyphs-path := (merge-pathnames #P"glyphs-msdf/" (asdf:system-source-directory :protoform))
       :for lisp-path := (merge-pathnames (make-pathname :name (format nil "~a-data" (write-to-string code))
							 :type "lisp")
					  msdf-glyphs-path)
       :with i := 0
       :do (let* ((bmp (read-from-string (read-file-string lisp-path))))
	     
    	     (assert (= (length bmp) (* 96 96 4)))

	     (fmt-model t "init-glyph-data" "Loaded ~S~%" lisp-path)
	     
	     (loop 
		:for c :across bmp
		:do (progn
		      (setf (mem-aref ptr :unsigned-char i) c)
		      (incf i)))))))
