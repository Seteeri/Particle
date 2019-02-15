(in-package :protoform.model)

;; postfix with -glyphs
(defparameter *shm-texture-glyphs* nil)

;; (defparameter *glyphs-msdf-shm* (list :texture-buffer
;; 				      "glyphs-msdf"
;; 				      "/protoform-glyphs-msdf"
;; 				      16465920 ; size of all ppm glyphs
;; 				      -1 -1
;; 				      :triple
;; 				      0
;; 				      :rgba8))

(defparameter *params-texture-glyphs-shm* (list :texture-buffer
						"/protoform-texture-glyphs"
						"/protoform-texture-glyphs"
						(/ 134217728 4)
						-1 -1
						:triple
						0
						:rgba8)) ; requires fmt type

(defun init-shm-texture-glyphs ()

  ;; Could convert lisp data straight to bytes...
  ;; Load into textures for now...

  (let ((shm (init-shm '*shm-texture-glyphs*)))
    (with-slots (ptr size)
	shm
      (loop
	 :for code :from 1 :to 255
	 :with msdf-glyphs-path := (merge-pathnames #P"glyphs-msdf/" (asdf:system-source-directory :protoform))
	 :with i := 0
	 :for lisp-path := (merge-pathnames (make-pathname :name (format nil "~a-data" (write-to-string code))
							   :type "lisp")
					    msdf-glyphs-path)
	 :do (loop 
		:for c :across (read-from-string (read-file-string lisp-path))
		:do (progn
		      (setf (mem-aref ptr :unsigned-char i) c)
		      (incf i)))))
    shm))
